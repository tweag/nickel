#!/usr/bin/env bash

# Perform clean up actions upon unexpected exit.
fn cleanup() {
  echo "++ Unexpected exit. Cleaning up..."
  set +e

  for ((i=${#cleanup_actions[@]}-1; i>=0; i--)); do
    ${cleanup_actions[$i]} || true
  done

  cleanup_actions=()
  exit 1
}

# Take a subdirectory containing a crate of the current workspace as the first
# argument and a variable name as the second argument. Read the crate version
# from Cargo.toml and populate the variable with an array of the three version
# numbers (major, minor, patch).
fn read_crate_version() {
  local -n version_array=$2 # use nameref for indirection: this will populate the variable named by the first argument
  local version=$(tomlq -r .package.version "$1/Cargo.toml")
  readarray -td'.' version_array <<< "$version"
}

# Take a string message as an argument and ask the user to confirm that the
# script should proceed with the next actions.
# If the user doesn't confirm, exit the script.
fn confirm_proceed() {
  read -p "$1. Proceed (y/n)?" -n 1 -r

  if [[ $REPLY =~ ^[Nn]$ ]]; then
    echo "++ Aborting..."
    cleanup
    exit 1
  fi
}

# Take a bash array representing a version (major, minor, patch) as an argument
# and print the corresponding version string ("major.minor.patch")
# This function uses nameref, so the argument must be a variable name and not a
# value.
#
# For example:
# ```
#   local version=(1 2 3)
#   print_version_array version
# ```
fn print_version_array() {
    local -n version=$1
    echo "${version[0]}.${version[1]}.${version[2]}"
}

# Go through a Cargo.toml file and bump all dependencies to local crates that are being updated (in practice, the crates populating version_map)
# Arguments:
# - $1: the type of Cargo.toml, either "workspace" or "crate"
# - $2: the path to the Cargo.toml file
# - $3: the version map, a bash associative array mapping updated crate names to
#   new versions. This argument is taken by nameref, so you must pass the name
#   of an existing variable containing the map, not a value. We do so because
#   passing associative arrays by value is painful in bash.
#
# For example:
# ```
#   local -A version_map
#   version_map["nickel-lang-core"]="1.2.3"
#   update_dependencies "workspace" "./Cargo.toml" version_map
# ```
fn update_dependencies() {
    local path_cargo_toml="$2"
    local -n version_map=$3

    # If we are looking at a crate's Cargo.toml file, the dependencies are
    # located at .dependencies. If we are looking at the workspace's Cargo.toml,
    # the dependencies are located at .workspace.dependencies. This difference
    # is abstracted away in `dependencies_path`
    local dependencies_path

    if [[ $1 == "workspace" ]]; then
        dependencies_path=".workspace.dependencies"
    elif [[ $1 == "crate" ]]; then
        dependencies_path=".dependencies"
    else
        echo "[Internal error] Invalid argument for update_dependencies(): expected \'crate\' or \'workspace\', got \'$1\'" >&2
        exit 1
    fi

    local dependencies=($(tomlq -r '('$dependencies_path' | keys[])' "$crate/Cargo.toml"))

    for $dependency in dependencies; do
        # If the dependency is in the version map, it means that we might have
        # bumped it as part of the release process. In that case, we need to
        # update the dependency to the new version.
        if [[ -v version_map["$dependency"] ]]; then
            # Cargo dependencies can be either specified as a simple version
            # string, as in
            # `foo_crate = "1.2.3"`
            # or as an object with a `version` field, as in
            # `foo_crate = { version = "1.2.3", features = ["bar"] }`
            # The updated thus depend on the type of the dependency field, which
            # can be determined by tomlq's `type` function
            local dependency_type=$(tomlq -r '('$dependencies_path'."'$dependency'" | type)' "$crate/Cargo.toml")
            local has_version=$(tomlq -r '('$dependencies_path'."'$dependency'" | has("version")' "$crate/Cargo.toml")

            if [[ $dependency_type == "string" ]]; then
                echo "Patching cross-dependency $dependency in $crate to version ${version_map[$dependency]}"
                tomlq -i $dependencies_path'."'$dependency'" = "'${version_map[$dependency]}'"' "$crate/Cargo.toml"
            # Most of local crates use the workspace's version by default, i.e.
            # are of the form `foo_crate = { workspace = true }`. In that case,
            # the update is already taken care of when updating the workspace's
            # Cargo.toml, so we don't do anything if the dependency doesn't have
            # a version field
            elif [[ $dependency_type == "object" && $has_version == "true" ]]; then
                echo "Patching cross-dependency $dependency in $crate to version ${version_map[$dependency]}"
                # The dependency might be set to follow the workspace's version,
                # in which case we don't touch it

                tomlq -i $dependencies_path'."'$dependency'".version = "'${version_map[$dependency]}'"' "$crate/Cargo.toml"
            fi

            git add "$crate/Cargo.toml"
            cleanup_actions+=("git restore \"$crate/Cargo.toml\"")
        fi
    done
}

fn print_usage_and_exit() {
    echo "Usage: $0 <major|minor|patch>" >&2
    exit 1
}

# Report progress to the user with adated indentation
fn report_progress() {
    echo "  -- $1"
}

trap cleanup ERR
set -eEuo pipefail

if [[ "$1" != "major" && "$1" != "minor" && "$1" != "patch" ]]; then
    echo "Invalid argument: $1" >&2
    print_usage_and_exit
fi

# A stack of actions to perform upon unexpected error. Cleanup actions are are
# indeed popped from the top of cleanup_actions (that is, in reverse order, when
# seen as an array)
cleanup_actions=()

echo <<EOF
++ Nickel release script
++
++ This script will:
++
++ - Bump version numbers in Cargo.toml files
++ - Bump local dependencies to local crates accordingly
++ - Commit and push the changes on a new release branch
++ - Make the remote 'stable' branch point to the release branch
++ - Publish relevant crate to crates.io
++
++ Sanity checks (build, test, publish --dry-run etc.) are performed along the
++ way. In case of failure, this release script will its best to restore things
to ++ the previous state as much as possible
EOF

confirm_proceed ""

git switch master

# Check that the working directory is clean
if ! git status --untracked-files=no --porcelain; then
    confirm_proceed "Working directory is not clean"
fi

echo "\n++ Prepare release branch from \'master\'"

# Directories of subcrates following their own independent versioning
local independentCrates=(core utils lsp/lsp-harness ./wasm-repl)
# All subcrate directories, including the ones above
local all_crates=("${independentCrates[@]}" cli lsp/nls pyckel)

local workspace_version=$(tomlq -r .workspace.package.version ./Cargo.toml)
local workspace_version_array
readarray -td'.' workspace_version_array <<< "$workspace_version"

local new_workspace_version

# We checked at the beginning of the script that $1 was either "major", "minor"
# or "patch", so we don't need to handle cath-all case.
if [[ $1 == "major" ]]; then
    new_workspace_version=$((workspace_version_array[0] + 1)).0.0
elif [[ $1 == "minor" ]]; then
    new_workspace_version=${workspace_version_array[0]}.$((workspace_version_array[1] + 1)).0
elif [[ $1 == "patch" ]]; then
    new_workspace_version=${workspace_version_array[0]}.${workspace_version_array[1]}.$((workspace_version_array[2] + 1))
fi

confirm_proceed "Updating to version $new_workspace_version"

report_progress "Creating release branch..."

local release_branch="$new_workspace_version-release"

git switch --create $release_branch
cleanup_actions+=("git branch -d $release_branch; git switch master")

report_progress "Bumping workspace version number..."

tomlq -i '.workspace.package.version = "'$newVersion'"' ./Cargo.toml
git add ./Cargo.toml
cleanup_actions+=("git restore ./Cargo.toml")

report_progress "Bumping other crates version numbers..."

local crate_version_array
local new_crate_version

for $crate in $independentCrates; do
    read_crate_version "$crate" crate_version_array

    new_crate_version=${crate_version_array[0]}.$((crate_version_array[1] + 1)).0

    read -p "$crate is currently in version $(print_version_array crate_version_array). Bump to the next version $new_crate_version [if no, you'll have a pause later to manually bump those versions if needed] (y/n) ?" -n 1 -r

    if [[ $REPLY =~ ^[Yy]$ ]]; then
      tomlq -i '.package.version = "'$newVersion'"' "$crate/Cargo.toml"
      git add "$crate/Cargo.toml"
      cleanup_actions+=("git restore \"$crate/Cargo.toml\"")
    fi
done

read -n 1 -s -r -p "Please manually update any crate version not automatically handled by this script so far if you need to, and then press any key to continue"

# Because the user might have updated the version numbers manually, we need to
# parse them again before updating cross-dependencies

local version_map
local crate_version
local crate_name

for $crate in $all_crates; do
    read_crate_version "$crate" crate_version_array
    crate_name=$(tomlq -r .package.name "$crate/Cargo.toml")
    version_map[$crate_name]=$(print_version_array crate_version_array)
done

report_progress "Updating cross-dependencies..."

for $crate in $all_crates; do
    update_dependencies "crate" "$crate/Cargo.toml" version_map
done

update_dependencies "workspace" "./Cargo.toml" version_map

# Patch workspace dependencies

report_progress "Building and running checks..."

nix flake check

report_progress "Pushing the release branch..."

git commit -m "Bump version to $new_workspace_version"
git push -u origin $release_branch

report_progress "Saving current \'stable\' branch to \'stable-local-save\'..."

# Delete the branch if already present, but if not, don't fail
git branch -D stable-local-save &>/dev/null || true
git checkout stable
git branch stable-local-save

git checkout stable
git reset --hard $release_branch
git push --force-with-lease

report_progress "If anything goes wrong from now on, you can restore the previous stable branch by resetting stable to stable-local-save"

# Reset cleanup actions as creating and pushing the release branch was successful
cleanup_actions=()

echo "++ Release branch successfully pushed!"
echo "++ Prepare the release to crates.io"

local crates_to_publish=(core cli lsp/nls)

report_progress "Removing \'nickel-lang-utils\' from dev-dependencies..."

for $crate in $crates_to_publish; do
    # Remove `nickel-lang-utils` from `dev-dependencies` of released crates.
    # Indeed, `nickel-lang-utils` is only used for testing or benchmarking and
    # it creates a circular dependency. We just don't publish it and cut it off
    # from dev-dependencies (which aren't required for proper publication on
    # crates.io)
    #
    # Note that tomlq doesn't fail if the key doesn't exist, so we can blindly
    # try to delete `nickel-lang-utils` even if it might not be there
    tomlq -i 'del(.dev-dependencies."nickel-lang-utils")' "$crate/Cargo.toml"
    git add "$crate/Cargo.toml"
    cleanup_actions+=("git restore \"$crate/Cargo.toml\"")
done

# Cargo requires to commit changes, but we'll reset them later
git commit -m "[release.sh] Remove nickel-lang-utils from dev-dependencies"

# We have had reproducibility issues before due to the fact that when installing
# the version of say `nickel-lang-cli` from crates.io, Cargo doesn't pick the
# current workspace file `Cargo.lock`, but regenerates a fresh one. This can
# lead to a local installation of `nickel-lang-cli` succeeding, but after
# publication, the version on crates.io wouldn't install.
#
# This is a bit unsatisfactory, but a cheap way to have good faith that the
# published version correctly builds and install is to temporarily delete
# `Cargo.lock` and try to install the nickel crates locally
report_progress "Trying to install \'nickel-lang-cli\' and \'nickel-lang-lsp\' locally... [WARNING: this will override your local Nickel installation]"

rm -f ./Cargo.lock

cleanup_actions+=("git restore ./Cargo.lock")

cargo install --force --path ./cli
cargo install --force --path ./lsp/nls

git restore ./Cargo.lock

report_progress "Successfully installed locally. Trying a dry run of cargo publish..."

cargo publish -p nickel-lang-core --dry-run
cargo publish -p nickel-lang-cli --dry-run
cargo publish -p nickel-lang-lsp --dry-run

report_progress "Dry run successfully passed"
confirm_proceed "Proceed with actual publication to crates.io ?"

cargo publish -p nickel-lang-core
cargo publish -p nickel-lang-cli
cargo publish -p nickel-lang-lsp

report_progress "Cleaning up..."

# Undo the previous commit removing `nickel-lang-utils` from dev-dependencies
git reset --hard HEAD~

cleanup_actions=()

echo <<EOF
++ SUCCESS
++
++ Successfully published to crates.io
++
++ Now, you need to:
++
++ - Do the GitHub release
++ - Redeploy the website
++
++ Please refer to RELEASING.md for more details
