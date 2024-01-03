#!/usr/bin/env bash

fn cleanup() {
  echo "++ Unexpected exit. Cleaning up..."
  set +e

  for ((i=${#cleanupActions[@]}-1; i>=0; i--)); do
    ${cleanupActions[$i]} || true
  done

  cleanupActions=()

  exit 1
}

trap cleanup ERR
set -eEuo pipefail

if [[ "$1" != "major" && "$1" != "minor" && "$1" != "patch" ]]; then
    echo "Invalid argument: $1" >&2
    echo "Usage: $0 <major|minor|patch>" >&2
    exit 1
fi

# A stack of actions to perform upon unexpected exit. Cleanup actions are are
# indeed popped from the top of cleanupActions (that is, in reverse order, when
# seen as an array)
cleanupActions=()

# Take a subdirectory containing a crate of the current workspace as the first
# argument and a variable name as the second argument. Read the crate version
# from Cargo.toml and populate the variable with an array of the three version
# numbers (major, minor, patch).
fn readCrateVersion() {
  local -n versionArray=$2 # use nameref for indirection: this will populate the variable named by the first argument
  local version=$(tomlq -r .package.version "$1/Cargo.toml")
  readarray -td'.' versionArray <<< "$version"
}

# Take a string message as an argument and ask the user to confirm the action.
# If the user doesn't confirm, exit the script.
fn confirmAction() {
  read -p "$1. Proceed (y/n)?" -n 1 -r

  if [[ $REPLY =~ ^[Nn]$ ]]; then
    echo "++ Aborting..."
    cleanup
    exit 1
  fi
}

# Take a bash array representing a version (major, minor, patch) as an argument
# and print the corresponding version string ("major.minor.patch")
fn printVersionArray() {
    local version=("$@")
    echo "${version[0]}.${version[1]}.${version[2]}"
}

# Go through a Cargo.toml file and bump all dependencies to local crates that are being updated (in practice, the crates populating versionMap)
# Arguments:
# - $1: the type of Cargo.toml, either "workspace" or "crate"
# - $2: the path to the Cargo.toml file
# - $3: the version map, a bash associative array mapping updated crate names to new versions
fn bumpDependencies() {
    local pathCargoToml="$2"
    local versionMap="$3"

    # If we are looking at a crate's Cargo.toml file, the dependencies are
    # located at .dependencies. If we are looking at the workspace's Cargo.toml,
    # the dependencies are located at .workspace.dependencies. This difference
    # is abstracted away in `depsPath`
    local depsPath

    if [[ $1 == "workspace" ]]; then
        depsPath=".workspace.dependencies"
    else
        depsPath=".dependencies"
    fi

    local dependencies=($(tomlq -r '('$depsPath' | keys[])' "$crate/Cargo.toml"))

    for $dependency in dependencies; do
        if [[ -v versionMap["$dependency"] ]]; then
            local depType=$(tomlq -r '('$depsPath'."'$dependency'" | type)' "$crate/Cargo.toml")
            local hasVersion=$(tomlq -r '('$depsPath'."'$dependency'" | has("version")' "$crate/Cargo.toml")

            if [[ $depType == "string" ]]; then
                echo "Patching cross-dependency $dependency in $crate to version ${versionMap[$dependency]}"
                tomlq -i $depsPath'."'$dependency'" = "'${versionMap[$dependency]}'"' "$crate/Cargo.toml"
            elif [[ $depType == "object" && $hasVersion == "true" ]]; then
                echo "Patching cross-dependency $dependency in $crate to version ${versionMap[$dependency]}"
                # The dependency might be set to follow the workspace's version,
                # in which case we don't touch it

                tomlq -i $depsPath'."'$dependency'".version = "'${versionMap[$dependency]}'"' "$crate/Cargo.toml"
            fi

            git add "$crate/Cargo.toml"
            cleanupActions+=("git restore \"$crate/Cargo.toml\"")
        fi
    done
}

echo <<EOF
++ Nickel release script
++
++ This script will:
++
++ - Automatically bump version numbers in Cargo.toml files
++ - Bump dependencies to local crates accordingly
++ - Commit and push the changes on a new release branch
++ - Make the stable branch point to the release branch
++ - Publish relevant crate to crates.io
++
++ Sanity checks (build, test, publish dry run etc.) are performed along the
++ way, and in case of failure, the release script will try to restore things to
++ the previous state as much as possible
EOF

confirmAction ""

# Check that the working directory is clean
if ! git status --untracked-files=no --porcelain; then
    confirmAction "Working directory is not clean"
fi

echo "\n++ Prepare release branch creation"

# Directories of subcrates following their own independent versioning
local independentCrates=(core utils lsp/lsp-harness ./wasm-repl)
# All subcrate directories, including the ones above
local allCrates=("${independentCrates[@]}" cli lsp/nls pyckel)

local workspaceVersion=$(tomlq -r .workspace.package.version ./Cargo.toml)
local workspaceVersionArray
readarray -td'.' workspaceVersionArray <<< "$workspaceVersion"

local newWorkspaceVersion

# We checked at the beginning of the script that $1 was either "major", "minor"
# or "patch", so we don't need to handle cath-all case.
if [[ $1 == "major" ]]; then
    newWorkspaceVersion=$((workspaceVersionArray[0] + 1)).0.0
elif [[ $1 == "minor" ]]; then
    newWorkspaceVersion=${workspaceVersionArray[0]}.$((workspaceVersionArray[1] + 1)).0
elif [[ $1 == "patch" ]]; then
    newWorkspaceVersion=${workspaceVersionArray[0]}.${workspaceVersionArray[1]}.$((workspaceVersionArray[2] + 1))
fi

confirmAction "Updating to version $(printVersionArray "${newWorkspaceVersion[@]}")"

echo "Creating release branch..."

local releaseBranch="$newWorkspaceVersion-release"

git switch --create $releaseBranch
cleanupActions+=("git branch -d $releaseBranch; git switch master")

echo "Bumping workspace version number..."

tomlq -i '.workspace.package.version = "'$newVersion'"' ./Cargo.toml
git add ./Cargo.toml
cleanupActions+=("git restore ./Cargo.toml")

echo "Bumping other crates version numbers..."

local crateVersionArray
local newCrateVersion

for $crate in $independentCrates; do
    readCrateVersion "$crate" crateVersionArray

    newCrateVersion=${crateVersionArray[0]}.$((crateVersionArray[1] + 1)).0

    read -p "$crate is currently in version $(printVersionArray "${crateVersionArray[@]}"). Bump to the next version $newCrateVersion [if no, you'll have a pause later to manually bump those versions if needed] (y/n) ?" -n 1 -r

    if [[ $REPLY =~ ^[Yy]$ ]]; then
      tomlq -i '.package.version = "'$newVersion'"' "$crate/Cargo.toml"
      git add "$crate/Cargo.toml"
      cleanupActions+=("git restore \"$crate/Cargo.toml\"")
    fi
done

read -n 1 -s -r -p "Please manually update any crate version not automatically handled by this script so far if you need to, and then press any key to continue"

# Because the user might have updated the version numbers manually, we need to
# parse them again before updating cross-dependencies

local versionMap
local crateVersion
local crateName

for $crate in $allCrates; do
    readCrateVersion "$crate" crateVersionArray
    crateName=$(tomlq -r .package.name "$crate/Cargo.toml")
    versionMap[$crateName]=$(printVersionArray "${crateVersionArray[@]}")
done

echo "Updating cross-dependencies..."

for $crate in $allCrates; do
    bumpDependencies "crate" "$crate/Cargo.toml" versionMap
done

bumpDependencies "workspace" "./Cargo.toml" versionMap

# Patch workspace dependencies

echo "Building and running checks..."

nix flake check

echo "Pushing the release branch..."

git commit -m "Bump version to $(printVersionArray "${newWorkspaceVersion[@]}"
git push -u origin $releaseBranch

echo "Saving current stable to stable-local-save..."

# Delete the branch if already present, but if not, don't fail
git branch -D stable-local-save &>/dev/null || true
git checkout stable
git branch stable-local-save

git checkout stable
git reset --hard $releaseBranch
git push --force-with-lease

echo "If anything goes wrong from now on, you can restore the previous stable branch by resetting stable to stable-local-save"

# Reset cleanup actions as creating and pushing the release branch was successful
cleanupActions=()

echo "++ Release branch successfully pushed!"

echo "++ Prepare the release to crates.io"

local cratesToPublish=(core cli lsp/nls)

echo "Removing \'nickel-lang-utils\' from dev-dependencies..."

for $crate in $cratesToPublish; do
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
    cleanupActions+=("git restore \"$crate/Cargo.toml\"")
done

# Cargo requires to commit changes, but we'll reset them later
git commit -m "[release.sh] Remove nickel-lang-utils from dev-dependencies"

# We have had reproducibility issues before due to the fact that when installing
# the version of say `nickel-lang-cli` from crates.io, Cargo doesn't pick the
# current workspace file `Cargo.lock`, but regerates a new one. This can lead
# to a local installation of `nickel-lang-cli` succeeding, but after
# publication, the version on crates.io wouldn't install.
#
# This is a bit unsatisfactory, but a cheap way to have good faith that the
# published version correctly builds and install is to temporarily delete
# `Cargo.lock` and try to install them locally

echo "Trying to install \'nickel-lang-cli\' and \'nickel-lang-lsp\' locally... [WARNING: this will override your local Nickel installation"

rm -f ./Cargo.lock

cleanupActions+=("git restore ./Cargo.lock")

cargo install --force --path ./cli
cargo install --force --path ./lsp/nls

git restore ./Cargo.lock

echo "Successfully installed locally. Trying a dry run of cargo publish..."

cargo publish -p nickel-lang-core --dry-run
cargo publish -p nickel-lang-cli --dry-run
cargo publish -p nickel-lang-lsp --dry-run

echo "Dry run successfully passed"

confirmAction "Proceed with actual publication to crates.io ?"

cargo publish -p nickel-lang-core
cargo publish -p nickel-lang-cli
cargo publish -p nickel-lang-lsp

echo "Cleaning up..."

# Undo the previous commit removing `nickel-lang-utils` from dev-dependencies
git reset --hard HEAD~

cleanupActions=()


echo <<EOF
++ Successfully published to crates.io
++ SUCCEEDED
++
++ Now, you need to:
++
++ - Do the GitHub release
++ - Redeploy the website
++
++ Please consult RELEASING.md for more details
