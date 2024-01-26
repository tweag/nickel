#!/usr/bin/env bash

# Nickel release script
#
# This script automates part of the process of releasing a new version of
# Nickel.
#
# For requirements, see RELEASING.md.
#
# [^tomlq-sed]: tomlq has an --in-place option that would make the update much
# more pleasant. Unfortunately, tomlq works by transcoding to
# JSON, passing the JSON to jq, and then transcoding back to
# TOML, which has the very unpleasant effect of removing all
# comments and changing entirely the formatting and the layout
# of the file. Thus, we resort to good old and ugly sed.
# This is of course less robust. For now, it seems largely sufficient, but it
# might break in the future if the Cargo.toml files style change.

# In some cases it can be useful to leave the workspace in the state it was to
# finish the release manually. In that case, set this variable to false.
DO_CLEANUP=true
# Where to branch off the release. It's `master` most of the time, but for patch
# releases it's common to not release current master but just cherry pick a few
# commits on top of the previous release.
#
# Ideally that would be specified as an argument. For the time being, having a
# variable makes it at least a bit more flexible than harcoding master.
RELEASE_BASE_BRANCH="master"

# Perform clean up actions upon unexpected exit.
cleanup() {
  set +e

  if [[ $DO_CLEANUP == true ]]; then
    echo "++ Unexpected exit. Cleaning up..."

    for ((i=${#cleanup_actions[@]}-1; i>=0; i--)); do
      echo "++ Running cleanup action: ${cleanup_actions[$i]}"
      ${cleanup_actions[$i]} || true
    done
  else
    echo "++ Unexpected exit. Leaving the workspace in its current state (DO_CLEANUP=$DO_CLEANUP)."
  fi

  cleanup_actions=()
  exit 1
}

# Take a subdirectory containing a crate of the current workspace as the first
# argument and a variable name as the second argument. Read the crate version
# from Cargo.toml and populate the variable with an array of the three version
# numbers (major, minor, patch).
read_crate_version() {
  local -n version_array=$2 # use nameref for indirection: this will populate the variable named by the first argument
  local version
  version=$(tomlq -r .package.version "$1/Cargo.toml")
  # Shellcheck isn't able to understand that we're populating the caller's
  # provided variable `version_array` via namerefs and claims it's unused.
  # shellcheck disable=SC2034
  readarray -td'.' version_array <<< "$version"
}

# Take a string message as an argument and ask the user to confirm that the
# script should proceed with the next actions.
# If the user doesn't confirm, exit the script.
confirm_proceed() {
  read -p "$1. Proceed (y/n)?" -n 1 -r
  echo ""

  if [[ $REPLY =~ ^[Nn]$ ]]; then
    echo "++ Aborting..."
    cleanup
  fi
}

# Take a bash array representing a version (major, minor, patch) or (minor,
# patch) as an argument and print the corresponding version string
# ("major.minor.patch") This function uses nameref, so the argument must be a
# variable name and not a value.
#
# For example:
# ```
#   local version=(1 2 3)
#   print_version_array version
# ```
print_version_array() {
    local -n version=$1
    local result

    result="${version[0]}"

    for component in "${version[@]:1}"; do
        result="$result.$component"
    done

    echo "$result"
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
update_dependencies() {
    local path_cargo_toml="$2"
    local -n local_version_map=$3

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
        echo "[Internal error] Invalid argument for update_dependencies(): expected 'crate' or 'workspace', got '$1'" >&2
        exit 1
    fi

    local -a dependencies
    readarray -t dependencies < <(tomlq -r '('$dependencies_path' | keys[])' "$path_cargo_toml")

    for dependency in "${dependencies[@]}"; do
        # If the dependency is in the version map, it means that we might have
        # bumped it as part of the release process. In that case, we need to
        # update the dependency to the new version.
        if [[ -v local_version_map["$dependency"] ]]; then
            # Cargo dependencies can be either specified as a simple version
            # string, as in
            # `foo_crate = "1.2.3"`
            # or as an object with a `version` field, as in
            # `foo_crate = { version = "1.2.3", features = ["bar"] }`
            # The updated thus depend on the type of the dependency field, which
            # can be determined by tomlq's `type` function
            local dependency_type
            local has_version

            dependency_type=$(tomlq -r '('$dependencies_path'."'"$dependency"'" | type)' "$path_cargo_toml")
            has_version=$(tomlq -r '('$dependencies_path'."'"$dependency"'" | has("version"))' "$path_cargo_toml")

            cleanup_actions+=('git restore '"$path_cargo_toml")

            if [[ $dependency_type == "string" ]]; then
                report_progress "Patching cross-dependency $dependency in $path_cargo_toml to version ${local_version_map[$dependency]}"
                # see [^tomlq-sed]
                sed -i 's/\('"$dependency"'\s*=\s*"\)[0-9]\+\.[0-9]\+\(\.[0-9]\+\)\?"/\1'"${local_version_map[$dependency]}"'"/g' "$path_cargo_toml"
            # Most of local crates use the workspace's version by default, i.e.
            # are of the form `foo_crate = { workspace = true }`. In that case,
            # the update is already taken care of when updating the workspace's
            # Cargo.toml, so we don't do anything if the dependency doesn't have
            # a version field
            elif [[ $dependency_type == "object" && $has_version == "true" ]]; then
                report_progress "Patching cross-dependency $dependency in $path_cargo_toml to version ${local_version_map[$dependency]}"
                # see [^tomlq-sed]
                # the regexp below recognizes dependencies of the form
                # `foo_crate = { version = "1.2.3", features = ["bar"], ..etc }`
                # Note that version must come first, which is the case currently
                # throughout the codebase
                sed -i 's/\('"$dependency"'\s*=\s*{\s*version\s*=\s*"\)[0-9]\+\.[0-9]\+\(\.[0-9]\+\)\?"/\1'"${local_version_map[$dependency]}"'"/g' "$path_cargo_toml"
            fi

            git add "$path_cargo_toml"
            cleanup_actions+=('git reset -- '"$path_cargo_toml")
        fi
    done
}

# Currently, topiary isn't published on crates.io, so we can't publish crates
# that depend on it.
#
# The version released on the stable branch, the release branch and in the
# GitHub release should support the format feature, but the version published on
# crates.io can't, for the time being.
#
# For cargo to accept to publish our crates, we have to remove the "format"
# feature from the list of features and from the list of default features, as
# well as removing all the dependencies enabled by this feature (even if they
# aren't used anymore, their mere presence in `Cargo.toml` without them being
# available on crates.io is forbidden by cargo).
remove_format_feature() {
    local path_cargo_toml
    local tmpfile

    path_cargo_toml="$1"
    tmpfile="$path_cargo_toml.tmp"
    local -a deps_to_remove

    cleanup_actions+=('git restore '"$path_cargo_toml")

    # We first extract the list of dependencies that are required by the format
    # feature, because we'll need to remove them all (it'll include topiary, but
    # might not be limited to it)
    #
    # We filter out dependencies (as `format = [..]` can also contain features,
    # such as `nickel-lang-core/format`), and then remove the `dep:` prefix.
    readarray -t deps_to_remove < <(tomlq -r '(.features.format | .[] | select(startswith("dep:")) | sub("dep:";""))' "$path_cargo_toml")

    # see [^tomlq-sed]
    # Removing the format feature is a bit more complicated than handling
    # version numbers, because the format feature is a list of strings, so we
    # resort to a stronger weapon: awk
    #
    # The following script looks for the `[features]` section, then for the
    # `default` key and remove "format" from the list of default features. It
    # also removes the feature format key (and its value) itself from the
    # [features] section.
    awk -F'[\n= ]+' '
    {
        if($0 ~ /^\[features\]$/) {
            a=1
        }
        else if(a==1 && $1=="format") {
            next
        }
        else if(a==1 && $0 ~ /^default ?= ?\[/) {
            gsub(/, *"format"|"format" *,?/,"")
        }
        else if(a==1 && $0 ~ /^$/) {
            a=0
        }
    }
    1' "$path_cargo_toml" > "$tmpfile" && mv "$tmpfile" "$path_cargo_toml"

    # We remove all dependencies required by the format feature that we just
    # removed
    for dep in "${deps_to_remove[@]}"; do
        # see [^tomlq-sed]
        report_progress "Removing dependency $dep from $path_cargo_toml"
        sed -i '/^'"$dep"'\s*=\s*{.*}$/d' "$path_cargo_toml"
    done

    git add "$path_cargo_toml"
    cleanup_actions+=('git reset -- '"$path_cargo_toml")
}

print_usage_and_exit() {
    echo "Usage: $0 <major|minor|patch>" >&2
    exit 1
}

# Report progress to the user with adapted indentation
report_progress() {
    echo "  -- $1"
}

trap cleanup ERR
set -eEuo pipefail

arg="${1:-}"

if [[ "$arg" == "" ]]; then
    echo "Missing argument" >&2
    print_usage_and_exit
elif [[ "$arg" != "major" && "$arg" != "minor" && "$arg" != "patch" ]]; then
    echo "Invalid argument: $arg" >&2
    print_usage_and_exit
fi

# A stack of actions to perform upon unexpected error. Cleanup actions are
# indeed popped from the top of cleanup_actions (that is, in reverse order, when
# seen as an array)
cleanup_actions=()

cat <<EOF
++ Nickel release script
++
++ This script will:
++
++ - Bump version numbers in Cargo.toml files
++ - Bump local dependencies to local crates accordingly
++ - Commit and push the changes on a new release branch
++ - Make the remote 'stable' branch point to the release branch
++ - Preprocess and publish relevant local crate to crates.io
++
++ Sanity checks (build, test, publish --dry-run etc.) are performed along the
++ way. In case of failure, this release script will its best to restore things
++ to the previous state as much as possible
EOF

confirm_proceed "++"
echo ""

# Moving to the root of the git project
cd "$(git rev-parse --show-toplevel)"

# Check that the working directory is clean
if [[ -n $(git status --untracked-files=no --porcelain) ]]; then
    confirm_proceed "++ [WARNING] Working directory is not clean. The cleanup code of this script might revert some of your uncommited changes"
fi

git switch "$RELEASE_BASE_BRANCH" > /dev/null

echo "++ Prepare release branch from '$RELEASE_BASE_BRANCH'"

# Directories of subcrates following their own independent versioning
independent_crates=(core utils lsp/lsp-harness ./wasm-repl)
# All subcrate directories, including the ones above
all_crates=("${independent_crates[@]}" cli lsp/nls pyckel)

workspace_version=$(tomlq -r .workspace.package.version ./Cargo.toml)
workspace_version_array=()
readarray -td'.' workspace_version_array <<< "$workspace_version"

# We checked at the beginning of the script that $1 was either "major", "minor"
# or "patch", so we don't need to handle the cath-all case.
if [[ $1 == "major" ]]; then
    new_workspace_version=$((workspace_version_array[0] + 1)).0.0
elif [[ $1 == "minor" ]]; then
    new_workspace_version=${workspace_version_array[0]}.$((workspace_version_array[1] + 1)).0
elif [[ $1 == "patch" ]]; then
    new_workspace_version=${workspace_version_array[0]}.${workspace_version_array[1]}.$((workspace_version_array[2] + 1))
fi

confirm_proceed "  -- Updating to version $new_workspace_version"

report_progress "Creating release branch..."

release_branch="$new_workspace_version-release"

if git rev-parse --verify --quiet "$release_branch" > /dev/null; then
    confirm_proceed "  -- [WARNING] The branch '$release_branch' already exists. The script will skip forward to publication to crates.io (but still run checks)."
    git switch "$release_branch"

    report_progress "Building and running checks..."

    nix flake check
else
    git switch --create "$release_branch" > /dev/null
    cleanup_actions+=("git branch -d $release_branch")
    cleanup_actions+=('git switch '"$RELEASE_BASE_BRANCH")

    report_progress "Bumping workspace version number..."

    # see [^tomlq-sed]
    sed -i 's/^\(version\s*=\s*"\)[0-9]\+\.[0-9]\+\(\.[0-9]\+\)\?"$/\1'"$new_workspace_version"'"/g' ./Cargo.toml
    cleanup_actions+=("git reset -- ./Cargo.toml")

    git add ./Cargo.toml
    cleanup_actions+=("git restore ./Cargo.toml")

    report_progress "Bumping other crates version numbers..."

    for crate in "${independent_crates[@]}"; do
        crate_version_array=()
        read_crate_version "$crate" crate_version_array

        new_crate_version=${crate_version_array[0]}.$((crate_version_array[1] + 1)).0

        read -p "  -- $crate is currently in version $(print_version_array crate_version_array). Bump to the next version $new_crate_version [if no, you'll have a pause later to manually bump those versions if needed] (y/n) ?" -n 1 -r
        echo ""

        if [[ $REPLY =~ ^[Yy]$ ]]; then
          # see [^tomlq-sed]
          sed -i 's/^\(version\s*=\s*"\)[0-9]\+\.[0-9]\+\(\.[0-9]\+\)\?"$/\1'"$new_crate_version"'"/g' "$crate/Cargo.toml"
          cleanup_actions+=('git restore '"$crate/Cargo.toml")

          git add "$crate/Cargo.toml"
          cleanup_actions+=('git reset -- '"$crate/Cargo.toml")
        fi
    done

    read -n 1 -s -r -p "  -- Please manually update any crate version not automatically handled by this script so far if you need to, and then press any key to continue"
    echo ""

    # Because the user might have updated the version numbers manually, we need to
    # parse them again before updating cross-dependencies

    declare -A version_map

    for crate in "${all_crates[@]}"; do
        crate_version_array=()
        read_crate_version "$crate" crate_version_array
        crate_name=$(tomlq -r .package.name "$crate/Cargo.toml")
        # Shellcheck isn't able to understand that we're passing `version_map`
        # to `update_dependencies` as a nameref and claims it's unused.
        # shellcheck disable=SC2034
        version_map[$crate_name]=$(print_version_array crate_version_array)
    done

    report_progress "Updating cross-dependencies..."

    for crate in "${all_crates[@]}"; do
        update_dependencies "crate" "$crate/Cargo.toml" version_map
    done

    # Patch workspace dependencies
    update_dependencies "workspace" "./Cargo.toml" version_map
    # We need to update the lockfile here, at least for the dependencies that we
    # might have bumped. We changed ./Cargo.toml but Nix tries to build with
    # --frozen, which will fail if the lockfile is outdated.
    cargo update "${!version_map[@]}" > /dev/null
    cleanup_actions+=("git restore ./Cargo.lock")

    git add ./Cargo.lock
    cleanup_actions+=("git reset -- ./Cargo.lock")

    report_progress "Building and running checks..."

    nix flake check

    report_progress "Checks run successfully."
    confirm_proceed "  -- Please add the release notes to RELEASES.md if not already done. Save but don't commit. Then press 'y'."

    report_progress "Pusing the release branch..."

    git commit -m "[release.sh] update to $new_workspace_version"
    git push -u origin "$release_branch"

    report_progress "Saving current 'stable' branch to 'stable-local-save'..."

    # Delete the branch if already present, but if not, don't fail
    git branch -D stable-local-save &>/dev/null || true
    git checkout stable
    git branch stable-local-save

    report_progress "If anything goes wrong from now on, you can restore the previous stable branch by resetting stable to stable-local-save"

    confirm_proceed "  -- Pushing the release branch to 'stable' and making it the new default"

    git checkout stable
    git reset --hard "$release_branch"
    git push --force-with-lease

    git checkout "$release_branch"

    echo "++ Release branch successfully pushed!"
fi

# Reset cleanup actions as creating and pushing the release branch was successful
cleanup_actions=()

cat <<EOF
++ Release branch successfully pushed!
++ CAUTION: '$release_branch' won't be cleaned up automatically from there, even
++          if the publication fails. If you call this script again with an
++          existing release branch, you'll be asked if you want to resume from
++          here.
++
++ Now preparing the release to crates.io
EOF

crates_to_publish=(core cli lsp/nls)

report_progress "Removing 'nickel-lang-utils' and 'lsp-test-harness' from dev-dependencies..."

for crate in "${crates_to_publish[@]}"; do
    # Remove `nickel-lang-utils` from `dev-dependencies` of released crates.
    # Indeed, `nickel-lang-utils` is only used for testing or benchmarking and
    # it creates a circular dependency. We just don't publish it and cut it off
    # from dev-dependencies (which aren't required for proper publication on
    # crates.io)
    #
    # see [^tomlq-sed]
    sed -i '/^nickel-lang-utils\.workspace\s*=\s*true\s*$/d' "$crate/Cargo.toml"
    sed -i '/^lsp-harness\.workspace\s*=\s*true\s*$/d' "$crate/Cargo.toml"
    cleanup_actions+=('git restore '"$crate/Cargo.toml")
done

report_progress "Removing 'lsp-harness' from dev-dependencies..."

for crate in "${crates_to_publish[@]}"; do
    # Remove `lsp-harness` from `dev-dependencies` of released crates.
    # `lsp-harness` is only used for testing the LSP and isn't published on 
    # crates.io, so we cut it off as well
    #
    # see [^tomlq-sed]
    sed -i '/^lsp-harness\.workspace\s*=\s*true$/d' "$crate/Cargo.toml"
    cleanup_actions+=('git restore '"$crate/Cargo.toml")
done

report_progress "Remove the format feature and topiary dependencies..."

for crate in "${crates_to_publish[@]}"; do
    remove_format_feature "$crate/Cargo.toml"

    git add "$crate/Cargo.toml"
    cleanup_actions+=('git reset -- '"$crate/Cargo.toml")
done

# Cargo requires to commit changes, but we'll reset them later
git commit -m "[release.sh][tmp] remove nickel-lang-utils and topiary from deps"
cleanup_actions+=("git reset --hard HEAD~")

# We have had reproducibility issues before due to the fact that when installing
# the version of say `nickel-lang-cli` from crates.io, Cargo doesn't pick the
# current workspace file `Cargo.lock`, but regenerates a fresh one. This can
# lead to a local installation of `nickel-lang-cli` succeeding, but after
# publication, the version on crates.io wouldn't install.
#
# This is a bit unsatisfactory, but a cheap way to have good faith that the
# published version correctly builds and install is to temporarily delete
# `Cargo.lock` and try to install the nickel crates locally
report_progress "Trying to install 'nickel-lang-cli' and 'nickel-lang-lsp' locally..."
report_progress "[WARNING] This will override your local Nickel installation"

rm -f ./Cargo.lock
cleanup_actions+=("git restore ./Cargo.lock")

cargo install --force --path ./cli
cargo install --force --path ./lsp/nls

git restore ./Cargo.lock

report_progress "Successfully installed locally. Trying a dry run of cargo publish 'nickel-lang-core'"

cargo publish -p nickel-lang-core --dry-run
confirm_proceed "  -- Dry run successful. Proceed with actual publication of 'nickel-lang-core' to crates.io ?"
cargo publish -p nickel-lang-core

report_progress "'nickel-lang-core' published successfully"
report_progress "Trying a dry run of cargo publish 'nickel-lang-cli'"

cargo publish -p nickel-lang-cli --dry-run
confirm_proceed "  -- Dry run successful. Proceed with actual publication of 'nickel-lang-cli' to crates.io ?"
cargo publish -p nickel-lang-cli

report_progress "'nickel-lang-cli' published successfully"
report_progress "Trying a dry run of cargo publish 'nickel-lang-lsp'"

cargo publish -p nickel-lang-lsp --dry-run
confirm_proceed "  -- Dry run successful. Proceed with actual publication of 'nickel-lang-lsp' to crates.io ?"
cargo publish -p nickel-lang-lsp

report_progress "'nickel-lang-lsp' published successfully"
report_progress "Cleaning up..."

# Undo the previous commit massaging dependencies, and restore Cargo.lock.
git reset --hard HEAD~

cleanup_actions=()

cat <<EOF
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
EOF

exit 0
