#!/usr/bin/env bash

# This script shrinks the libnickel_lang.a static library, which is otherwise
# a shocking 150MB. We achieve this by unpacking the archive and re-linking
# most of the object files into one big object, while removing unreachable code.
#
# One way to do this on Linux is with `ld -r --gc-sections`, but `--gc-sections`
# isn't supported by the MacOS linker, and the analogous `-dead_strip` doesn't
# work with `-r`. So what we actually do is extract the llvm bitcode from the
# object files that have it (which is most of them), link them into one bi
# llvm bitcode file, and then remove dead code from that.

set -euo pipefail

OUTPUT_FILE=""
INPUT_FILE=""

usage() {
  echo "Usage: shrink_archive.sh [options] <INPUT.a>"
  echo ""
  echo "Options:"
  echo "  -o, --output <file>  Output file (required)"
  echo "  -h, --help           Show this message"
}

die() {
  echo "Error: $*" >&2
  usage
  exit 1
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    -o|--output)
      OUTPUT_FILE="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    -*)
      die "unknown option $1"
      ;;
    *)
      if [[ -z "$INPUT_FILE" ]]; then
        INPUT_FILE="$1"
      else
        die "more than one input file provided"
      fi
      shift
      ;;
  esac
done

if [[ -z "$INPUT_FILE" ]]; then
  die "missing required INPUT argument"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  die "missing required OUTPUT argument"
fi

TEMP_DIR=$(mktemp -d)

# shellcheck disable=SC2064 # we want to expand $TEMP_DIR now, not later
trap "rm -rf '$TEMP_DIR'" EXIT INT TERM

llvm-ar x "$INPUT_FILE" --output="$TEMP_DIR"
pushd "$TEMP_DIR" > /dev/null

# Make a list of the symbols we want to export, which look like nickel_context_alloc
# or nickel_expr_is_record.
#
# On macOS, the symbols start with an underscore. Apparently that's part of their
# platform ABI.
llvm-nm nickel_lang.*.o | grep -E ' T _?nickel' | cut -f 3 -d ' ' > symlist.txt

echo "Extracting LLVM bitcode..."
for f in *.o; do
  if llvm-objcopy --dump-section=.llvmbc="$f.bc" "$f" 2>/dev/null; then
    rm "$f"
  elif llvm-objcopy --dump-section=__LLVM,__bitcode="$f.bc" "$f" 2>/dev/null; then
    rm "$f"
  fi
done

# We've deleted all the .o files that had a bitcode section, but there are usually
# some .o files left (I think some of them come from malachite's hand-written assembly,
# for example). We need to be sure not to garbage-collect any symbols needed by those
# remaining .o files.
llvm-nm ./*.o | grep ' U ' | awk '{print $2}' >> symlist.txt

# Although macOS symbols start with an underscore, the `opt` command expects its
# symbol list with the underscores stripped.
if uname -a | grep Darwin; then
  cut -c 2- symlist.txt > _symlist.txt
  mv _symlist.txt symlist.txt
fi

echo "Linking LLVM bitcode..."
llvm-link -o combined.bc ./*.bc

echo "Removing dead LLVM bitcode..."
opt --internalize-public-api-file=symlist.txt --passes='internalize,globaldce' combined.bc -o small.bc

echo "Re-compiling to object file..."
llc --filetype=obj --relocation-model=pic small.bc -o nickel_lang.o

# llvm-strip doesn't support --strip-unneeded for Mach-O
if uname -a | grep -v Darwin; then
  echo "Stripping unneeded symbols..."
  llvm-strip --strip-unneeded nickel_lang.o
fi

llvm-ar rcs "$OUTPUT_FILE" ./*.o
popd > /dev/null

mv "$TEMP_DIR/$OUTPUT_FILE" "$OUTPUT_FILE"

ls -sh "$OUTPUT_FILE"
