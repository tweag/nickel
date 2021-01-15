#!/usr/bin/env bash

HERE=$(dirname $0)
CHANNEL=$1

case $CHANNEL in
  stable|beta|nightly)
    ;;
  *)
    echo "You need to select one of the following channels to update:"
    echo " - stable"
    echo " - beta"
    echo " - nightly"
    exit 1
    ;;
esac


echo "Downloading the latest $CHANNEL channel information ..."
curl -Q "https://static.rust-lang.org/dist/channel-rust-$CHANNEL.toml" > $HERE/tmp.toml

CHANNEL_FILE=$HERE/channel_$CHANNEL.toml
CHANNEL_DATE=$(nix-instantiate --eval -E "(builtins.fromTOML (builtins.readFile $HERE/tmp.toml)).date")
CHANNEL_SHA=$(nix hash file $HERE/tmp.toml)

rm -f tmp.toml

echo "channel=\"$CHANNEL\""    > $CHANNEL_FILE
echo "date=$CHANNEL_DATE"    >> $CHANNEL_FILE
echo "sha256=\"$CHANNEL_SHA\"" >> $CHANNEL_FILE

echo "Channel info written in $CHANNEL_FILE"
