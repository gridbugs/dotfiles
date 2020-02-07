#!/bin/bash
set -euxo pipefail

BUILD_DIR=$1
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
CONFIG=$DIR/../dwm/config.h

NAME=dwm
VERSION=6.2
SRC_FILENAME="dwm-$VERSION.tar.gz"
SRC_DIR="dwm-$VERSION"
SRC_URL="https://dl.suckless.org/dwm/$SRC_FILENAME"
SRC_CHECKSUM="97902e2e007aaeaa3c6e3bed1f81785b817b7413947f1db1d3b62b8da4cd110e"
NOBORDER_FILENAME="dwm-noborder-$VERSION.diff"
NOBORDER_URL="https://dwm.suckless.org/patches/noborder/$NOBORDER_FILENAME"
NOBORDER_CHECKSUM="9bbf5f963e5a2d23ae4b8731f0c179a8615de5715a2dbf683fbe02115e24efe0"

mkdir -p $BUILD_DIR

echo "Working in $BUILD_DIR"
pushd $BUILD_DIR

wget $SRC_URL -O $SRC_FILENAME
echo "$SRC_CHECKSUM $SRC_FILENAME" | sha256sum -c || exit 1

wget $NOBORDER_URL -O $NOBORDER_FILENAME
echo "$NOBORDER_CHECKSUM $NOBORDER_FILENAME" | sha256sum -c || exit 1

tar xvzf $SRC_FILENAME
patch -d $SRC_DIR -p1 < $NOBORDER_FILENAME
ln -sf $CONFIG $(pwd)/$SRC_DIR
