#!/usr/bin/env bash
set -euxo pipefail

BUILD_DIR=$1
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
CONFIG=$DIR/../dwm/config.h

source $DIR/common.sh

NAME=dwm
VERSION=6.2
SRC_FILENAME="dwm-$VERSION.tar.gz"
SRC_DIR="dwm-$VERSION"
SRC_URL="https://dl.suckless.org/dwm/$SRC_FILENAME"
SRC_CHECKSUM="97902e2e007aaeaa3c6e3bed1f81785b817b7413947f1db1d3b62b8da4cd110e"

NOBORDER_FILENAME="dwm-noborder-$VERSION.diff"
NOBORDER_URL="https://dwm.suckless.org/patches/noborder/$NOBORDER_FILENAME"
NOBORDER_CHECKSUM="9bbf5f963e5a2d23ae4b8731f0c179a8615de5715a2dbf683fbe02115e24efe0"

BSTACK_FILENAME="dwm-bottomstack-6.1.diff"
BSTACK_URL="https://dwm.suckless.org/patches/bottomstack/$BSTACK_FILENAME"
BSTACK_CHECKSUM="ea5a7ed499a20abbbca0eec8679bbf98ee188a2d57ac59f75bb0893a4d7eee4f"

PERTAG_FILENAME="dwm-pertag-$VERSION.diff"
PERTAG_URL="https://dwm.suckless.org/patches/pertag/$PERTAG_FILENAME"
PERTAG_CHECKSUM="055da0f12dbfde9e50df54e1f2d87966466404a36c056efb94bb21ab03b94b10"

mkdir -p $BUILD_DIR

echo "Working in $BUILD_DIR"
pushd $BUILD_DIR

curl $SRC_URL --output $SRC_FILENAME
checksum $SRC_CHECKSUM $SRC_FILENAME

curl $NOBORDER_URL --output $NOBORDER_FILENAME
checksum $SRC_CHECKSUM $SRC_FILENAME

curl $BSTACK_URL --output $BSTACK_FILENAME
checksum $BSTACK_CHECKSUM $BSTACK_FILENAME

curl $PERTAG_URL --output $PERTAG_FILENAME
checksum $PERTAG_CHECKSUM $PERTAG_FILENAME

tar xvzf $SRC_FILENAME
patch -d $SRC_DIR -p1 < $NOBORDER_FILENAME
patch -d $SRC_DIR -p1 < $BSTACK_FILENAME
patch -d $SRC_DIR -p1 < $PERTAG_FILENAME
ln -sf $CONFIG $(pwd)/$SRC_DIR
