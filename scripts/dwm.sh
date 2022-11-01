#!/usr/bin/env bash
set -euxo pipefail

BUILD_DIR=$1
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DWM_DIR=$DIR/../dwm
CONFIG=$DWM_DIR/config.h

source $DIR/common.sh

NAME=dwm
VERSION=6.4
SRC_FILENAME="dwm-$VERSION.tar.gz"
SRC_DIR="dwm-$VERSION"
SRC_URL="https://dl.suckless.org/dwm/$SRC_FILENAME"
SRC_CHECKSUM="fa9c0d69a584485076cfc18809fd705e5c2080dafb13d5e729a3646ca7703a6e"

NOBORDER_FILENAME="dwm-noborder-6.2.diff"
NOBORDER_URL="https://dwm.suckless.org/patches/noborder/$NOBORDER_FILENAME"
NOBORDER_CHECKSUM="9bbf5f963e5a2d23ae4b8731f0c179a8615de5715a2dbf683fbe02115e24efe0"

PERTAG_FILENAME="dwm-pertag-6.2.diff"
PERTAG_URL="https://dwm.suckless.org/patches/pertag/$PERTAG_FILENAME"
PERTAG_CHECKSUM="055da0f12dbfde9e50df54e1f2d87966466404a36c056efb94bb21ab03b94b10"

mkdir -p $BUILD_DIR

echo "Working in $BUILD_DIR"
pushd $BUILD_DIR

get $SRC_URL $SRC_FILENAME
checksum $SRC_CHECKSUM $SRC_FILENAME

get $NOBORDER_URL $NOBORDER_FILENAME
checksum $SRC_CHECKSUM $SRC_FILENAME

get $PERTAG_URL $PERTAG_FILENAME
checksum $PERTAG_CHECKSUM $PERTAG_FILENAME

tar xvzf $SRC_FILENAME
patch -d $SRC_DIR -p1 < $NOBORDER_FILENAME
patch -d $SRC_DIR -p1 < $DWM_DIR/bottomstack-custom.diff
patch -d $SRC_DIR -p1 < $PERTAG_FILENAME
patch -d $SRC_DIR -p1 < $DWM_DIR/replace-space.diff
patch -d $SRC_DIR -p1 < $DWM_DIR/usercflags.diff
patch -d $SRC_DIR -p1 < $DWM_DIR/local.diff
ln -sf $CONFIG $(pwd)/$SRC_DIR
