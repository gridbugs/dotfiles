echo "Deciding which checksum tool to use..."
if type sha256sum 2> /dev/null; then
    echo "Using sha256sum"
    function checksum {
        csum=$1
        file=$2
        echo "$csum  $file" | sha256sum -c
    }
elif type sha256 2> /dev/null; then
    if test "$(uname)" == "OpenBSD"; then
        echo "Using OpenBSD sha256"
        function checksum {
            csum=$1
            file=$2
            sha256 -C <(echo $csum $file) $file
        }
    else
        echo "Using sha256"
        function checksum {
            csum=$1
            file=$2
            sha256 -c $csum $file
        }
    fi
else
    echo "Using nothing"
    function checksum {
        echo "no sha256 checksum tool found"
    }
fi

echo "Deciding which download tool to use..."
if type wget 2> /dev/null; then
    echo "Using wget"
    function get {
        url=$1
        dest=$2
        wget $url --output-document=$2
    }
else
    echo "Using curl"
    function get {
        url=$1
        dest=$2
        curl $url --output $2
    }
fi
