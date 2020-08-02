echo "Deciding which checksum tool to use..."
if type sha256sum 2> /dev/null; then
    echo "Using sha256sum"
    function checksum {
        csum=$1
        file=$2
        echo "$csum $file" | sha256sum -c
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
