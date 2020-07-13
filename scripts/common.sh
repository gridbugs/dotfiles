if type sha256sum 2> /dev/null; then
    function checksum {
        csum=$1
        file=$2
        echo "$csum $file" | sha256sum -c
    }
elif type sha256 2> /dev/null; then
    function checksum {
        csum=$1
        file=$2
        sha256 -c $csum $file
    }
else
    function checksum {
        echo "no sha256 checksum tool found"
    }
fi
