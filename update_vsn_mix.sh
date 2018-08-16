#!/bin/bash

case "$(uname -s)" in
    "Darwin")
        GSED="$(which gsed)"
        if [ ! -x "$GSED" ]; then
            echo "Requires gsed... try: brew install gnu-sed"
            exit 1
        fi
        ;;
    "Linux")
        GSED="sed"
        if [ ! -x "$GSED" ]; then
            echo "Requires sed command"
            exit 1
        fi
        ;;
    *)
        echo "Platform unknown"
        exit 2
        ;;
esac

VSN="$(git describe --tags --abbrev=0)"
echo "Update version to $VSN in mix.exs"
$GSED -i "s/\\@version \"[^\"]*\"/@version \"$VSN\"/" mix.exs
