#!/bin/sh

find . -iregex '.*[.]ml[i]?' -not -path './_build/*' | while read f
do
    ocp-indent -i "$f"
    sed -i "$f" -e 's/ *$//g'
done
