#!/bin/sh

set -e

# Identify the next day.
currentMax=$(find . -type d -name "Day*" | egrep -o "[0-9]+" | sort -nr | head -n 1)
nextDayNo=$(echo "$currentMax" + 1 | bc)
echo "Starting day $nextDayNo"
cp -r _tpl/ "Day$nextDayNo"

# Format the template files.
sed -i "" "s/__day__/$nextDayNo/g" "Day$nextDayNo/Advanced.hs"
sed -i "" "s/__day__/$nextDayNo/g" "Day$nextDayNo/Makefile"
sed -i "" "s/__day__/$nextDayNo/g" "Day$nextDayNo/Simple.hs"
