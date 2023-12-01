#!/bin/sh

set -e

find . -type d -name "Day*" -exec sh -c "cd {}; make test" \;
