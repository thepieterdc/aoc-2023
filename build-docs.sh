#!/bin/sh

set -e

haddock --html -o docs/ Utils/*.hs
