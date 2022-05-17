#!/bin/bash

sed 's/(\([^ ]\)/( \1/g' $1 | sed 's/\([^ ]\))/\1 )/g' | sed 's/(\([^ ]\)/( \1/g' | sed 's/\([^ ]\))/\1 )/g' | sed 's/\s\+$//g' > .temporary_file

cp .temporary_file $1

rm .temporary_file


