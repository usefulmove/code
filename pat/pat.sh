#!/bin/bash

sed 's/(\([^ ]\)/( \1/g' $1 | sed 's/\([^ ]\))/\1 )/g' | sed 's/\s\+$//g'

