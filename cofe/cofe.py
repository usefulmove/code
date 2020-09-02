#!/usr/bin/python -tt

'''
program: cofe
filename: cofe.py
description: this program sums the file sizes for all of the files
             within a given directory that match a specific pattern.
usage: cofe [option] <pattern> [directory]
options: --verbose, turn on verbose (debug) mode
         --help, display help and usage information
         --version, display program version
created: 16 apr 2013 rde
updated: 19 apr 2013 rde
'''


import sys      # system specific parameters and functions
import os       # operating system interface
import fnmatch  # unix filename pattern matching

version = "0.0.8"  # software version


def printusage():
    print "usage: cofe [option] <pattern> [directory]"


def main():
    verbose = False

    if len( sys.argv) > 1:

        # verbose option
        if sys.argv[1] == "--verbose":
            verbose = True
            sys.argv.pop( 1)

            if len( sys.argv) < 2:
                # incorrect arguments
                printusage()
                return

        # verision option
        if sys.argv[1] == "--version":
            print "cofe (version %s)" % version 
            return

        # help option
        if sys.argv[1] == "--help":
            printusage()
            return

        # read pattern argument
        pattern_arg = sys.argv[1]
        sys.argv.pop( 1)
        if verbose:
            print "pattern argument: '" + pattern_arg + "'"

        # read directory argument
        if len( sys.argv) > 1:
            base_directory = sys.argv[1]
            sys.argv.pop( 1)

            # TODO add invalid argument check
        else:
            base_directory = os.getcwd()  # get current working directory

        if verbose:
            print "directory: " + base_directory

    else:  # no arguments
        printusage()
        return


    # create pattern list for match filter
    adjusted_pattern_list = []

    if pattern_arg == "_media_":
        pattern_list = ['*.jpg', '*.jpeg', '*.gif', '*.png', '*.mov', '*.mp4',
                        '*.m4v', '*.mp3', '*.wmv', '*.avi', '*.cine']
        for p in pattern_list:
            adjusted_pattern_list.append( p.lower())
            adjusted_pattern_list.append( p.upper())
    elif pattern_arg == "_all_":
        pattern_list = ['*']
        for p in pattern_list:
            adjusted_pattern_list.append( p.lower())
            # note: no idea why this doesn't work when handled in the same
            #       manner as _all_ or the pass through case. when the
            #       p.upper() patterns are appended, duplicate results are
            #       included in the output.
    elif pattern_arg == "_plus_":
        pattern_list = ['*.png', '*.mp4', '*.m4v', '*.mp3']
        for p in pattern_list:
            adjusted_pattern_list.append( p.lower())
            # note: no idea why this doesn't work when handled in the same
            #       manner as _all_ or the pass through case. when the
            #       p.upper() patterns are appended, duplicate results are
            #       included in the output.
    else:
        pattern_list = [pattern_arg]
        for p in pattern_list:
            adjusted_pattern_list.append( p.lower())
            adjusted_pattern_list.append( p.upper())


    # search for files matching the pattern(s)
    count = 0
    total_size_bytes = 0

    for filepath, directories, file in os.walk( base_directory):
        for match_pattern in adjusted_pattern_list:
            for file_match in fnmatch.filter( file, match_pattern):
                count += 1

                # generate file string including file path
                if filepath[-1:] != "/":
                    filepath = filepath + "/"  # add trailing '/'

                # calculate file size
                if os.path.isfile( filepath + file_match):
                    filestats = os.stat( filepath + file_match)
                    size_bytes = float( filestats.st_size)
                    total_size_bytes += size_bytes

                    if verbose:
                        print "match: %s - %0.0f bytes" % (filepath + file_match, size_bytes)

    # format and display total
    if total_size_bytes < 1024.0:
        format_unit_name = "bytes"
        format_size = total_size_bytes
    elif total_size_bytes < 1024.0**2:
        format_unit_name = "KB"
        format_size = total_size_bytes / 1024.0
    elif total_size_bytes < 1024.0**3:
        format_unit_name = "MB"
        format_size = total_size_bytes / 1024.0**2
    else:
        format_unit_name = "GB"
        format_size = total_size_bytes / 1024.0**3

    print "%d files matching '%s' = %0.3f %s" % (count, pattern_arg, format_size, format_unit_name)


if __name__ == '__main__':
    main()
