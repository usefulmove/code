#!/usr/bin/python -tt

'''
program: check_bin
filename: check_bin.py
description: this program performs a file integrity check by compares a generated
             message digest with the value stored in a md5.txt file containing
             file-to-digest associations.
usage: check_bin [directory]
options: --verbose, turn on verbose (debug) mode
         --help, display help and usage information
         --version, display program version
created: 19 apr 2013 rde
updated: 19 apr 2013 rde
'''


import sys      # system specific parameters and functions
import os       # operating system interface
import re       # regular expression operations
import hashlib  # secure hashes and message digests


version = "0.0.3"  # program version
bverbose = False


def main():
    global bverbose

    if len( sys.argv) > 1:

        # version option
        if sys.argv[1] == "--version":
            print "check_bin (version %s)" % version
            return

        # help option
        if sys.argv[1] == "--help":
            print "check_bin utility to check file integrity"
            printusage()
            return

        # verbose option
        if sys.argv[1] == "--verbose":
            bverbose = True
            sys.argv.pop( 1)

        # read directory argument
        if len( sys.argv) > 1:
            base_directory = sys.argv[1]
            sys.argv.pop( 1)
        else:
            base_directory = os.getcwd()  # get current working directory

    else:  # no arguments
        base_directory = os.getcwd()  # get current working directory


    # generate file string including file path
    if base_directory[-1:] != "/":
        base_directory = base_directory + "/"  # add trailing '/'

    # ensure directory is valid
    if not os.path.isdir( base_directory):
        print "error: invalid directory"
        return

    if bverbose:
        print "directory: " + base_directory

    # read in digest file
    digest_filename = "md5.txt"

    if not os.path.isfile( base_directory + digest_filename):
        print "error: problem finding md5 digest file"
        return

    digest_file = open( base_directory + digest_filename, 'r')
    stored_digest_data = digest_file.readlines()
    digest_file.close()

    # parse for associations and perform integrity check
    corrupted_files = [] # list of corrupt files

    file_name = [None] * len( stored_digest_data)
    stored_file_digest = [None] * len( stored_digest_data)

    for i, line in enumerate( stored_digest_data):
        doublet = line.split(' ')

        file_name[i] = doublet[1].strip( '*')
        file_name[i] = file_name[i].strip( '\n')
        file_name[i] = file_name[i].strip( '\r')

        stored_file_digest[i] = doublet[0].strip( '*')
        stored_file_digest[i] = stored_file_digest[i].strip( '\n')
        stored_file_digest[i] = stored_file_digest[i].strip( '\r')

        if bverbose:
            print "%s: %s (%s)" % (digest_filename, file_name[i] ,stored_file_digest[i])

        if not re.search( "\.txt", file_name[i], re.IGNORECASE):  # check all except .txt files
            # perform integrity check on each file
            if not check_file( base_directory + file_name[i], stored_file_digest[i]):
                print "integrity check FAILED for %s" % file_name[i]
                corrupted_files.append( file_name[i])

    # display results
    if len( corrupted_files) == 0:
        print "\noutput: all integrity checks PASSED"
    else:
        print "\noutput: some or all integrity checks FAILED"
        for name in corrupted_files:
            print "    %s" % name


# generate message digest
def generate_digest( file_loc):
    file = open( file_loc, 'r')
    data = file.read()  # read into RAM

    generated_digest = hashlib.md5( data).hexdigest()  # generate digest
    file.close()

    if bverbose:
        print "generated md5 for %s = %s" % ( extract_filename( file_loc), generated_digest)

    return generated_digest


# perform integrity check on a file
def check_file( file_loc, digest_hex):
    global bverbose

    if bverbose:
        print "checking '%s'" % extract_filename( file_loc)

    f_digest = generate_digest( file_loc)

    # compare digests with expected values
    if f_digest == digest_hex:
        return True
    else:
        return False


def extract_filename( path):
    regexp = "[^/]+$"
    match = re.search( regexp, path, re.IGNORECASE)
    return path[ match.start() : match.end() ]


def printusage():
    print "usage: check_bin [directory]"


if __name__ == '__main__':
    main()
