#!/usr/bin/python -tt

import sys
import os

def main():
    filename = "harry-potter-book-one"

    # initialize variables
    number_r = 0.0
    number_s = 0.0
    number_w = 0.0
    number_t = 0.0
    number_e = 0.0
    number_z = 0.0
    total_number_letters = 0.0

    # read in data
    with open(filename,'r') as f:
        for line in f:
            for letter in line:
                total_number_letters += 1.0

                if letter == 'r' or letter == "R":
                    number_r += 1.0
                if letter == 's' or letter == "S":
                    number_s += 1.0
                if letter == 'w' or letter == "W":
                    number_w += 1.0
                if letter == 't' or letter == "T":
                    number_t += 1.0
                if letter == 'e' or letter == 'E':
                    number_e += 1.0
                if letter == 'z' or letter == 'Z':
                    number_z += 1.0

    # calculate statistics
    percentage_r = number_r / total_number_letters * 100.0
    percentage_s = number_s / total_number_letters * 100.0
    percentage_w = number_w / total_number_letters * 100.0
    percentage_t = number_t / total_number_letters * 100.0
    percentage_e = number_e / total_number_letters * 100.0
    percentage_z = number_z / total_number_letters * 100.0

    print "\n"
    print "The total number of characters in the data set ({:s}) is {:0.0f}.".format(filename,total_number_letters)
    print "\n"
    print "The number of R's in the data set is {:0.0f} ({:0.2f} %).".format(number_r, percentage_r)
    print "The number of S's in the data set is {:0.0f} ({:0.2f} %).".format(number_s, percentage_s)
    print "The number of W's in the data set is {:0.0f} ({:0.2f} %).".format(number_w, percentage_w)
    print "The number of T's in the data set is {:0.0f} ({:0.2f} %).".format(number_t, percentage_t)
    print "The number of E's in the data set is {:0.0f} ({:0.2f} %).".format(number_e, percentage_e)
    print "The number of Z's in the data set is {:0.0f} ({:0.2f} %).".format(number_z, percentage_z)
    print "\n"

    return os.EX_OK

if __name__ == '__main__':
    main()
