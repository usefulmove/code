#!/usr/bin/python -tt
'''********************************************************************************
* Program: cointoss
* Filename: cointoss.py
* Description: This program...
* Usage: cointoss [options]
* Options: -n, number of coin flips to analyze
* Created: 16 May 2012 rde
* Updated: 27 May 2012 rde
********************************************************************************'''

import sys
import random  # pseudo-random number generation
import os
from collections import deque

# program version
PROGRAM_VERSION = "0.1.3p"

# command syntax
COINTOSS_SYNTAX = "cointoss [OPTIONS]"

# output control flags
debug = False
verbose = False


'''********************************************************************************
* Class: Coin (two-sided)
********************************************************************************'''
class Coin:
    def __init__( self):  # constructor
        random.seed()  # seed random number generator
        self.HEADS = True
        self.TAILS = False
        self.side = self.HEADS

    '''****************************************************************************
    * Method: flip
    * Description: This method is used to simulate the flipping of the coin such
    *       that the (visible) side result is random.
    * Usage: coin_obj.Flip()
    * Arguments: none
    * Return: none
    ****************************************************************************'''
    def Flip( self):
        if random.random() < 0.5:
            self.side = self.TAILS
        else:
            self.side = self.HEADS

    '''****************************************************************************
    * Method: getside
    * Description: This method is used to access the (visible) side of the coin
    *       object.
    * Usage: coin_obj.GetSide()
    * Arguments: none
    * Return: side (HEADS or TAILS)
    ****************************************************************************'''
    def GetSide( self):
        return self.side


'''********************************************************************************
* Class: Cointosser
********************************************************************************'''
class Cointosser:
    def __init__( self):  # constructor
        self.PATTERNLENGTH = 3
        self.test_coin = Coin()  # coin
        self.cresults = deque()  # results data
        self.match_counter = 0
        self.match_count_sum = 0  # match count total
        self.matches = 0  # number of matches
        self.number_heads = 0  # number of heads
        self.number_tails = 0  # number of tails

    '''****************************************************************************
    * Method: Toss
    * Description: This method is used to cause the cointosser to flip a coin.
    * Usage: cointosser_obj.Toss()
    * Arguments: none
    * Return: none
    ****************************************************************************'''
    def Toss( self):
        self.test_coin.Flip()  # flip coin

        # record result
        self.cresults.append( self.test_coin.GetSide())
        if self.cresults.__len__() > 3:
            self.cresults.popleft()  # remove front node

        if self.test_coin.GetSide() == self.test_coin.HEADS:
            self.number_heads += 1
        else:
            self.number_tails += 1
        self.match_counter += 1

    '''****************************************************************************
    * Method: TossAndCheck
    * Description: This method is used to cause the cointosser to flip a coin and
    *       check the result history for a match.
    * Usage: cointosser_obj.TossAndCheck()
    * Arguments: none
    * Return: none
    ****************************************************************************'''
    def TossAndCheck( self):
        # flip coin and record result
        self.Toss()
    
        # check for pattern match
        array_index = 0
        if list( self.cresults) != self.match_pattern:
            return  # no match

    
        if self.match_counter < 3:
            return
            '''
            These matches have to be rejected because the test is for number of
            coin tosses required to match the pattern. If these are not
            rejected, the program may falsely identify matches based on coin
            tosses from previous trials.
            '''
    
        # match
        self.matches += 1
        self.match_count_sum += self.match_counter
        self.match_counter = 0  # reset match counter
        if debug:
            print "  pattern match"

    '''****************************************************************************
    * Method: SetPattern
    * Description: This method is used to set the match pattern.
    * Usage: cointosser_obj.SetPattern( match_sequence)
    * Arguments: match_sequence -- list containing sequence that identifies a match
    * Return: none
    ****************************************************************************'''
    def SetPattern( self, match_sequence):
        self.match_pattern = match_sequence

    '''****************************************************************************
    * Method: GenerateResults
    * Description: This method is used to calculate coin toss pattern match results
    *       and display them.
    * Usage: cointosser_obj.GenerateResults()
    * Arguments: match_sequence -- array containing sequence that identifies a
    *       match
    * Return: none
    ****************************************************************************'''
    def GenerateResults( self):
        if self.matches > 0:
            srchCountAvg = float( self.match_count_sum) / float( self.matches)
        else:
            srchCountAvg = 0
    
        pattern = ""
        for count in range( self.PATTERNLENGTH):
            if self.match_pattern[ count] == True:
                type = "1"
            else:
                type = "0"
            pattern += type

        print "  average search for " + pattern,
        print "pattern took %2.2f" % srchCountAvg,
        print "trials"
    
        if verbose:
            print "  (" + str( self.matches),
            print "instances of pattern were observed in data)"


    '''****************************************************************************
    * Method: GetHeads
    * Description: This method is used to retrieve the number of heads results
    *       observed since the trial beginning.
    * Usage: num_heads = cointosser_obj.GetHeads()
    * Arguments: none
    * Return: num_heads -- number of since trial start
    ****************************************************************************'''
    def GetHeads( self):
        return self.number_heads

    '''****************************************************************************
    * Method: GetTails
    * Description: This method is used to retrieve the number of tails results
    *       observed since the trial beginning.
    * Usage: num_tails = cointosser_obj.GetTails()
    * Arguments: none
    * Return: num_tails -- number of since trial start
    ****************************************************************************'''
    def GetTails( self):
        return self.number_tails

    '''****************************************************************************
    * Method: GetMatches
    * Description: This method is used to retrieve the number of pattern matches
    *       observed since the trial beginning.
    * Usage: num_matches = cointosser_obj.GetMatches()
    * Arguments: none
    * Return: num_matches -- number of since trial start
    ****************************************************************************'''
    def GetMatches( self):
        return self.match_count_su



def syntax_error():
    print "cointoss: incorrect syntax"
    print "usage: " + COINTOSS_SYNTAX + ""
    print "type 'cointoss --help' for more information"



def invalid_option( option):
    print "cointoss: invalid option '" + str( option) + "'"
    print "type 'cointoss --help' for more information"



'''********************************************************************************
* Procedure: main
* Description: main program execution function
* Arguments: none
********************************************************************************'''
def main():
    coin_toss_total = 1000000  # total number of coin tosses (default)

    CoinTester1 = Cointosser()
    CoinTester2 = Cointosser()

    pattern_one = [False, True, True]
    pattern_two = [True, False, True]

    global debug
    global verbose
    global COINTOSS_SYNTAX
    global PROGRAM_VERSION

    if verbose:
        print "  cointoss program"

    # read options
    while len( sys.argv) > 1:

        arg = sys.argv.pop(1)

        if (arg == '-n') | (arg == '-N'):
            # number of iterations
            coin_toss_total = int( sys.argv.pop(1))
        elif (arg == '-v') | (arg == '--verbose'):
            # verbose mode
            verbose = True
            print "  (verbose mode on)"
        elif arg == '-D':
            # debug mode
            debug = True
            print "  (debug mode on)"
            if coin_toss_total > 100:
                coin_toss_total = 100
        elif arg == '--help':
            # help
            print "  usage: " + COINTOSS_SYNTAX + ""
            print "  options"
            print "      -n  NUM         set number of cycles"
            print "      -v, --verbose   enable verbose mode"
            print "      -D              enable debug mode"
            print "      --help          display help"
            print "      --version       display version information"
            return os.EX_OK  # exit program
        elif arg == '--version':
            # version
            print "  cointoss (version " + PROGRAM_VERSION + ")"
            return os.EX_OK
        else:
            invalid_option( arg)
            return os.EX_USAGE

    # set match pattern
    CoinTester1.SetPattern( pattern_one)
    CoinTester2.SetPattern( pattern_two)

    # perform test
    print "  performing analysis for " + str( coin_toss_total) + " cycles..."

    for count in range( coin_toss_total):
        if debug:
            print "  ---------- cycle number " + str( count) + " ----------"

        if count < CoinTester1.PATTERNLENGTH:
            # preliminary coin tosses
            CoinTester1.Toss()
            CoinTester2.Toss()
        else:
            CoinTester1.TossAndCheck()
            CoinTester2.TossAndCheck()



    # read and analyze data
    if verbose:
        print "  sanity check: head count total for coin #1 is " + str( CoinTester1.GetHeads()),
        print "(%2.2f" % (float(CoinTester1.GetHeads()) / float(coin_toss_total) * 100.0) + "%)"
        print "  sanity check: head count total for coin #2 is " + str( CoinTester2.GetHeads()),
        print "(%2.2f" % (float(CoinTester2.GetHeads()) / float(coin_toss_total) * 100.0) + "%)"

    CoinTester1.GenerateResults()
    CoinTester2.GenerateResults()

    return os.EX_OK


if __name__ == '__main__':
    main()
