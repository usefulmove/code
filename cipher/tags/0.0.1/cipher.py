#!/usr/bin/python -tt
'''********************************************************************************
* program: cipher (vigenere)
* filename: cipher.py
* usage: cipher [options] text_to_encode
* options: (see command usage)
********************************************************************************'''

import os
import sys

# program version
PROGRAM_VERSION = "0.0.1"

# command syntax
COMMAND_SYNTAX = "cipher [options] text_to_encode"

# output control
bDebug = False # debug flag


'''********************************************************************************
* class: Cipher
********************************************************************************'''
class Cipher():
    kindex = 0 # initialize key index

    # build dictionary
    d = {}
    d['A'] = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcba'
    d['B'] = 'BCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaA'
    d['C'] = 'CDEFGHIJKLMNOPQRSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaAB'
    d['D'] = 'DEFGHIJKLMNOPQRSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABC'
    d['E'] = 'EFGHIJKLMNOPQRSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCD'
    d['F'] = 'FGHIJKLMNOPQRSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDE'
    d['G'] = 'GHIJKLMNOPQRSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEF'
    d['H'] = 'HIJKLMNOPQRSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFG'
    d['I'] = 'IJKLMNOPQRSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGH'
    d['J'] = 'JKLMNOPQRSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHI'
    d['K'] = 'KLMNOPQRSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJ'
    d['L'] = 'LMNOPQRSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJK'
    d['M'] = 'MNOPQRSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKL'
    d['N'] = 'NOPQRSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLM'
    d['O'] = 'OPQRSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMN'
    d['P'] = 'PQRSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNO'
    d['Q'] = 'QRSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOP'
    d['R'] = 'RSTUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQ'
    d['S'] = 'STUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQR'
    d['T'] = 'TUVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRS'
    d['U'] = 'UVWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRST'
    d['V'] = 'VWXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRSTU'
    d['W'] = 'WXYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRSTUV'
    d['X'] = 'XYZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRSTUVW'
    d['Y'] = 'YZ_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRSTUVWX'
    d['Z'] = 'Z_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRSTUVWXY'
    d['_'] = '_0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRSTUVWXYZ'
    d['0'] = '0123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRSTUVWXYZ_'
    d['1'] = '123456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRSTUVWXYZ_0'
    d['2'] = '23456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRSTUVWXYZ_01'
    d['3'] = '3456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRSTUVWXYZ_012'
    d['4'] = '456789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRSTUVWXYZ_0123'
    d['5'] = '56789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRSTUVWXYZ_01234'
    d['6'] = '6789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRSTUVWXYZ_012345'
    d['7'] = '789 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456'
    d['8'] = '89 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRSTUVWXYZ_01234567'
    d['9'] = '9 zyxwvutsrqponmlkjihgfedcbaABCDEFGHIJKLMNOPQRSTUVWXYZ_012345678'
    reference_string = d['A']

    def __init__( self, encryption_key=None): # constructor
        if encryption_key==None:
            self.key = "phidias_061803" # default encryption key 
        else:
            self.key = encryption_key
        self.key = self.key.upper()

    def encode( self, ch):
        if self.is_valid( ch):
            ind = self.reference_string.find( ch)
            conversion_string = self.d[ self.get_key_val()]
            coded_ch = conversion_string[ ind]
            debug_print( "Cipher.encode({})={} : ({}|{})".format( ch, coded_ch, self.get_index(), self.get_key_val()))
            self.increment_kindex()
            return coded_ch
        else:
            return ch

    def decode( self, ch):
        if self.is_valid( ch):
            conversion_string = self.d[ self.get_key_val()]
            ind = conversion_string.find( ch)
            decoded_ch = self.reference_string[ ind]
            debug_print( "Cipher.decode({})={} : ({}|{})".format( ch, decoded_ch, self.get_index(), self.get_key_val()))
            self.increment_kindex()
            return decoded_ch
        else:
            return ch

    def get_index( self): # get encryption key index
        return self.kindex

    def increment_kindex( self): # increment encryption key index
        self.kindex += 1
        if self.kindex >= len( self.key):
            self.kindex = 0 # reset (roll over)

    def get_key_val( self): # get encryption key value at current index postion
        return self.key[ self.get_index()]

    def is_valid( self, ch):
        if self.reference_string.find( ch) >= 0:
            return True
        else:
            return False

    def reset( self):
        self.kindex = 0 # reset key index


def print_cmd_usage(): # command usage
    print "  usage: " + COMMAND_SYNTAX + ""
    print "  options"
    print "      -e                          encode (default)"
    print "      -d                          decode"
    print "      -c                          copy encoded content to clipboard"
    print "      --key [encryption key]      specify private encryption key"
    print "      --help                      display help (this message)"
    print "      --version                   display version information"
    print "      --debug                     enable debug mode"
  

def debug_print( msg):
    global bDebug
    if bDebug:
        print " dbg: " + msg


def main():
    global bDebug
    global PROGRAM_VERSION

    encode_option = True # default to encode mode
    encryption_key = None
    bClip = False # do not copy encoded message to clipboard by default

    # read command line options
    if len( sys.argv) == 1:
        print_cmd_usage()
        return os.EX_OK # exit program

    while len( sys.argv) > 1:
        arg = sys.argv.pop(1)

        if (arg == '-e'):
            encode_option = True # encode mode
        elif (arg == '-d'):
            encode_option = False # decode mode
        elif (arg == '-c'):
            bClip = True
        elif arg == '--debug':
            bDebug = True # debug mode
            debug_print( "(debug mode)")
        elif arg == '--key':
            encryption_key = sys.argv.pop(1)
        elif arg == '--help':
            print_cmd_usage()
            return os.EX_OK # exit program
        elif arg == '--version':
            print "  cipher (version " + PROGRAM_VERSION + ")"
            return os.EX_OK # exit program
        else:
            original_message = arg
            if len( arg) < 2:
                invalid_option( arg)
                return os.EX_USAGE

    # convert message
    if encryption_key != None:
        c = Cipher( encryption_key) # instanatiate cipher object with unique key
    else:
        c = Cipher() # instantiate cipher object (use default encryption key)

    c.reset()
    encoded_message = ""
    if encode_option:
        for i in enumerate( original_message):
            ch = c.encode( original_message[ i[0]])
            encoded_message = encoded_message + ch
        encoded_message = encoded_message[::-1] # reverse
    else:
        original_message = original_message[::-1] # reverse
        for i in enumerate( original_message):
            ch = c.decode( original_message[ i[0]])
            encoded_message = encoded_message + ch

    # print encoded message to terminal
    print encoded_message

    # copy to clipboard
    if bClip:
        operating_system = sys.platform
        if operating_system == "darwin":
            os.system( "echo '{}' | pbcopy".format( encoded_message)) # osx
            cclip_msg = "(copied to clipboard)"
        elif operating_system == "linux2":
            os.system( "echo '{}' | xclip -selection clipboard".format( encoded_message)) # linux with xclip
            cclip_msg = "(copied to clipboard)"
        else:
            cclip_msg = "(could not copy message to clipboard)"
        print cclip_msg

    return os.EX_OK # exit program


if __name__ == '__main__':
    main()
