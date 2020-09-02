#!/usr/bin/python -tt
# usage: weather (zip code OR city, state)

import sys
import urllib  # URL library
import re      # regular expression support
import time    # time access and conversions

version = "201.205.162.026"  # software version


def main():
    verbose = False

    if len( sys.argv) > 1:
        # verbose flag
        if sys.argv[1] == "--verbose":
            verbose = True
            sys.argv.pop( 1)
        
        # verision flag
        if sys.argv[1] == "--version":
            print "version " + version
            return

        # help flag
        if sys.argv[1] == "--help":
            printusage()
            return


        if verbose:
            main_timer = TTimer()
            page_timer  = TTimer()
            parse_timer = TTimer()

            main_timer.start()  # start main timer
        
        # form URL from arguments
        base_url = "http://www.weather.com/search/enhancedlocalsearch?whatprefs=&what=WeatherLocalUndeclared&lswe=&lswa=&loctypes=1003%2C1001%2C1000%2C1%2C9%2C5%2C11%2C13%2C19%2C20&from=searchbox_localwx&googleTypeSearch=on&where="
        location = ""

        for arg in range( 1, len( sys.argv)):
            location = location + sys.argv[arg] + " "
        location = location.strip()

        url = base_url + location


        # retrieve contents of web page
        print "retrieving data..."
        if verbose:
            page_timer.start()  # start page timer

        weather_page = urllib.urlopen( url)  # open page
        page_info = weather_page.info()  # get page info


        # parse web page contents for data and display
        if page_info.gettype() == 'text/html':
            page_data = weather_page.read()  # read page

            if verbose:
                print "page retrieval time: %2.3f seconds" % page_timer.stop()  # stop page timer
                parse_timer.start()  # start parsing timer


            # find current temperature
            current_temp = findmatch( "itemprop=\"temperature-fahrenheit\">(\d\d?)", page_data)
            if current_temp:
                # find location string if zip code was passed in, otherwise find zip code
                zip_arg = findmatch( "(\d\d\d\d\d)", location)
                if zip_arg:
                    parsed_location = findmatch( "<h1>(.*)Weather.*</h1>", page_data)
                    if parsed_location:
                        location = parsed_location.strip()
                else:
                    zip = findmatch( "zip...(\d{5})", page_data)
                    if zip:
                        location = location + " (" + zip + ")"

                if verbose:
                    print "page parse time: %2.3f seconds" % parse_timer.stop()  # stop parse timer

                # output
                print "%s - %s deg F" % (location.lower(), current_temp)
            else:
                printerror( 253, "failed to retrieve information for " + location + " - location not unique or invalid")
        else:
            printerror( 254, "failed to retrieve information for " + location)

        if verbose:
            print "execution time: %2.3f seconds" % main_timer.stop()  # stop main timer
    else:
        printusage()  # print usage info


def findmatch( regex, string, group_index = 1, flags = re.DOTALL):
    match = re.search( regex, string, flags)
    if match:
        return match.group( group_index)
    return None  # no match


def printusage():
    print "usage: weather (zip code OR city, state)"


def printerror( error_num, errmessage):
    print "error " + str( error_num) + ": " + errmessage


# timer class
class TTimer:
    def __init__( self, time = 0):
        self.start_time = time
        self.stop_time = time

    def start( self):
        self.start_time = time.clock()
    
    def stop( self):
        self.stop_time = time.clock()
        return (self.stop_time - self.start_time)
    

if __name__ == '__main__':
    main()
