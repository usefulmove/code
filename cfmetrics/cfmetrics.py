#!//anaconda/bin/python -tt

'''********************************************************************************
* program: calculate complaint file metrics
* filename: compfile_metrics.py
* usage: compfile_metrics [--debug] file_path [date_ref]
*        (date_ref in {yyyymmdd} format)
********************************************************************************'''

import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as p
import extens as es
from datetime import datetime as dt

app_version = "0.0.4 (beta)"
usage_message = "usage: %s [--debug] [-w number_of_days] file_path [date_ref]" %(sys.argv[0])

#define data indicies
indID = 0 #database identifer
indSUM = 1 #summary
indSTATUS = 2 #status
indRES = 5 #resolution
indCRDATE = 6 #date created
indSTEPS = 8 #step count
indUDI = 9 #unique device identifer
indACCT = 10 #account
indPROD = 17 #product category
indCCAT = 20 #complaint category (major, minor, etc.)
indCLDATE = 23 #date closed
indODAYS = 27 #days open

dwin = 7 #default window for metric calculations (number opened or closed in the past n days, etc.)


#import data from data file
if len(sys.argv) > 1:
    while len(sys.argv) > 1:
        argie = sys.argv.pop(1)
        es.dbg("arg read = %s" %(argie))

        if argie == '--debug' or argie == '-d': #debug flag
            es.dbgflag = True
            es.dbg("debug mode")
        elif argie == '--window' or argie == '-w': #modify window for evaluation
            dwin = int(sys.argv.pop(1))
            es.dbg("arg read (evaluation window) = %d" %(dwin))
        elif argie == '--help' or argie == '-h': #display help/usage information
            print "  %s" %(usage_message)
            quit()
        elif argie == '--version' or argie == '-v': #display version information
            print "  %s - version %s" %(sys.argv[0], app_version)
            quit()
        else:
            dfpath = argie
            es.dbg("file path = %s" %(dfpath))

            #look for date reference argument
            if len(sys.argv) > 1:
                date_ref = dt.strptime(sys.argv.pop(1), '%Y%m%d').date()
                es.dbg("arg read (date reference) = %s" %(str(date_ref)))
            else:
                date_ref = dt.today().date()
                es.dbg("date reference set to %s" %(str(date_ref)))

else:
    print "  error (10) - %s" %(usage_message)
    quit()

dataf = pd.read_excel(dfpath) #read in as pandas data frame
data_na = dataf.values #convert data to numpy array


#calculate percentage open over 60 days (last 6 months)
open60plus = 0 #number of complaints open over 60 days
openwin6mo = 0 #number of complaints opened within 6 months
opentotal = 0 #number of open complaints
openNdays = 0 #number opened within 15 days
closedNdays = 0 #number closed within 15 days


es.dbg("reference date ... %s" %(date_ref.__format__("%d%b%Y")).upper())


for i in range(len(data_na[:,][:,indID])):
    es.dbg("tracID = " + str(data_na[i,indID]))

    if data_na[i,indSTATUS] <> 'closed':
        if es.isfloat(i):
            if int(float(data_na[i,indODAYS])) > 60:
                open60plus += 1
        opentotal += 1

    date_create = pd.Timestamp(data_na[i,indCRDATE]).date()
    #date_create = dt.strptime(data_na[i,indCRDATE], '%d %m %Y').date() #used for reading from csv
        
    date_delta = date_ref - date_create
    if date_delta.days <= int(6.0*365.2/12.0):
        openwin6mo += 1
    if date_delta.days <= dwin:
        openNdays += 1

    type_name = type(data_na[i,indCLDATE]).__name__
    if type_name == 'str' or type_name == 'unicode':
        if data_na[i,indCLDATE] <> "n/a" and data_na[i,indCLDATE] <> "":
            try:
                date_closed = dt.strptime(data_na[i,indCLDATE], '%d %m %Y').date()
            except ValueError as e:
                date_closed = None
            else:
                date_delta = date_ref - date_closed
                degug_print("%d days to closure" %(date_delta.days), DEBUG)
                if date_delta.days <= dwin:
                    closedNdays += 1
    elif type_name == 'datetime':
        date_closed = data_na[i,indCLDATE].date()
        date_delta = date_ref - date_closed
        es.dbg("%d days to closure" %(date_delta.days))
        if date_delta.days <= dwin:
            closedNdays += 1


print "  open complaints = %d" %(opentotal)
print "  opened %d days = %d" %(dwin, openNdays)
print "  closed %d days = %d" %(dwin, closedNdays)
print "  complaints open over 60 days = %d" %(open60plus)
print "  complaints opened within 6 months = %d" %(openwin6mo)
print "  percentage open under 60 days = %.2f %%" %(100.0*(1.0-float(open60plus)/float(openwin6mo)))
