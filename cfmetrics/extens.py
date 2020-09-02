'''********************************************************************************
* extens python extensions methods
********************************************************************************'''

#float type check
def isfloat(string):
    try:
        float(string)
        return True
    except:
        return False


#set debug state (boolean)
global dbgflag
dbgflag = False

#debug message method
def dbg(string):
    if dbgflag:
        print "dgb: %s" %(string)
