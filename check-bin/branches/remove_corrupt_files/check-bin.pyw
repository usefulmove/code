#!/usr/bin/python -tt

'''
Program: check-bin
Filename: check-bin.pyw
Description: This program performs a file integrity checks by comparing generated message
             digest values with the corresponding values stored in files containing the
             file-to-digest associations.
Created: 24 Apr 2013
Updated: 30 Apr 2013
'''


import sys      # system specific parameters and functions
import os       # operating system interface
import re       # regular expression operations
import hashlib  # secure hashes and message digests
import fnmatch  # unix filename pattern matching
import wx       # wxpython GUI module
import shutil   # high-level file operations


version = "0.0.3"  # program version
bverbose = False
SYSTEM_ERROR = 100
FILE_NOT_FOUND = 0
core_dir_name   = "Core"       # core directory
periph_dir_name = "Periphs A"  # periphals directory
corrupt_files = [] # list of corrupt files


class CheckBinWindow( wx.Frame):  # window definition
    global bverbose
    global corrupt_files

    def __init__( self, parent, title):
        frame_width = 400
        frame_height = 150
        wx.Frame.__init__( self, parent, title=title, size=(frame_width,frame_height),
                           style=wx.SYSTEM_MENU|wx.CAPTION|wx.CLOSE_BOX|wx.CLIP_CHILDREN)

        self.CreateStatusBar()  # create status bar at the bottom of the window
        self.SetStatusText( version)

        default_button_size = wx.Button.GetDefaultSize()

        # create execute button
        eb_hsize = default_button_size[0] * 2.0
        eb_vsize = default_button_size[1]
        eb_hpos = ( frame_width - eb_hsize) / 2.0
        eb_vpos = frame_height - 83
        ex_button = wx.Button( self, id=wx.ID_ANY, label="Perform Check", pos=(eb_hpos,eb_vpos),
                               size=(eb_hsize,eb_vsize))
        ex_button.Bind( wx.EVT_BUTTON, self.onExecuteButton)

        # create directory search dialog and button
        dir_box_hpos = 10
        dir_box_vpos = 25
        dir_box_vsize = 25
        dir_box_hsize = frame_width - default_button_size[0] - 20 
        self.dir_box = wx.TextCtrl( self, pos=(dir_box_hpos,dir_box_vpos),
                                    size=(dir_box_hsize,dir_box_vsize))
        self.dir_box.SetValue( "(choose directory)")

        db_hpos = dir_box_hpos + dir_box_hsize
        db_vpos = dir_box_vpos
        dir_button = wx.Button( self, id=wx.ID_ANY, label="Directory", pos=(db_hpos,db_vpos),
                                size=(default_button_size[0],default_button_size[1]))
        dir_button.Bind( wx.EVT_BUTTON, self.onDirectoryButton)

        self.Show( True)  # show window


    def onDirectoryButton( self, event):
        if bverbose:
            print "directory button pressed"

        start_path = os.getcwd()  # get current working directory
        if os.path.isdir( self.dir_box.GetValue()):
            start_path = self.dir_box.GetValue()  # open directory dialog at location shown

        dialog = wx.DirDialog( self, message="Choose a directory:", defaultPath=start_path,
                               style=wx.DD_DEFAULT_STYLE|wx.DD_DIR_MUST_EXIST)

        if dialog.ShowModal() == wx.ID_OK:
            dir = dialog.GetPath()
            if bverbose:
                print "'%s' selected" % dir
            if os.path.isdir( dir):
                self.dir_box.SetValue( dir)

        dialog.Destroy()


    def onExecuteButton( self, event):
        global corrupt_files

        if bverbose:
            print "execute button pressed"
        base_directory = self.dir_box.GetValue()
        if bverbose:
            print "directory: " + base_directory

        # generate file string including file path
        if base_directory[-1:] != "/":
            base_directory = base_directory + "/"  # add trailing '/'

        # ensure directory is valid
        if not os.path.isdir( base_directory):
            display_error( "ERROR:  Invalid directory")
            return

        corrupt_files = []  # clear corrupt files list


        # check Core subfolder
        if bverbose:
            print "checking core directory"
        error = CheckDirIntegrity( base_directory, core_dir_name)
        if error == SYSTEM_ERROR:
            return

        # check peripherals subfolder
        if bverbose:
            print "checking peripherals directory"
        error = CheckDirIntegrity( base_directory, periph_dir_name)
        if error == SYSTEM_ERROR:
            return


        # display results
        if len( corrupt_files) == 0:
            display_message = "PASS:  All file integrity checks were successful."
            if bverbose:
                print display_message
            wx.MessageBox( display_message, "Integrity Check Result",
                           style=wx.OK|wx.ICON_INFORMATION|wx.STAY_ON_TOP)
        else:
            display_message = "FAIL:  Some or all file integrity checks failed."
            file_message = ""
            for name in corrupt_files:
                file_message = file_message + name + "\n"
            display_message = display_message + "\n\n" + file_message
            display_error( display_message)


def CheckDirIntegrity( base_dir, sub_dir):
    global corrupt_files

    path_to_file = base_dir + sub_dir + "/"

    if not os.path.isdir( path_to_file):  # does the subdirectory exist?
        display_error( "ERROR:  Directory structure is not consistent with this released software bundle.")
        return SYSTEM_ERROR

    # find digest file
    digest_filename = ""
    for file in os.listdir( path_to_file):
        if fnmatch.fnmatch( file, 'md5*.txt'):  # look for file matching this pattern
            digest_filename = file
            if bverbose:
                print "digest file found: %s" % digest_filename
    if digest_filename == "":
        display_error( "ERROR:  Could not find md5 digest file in directory." + "\n\n" + path_to_file)
        return SYSTEM_ERROR

    if not os.path.isfile( path_to_file + digest_filename):  # does the file exist?
        display_error( "ERROR:  Could not locate md5 digest file. (2)" + "\n\n" + path_to_file + digest_filename)
        return SYSTEM_ERROR

    # read in digest file
    digest_file = open( path_to_file + digest_filename, 'r')
    stored_digest_data = digest_file.readlines()
    digest_file.close()

    # parse for associations and perform integrity check
    file_name = [None] * len( stored_digest_data)
    stored_file_digest = [None] * len( stored_digest_data)

    for i, line in enumerate( stored_digest_data):
        doublet = line.split('*')

        file_name[i] = doublet[1].strip( ' ')
        file_name[i] = file_name[i].strip( '\n')
        file_name[i] = file_name[i].strip( '\r')
        file_name[i] = file_name[i].strip( ' ')

        stored_file_digest[i] = doublet[0].strip( ' ')
        stored_file_digest[i] = stored_file_digest[i].strip( '\n')
        stored_file_digest[i] = stored_file_digest[i].strip( '\r')
        stored_file_digest[i] = stored_file_digest[i].strip( ' ')

        if bverbose:
            print "%s: %s (%s)" % ( digest_filename, file_name[i], stored_file_digest[i])

        if not re.search( "^md5", file_name[i], re.IGNORECASE):  # ignore filenames starting with "md5"
            # perform integrity check on each file
            if not check_file( path_to_file + file_name[i], stored_file_digest[i]):
                corrupt_files.append( "%s/%s" % ( sub_dir, file_name[i]))  # add filename to corrupt files list
                remove_corrupt( path_to_file + file_name[i])  # move file to corrupt files directory

    return 0


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


def extract_dir( path):
    regexp = "[^/]+$"
    match = re.search( regexp, path, re.IGNORECASE)
    return path[ 0 : match.start() ]


# generate message digest
def generate_digest( file_loc):
    global bverbose

    if os.path.isfile( file_loc): 
        file = open( file_loc, 'rb')  # open file in binary mode for cross-platform compatibility
        data = file.read()  # read into RAM

        generated_digest = hashlib.md5( data).hexdigest()  # generate digest
        file.close()

        if bverbose:
            print "generated md5 for %s = %s" % ( extract_filename( file_loc), generated_digest)

        return generated_digest
    else:
        return FILE_NOT_FOUND


def display_error( err_msg):
    global bverbose

    if bverbose:
        print err_msg
    wx.MessageBox( err_msg, "Integrity Check Error",
                   style=wx.OK|wx.ICON_EXCLAMATION|wx.STAY_ON_TOP)


# move corrupt files to the corrupt file directory
def remove_corrupt( filepath):
    if os.path.isfile( filepath):  # check that the file exists
        cdir_name = "corrupt do not use"

        corrupt_dir = extract_dir( filepath) + cdir_name

        if not os.path.isdir( corrupt_dir):  # create directory if it does not exist
            os.mkdir( corrupt_dir)

        for file in corrupt_files:
            try:
                shutil.move( filepath, corrupt_dir)
            except:
                pass # do nothing to handle this exception


if __name__ == "__main__":
    app = wx.App( False)
    frame = CheckBinWindow( None, "Binary Integrity Check")
    app.MainLoop()
