#basic operations for files
#original author: rm3086 (at) columbia (dot) edu
#added and modified by Handong Ma: hm2588 (at) columbia (dot) edu

import csv, shutil, os, glob, cPickle
from log import strd_logger
import re
import sys
import inspect

# logger
global log
log = strd_logger('file')


#$$$$$$$$$#
def ifContinue():
    decision = raw_input('Wait for your command: continue(c) or stop(any key)')
    if decision == 'c':
        return True
    else:
        sys.exit("stopped")

#$$$$$$$# check if a file exist
def file_exist (fname):
    try:
        open(fname,'r')
        return True
    except IOError:
        return False


#$$$$$$$$#
def mkdir(dirname):
    if not dirname:
        return False
    try:
        os.makedirs(dirname)
    except OSError:
        pass
    except Exception as e:
        log.error (e)
        return False
    return True

#$$$$$$$$$#
def backup(affectedFileList):
    total_length = len(affectedFileList)
    wait = 0
    while len(affectedFileList) > 0:
        file = affectedFileList.pop(0)
        if file_exist(file):
            path = os.path.dirname(file)
            name = os.path.basename(file)
            new_path = path+'/backup/'
            mkdir(new_path)
            if file_exist(new_path+name):
                try:
                    if wait <= total_length:
                        wait += 1
                        affectedFileList.append(file)
                        continue
                    else:
                        print "whether want to overwrite earlier backed file: " + str(file)
                        ifContinue()
                        shutil.copyfile(file,new_path+name)
                        print "successfully update old backup" + str(file)
                except Exception as e:
                    print 'error when backup'
            else:
                shutil.copyfile(file,new_path+name)
                print "successfully new backups: " + str(file)


#check the dir name whether is standard with a '/' in the end
def check_dir(dirName):
    if re.findall('/$',dirName):
        return str(dirName)
    else:
        return str(dirName + '/')


#$$$$$$$$$$$$#return and print variable name
#error

#$$$$$$$$$$$$#return and print variable name

def retrieve_name(var):
    callers_local_vars = inspect.currentframe().f_back.f_locals.items()
    return [var_name for var_name, var_val in callers_local_vars if var_val is var][0]

###
def continueOrNot():
    decision = raw_input('File exists! Wait for your decision: continue(c) or stop(any key)')
    if decision == 'c':
        return True
    else:
        sys.exit("stopped")

#$$$$$$$$$#
def checkFileIfContinue(affectedFileList):
    call = 0
    for file in affectedFileList:
        if file_exist(file):
            call = 1
            break
    if call==1:
        decision = raw_input('File exists! Wait for your decision: continue(c) or stop(any key)')
        if decision == 'c':
            return True
        else:
            sys.exit(0)
    else:
        return True



# create directory if not existing
#$$$$$$$$$$$$$#
#mkdir(above)

# create directory (delete if one with the same name already exists)
def mk_new_dir (dirname):
    try:
        os.makedirs (dirname)
    except OSError:
        shutil.rmtree(dirname)
        os.makedirs (dirname)
    except Exception as e:
        log.error (e)
        return False
    return True


# copy a file from "source" to "destination"
def fcopy (source, destination):
    try:
        shutil.copy2 (source, destination)
    except Exception as e:
        log.error(e)
        return False
    return True


# return the files of a directory with extension "ext"
def flist (directory, ext):
    try:
        os.chdir(directory)
        if ext[0:2] != '*.':
            ext = '*.' + ext
        data = []
        for f in glob.glob(ext):
            data.append(f.strip())
        return data
    except Exception as e:
        log.error(e)
        return None


### read operations ###

# read a text file
# @param struct: save data to (1) list, (2) set
def read_file (filename, struct = 1, logout = True):
    try:
        fid = open(filename, 'r')
        if struct == 2:
            # set
            data = set()
            for line in fid:
                if len(line) > 0:
                    data.add (line.strip())
        else:
            # default - list
            data = []
            for line in fid:
                if len(line) > 0:
                    data.append (line.strip())
        fid.close()
        return data
    except Exception as e:
        if logout is True:
            log.error(e)
        return None

# read a text file, break lines according to skip
# @param skip: character to skip (default ' ')
def read_file_tokenized (filename, skip = ' ', logout = True):
    try:
        data = []
        fid = open (filename, 'r')
        for line in fid:
            line = line.strip()
            data.append (line.split(skip))
        fid.close()
        return data
    except Exception as e:
        if logout is True:
            log.error(e)
        return None

# read text
def read_text (filename, logout = True):
    try:
        fid = open (filename,'r')
        data = fid.read ()
        data = data.replace ('\n',' ').replace('\t',' ')
        data = ' '.join(data.split()).strip()
        fid.close()
        return data
    except Exception as e:
        if logout is True:
            log.error(e)
        return None


# read data from a csv file
def read_csv(filename, header='T', logout=True):
    try:
        fhIn = open(filename, "rU")
        name = []
        reader = csv.reader(fhIn)
        data = []
        firstLine = 'T'
        for r in reader:
            if header == 'T' and firstLine == 'T':
                name = r
                firstLine = 'F'
                continue
            if not len(str(r)) or str(r).startswith('#'):
                print 'skipped line:' + r
                continue
            if (len(r) != len(name)):
                print "Warning: line length differs with name(%s) in line:" % len(name) + r
            data.append(r)
        return data, name
    except Exception as e:
        if logout is True:
            log.error(e)
        return None

# read a dictionary from a csv file
# @param iKey: column to consider as key (default 0)
# @param iData: column to consider as data (default 1)
def read_csv_as_dict (filename, iKey = 0, iData = 1, logout = True):
    try:
        reader = csv.reader (open(filename, "r"))
        data = {}
        for r in reader:
            data[r[iKey].strip()] = r[iData].strip()
        return data
    except Exception as e:
        if logout is True:
            log.error(e)
        return None

# read a dictionary from a csv file (column '0' is the keys)
def read_csv_as_dict_with_multiple_items (filename, logout = True):
    try:
        reader = csv.reader (open(filename, "r"))
        data = {}
        for r in reader:
            data[r[0].strip()] = r[1:len(data)]
        return data
    except Exception as e:
        if logout is True:
            log.error(e)
        return None

# read an object (list, dictionary, set) from a serialized file
def read_obj (filename, logout = True):
    try:
        data = cPickle.load (open(filename, 'rb'))
        return data
    except Exception as e:
        if logout is True:
            log.error(e)
        return None


### write operations ###

# write data to a csv file
def write_csv (filename, data, logout = True):
    try:
        doc = csv.writer (open(filename, 'wb'), delimiter=',', quotechar='"', quoting=csv.QUOTE_ALL)
        for d in data:
            doc.writerow (d)
        return True
    except Exception as e:
        if logout is True:
            log.error(e)
        return False

# write data to a text file
def write_file (filename, data, logout = True):
    try:
        fid = open (filename,'w')
        for d in data:
            fid.write('%s\n' % d.encode('utf-8'))
        fid.close()
        return True
    except Exception as e:
        if logout is True:
            log.error(e)
        return False

# write text
def write_text (filename, data, logout = True):
    try:
        fid = open (filename,'w')
        fid.write('%s' % data.encode('utf-8'))
        fid.close()
        return True
    except Exception as e:
        if logout is True:
            log.error(e)
        return False


# write an object (list, set, dictionary) to a serialized file
def write_obj (filename, data, logout = True):
    try:
        cPickle.dump(data, open(filename, 'wb'))
        return True
    except Exception as e:
        if logout is True:
            log.error(e)
        return False


def readCol(fileName, colNum, sep='\t', header='F'):
    """

    :param fileName:
    :param colNum: from 1~
    :param sep: default as \t
    :param header: default as F
    """
    try:
        fh = open(fileName,'r')
        if header == 'T':
            fh.readline()
        lines = fh.readlines()
        col = [i.replace('\n', '').split(sep)[colNum-1] for i in lines]
        fh.close()
        return col
    except IOError:
        return False


#readCol('../data/preparation/allCDE', 1)





