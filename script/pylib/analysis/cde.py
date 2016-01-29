__author__ = 'mahandong'
var = """
The functions in the package serves as the preparation stage for the project of semantic network.
The function should only be run once!
The output file serves as the basis for further analysis. Don't move or change manually.
"""

import os
import re
import csv
from ..util.log import strd_logger
from ..util.file import *
#from ..util.data import *
import shutil

global log
log = strd_logger('file')

#simple append the content of files, make sure the format is the same
def appendFiles(listOfPathAndFiles, outFilePathAndName, header2Jump=0):
    try:
        if os.path.exists(outFilePathAndName):
            os.remove(outFilePathAndName)
            print ("file removed:%s" % outFilePathAndName)
        fhOut = open(outFilePathAndName,'w')
    except IOError:
        return False
    for file in listOfPathAndFiles:
        if os.path.exists(file):
            try:
                fhIn = open(file, 'r').readlines()
                jumped = 0
                for line in fhIn:
                    if jumped < header2Jump:
                            jumped += 1
                            continue
                    fhOut.write(line.strip()+'\n')
            except Exception as e:
                log.error(e)
                return False
    fhOut.close()

#csv input and output; unknown length of header; the last col is list, need to sep by '-'
def appendFiles1(listOfPathAndFiles, outFilePathAndName, header2Jump=0, elementNumPerLine=0):
    try:
        if os.path.exists(outFilePathAndName):
            os.remove(outFilePathAndName)
            #print ("file removed:%s" % outFilePathAndName)
        #fhOut = open(outFilePathAndName,'w')
        doc = csv.writer(open(outFilePathAndName, 'wb'), delimiter=',', quotechar='"', quoting=csv.QUOTE_ALL)
        doc.writerow(['CDEs', 'Frequency (0-1 normalized)', 'UMLS Semantic Types'])
    except IOError:
        return False
    for file in listOfPathAndFiles:
        if os.path.exists(file):
            try:
                fhIn = readCSV(file)
                jumped = 0
                omit = 0
                for line in fhIn:
                    if jumped < header2Jump:
                        jumped += 1
                        continue
                    if elementNumPerLine > 0:
                        if len(line) != elementNumPerLine:
                            omit += 1
                            continue
                    line[elementNumPerLine-1] = re.sub('body part, organ, or organ component', 'body part organ or organ component',line[elementNumPerLine-1])#get rid of the f**king outlier
                    line[elementNumPerLine-1] = re.sub('\', \'',' - ',line[elementNumPerLine-1])
                    line[elementNumPerLine-1] = str(line[elementNumPerLine-1]).strip('[]')
                    line[elementNumPerLine-1] = str(line[elementNumPerLine-1]).strip('\'\'')
                    doc.writerow(line)
                    #[fhOut.write("%s," % l) for l in line]
                    #fhOut.write('\n')
                if omit > 0:
                    print('omit Number of lines: %s in file: %s' % (omit, file))
            except Exception as e:
                log.error(e)
                return False
    #fhOut.close()


def combineFiles(listOfPathAndFiles, outFilePathAndName, combine=1, header=0):
    try:
        if os.path.exists(outFilePathAndName):
            os.remove(outFilePathAndName)
            print ("file removed:%s" % outFilePathAndName)
        fhOut = open(outFilePathAndName,'w')
    except IOError:
        return False
    allSet = set()
    allList = []
    for file in listOfPathAndFiles:
        if os.path.exists(file):
            try:
                fhIn = open(file, 'r')
                if combine == 1:
                    if header == 1:
                        fhIn.readline()
                        header = 0
                    for lines in fhIn:
                        allSet.add(lines.strip())
                if combine == 0:
                    if header == 1:
                        fhIn.readline()
                        header=0
                    for lines in fhIn:
                        allList.append(lines.strip())
                elif combine != 1 & combine != 0:
                    print ("combine or not, input:%s\n" % combine)
                    return False
            except Exception as e:
                log.error(e)
                return False
    if combine == 1:
        for classes in sorted(allSet):
            fhOut.write('%s\n' % (classes))  #            fhOut.write(classes+','+fileNames+"\n")
    elif combine == 0:
        for numbers in sorted(allList):
            fhOut.write('%s\n' % (numbers))  #            fhOut.write(numbers+','+fileNames+'\n')
    fhOut.close()
#combineFiles(target,'../result/allST_combine',1,0)

def cytoscapePrepare(file2modify, col2sep, lineSep='\t', repeatSep=" - ", sourceAdd="CT"):
    """
    :param file:
    default:combine
    """
    try:
        fhIn = open(file2modify,'r').readlines()
        all = []
        targetPath = os.path.dirname(file2modify)+'/cytoscape/'
        mkdir(targetPath)
        fhOut = open(targetPath+os.path.split(file2modify)[1]+'_'+sourceAdd, 'w')
        for line in fhIn:
            line = line.strip().split(lineSep)
            need = 0
            repeater = []
            holder = []
            for element in line:
                if element == line[col2sep-1]:
                    if re.findall(repeatSep, element):
                        need += 1
                        repeater = element.split(repeatSep)
                    else:
                        holder.append(element)
                else:
                    holder.append(element)
            if need == 0:
                holder.append(sourceAdd)
                for i in holder:
                    fhOut.write('%s\t' % i)
                fhOut.write('\n')
                all.append(holder)
            if need == 1:
                for i in repeater:
                    holder1 = holder[:]
                    holder1.append(i)
                    holder1.append(sourceAdd)
                    fhOut.writelines(["%s\t" % i for i in holder1])
                    fhOut.write('\n')
                    all.append(holder1)
            if need > 1:
                print "Error: contain too many places to separate: "
                return False
        return all

    except Exception as e:

        print ("error in sepLine2List: %s" % e.message)
        return False

#prepare combine table and node attribute
def cytoscapePrepare2(inPath,outPath):
    exuniq = cytoscapePrepare(inPath+'ex_top5', 5, '\t', ' - ', "EXUNIQUE")
    inuniq = cytoscapePrepare(inPath+'in_top5', 5, '\t', ' - ', "INUNIQUE")
    incomm = cytoscapePrepare(inPath+'intersection_sig.in', 5, '\t', ' - ', "INCOMMON")
    excomm = cytoscapePrepare(inPath+'intersection_sig.ex', 5, '\t', ' - ', "EXCOMMON")
    listFile = [outPath+'ex_top5_EXUNIQUE',outPath+'in_top5_INUNIQUE',outPath+'intersection_sig.ex_EXCOMMON',outPath+'intersection_sig.in_INCOMMON']
    appendFiles(listFile,outPath+'combine_table',0)
    fhIn = open(outPath+'combine_table','r').readlines()
    diseases = set()
    [diseases.add(lines.strip().split('\t')[1]) for lines in fhIn]
    cdes = set()
    [cdes.add(lines.strip().split('\t')[2]) for lines in fhIn]
    fhOut = open(outPath+'node_attribute','w')
    [fhOut.write(d+'\tDisease\n') for d in diseases]
    [fhOut.write(d+'\tCEF\n') for d in cdes]
    fhOut.close()

#prepare separated file for ST
def cytoscapePrepare3(tableFile, listST, outPath):
    mkdir(outPath)
    fhIn = open(tableFile,'r').readlines()
    for currentST in listST:
        fhOut = open(outPath+'table_'+currentST,'w')
        fhOut1 = open(outPath+'table_'+currentST+'_nodeAttribute','w')
        for line in fhIn:
            line = line.strip().split('\t')
            line[3] = str(abs(float(line[3])))
            if line[4] == currentST:
                fhOut1.write(str(line[1])+'\tDISEASE\n')
                fhOut1.write(str(line[2])+'\tCEF\n')
                [fhOut.write(lines+'\t') for lines in line]
                fhOut.write('\n')
        fhOut.close()
        fhOut1.close()


def dirname2list(dir, out):

    diseaseList = os.listdir(dir)
    try:
        fhOut = open(out,'w')
        number = 0
        for i in range(1,len(diseaseList)):
            if os.path.isdir(dir + '/' + diseaseList[i]):
                if re.findall('^.', diseaseList[i]):
                    next
                number += 1
                fhOut.write('%s\t%s\n' % (number, diseaseList[i]))
    except Exception as e:
        log.error (e)
        print("ERROR in function dirname2list:%s" % e)
        return None

def filename2list(dir, out):

    diseaseList = os.listdir(dir)
    try:
        fhOut = open(out,'w')
        number = 0
        for i in range(1,len(diseaseList)):
            if os.path.isfile(dir + '/' + diseaseList[i]):
                if re.findall('^.', diseaseList[i]):
                    next
                number += 1
                fhOut.write('%s\t%s\n' % (number, diseaseList[i]))
    except Exception as e:
        log.error (e)
        print("ERROR in function dirname2list:%s" % e)
        return None


def dirnameSimilarity(dir1, dir2, outPath):
#    dir1 = os.path.abspath(dir1)
#    dir2 = os.path.abspath(dir2)
#    outPath = os.path.abspath(outPath)
    mkdir(outPath)
    outPathAndFile = check_dir(outPath) + 'targetNameList'
    dirname2list(dir1, outPathAndFile+'1')
    nameDir1 = open(outPathAndFile+'1','r').readlines()
#    os.remove(outPathAndFile+'1')
    filename2list(dir2, outPathAndFile+'2')
    nameDir2 = open(outPathAndFile+'2','r').readlines()
#    os.remove(outPathAndFile+'2')
    names1 = [line.strip().split('\t')[1] for line in nameDir1]
    names2 = {os.path.splitext(line.strip().split('\t')[1])[0]:os.path.splitext(line.strip().split('\t')[1])[1] for line in nameDir2}

    for disease in names1:
        currentDir = check_dir(check_dir(dir1)+disease)
        outDir = check_dir(check_dir(outPath)+disease)
        targetMain = [tryName for tryName in names2.keys() if re.findall(disease, tryName)]
        [targetMain.append(tryName) for tryName in names2.keys() if re.findall(tryName, disease)]
        if len(targetMain):
            mkdir(outDir)
            originalFile = [str(check_dir(dir2))+str(main)+str(names2[main]) for main in targetMain]
            [shutil.copy(str(File),outDir) for File in originalFile]
        if len(targetMain):
            print("Full match Number: "+disease+': '+str(len(targetMain))+'\n')
    #["Partial match: "+match+'\n' for match in names1 for tryname in names2 if re.findall(match, tryname)]

#dirnameSimilarity(dataRootPath,"../data/pubmed",'../result/match')


def get_cde_st_table(listOfPathAndFiles, listOfColSelected, outFilePathAndName, combine=1, header=0, sep='\t'):
    try:
        if os.path.exists(outFilePathAndName):
            os.remove(outFilePathAndName)
            print ("file removed:%s" % outFilePathAndName)
        fhOut = open(outFilePathAndName,'w')
    except IOError:
        return False
    allSet = set()
    allList = []
    for file in listOfPathAndFiles:
        if os.path.exists(file):
            try:
                fhIn = open(file, 'r')
                if combine == 1:
                    if header == 1:
                        fhIn.readline()
                        header = 0
                    for lines in fhIn:
                        refine = []
                        lines = lines.strip().split(sep)
                        for col in listOfColSelected:
                            refine.append(lines[col-1])
                        allSet.add('\t'.join(refine))
                if combine == 0:
                    if header == 1:
                        fhIn.readline()
                        header=0
                    for lines in fhIn:
                        refine = []
                        lines = lines.strip().split(sep)
                        for col in listOfColSelected:
                            refine.append(lines[col-1])
                        allList.append('\t'.join(refine))
                elif combine != 1 & combine != 0:
                    print ("combine or not, input:%s\n" % combine)
                    return False
            except Exception as e:
                log.error(e)
                return False
    if combine == 1:
        for classes in sorted(allSet):
            fhOut.write('%s\n' % (classes))  #            fhOut.write(classes+','+fileNames+"\n")
    elif combine == 0:
        for numbers in sorted(allList):
            fhOut.write('%s\n' % (numbers))  #            fhOut.write(numbers+','+fileNames+'\n')
    fhOut.close()

def get_sep_col_2_list(dirName, targetFilename, columnNum, outFilePathandName, combine=1, header=1):
    """
    This function gets all features in different diseases(separated dir, same file name)
    input1: dir name
    input2: target file name
    input3: number of column want to catch (start from 1)
    input4: out file name
    input5: (optional) whether to combine (0,1)
    output:data/preparation/allCDE
    """

    print("Cautious: read CSV files as default! (if not, check ',' separator)\n")
    if re.findall('/$',dirName):
        dir = dirName
    else:
        dir = dirName + '/'
    target = targetFilename#

    fileNames = os.listdir(dir)
    allClass = set()
    allNumber = []
    for i in fileNames:
        if os.path.isfile(dir.join(fileNames)):
            print("there are unexpected files in data folder: omitted\n")
            next
        skip = header #header
        newPath = os.path.join(dir, i)
        newTarget = os.path.join(newPath,target)
        try:
            if os.path.exists(newTarget):
                fhIn = read_csv(newTarget)
                for line in fhIn:
                    if skip == 1:
                        skip = 0
                        continue
                    col = columnNum - 1
                    if combine == 0:
                        allNumber.append(line[col])
                    elif combine == 1:
                        allClass.add(line[col])
                    else:
                        print("error: combine or not?")

        except Exception as e:
            log.error(e)
            print("ERROR in function mapDiseaseName:%s" % e)
            return False
    if combine == 1:
        print('Combine or not: %s\t%s number in total: %s ' % (combine,outFilePathandName, len(allClass)))
    elif combine == 0:
        print('Combine or not: %s\t%s number in total: %s ' % (combine,outFilePathandName,len(allNumber)))
#output file
    outFileName = outFilePathandName
    try:
        fhOut = open(outFileName, 'w')
    except:
        print "open outfile error in get_sep_col_2_list\n"
        return False
    if combine == 1:
        for classes in sorted(allClass):
            fhOut.write('%s\n' % (classes))  #            fhOut.write(classes+','+fileNames+"\n")
    elif combine == 0:
        for numbers in sorted(allNumber):
            fhOut.write('%s\n' % (numbers))  #            fhOut.write(numbers+','+fileNames+'\n')
    fhOut.close()
#targetFile = "cde-exclusion.csv" #cde-inclusion.csv
#get_sep_col_2_list(dataRootPath,targetFile,3,"../result/allST")



def integrateSepFiles(dirName, targetFileName, outFilePath):
    print("Cautious: read CSV files as default! (if not, check ',' separator)\n")
    id = 0
    head = ""
    firstFile = True
    fhOut = open(outFilePath,'w')
    if re.findall('/$',dirName):
        dir = dirName
    else:
        dir = dirName + '/'
    for current in os.listdir(dirName):
        pathAna = dir + current + '/'
        if os.path.isdir(pathAna):
            if os.path.exists(pathAna+targetFileName):
                id += 1
                try:
                    fhIn = readCSV(pathAna + targetFileName, "F")
                    if not firstFile:
                        test = fhIn.pop(0)
                        if not test == head:
                            print("header in files disagree:%s!=%s\nfiles:%s\n" % (test, head,pathAna+targetFileName))
                            return False
                    if firstFile:
                        head = fhIn.pop(0)
                        fhOut.write("id\tname")
                        for j in head:
                            fhOut.write("\t%s" % j)
                        fhOut.write('\n')
                        firstFile = 0
                    for line in fhIn:
                        fhOut.write("%s\t%s" % (id,current))
                        for i in line:
                            fhOut.write("\t%s" % (i))
                        fhOut.write('\n')

                except IOError:
                    print("error in integrateSepFiles")
                    return False
    fhOut.close()
#integrateSepFiles(dataRootPath,targetFile,"../result/"+group+"_table")


Did2Name = {}
Name2Did = {}
def mapDiseaseName(diseaseNamePath):
    """
    :rtype : object
    :param D:
    :return: dictionaries:Did2Name, Name2Did
    """

    try:
        fhIn = open(diseaseNamePath, 'r')
        for line in fhIn:
            line = line.split('\t')
            Did2Name[line[0]] = line[1].rstrip()
            Name2Did[line[1].rstrip()] = line[0]
        return(Did2Name, Name2Did)
        fhIn.close()
    except Exception as e:
        log.error (e)
        print("ERROR in function mapDiseaseName:%s" % e)
        return False



def sepLine2List(file2sep, sep="-"):
    """

    :param file:
    default:combine
    """
    try:
        fhIn = open(file2sep,'r')
        all = set()
        for line in fhIn:
            if line == '':
                next()
            line = line.replace(sep,'\n').rstrip()
            line = line.split('\n')
            for i in line:
                all.add(i)
        fhIn.close()

        fhOut = open(file2sep+'_sep','w')
        for i in all:
            fhOut.write(i+'\n')
        fhOut.close()
        print("Entry number left: %s" % len(all))
    except:
        print "error in sepLine2List"
        return False

#sepLine2List("../result/allST"," - ")

