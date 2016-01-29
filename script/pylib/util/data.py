__author__ = 'mahandong'

import os
import re
from file import *
from log import strd_logger
import numpy


def combineFiles(listOfPathAndFiles,outFilePathAndName, combine=1, header=0):
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


#have separated lists with same set of IDs, want to combine them and give entries in each list a unique tag
#BBW_LIST[DRUG_ID] and ROBUST_LIST[DRUG_ID] ==> COMBINED_LIST[DRUG_ID] = 'BBW'/'ROBUST'
#input is a combined dic = {"tag name 1":[list1],"tag name 2":[list2]}
#return a dic dic[drug id] = tag
def taggingMultipleLists(combinedContentDic, duplicatedTags=True):

    all_ID =[]
    all_content = {}
    for tag in combinedContentDic.keys():
        all_ID.extend(combinedContentDic[tag])
    all_ID_set = set(all_ID)
    for entry in all_ID_set:
        tag4entry = ''
        for tag in combinedContentDic.keys():
            if entry in combinedContentDic[tag]:
                if not duplicatedTags and tag4entry != '' and tag != tag4entry:
                        print 'entry '+ entry + ' has duplicated tags: ' + tag4entry + ' and ' + tag
                        exit(0)
                tag4entry += tag
        all_content[entry] = tag4entry
    return all_content


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


def sepCol2List(dirName, targetFilename, columnNum, outFilePathandName, combine=1, header=1):
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
#sepCol2List(dataRootPath,targetFile,2,"../result/alLFreq", 0)


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
                    fhIn = read_csv(pathAna + targetFileName, "F")  ##changed from readCSV to read_csv without check
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


def compareTwoNameLists(nameList1, nameList2, manuallyAssertEqualDic=None, detailInfo = True):

    ###
    if not manuallyAssertEqualDic:
        manuallyAssertEqualDic = {}

    if len(nameList2)>1:
        totalUnmatched = []
        totalUnsure = []
        totalMatched=[]
        unsurePair = {}
        matchedPair = {}
        for currentName1 in nameList1:
            findFlag = 0
            unsureFlag = 0
            for currentName2 in nameList2:
                if currentName2 == currentName1:
                    findFlag = 1
                elif currentName2.find(currentName1) >= 0:
                    unsureFlag = 1
                    unsurePair[currentName1] = currentName2
                    totalUnsure.append(currentName1)
                elif currentName1.find(currentName2) >= 0:
                    unsureFlag = 1
                    unsurePair[currentName2] = currentName1
                    totalUnsure.append(currentName2)

                if findFlag == 1:
                    totalMatched.append(currentName1)
                    matchedPair[currentName1] = currentName2
                    break

                if 1:
                    if currentName2 in manuallyAssertEqualDic.keys() and currentName1 == manuallyAssertEqualDic[currentName2]:
                        unsureFlag = 0
                        findFlag = 1
                        if currentName2 in totalUnsure:
                            totalUnsure.remove(currentName2)
                        totalMatched.append(currentName1)
                        matchedPair[currentName1] = currentName2
                        break

            if findFlag == 0 and unsureFlag == 0:
                totalUnmatched.append(currentName1)
                #print "can not match drug: ", currentName1, 'in PDR database'
        totalUnsure = set(totalUnsure)
        totalUnmatched = set(totalUnmatched)
        totalMatched = set(totalMatched)
        print 'Out of ', len(nameList1), 'entries in nameList1, ',len(totalMatched), ' matched, some inexact matches are manually assert true, ', len(totalUnmatched), ' can not be found any matches. ',len(totalUnsure-totalMatched),' are still unsure/rejected'
        if detailInfo:
            print "Unmatched Drugs: ", totalUnmatched
            print "Matched Drugs: ", totalMatched
            print "#####"
            print "There are ",len(totalUnsure-totalMatched),"unsure entries that is not covered: ", list(totalUnsure-totalMatched)
            for i in list(totalUnsure-totalMatched):
                print '\''+i+'\' : \''+unsurePair[i]+'\','
                print '\''+unsurePair[i]+'\' : \''+i+'\','
            print "Add (copy-paste) the pairs that you think refers to the same entry to the manuallyAssertEqualDic"
            print "#####"
        return matchedPair, unsurePair, totalUnmatched
    else:
        print "nothing in namelist2"
        continueOrNot()


#input is a Dic = {"col name1":{"id":content1...},"col name2":{"id":content2...}}
def saveMultipleDictionariesToTable(combinedContentDic, outFilePathAndName, intersectionOnly=False, sep='\t',naFormat='NA',idName='ID'):

    fhOut = outFilePathAndName
    if checkFileIfContinue([fhOut]):
        print 'program is still running...'
    else:
        print 'file exists, should exit here'

    fhOut_file = open(fhOut, 'w')

    colNames = combinedContentDic.keys()

    fhOut_file.write(idName+sep)
    fhOut_file.write(sep.join(colNames)+'\n')


    all_id_set = set()
    all_id = []
    if not intersectionOnly:
        for col in colNames:
            all_id.extend(combinedContentDic[col].keys())
            all_id_set = set(all_id)
    else:
        for col in colNames:
            if len(all_id_set) == 0:
                all_id_set = set(combinedContentDic[col].keys())
            else:
                all_id_set = all_id_set.intersection(combinedContentDic[col].keys())

    all_content = {}
    for uniqueID in all_id_set:
        line = [uniqueID]
        for colName in colNames:
            if uniqueID in combinedContentDic[colName]:
                line.append(str(combinedContentDic[colName][uniqueID]))
            else:
                line.append(str(naFormat))
        all_content[uniqueID] = line
        fhOut_file.write(sep.join(line)+'\n')
    fhOut_file.close()
    print 'file successfully saved to '+ outFilePathAndName
    return all_content