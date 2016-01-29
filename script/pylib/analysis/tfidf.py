__author__ = 'mahandong'
import pickle
from pylib.onlineresource.ctgov import *
from pylib.onlineresource.sales import *
from pylib.onlineresource.pdr import *
import itertools
import signal
import string
import collections
from pylib.db.findTrials import *
from pylib.db.writeTrainingDB import *
from pylib.util.file import *
from pylib.util.stat import *
from pylib.analysis.preprocessing import *
from pylib.util.web import *
import MySQLdb as mdb
from stat_parser import Parser
import os
import sys
import math
import csv
import urllib2
from lxml import etree
import urllib
from bs4 import BeautifulSoup
import time
import nltk
from pylib.onlineresource.extractXML import *
import numpy as np
from nltk.corpus import stopwords


#return format is a list, with drug names as keys and number of occurrence in all the years as values
#ex: {'diovan': 8, 'epipen': 2, 'biaxin xl': 4, 'cellcept': 8, 'nuvaring': 5...}
def drugList_occursAtLeastOnceInAllYears():
    path = '../result/topSales/'
    dirList=os.listdir(path)
    if len(dirList)<2:
        print 'Warning! Too little data in directory: ', path
        continueOrNot()
    yearInRecord = 0
    drugList = {}

    for file in dirList:
        if os.path.isfile(path+file) and not file.startswith('.'):
            year = file.split('_')[1]
            yearInRecord += 1
            content = ''
            try:
                content = open(path+file,'r').read().rstrip().split('\n')
            except Exception:
                print 'error 37'
                print Exception
            for line in content:
                    drugName = line.split('\t')[1].lower()
                    if drugName in drugList.keys():
                        drugList[drugName].append(int(year))
                    else:
                        drugList[drugName] = [int(year)]
    for drug in drugList.keys():
        drugList[drug] = str(sorted(drugList[drug]))
    print 'The number of years with SALES record is: ', yearInRecord, "years"
    return drugList


#input format is drug array
#output format is 2 drug lists (bbw, robust) with BBW info as values
def drugList_findBBWInfo(drugList, detailInfo = True):
    path = '../result/PDR/'
    targetFile = 'drugLabelContent'
    content = ''
    drugList_withBBWInfo = {}
    drugList_robust = {}
    ###
    manuallyAssertEqualDic={
    'taclonex-topical-suspension':'taclonex',
    'exelon-patch':'exelon',
    'zomig-zomig-zmt':'zomig',
    'clobex-spray':'clobex',
    'prempro-premphase':'prempro',
    'flovent-hfa':'flovent',
    'tobradex-st':'tobradex',
    'differin-lotion':'differin',
    'norvir-oral-solution-and-tablets':'norvir',
    'prilosec-otc':'prilosec',
    'nexium-iv':'nexium',
    'xalatan-ophthalmic-solution-single-bottle':'xalatan',
    'zithromax-for-injection':'zithromax',
    'depakote-tablets':'depakote',
    'dovonex-scalp-solution':'dovonex',
    'sporanox-oral-solution':'sporanox',
    'zofran-odt-orally-disintegrating-tablets-oral-solution-and-tablets':'zofran',
    'androgel-162':'androgel',
    'bactroban-ointment':'bactroban',
    'boniva-tablets':'boniva',
    'xopenex-inhalation-solution-concentrate':'xopenex',
    'lidoderm-patch':'lidoderm',
    'ddavp-tablets':'ddavp',
    'protonix-iv':'protonix',
    'aviane-28':'aviane',
    'asacol-hd':'asacol',
    'keppra-xr':'keppra',
    'premarin-tablets':'premarin tabs',
    'fentanyl-citrate':'fentanyl oral citrate',
    'duac':'duac care system',
    'humulin-70-30':'humulin 70/30',
    'humalog':'humalog kwikpen',
    'ortho-tri-cyclen-ortho-cyclen':'ortho tri-cyclen lo',
    'ortho-evra':'ortho evra',
    'humalog-mix75-25':'humalog mix 75/25 pen',
    'vancocin-hydrochloride':'vancocin hcl',
    'effexor-xr':'effexor xr',
    'inderal-la':'inderal la',
    'advair-diskus':'advair diskus',
    'allegra-d-12-hour':'allegra-d 12 hour',
    'budeprion-xl':'budeprion xl',
    'premarin-vaginal-cream':'premarin vaginal',
    'invega-sustenna':'invega sustenna',
    'augmentin-xr':'augmentin xr',
    'actoplus-met':'actoplus met',
    'miacalcin-nasal-spray':'miacalcin nasal',
    'ultram-er':'ultram er',
    'diovan-hct':'diovan hct',
    'ciprodex':'ciprodex otic',
    'adderall-xr':'adderall xr',
    'rhinocort-aqua':'rhinocort aqua',
    'lantus':'lantus solostar',
    'seroquel-xr':'seroquel xr',
    'percocet':'percocet-10',
    'glucotrol-xl':'glucotrol xl',
    'proventil-hfa':'proventil hfa',
    'pulmicort-respules':'pulmicort respules',
    'combivent-respimat':'combivent respimat',
    'lamisil-tablets': 'lamisil oral',
    'wellbutrin-sr':'wellbutrin sr',
    'fosamax-plus-d':'fosamax plus d',
    'coumadin':'coumadin tabs',
    'allegra-d-24-hour':'allegra-d 24 hour',
    'vivelle-dot':'vivelle dot',
    'micardis-hct':'micardis hct',
    'prevnar-13':'prevnar 13',
    'proair-hfa':'proair hfa',
    'xopenex-hfa':'xopenex hfa',
    'maxalt':'maxalt mlt',
    'travatan-z':'travatan z',
    'serevent-diskus':'serevent diskus',
    'nasacort-allergy-24hr':'nasacort aq',
    'imitrex-injection': 'imitrex inj',
    'entocort-ec':'entocort ec',
    'alphagan-p':'alphagan p',
    'childrens-zyrtec-syrup':'zyrtec syrup',
    'ketek':'ketek pack',
    'yasmin':'yasmin 28',
    'dexilant':'dexilant/kapidex',
    'geodon':'geodon oral',
    'glucophage-glucophage-xr':'glucophage xr',
    'detrol-la':'detrol la',
    'paxil-cr':'paxil cr',
    "exelon-patch":'exelon patch',
    'budeprion-sr':'budeprion sr',
    'risperdal-consta':'risperdal consta',
    'imitrex-tablets':'imitrex oral',
    'novolog-mix-70-30':'novolog mix 70/30',

    }
    ###
    try:
        content = open(path+targetFile,'r').read().rstrip().split('\n')
    except Exception:
        print 'error 61'
        print Exception
    if len(content)>1:
        totalUnmatchedDrug = []
        totalUnsureDrug = []
        totalMatchedDrug=[]
        unsurePair = {}
        for currentDrug in drugList:
            findFlag = 0
            unsureFlag = 0
            for line in content:
                line = line.split('\t')
                drugName = line[0].lower()
                BBW_Flag = line[2]
                text = line[3]
                if drugName == currentDrug or drugName == currentDrug.replace(' ', '-'):
                    findFlag = 1
                elif drugName.find(currentDrug) >= 0:
                    unsureFlag = 1
                    unsurePair[currentDrug] = ':'.join(["\'"+drugName+"\'", "\'"+currentDrug+"\',"])
                    totalUnsureDrug.append(currentDrug)

                if findFlag == 1:
                    if currentDrug in drugList_withBBWInfo.keys():
                        print 'Warning135: find duplicate drug entries in PDR database for drug:', currentDrug
                        continueOrNot()
                    else:
                        totalMatchedDrug.append(currentDrug)
                        if int(BBW_Flag) == 1:
                            drugList_withBBWInfo[currentDrug] = text
                        if int(BBW_Flag) == 0:
                            drugList_robust[currentDrug] = text
                    break

                if 1:
                    if drugName in manuallyAssertEqualDic.keys() and currentDrug == manuallyAssertEqualDic[drugName]:
                        unsureFlag = 0
                        findFlag = 1
                        if currentDrug in drugList_withBBWInfo.keys():
                            print 'Warning148: find duplicate drug entries in PDR database for drug:', currentDrug
                            continueOrNot()
                        else:
                            totalMatchedDrug.append(currentDrug)
                            if int(BBW_Flag) == 1:
                                drugList_withBBWInfo[currentDrug] = text
                            if int(BBW_Flag) == 0:
                                drugList_robust[currentDrug] = text
                        break


            if findFlag == 0 and unsureFlag == 0:
                totalUnmatchedDrug.append(currentDrug)
                #print "can not match drug: ", currentDrug, 'in PDR database'
        totalUnsureDrug = set(totalUnsureDrug)
        totalUnmatchedDrug = set(totalUnmatchedDrug)
        totalMatchedDrug = set(totalMatchedDrug)
        print 'Out of ', len(drugList), 'drugs, ',len(totalMatchedDrug), ' matched (',len(totalUnsureDrug)-len(totalUnsureDrug-totalMatchedDrug), 'find inexact matches and are manually asserted true), ', len(totalUnmatchedDrug), ' can not be found any matches. ',len(totalUnsureDrug-totalMatchedDrug),' are still unsure/rejected'
        print 'Out of ', len(totalMatchedDrug), ' matched drugs, ', len(set(drugList_withBBWInfo.keys())), 'are BBW drugs; ', len(set(drugList_robust.keys())), ' are robust drugs'
        if detailInfo:
            print "Unmatched Drugs: ", totalUnmatchedDrug
            print "Matched Drugs: ", totalMatchedDrug
            print "#####"
            print "There are ",len(totalUnsureDrug-totalMatchedDrug),"unsure drugs that is not covered: ", list(totalUnsureDrug-totalMatchedDrug)
            for i in list(totalUnsureDrug-totalMatchedDrug):
                print unsurePair[i]
            print "Add the pairs that you think refers to the same drug to the manuallyAssertEqualDic"
            print "#####"
        return drugList_withBBWInfo, drugList_robust
    else:
        print 'Warning: no content in file', targetFile
        continueOrNot()


#period 0:pre; 1:post
#return format list:drug as keys and ctlist as values
def getCTListWithPeriodFromDrugList_local(drugList, period=0):
    ctList = {}
    for drug in drugList:
        try:
            ct = findCTListByDrug_local(fh_ctgov_drugTrialList_tab,drug, period)
            if len(ct) > 0:
                ctList[drug] = ct
            else:
                ctList[drug] = ''
        except Exception:
            print 'Warning 118: '
    return ctList


#input format is two drug-trialList list like this: {'arimidex': '', 'zyrtec-d': '', 'singulair': '', 'pentasa': ['NCT00545740', 'NCT00751699'],...}
#return two same format lists with eligible entries
def selectEligibleDrugs(ctList_pre, ctList_post, lowerLimitPerPeriod):
    a_pre, tmp1 = table(ctList_pre)
    a_post, tmp2 = table(ctList_post)
    eliDrugList_pre = {}
    eliDrugList_post = {}
    drugSet = set(a_pre.keys())
    drugSet.union(set(a_post.keys()))
    discardDrug = []
    for drug in drugSet:
        if a_pre[drug] > lowerLimitPerPeriod and a_post > lowerLimitPerPeriod:
            if drug in eliDrugList_pre.keys() or drug in eliDrugList_post.keys():
                print 'warning: possible wrong input list, need further analysis codes'
                sys.exit()
            eliDrugList_post[drug] = ctList_post[drug]
            eliDrugList_pre[drug] = ctList_pre[drug]
        else:
            discardDrug.append(drug)
    print 'there are ', str(len(drugSet)), ' drugs in input lists, and ', str(len(discardDrug)),' are discarded! ', str(len(drugSet)-len(discardDrug)), ' remaining!'
    return eliDrugList_pre, eliDrugList_post


def saveSelectedList(target, outDir, varName):

    mkdir(outDir)
    fh_out = open(str(outDir)+varName, 'w')

    if isinstance(target,dict):
        for key in target.keys():
            if isinstance(target[key],tuple):
                target[key] = list(target[key])
            if isinstance(target[key],list):
                for ele in target[key]:
                    fh_out.write(key+'\t'+ele+'\n')
            if isinstance(target[key],dict):
                for ele in target[key].keys():
                    fh_out.write(key+'\t'+ele+target[key][ele]+'\n')
            if isinstance(target[key],str):
                fh_out.write(key+'\t'+target[key]+'\n')
    print 'variable List: '+varName+ ' successfully saved!'
    fh_out.close()


def extractComponentFromXML(ctList):

    CTXMLDict = retrieveCTXMLFromCTlist(ctList)
    print "##The length of the input queries is: ", str(len(ctList))
    print '##the length of the retrieved CT number(unique) is: ', str(len(CTXMLDict.keys()))
    CTCompDict = {}
    count = 0
    for key in CTXMLDict.keys():
        count+=1
        #print 'processing the ', count,' trials: ', key
        try:
            (id, brief_title, official_title, conditions, agency, agency_class, source, authority, brief_summary, overall_status, start_date, gender, minimum_age, maximum_age, study_pop, criteria, enrollment, phase, study_type, location, intervention_type, intervention_name, enrollment_type) = extract_component(CTXMLDict[key])

        except Exception:
            print Exception
            print 'skip ', key
            CTCompDict[key] = ''
            continue
        if not criteria.startswith('Please contact site') and criteria.strip() !='' and len(enrollment)>0 :#refinement!
            CTCompDict[key] = (id, brief_title, official_title, conditions, agency, agency_class, source, authority, brief_summary, overall_status, start_date, gender, minimum_age, maximum_age, study_pop, criteria, enrollment, phase, study_type, location, intervention_type, intervention_name, enrollment_type)
        else:
            CTCompDict[key] = ''

    return CTCompDict

#for timeout use
class Timeout():
  """Timeout class using ALARM signal"""
  class Timeout(Exception): pass

  def __init__(self, sec):
    self.sec = sec

  def __enter__(self):
    signal.signal(signal.SIGALRM, self.raise_timeout)
    signal.alarm(self.sec)

  def __exit__(self, *args):
    signal.alarm(0) # disable alarm

  def raise_timeout(self, *args):
    raise Timeout.Timeout()



def extractComponentFromXML_parse(ctList, instantSaver, doParsing = False):

    if not os.path.exists(os.path.dirname(instantSaver)):
        mkdir(instantSaver)

    tmpSaver = open(instantSaver, 'ab+')
    tmpSaver.seek(0, os.SEEK_SET) ###
    # current content in tmp file (cache)
    alreadyHere = []
    tmpContent ={}
    lostList = []
    for i in tmpSaver:
        try:
            #print re.search('^\|(NCT\d+)\|',i).group(1)
            NCTid = re.search('^\|(NCT\d+)\|',i).group(1)
            tmpContent[NCTid] = i.rstrip().split('|')[1:]
            alreadyHere.append(NCTid)
        except AttributeError:
            print 'Instant saved file error: format error'
            if continueOrNot():
                continue
    print 'NCT already in tmp file: ', len(alreadyHere)
    #entries requires to be searched online
    needOnlineSearch = list(set(ctList)-set(alreadyHere))
    print 'NCT requires a online search: ', len(needOnlineSearch)

    CTXMLDict = retrieveCTXMLFromCTlist(needOnlineSearch)  # do not have all keys in ctList if (1. exists a local data; 2. can not retrieve web page)
    print "##The length of the input ctList (unique) is: ", str(len(set(ctList)))

    CTCompDict = {}
    count = 0
    for key in ctList:  # only entry in list will be returned, tmp file may contain extra entries, make sure to use the out file for analysis
        #retrieve locally
        tmp = []
        if key in alreadyHere:
            #print 'Pass (retrieved locally): ', key
            CTCompDict[key] = tmpContent[key]
            continue
        count += 1
        #retrieve online
        if key in CTXMLDict.keys():
            try:
                CTCompDict[key] = extract_component(CTXMLDict[key])
                tmp = list(extract_component(CTXMLDict[key]))
            except Exception:
                print str(Exception)+'skip ', key
                continue
        else:
            lostList.append(key)

        '''
        ##parsing, slow
        (id, brief_title, official_title, conditions, agency, agency_class, source, authority, brief_summary, overall_status, start_date, gender, minimum_age, maximum_age, study_pop, criteria, enrollment, phase, study_type, location, intervention_type, intervention_name, enrollment_type) = CTCompDict[key]

        if not criteria.startswith('Please contact site') and criteria.strip() !='' and len(enrollment)>0 :#refinement!
            if doParsing:

                rules = []
                try:
                    print 'parsing: ',id
                    flaggedCri = setFlag(criteria)
                    for inc in flaggedCri[0]:
                        try:
                            with Timeout(60):
                                parsed = parse_stat_sentence(inc, None, True)
                                rules.append(parsed)
                        except Timeout.Timeout:
                            print 'skip a lone sentence in trial: ', id
                            continue
                        #print 'inc', parse_stat_sentence(inc, None, True)
                    for exc in flaggedCri[1]:
                        try:
                            with Timeout(60):
                                negate = ['*NEGATE* ' + str(i) for i in parse_stat_sentence(exc, None, True)]
                                rules.append(negate)
                        except Timeout.Timeout:
                            print 'skip a lone sentence in trial: ', id
                            continue
                        #print 'exc', negate
                    rules = list(itertools.chain(*rules))
                    #print rules
                except Exception:
                    print Exception, 'parsing error at ', id
                    continue
                CTCompDict[key] = (id, brief_title, official_title, conditions, agency, agency_class, source, authority, brief_summary, overall_status, start_date, gender, minimum_age, maximum_age, study_pop, criteria, enrollment, phase, study_type, location, intervention_type, intervention_name, enrollment_type, rules)
                tmp = [id, brief_title, official_title, conditions, agency, agency_class, source, authority, brief_summary, overall_status, start_date, gender, minimum_age, maximum_age, study_pop, criteria, enrollment, phase, study_type, location, intervention_type, intervention_name, enrollment_type, rules]

        '''
        if len(tmp) > 0:
            try:
                all = ''
                for i in tmp:
                    all += "|"+str(i)
                tmpSaver.write(all+'\n')
            except Exception:
                print 'instant saver error: can not write file at:', id
                sys.exit()
    tmpSaver.close()
    print '##the length of the retrieved ctList(unique) is: ', str(len(CTCompDict.keys()))
    print '##the length of the lost ctList(unique) is: ', str(len(set(lostList)))
    return CTCompDict


def saveComp(targetDict, outDir, varName):
    try:
        file = outDir+varName
        print varName+'result saves to: '+ file
        write_csv(file, targetDict.values())
    except:
        print 'error writing csv, trying to store in temp file...'
        try:
            f = open(outDir+varName+'.pckl', 'w')
            pickle.dump(targetDict, f)
            f.close()
            print 'pickled: '+ varName
        except:
            print 'failed in storing '+ varName


#ref: http://www.cs.duke.edu/courses/spring14/compsci290/assignments/lab02.html
import nltk
import string
import os
import numpy as np
from sklearn.feature_extraction.text import TfidfVectorizer
from nltk.stem.porter import PorterStemmer
from sklearn.metrics.pairwise import linear_kernel

def stem_tokens(tokens, stemmer):
    stemmed = []
    for item in tokens:
        stemmed.append(stemmer.stem(item))
    return stemmed

def tokenize(text):
    #remove stopwords
    stemmer = PorterStemmer()
    from nltk.corpus import stopwords
    stopwords = stopwords.words('english')
    tokens = nltk.word_tokenize(text)
    token_removeStop = [i for i in tokens if i not in stopwords]
    #stemming
    stems = stem_tokens(token_removeStop, stemmer)
    return stems

#http://stats.stackexchange.com/questions/29578/jensen-shannon-divergence-calculation-for-3-prob-distributions-is-this-ok
def jsd(x,y): #Jensen-shannon divergence
    import warnings
    warnings.filterwarnings("ignore", category = RuntimeWarning)
    x = np.array(x)
    print x
    y = np.array(y)
    print y
    d1 = x*np.log2(2*x/(x+y))
    print d1
    d2 = y*np.log2(2*y/(x+y))
    print d2
    d1[np.isnan(d1)] = 0
    d2[np.isnan(d2)] = 0
    print sum(d1)
    print sum(d2)
    d = 0.5*np.sum(d1+d2)
    return d
#jsd(np.array([0.5,0.5,0]),np.array([0,0.1,0.9]))

def kld( p, q):
    from numpy import zeros, array
    from math import sqrt, log
    """ Compute KL divergence of two vectors, K(p || q)."""
    return sum(_p * log(_p / _q) for _p, _q in zip(p, q) if _p != 0)


#a = dict(zip(feature_names, corpusList))



#str = 'this sentence has unseen text such as computer but also king lord juliet'
#response = tfidf.transform([str])
#for col in response.nonzero()[1]:
#    print feature_names[col], ' - ', response[0, col]

'''
from nltk.stem.porter import PorterStemmer

def stem_tokens(tokens, stemmer):
    stemmed = []
    for item in tokens:
        stemmed.append(stemmer.stem(item))
    return stemmed

def tokenize(text):
    tokens = nltk.word_tokenize(text)
    stems = stem_tokens(tokens, stemmer)
    return stems

source = '/Users/mahandong/Dropbox/research/chunhua project/EliTES/result/selected_drug_trial_List/backup/ctList_BBW_post_comp'
fhIn = open(source,'r')
stemmer = PorterStemmer()

token_dict = []

for line in fhIn:
    if line.split('\",\"')[15] != "":
        criteria = line.split('\",\"')[15]
        lowers = criteria.lower()
        no_punctuation = lowers.translate(None, string.punctuation)
        token_dict.append(no_punctuation)

from sklearn.feature_extraction.text import CountVectorizer
vectorizer = CountVectorizer(min_df=1)
matrix = vectorizer.fit_transform(token_dict).toarray()

feature_names = [x.encode('ascii') for x in vectorizer.get_feature_names()]
corpusList = matrix.sum(axis=0)
corpusTotal = sum(corpusList)

row=1

# new_corpusList = []
# new_rowNormList = []
# new_featureNames = []
# rowNormList = matrix[row]
# for i in range(len(rowNormList)):
#     if rowNormList[i] != 0:
#         new_corpusList.append(corpusList[i])
#         new_rowNormList.append(rowNormList[i])
#         new_featureNames.append(feature_names[i])

#dictionary = dict(zip(feature_names, countList))
'''