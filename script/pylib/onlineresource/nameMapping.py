__author__ = 'mahandong'
import os, sys
from ..util.file import *
from ..util.data import *
from ..util import web as web
import xml.etree.ElementTree as xml_parser
import csv

def mappingMarketingDate_fromDrugList(targetDrugList, fh_ndc_source):

    targetDrugType = 'HUMAN PRESCRIPTION DRUG' #all upper case

    #keys are names from candiDrugList~sales info
    #values are names from ndc
    manuallyAssertEqualDic={
'catapres' : 'catapres-tts',
'catapres-tts' : 'catapres',
'ventolin' : 'ventolin hfa',
'ventolin hfa' : 'ventolin',
'zovirax' : 'zovirax topical',
'zovirax topical' : 'zovirax',
'percocet' : 'percocet-10',
'percocet-10' : 'percocet',
'ultram' : 'ultram er',
'ultram er' : 'ultram',
'entocort' : 'entocort ec',
'entocort ec' : 'entocort',
'duac' : 'duac care system',
'duac care system' : 'duac',
'atorvastatin' : 'amlodipine and atorvastatin',
'amlodipine and atorvastatin' : 'atorvastatin',
'imitrex' : 'imitrex inj',
'imitrex inj' : 'imitrex',
'budeprion' : 'budeprion sr',
'budeprion sr' : 'budeprion',
'pulmicort' : 'pulmicort turbuhaler',
'pulmicort turbuhaler' : 'pulmicort',
'advair' : 'advair hfa',
'advair hfa' : 'advair',
'toprol' : 'toprol xl',
'toprol xl' : 'toprol',
'acetaminophen' : 'acetaminophen/hydrocodone',
'acetaminophen/hydrocodone' : 'acetaminophen',
'sprintec' : 'tri-sprintec',
'tri-sprintec' : 'sprintec',
'effexor' : 'effexor xr',
'effexor xr' : 'effexor',
'focalin' : 'focalin xr',
'focalin xr' : 'focalin',
'dextroamphetamine' : 'amphetamine/dextroamphetamine',
'amphetamine/dextroamphetamine' : 'dextroamphetamine',
'kapidex' : 'dexilant/kapidex',
'dexilant/kapidex' : 'kapidex',
'coumadin' : 'coumadin tabs',
'coumadin tabs' : 'coumadin',
'ciprodex' : 'ciprodex otic',
'ciprodex otic' : 'ciprodex',
'lamisil' : 'lamisil oral',
'lamisil oral' : 'lamisil',
'ditropan' : 'ditropan xl',
'ditropan xl' : 'ditropan',
'humulin' : 'humulin n',
'humulin n' : 'humulin',
'adderall' : 'adderall xr',
'adderall xr' : 'adderall',
'premarin' : 'premarin vaginal',
'premarin vaginal' : 'premarin',
'tussionex' : 'tussionex pennkinetic',
'tussionex pennkinetic' : 'tussionex',
'proair' : 'proair hfa',
'proair hfa' : 'proair',
'donepezil' : 'donepezil hydrochloride',
'donepezil hydrochloride' : 'donepezil',
'ketek' : 'ketek pack',
'ketek pack' : 'ketek',
'inderal' : 'inderal la',
'inderal la' : 'inderal',
'yasmin' : 'yasmin 28',
'yasmin 28' : 'yasmin',
'miacalcin' : 'miacalcin nasal',
'miacalcin nasal' : 'miacalcin',
'vancocin' : 'vancocin hcl',
'vancocin hcl' : 'vancocin',
'augmentin' : 'augmentin xr',
'augmentin xr' : 'augmentin',
'travatan' : 'travatan z',
'travatan z' : 'travatan',
'wellbutrin' : 'wellbutrin sr',
'wellbutrin sr' : 'wellbutrin',
    }

    candiDrugDateDic = {}
    print 'Mapping '+ targetDrugType
    ndc = None
    try:
        ndc,name = read_csv(fh_ndc_source)
    except IOError:
        print "input file problem"
    ndcDrugDateDic = {}
    #NDC file to dic, select drug type
    if len(ndc)>1:
        for entry in ndc:
            if entry[2].upper() == targetDrugType: #only select 'HUMAN PRESCRIPTION DRUG'
                drug = entry[3].lower()
                marketDate = int(entry[8]) #YYYYMMDD
                #company = entry[12]
                if drug in ndcDrugDateDic:
                    if marketDate >= ndcDrugDateDic[drug]: #choose the earliest approved date  # and
                        continue
                    else:
                        ndcDrugDateDic[drug] = marketDate
                else:
                    ndcDrugDateDic[drug] = marketDate
    else:
        print 'NDC file empty'
        ifContinue()

    matched, unsure, unmatched = compareTwoNameLists(targetDrugList,ndcDrugDateDic.keys(), manuallyAssertEqualDic)

    for matchedDrug in matched.keys():
        if matchedDrug in ndcDrugDateDic:
            candiDrugDateDic[matchedDrug] = ndcDrugDateDic[matchedDrug]
        elif matchedDrug in manuallyAssertEqualDic and manuallyAssertEqualDic[matchedDrug] in ndcDrugDateDic:
            candiDrugDateDic[matchedDrug] = ndcDrugDateDic[manuallyAssertEqualDic[matchedDrug]]

    return  candiDrugDateDic