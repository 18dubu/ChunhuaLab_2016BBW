__author__ = 'mahandong'

"""
Project: EliTES (Eligibility criteria Tracking and Estimation System)

The program is reconstructed by Handong Ma (hm2588 at columbia dot edu) with previous work of Prof. Chunhua Weng's lab

The purpose of this project is to track the trend of clinical trial eligibility criteria and
1) assess trend shift and possible bias
2) try to estimate the future drug performance
3) generate the eligibility patients' profile and real-world patients' profile, find the gap between those two

All right reserved
No redistribute without consent or permission

Methods summary: # with existing MySQL database
query: return trial list [[CT1 for drug1,CT2 for drug 1...],[CT for drug2]...] with the search of a certain drug name list
    (from clinicaltrials.gov search engine, not from database)
extractRule/extractConcept
mappingUMLS
compareRules/compareConcepts/compareTrials
aggregateRules/aggregateConcepts

"""

import pylib.analysis.tfidf as tfidf
import pylib.onlineresource.nameMapping as map
import pylib.onlineresource.ctgov as ctgov
import pylib.util.data as data
import pylib.util.file as file
import pylib.onlineresource.sales as sales
import pylib.onlineresource.pdr as pdr
from parameters import *
import itertools


sampleDrug_pos = para_sampleDrug_pos
sampleDrug_bbw = para_sampleDrug_bbw
sampleDrug = sampleDrug_pos
samplePeriod = para_samplePeriod

#########################################################################################
###retrive drug-trial numbers based on clinicaltrials.gov search, for future selection, runtime: O(1h)
### based on NDC drug names, no mapping to drugs.com or PDR
if 0:
    ##original procedure, not used now
    ctgov.retrieveOnlineDrugTrialInfo(fh_ndc_source, fh_ctgov_drugTrialContent_csv, fh_ctgov_drugTrialContent_tab, fh_ctgov_drugTrialList_tab, fh_ctgov_drugTrialTmp)


###find top sale drugs
###current format support before year 2011, for 2011 and 2012 data, need further modifications
if 0:
    sales.get_sales_table(para_drugSalesTargetYear_List, para_topSales_target_dir)


###From PDR website crawl all possible drug summaries including Boxed Warning text,
if 0:
    pdr.crawl_PDR_drugSummary_BBW(para_pdr_target_dir, para_pdr_source_url)


###use stat_parser to parse input text one by one, human can modify output,
###output phrases stored in my-sql DB for future usage
#manual_train()


###actual parsing function
#parse_stat(text)

####################################
###TF-IDF
candiDrugList = None
ctList_robust_pre = None
ctList_robust_post = None
ctList_BBW_pre = None
ctList_BBW_post = None

#########################################################################################
methodToSelectDrugDomain = 'POPULAR'  # or "POPULAR" or "PDR" or "NDC"
candiDrugList_salesInfo = {}
if methodToSelectDrugDomain == 'POPULAR':
    #step1: using locally saved drugs.com data to find all drug list

    #need manual correct
    # 1. add manually years 2011-2013
    # 2. change name of the file of year 2010

    # store year of popularity
    candiDrugList_salesInfo = tfidf.drugList_occursAtLeastOnceInAllYears()
    #return is list format, with drug names as keys and number of occurrence in all the years as values
    candiDrugList = candiDrugList_salesInfo.keys()




#########################################################################################
#step2: map drugs.com name to PDR to ensure drugs can be mapped to BBW info
candiDrugList_BBW, candiDrugList_robust = tfidf.drugList_findBBWInfo(candiDrugList)#output format is drug list with BBW info as values
candiDrugList_drugGroup = data.taggingMultipleLists({'BBW':candiDrugList_BBW.keys(),'ROBUST':candiDrugList_robust.keys()})

tfidf.saveSelectedList(candiDrugList_BBW,'../result/','candiDrugList_BBW')
tfidf.saveSelectedList(candiDrugList_robust,'../result/','candiDrugList_robust')

#step3: map drugs.com-PDR name to NDC and marketing information
##return format: dictionary[drug name] = marketing date YYMMDD
candiDrugList_BBW_dated = map.mappingMarketingDate_fromDrugList(candiDrugList_BBW.keys(), fh_ndc_source)
candiDrugList_robust_dated = map.mappingMarketingDate_fromDrugList(candiDrugList_robust.keys(), fh_ndc_source)

candiDrugList_dated = map.mappingMarketingDate_fromDrugList(candiDrugList, fh_ndc_source)

#save drug-marketingDate-BBW table
combinedDic = {'DRUG_GROUP':candiDrugList_drugGroup, 'MARKETING_DATE':candiDrugList_dated, "BBW_TEXT": candiDrugList_BBW, 'POPULAR_YEAR':candiDrugList_salesInfo}

candiDrugDic_dated_bbw = data.saveMultipleDictionariesToTable(combinedDic, '../result/drug_Date_BBW_table')


#########################################################################################
methodToSelectTrials = 'PHASE' #  or 'PHASE' or 'PHASE_DATE'

if methodToSelectTrials == 'PHASE_DATE':
    #try to use previous result from retrieveOnlineDrugTrialInfo function to get trial list for drugs

    #step4: mapping drugs to NDC for marketing date
    #then using ct.gov search api to get trial list for each drug with marketing date
    #Time complexity: O(5min)
    ctList_BBW_pre, ctList_BBW_post = ctgov.retrieveOnlineDrugTrialInfo_fromDrugNameAndMarketingDateDic(candiDrugList_BBW_dated, fh_ctgov_drugTrialContent_csv+'_BBW', fh_ctgov_drugTrialContent_tab+'_BBW', fh_ctgov_drugTrialList_tab+'_BBW', fh_ctgov_drugTrialTmp+'_BBW')
    ctList_robust_pre, ctList_robust_post = ctgov.retrieveOnlineDrugTrialInfo_fromDrugNameAndMarketingDateDic(candiDrugList_robust_dated, fh_ctgov_drugTrialContent_csv+'_ROBUST', fh_ctgov_drugTrialContent_tab+'_ROBUST', fh_ctgov_drugTrialList_tab+'_ROBUST', fh_ctgov_drugTrialTmp+'_ROBUST')

    #original procedure, not used now
    #ctList_robust_pre = getCTListWithPeriodFromDrugList_local(candiDrugList_robust, 0) # {'arimidex': '', 'zyrtec-d': '', 'singulair': '', 'pentasa': ['NCT00545740', 'NCT00751699'],...}
    #ctList_robust_post = getCTListWithPeriodFromDrugList_local(candiDrugList_robust, 1)
    #ctList_BBW_pre = getCTListWithPeriodFromDrugList_local(candiDrugList_BBW, 0) # {'arimidex': '', 'zyrtec-d': '', 'singulair': '', 'pentasa': ['NCT00545740', 'NCT00751699'],...}
    #ctList_BBW_post = getCTListWithPeriodFromDrugList_local(candiDrugList_BBW, 1) # {'arimidex': '', 'zyrtec-d': '', 'singulair': '', 'pentasa': ['NCT00545740', 'NCT00751699'],...}


if methodToSelectTrials == 'PHASE':
    ctList_BBW_pre, ctList_BBW_post = ctgov.retrieveOnlineDrugTrialInfo_fromDrugNameList(candiDrugList_BBW.keys(), fh_ctgov_drugTrialContent_csv+'_BBW', fh_ctgov_drugTrialContent_tab+'_BBW', fh_ctgov_drugTrialList_tab+'_BBW', fh_ctgov_drugTrialTmp+'_BBW')
    ctList_robust_pre, ctList_robust_post = ctgov.retrieveOnlineDrugTrialInfo_fromDrugNameList(candiDrugList_robust.keys(), fh_ctgov_drugTrialContent_csv+'_ROBUST', fh_ctgov_drugTrialContent_tab+'_ROBUST', fh_ctgov_drugTrialList_tab+'_ROBUST', fh_ctgov_drugTrialTmp+'_ROBUST')

# same DATED drug_trial_content_tab_BBW/ROBUST tables when using retrieveOnlineDrugTrialInfo_fromDrugNameList function
#   instead of retrieveOnlineDrugTrialInfo_fromDrugNameAndMarketingDateDic
if 1:
    ctNum_BBW_pre = {}
    ctNum_BBW_post = {}
    ctNum_robust_pre = {}
    ctNum_robust_post = {}
    for key in ctList_BBW_pre.keys():
        ctNum_BBW_pre[key] = len(ctList_BBW_pre[key])
    for key in ctList_BBW_post.keys():
        ctNum_BBW_post[key] = len(ctList_BBW_post[key])
    for key in ctList_robust_pre.keys():
        ctNum_robust_pre[key] = len(ctList_robust_pre[key])
    for key in ctList_robust_post.keys():
        ctNum_robust_post[key] = len(ctList_robust_post[key])

    combinedDic_BBW = {'preTrialNum':ctNum_BBW_pre,'postTrialNum':ctNum_BBW_post,'marketingDate':candiDrugList_dated}
    combinedDic_robust = {'preTrialNum':ctNum_robust_pre,'postTrialNum':ctNum_robust_post,'marketingDate':candiDrugList_dated}

    candiDrugDic_numbered_dated_BBW = data.saveMultipleDictionariesToTable(combinedDic_BBW, '../result/drug_trial_content_tab_BBW',True,'\t','NA','drugName')
    candiDrugDic_numbered_dated_robust = data.saveMultipleDictionariesToTable(combinedDic_robust, '../result/drug_trial_content_tab_ROBUST',True,'\t','NA','drugName')

    #[candiDrugDic_numbered_dated_BBW[x].append('BBW') for x in candiDrugDic_numbered_dated_BBW.keys()]
    #[candiDrugDic_numbered_dated_robust[x].append('ROBUST') for x in candiDrugDic_numbered_dated_robust.keys()]
#########################################################################################
#step5: find eligibile drugs with lower limit trial numbers per period
# #return format is: {'solodyn': ['NCT00203697', 'NCT00203112'...],'drug2':[...]}
ctList_robust_pre_list, ctList_robust_post_list = tfidf.selectEligibleDrugs(ctList_robust_pre, ctList_robust_post, para_lowerLimit_CTperPerid)
ctList_BBW_pre_list, ctList_BBW_post_list = tfidf.selectEligibleDrugs(ctList_BBW_pre, ctList_BBW_post, para_lowerLimit_CTperPerid)



SAVE_SELECTED_LIST = 1
#save selected list to file
if SAVE_SELECTED_LIST:
    tfidf.saveSelectedList(ctList_robust_pre_list, para_selected_drugTrialList_dir, file.retrieve_name(ctList_robust_pre_list))
    tfidf.saveSelectedList(ctList_robust_post_list, para_selected_drugTrialList_dir, file.retrieve_name(ctList_robust_post_list))
    tfidf.saveSelectedList(ctList_BBW_post_list, para_selected_drugTrialList_dir, file.retrieve_name(ctList_BBW_post_list))
    tfidf.saveSelectedList(ctList_BBW_pre_list, para_selected_drugTrialList_dir, file.retrieve_name(ctList_BBW_pre_list))

#retrieve CT xml
SAVECOMP = 1
if 1:
    print '####analyzing ctList_BBW_post_comp'
    ctList_BBW_post_comp = tfidf.extractComponentFromXML_parse(list(itertools.chain(*ctList_BBW_post_list.values())), para_selected_drugTrialList_dir+'instantSave/ctList_BBW_post_comp.tmp')#choose whether do parsing here
    if SAVECOMP == 1:
        tfidf.saveComp(ctList_BBW_post_comp,para_selected_drugTrialList_dir,file.retrieve_name(ctList_BBW_post_comp))
    print '####analyzing ctList_BBW_pre_comp'
    ctList_BBW_pre_comp = tfidf.extractComponentFromXML_parse(list(itertools.chain(*ctList_BBW_pre_list.values())), para_selected_drugTrialList_dir+'instantSave/ctList_BBW_pre_comp.tmp')
    if SAVECOMP == 1:
        tfidf.saveComp(ctList_BBW_pre_comp,para_selected_drugTrialList_dir,file.retrieve_name(ctList_BBW_pre_comp))
    print '###analyzing ctList_robust_post_comp'
    ctList_robust_post_comp = tfidf.extractComponentFromXML_parse(list(itertools.chain(*ctList_robust_post_list.values())), para_selected_drugTrialList_dir+'instantSave/ctList_robust_post_comp.tmp')
    if SAVECOMP == 1:
        tfidf.saveComp(ctList_robust_post_comp,para_selected_drugTrialList_dir,file.retrieve_name(ctList_robust_post_comp))
    print '###analyzing ctList_robust_pre_comp'
    ctList_robust_pre_comp = tfidf.extractComponentFromXML_parse(list(itertools.chain(*ctList_robust_pre_list.values())), para_selected_drugTrialList_dir+'instantSave/ctList_robust_pre_comp.tmp')
    if SAVECOMP == 1:
        tfidf.saveComp(ctList_robust_pre_comp, para_selected_drugTrialList_dir, file.retrieve_name(ctList_robust_pre_comp))

