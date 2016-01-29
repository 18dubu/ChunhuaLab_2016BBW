__author__ = 'mahandong'
import os, sys
from ..util.file import *
from ..util import web as web
import xml.etree.ElementTree as xml_parser
import csv

#from ..util.log import ext_print

def get_all_disease_name(output):
    output = []
    output = [name for name in os.listdir(".") if os.path.isdir(name)]
    disease_list = []
    single_disease = []
    for disease in output:
        single_disease = get_disease_clinical_trials(disease)
        disease_list.extend(single_disease)
    #single_disease = get_disease_clinical_trials("abnormalities-multiple")
    if len(single_disease) == 0:
        print "no trials found"
    disease_list.extend(single_disease)
    fout = "trial_ids_all.csv"
    #fout = "abnormalitlies-multiple.csv"
    write_csv(fout, disease_list)
    return True


def get_average_num_cde(output):
    output = []
    output = [name for name in os.listdir(".") if os.path.isdir(name)]
    #disease_list = []
    #single_disease = []
    cde_output = []
    num_disease = 0
    total_num_cdes = 0
    for disease in output:
        dir = os.path.join('/Users/zhehe/Documents/programs/data_preparation/all-years-threshold-3percent', disease)
        dir += "/cde-inclusion.csv"
        #print dir
        try:
            texts = read_csv(dir)
            single_disease = get_disease_clinical_trials(disease)
            if (texts is not None) and (len(single_disease) != 0):
                num_disease += 1
                # the first row is caption, should not be counted
                total_num_cdes += len(texts) - 1
                cde_output.append((disease, (len(texts) - 1)))
        except    ValueError:
            print "no such file"
    print "total number of diseases is %d" % (num_disease)
    print "average number of all CDEs for each disease is %.1f" % (total_num_cdes / (num_disease))
    #single_disease = get_disease_clinical_trials(disease)
    #disease_list.extend(single_disease)

    fout = "num_CDEs_for_each_disease.csv"
    write_csv(fout, cde_output)
    return True


# generate a CSV file containing disease, section, CDE, frequency, UMLS Semantic Type

def generate_all_CDE_csv(section):
    output = []
    output = [name for name in os.listdir(".") if os.path.isdir(name)]
    cde_list = []  # save all CDEs for all the diseases
    for disease in output:
        dir = os.path.join('/Users/zhehe/Documents/programs/data_preparation/all-years-threshold-3percent', disease)
        dir += "/cde-%s" % section + ".csv"
        try:
            texts = read_csv(dir)
            if texts is None or len(texts) <= 0:
                print 'input data error, please check either no such file or no data --- interrupting'
                continue
            for i in xrange(len(texts) - 1):
                cde_list.append((disease, section, texts[i + 1][0], texts[i + 1][1], texts[i + 1][2]))
        except ValueError:
            print "no such file"
    fout = "CDEs_for_all_diseases_%s" % section + ".csv"
    write_csv(fout, cde_list)
    return True



def ctFullSearch(cond='&', intr='&', spon='&', phase='phase=&phase=&phase=&phase=&phase=&',
                 rcv_s='', rcv_e='', type='Intr&', fund='&'):
    '''
    print"""
    Note that:
    para: end with &, except for rcv_s/rcv_e
    phase: 4(Phase0),0(Phase1),1(Phase2),2(Phase3),3(Phase4),leave blank if not needed
    fund: 0(NIH), 1(Other U.S. Federal Agency), 2(industry), 3(All others (Individual, University, Organization, ... )
    received data: format as "MM/DD/YYYY"
    """
    '''
    if len(rcv_e)>0:
        rcv_e.replace('/','%2F')
        rcv_e += "&"
        rcv_e = 'rcv_e='+rcv_e
    if len(rcv_s)>0:
        rcv_s.replace('/','%2F')
        rcv_s += "&"
        rcv_s = 'rcv_s='+rcv_s


    url = 'http://clinicaltrials.gov/search?type=' + type + 'cond=' + cond.replace(' ', '+') + 'intr=' + intr.replace(
        ' ', '+') + 'spons=' + spon.replace(' ', '+') + phase + 'fund=' + fund + rcv_s + rcv_e + '&displayxml=true&count='
    #print url
    # get the number of studies available (request 0 studies as result)
    xmltree = xml_parser.fromstring(web.download_web_data('%s%s' % (url, '0')))
    nnct = xmltree.get('count')
    # get the list of clinical studies
    xmltree = xml_parser.fromstring(web.download_web_data('%s%s' % (url, nnct)))
    lnct = xmltree.findall('clinical_study')
    #criterias = []
    trial_ids = []

    url_trial = 'http://clinicaltrials.gov/show/%s?displayxml=true'
    for nct in lnct:
        ids = nct.find('nct_id')
        if ids is not None:
            trial_ids.append(ids.text)
        else:
            print 'no id'
    #fout = "trial_ids_for_%s.csv" %disease

    #write_csv (fout, trial_ids)
    return trial_ids


#modified for intervention trial search for a certain interventions
def get_disease_clinical_trials(disease, intr='', ):
    # base url
    url = 'http://clinicaltrials.gov/search?type=intr&intr=' + disease.replace(' ', '+') + '&displayxml=true&count='
    # get the number of studies available (request 0 studies as result)
    xmltree = xml_parser.fromstring(web.download_web_data('%s%s' % (url, '0')))
    nnct = xmltree.get('count')
    # get the list of clinical studies
    xmltree = xml_parser.fromstring(web.download_web_data('%s%s' % (url, nnct)))
    lnct = xmltree.findall('clinical_study')

    #criterias = []
    trial_ids = []

    url_trial = 'http://clinicaltrials.gov/show/%s?displayxml=true'
    for nct in lnct:
        ids = nct.find('nct_id')
        if ids is not None:
            trial_ids.append((ids.text, disease))
        else:
            print 'no id'
    #fout = "trial_ids_for_%s.csv" %disease

    #write_csv (fout, trial_ids)
    return trial_ids


#query function to return the trial ids for a list of drugs
def query(intr_list):
    trial_id = []
    for intr in intr_list:
        trial_id.append([i[0] for i in get_disease_clinical_trials(intr)])
    return trial_id

#########################
##retrive drug-trial numbers based on clinicaltrials.gov search, for future selection, runtime: O(1h)
def retrieveOnlineDrugTrialInfo(fh_ndc_source, fh_ctgov_drugTrialContent_csv, fh_ctgov_drugTrialContent_tab, fh_ctgov_drugTrialList_tab, fh_ctgov_drugTrialTmp):
    targetDrugType = 'HUMAN PRESCRIPTION DRUG'
    lowerLimit_pre = 5
    lowerLimit_post = 5

    affectedFiles = [fh_ctgov_drugTrialContent_csv,fh_ctgov_drugTrialContent_tab, fh_ctgov_drugTrialList_tab,fh_ctgov_drugTrialTmp]
    ###
    checkFileIfContinue(affectedFiles)

    fhOut_content = open(fh_ctgov_drugTrialContent_csv, 'w')
    fhOut_backup = open(fh_ctgov_drugTrialContent_tab, 'w')
    fhOut_trialList = open(fh_ctgov_drugTrialList_tab, 'w')
    fhOut_tmp = open(fh_ctgov_drugTrialTmp, 'w')
    fhOut_tmp.write("drugName\tpreTrialNum\tpostTrialNum\tmarketingDate\tcompany\tsubstanceName\tpharmClass\n")

    ##use ndc product table to generate overall drug list
    try:
        ndc,name = read_csv(fh_ndc_source)
    except IOError:
        print "input file problem"

    ndcDrugList = {}
    trialNum = {}
    trialList = {}
    checked = {}
    ii = 0
    for entry in ndc:
        if ii % 100 == 0:
            perc = ii/len(ndc)*100
            print "line: " + str(ii) + " finished! ("+ str(perc)+"%)"
        ii+=1
        if entry[2] == targetDrugType: #only select 'HUMAN PRESCRIPTION DRUG'
            if entry[3] in checked.keys() and int(entry[8]) >= int(checked[entry[3]]): #choose the earliest approved date
                continue
            drug = entry[3]
            company = entry[12]
            marketDate = entry[8] #YYYYMMDD
            #change date format to mm/dd/yyyy
            year = ''.join(list(str(marketDate))[0:4])
            month = ''.join(list(str(marketDate))[4:6])
            day = ''.join(list(str(marketDate))[6:8])
            date = '/'.join([month,day,year])
            try:
                ctList1 = ctFullSearch('&',drug.replace(" ","+").replace("AND",'')+'&','&','phase=4&phase=0&phase=1&phase=2&phase=&','',date)
                ctList2 = ctFullSearch('&',drug.replace(" ","+").replace("AND",'')+'&','&','phase=&phase=&phase=&phase=&phase=3&',date,'')
                #ctList = get_disease_clinical_trials(drug)
                ctNum1 = len(ctList1)
                ctNum2 = len(ctList2)
            except Exception as e:
                print "error occurred and stopped in line" + str(ii) + "(entry: " + str(entry) + ')'
                continue
            if ctNum1 > lowerLimit_pre and ctNum2 > lowerLimit_post:
                #print drug, company, entry[8],ctNum1,ctNum2
                ndcDrugList[drug] = [str(entry[3]).replace(',','|'), str(ctNum1), str(ctNum2), str(entry[8]).replace(',','|'), str(entry[12]).replace(',','|'), str(entry[13]).replace(',','|'), str(entry[16]).replace(',','|')]
                #print ndcDrugList[drug]
                trialNum[drug] = [ctNum1, ctNum2]
                trialList[drug] = [ctList1, ctList2]
                fhOut_tmp.write("\t".join(ndcDrugList[drug])+'\n')
                #print ndcDrugList[drug]
            checked[entry[3]] = entry[8]

    out = csv.writer(fhOut_content, delimiter=',')
    names = ['drugName','preTrialNum','postTrialNum','marketingDate','company','substanceName','pharmClass']
    out.writerow(names)
    fhOut_backup.write("drugName\tpreTrialNum\tpostTrialNum\tmarketingDate\tcompany\tsubstanceName\tpharmClass\n")
    fhOut_trialList.write("drugName\tpreTrialList\tpostTrialList\n")
    for drugs in ndcDrugList.keys():
        fhOut_backup.write("\t".join(ndcDrugList[drugs])+'\n')
        out.writerow(ndcDrugList[drugs])

        fhOut_trialList.write(drugs+'\t')
        for stage in range(2):
            fhOut_trialList.write(",".join(trialList[drugs][stage])+'\t')
        fhOut_trialList.write('\n')

    fhOut_content.close()
    fhOut_backup.close()
    fhOut_trialList.close()
    fhOut_tmp.close()

    backup(affectedFiles)
    print 'process finished: drug-trial number retrieved!'
#sample run:
# retrieveOnlineDrugTrialInfo(fh_ndc_source, fh_ctgov_drugTrialContent_csv, fh_ctgov_drugTrialContent_tab, fh_ctgov_drugTrialList_tab, fh_ctgov_drugTrialTmp)


#give a list of drug names and retrieve trial number information from ct.gov
#no date was used in this function and the only constraints are the phases of the trial
#return a dic with drug name as key and list of trial id as value.
#save relevant files
def retrieveOnlineDrugTrialInfo_fromDrugNameList(DrugNameList, fh_ctgov_drugTrialContent_csv, fh_ctgov_drugTrialContent_tab, fh_ctgov_drugTrialList_tab, fh_ctgov_drugTrialTmp):

    ctList_pre = {}
    ctList_post = {}

    affectedFiles = [fh_ctgov_drugTrialContent_csv,fh_ctgov_drugTrialContent_tab, fh_ctgov_drugTrialList_tab,fh_ctgov_drugTrialTmp]
    ###
    if not checkFileIfContinue(affectedFiles):
        exit(0)
    print "The program is still running..."

    fhOut_content = open(fh_ctgov_drugTrialContent_csv, 'w')
    fhOut_backup = open(fh_ctgov_drugTrialContent_tab, 'w')
    fhOut_trialList = open(fh_ctgov_drugTrialList_tab, 'w')
    fhOut_tmp = open(fh_ctgov_drugTrialTmp, 'w')
    fhOut_tmp.write("drugName\tpreTrialNum\tpostTrialNum\n")


    ndcDrugList = {}
    trialNum = {}
    trialList = {}
    ii=0
    for currentDrug in DrugNameList:
        ii += 1
        if 1: #only select 'HUMAN PRESCRIPTION DRUG'

            try:
                #phase 1~3
                ctList1 = ctFullSearch('&',currentDrug.replace(" ", "+").replace("AND",'')+'&','&','phase=4&phase=0&phase=1&phase=2&phase=&')
                ctNum1 = len(ctList1)
                #phase 4
                ctList2 = ctFullSearch('&',currentDrug.replace(" ", "+").replace("AND",'')+'&','&','phase=&phase=&phase=&phase=&phase=3&')
                ctNum2 = len(ctList2)

            except Exception as e:
                print "error occurred and stopped in line" + str(ii) + "(entry: " + str(currentDrug) + ')'
                continue

            if ctNum1 > 0 and ctNum2 > 0:
                ndcDrugList[currentDrug] = [str(currentDrug).replace(',','|'), str(ctNum1), str(ctNum2)]
                trialNum[currentDrug] = [ctNum1, ctNum2]
                trialList[currentDrug] = [ctList1, ctList2]
                fhOut_tmp.write("\t".join(ndcDrugList[currentDrug])+'\n')

                ctList_pre[currentDrug] = ctList1
                ctList_post[currentDrug] = ctList2

    out = csv.writer(fhOut_content, delimiter=',')
    names = ['drugName','preTrialNum','postTrialNum']
    out.writerow(names)
    fhOut_backup.write("drugName\tpreTrialNum\tpostTrialNum\n")
    fhOut_trialList.write("drugName\tpreTrialList\tpostTrialList\n")
    for drugs in ndcDrugList.keys():
        fhOut_backup.write("\t".join(ndcDrugList[drugs])+'\n')
        out.writerow(ndcDrugList[drugs])

        fhOut_trialList.write(drugs+'\t')
        for stage in range(2):
            fhOut_trialList.write(",".join(trialList[drugs][stage])+'\t')
        fhOut_trialList.write('\n')

    fhOut_content.close()
    fhOut_backup.close()
    fhOut_trialList.close()
    fhOut_tmp.close()

    #backup(affectedFiles)
    print 'process finished: drug-trial number retrieved!'

    return ctList_pre, ctList_post



##retrive drug-trial numbers based on clinicaltrials.gov search, for future selection, runtime: O(1h)
#date format should be: YYYYMMDD
#no lower limit selection at this stage
def retrieveOnlineDrugTrialInfo_fromDrugNameAndMarketingDateDic(DrugDateDic, fh_ctgov_drugTrialContent_csv, fh_ctgov_drugTrialContent_tab, fh_ctgov_drugTrialList_tab, fh_ctgov_drugTrialTmp):
    targetDrugType = 'HUMAN PRESCRIPTION DRUG'

    ctList_pre = {}
    ctList_post = {}

    affectedFiles = [fh_ctgov_drugTrialContent_csv,fh_ctgov_drugTrialContent_tab, fh_ctgov_drugTrialList_tab,fh_ctgov_drugTrialTmp]
    ###
    if not checkFileIfContinue(affectedFiles):
        exit(0)
    print "The program is still running..."

    fhOut_content = open(fh_ctgov_drugTrialContent_csv, 'w')
    fhOut_backup = open(fh_ctgov_drugTrialContent_tab, 'w')
    fhOut_trialList = open(fh_ctgov_drugTrialList_tab, 'w')
    fhOut_tmp = open(fh_ctgov_drugTrialTmp, 'w')
    fhOut_tmp.write("drugName\tpreTrialNum\tpostTrialNum\tmarketingDate\n")


    ndcDrugList = {}
    trialNum = {}
    trialList = {}
    ii=0
    for currentDrug in DrugDateDic.keys():
        ii += 1
        if 1: #only select 'HUMAN PRESCRIPTION DRUG'

            marketDate = DrugDateDic[currentDrug] #YYYYMMDD
            #change date format to mm/dd/yyyy
            year = ''.join(list(str(marketDate))[0:4])
            month = ''.join(list(str(marketDate))[4:6])
            day = ''.join(list(str(marketDate))[6:8])
            date = '/'.join([month,day,year])
            try:
                #phase 1~3
                ctList1 = ctFullSearch('&',currentDrug.replace(" ", "+").replace("AND",'')+'&','&','phase=4&phase=0&phase=1&phase=2&phase=&','',date)
                ctNum1 = len(ctList1)
                #phase 4
                ctList2 = ctFullSearch('&',currentDrug.replace(" ", "+").replace("AND",'')+'&','&','phase=&phase=&phase=&phase=&phase=3&',date,'')
                ctNum2 = len(ctList2)

            except Exception as e:
                print "error occurred and stopped in line" + str(ii) + "(entry: " + str(currentDrug) + ')'
                continue

            if ctNum1 > 0 and ctNum2 > 0:
                ndcDrugList[currentDrug] = [str(currentDrug).replace(',','|'), str(ctNum1), str(ctNum2), str(DrugDateDic[currentDrug]).replace(',','|')]
                trialNum[currentDrug] = [ctNum1, ctNum2]
                trialList[currentDrug] = [ctList1, ctList2]
                fhOut_tmp.write("\t".join(ndcDrugList[currentDrug])+'\n')

                ctList_pre[currentDrug] = ctList1
                ctList_post[currentDrug] = ctList2

    out = csv.writer(fhOut_content, delimiter=',')
    names = ['drugName','preTrialNum','postTrialNum','marketingDate']
    out.writerow(names)
    fhOut_backup.write("drugName\tpreTrialNum\tpostTrialNum\tmarketingDate\n")
    fhOut_trialList.write("drugName\tpreTrialList\tpostTrialList\n")
    for drugs in ndcDrugList.keys():
        fhOut_backup.write("\t".join(ndcDrugList[drugs])+'\n')
        out.writerow(ndcDrugList[drugs])

        fhOut_trialList.write(drugs+'\t')
        for stage in range(2):
            fhOut_trialList.write(",".join(trialList[drugs][stage])+'\t')
        fhOut_trialList.write('\n')

    fhOut_content.close()
    fhOut_backup.close()
    fhOut_trialList.close()
    fhOut_tmp.close()

    #backup(affectedFiles)
    print 'process finished: drug-trial number retrieved!'

    return ctList_pre, ctList_post

#sample run:


def retrieveCTXMLFromCTlist(CTList):

    content = {}
    url_trial = 'http://clinicaltrials.gov/show/%s?displayxml=true'
    for nct in CTList:
        ids = nct
        if ids is not None:
            try:
                ct_xml = web.download_web_data(url_trial % ids)
                if ct_xml:
                    content[ids] = ct_xml
            except Exception:
                print str(Exception) + '@retrieveCTXMLFromCTlist' + ids
    return content


# processing the command line options
# import argparse
#
#
# def _process_args():
#     parser = argparse.ArgumentParser(description='')
#     parser.add_argument('-o', default=None, help='output directory/folder')
#     return parser.parse_args(sys.argv[1:])
#
#
# if __name__ == '__main__':
#     print ''
#     args = _process_args()
#     #generate_all_CDE_csv('exclusion')
#     #get_average_num_cde (args.o)
#     get_all_disease_name(args.o)
#     print ''