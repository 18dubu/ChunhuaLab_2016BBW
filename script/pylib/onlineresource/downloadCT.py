# -*- encoding: utf-8 -*-
# Download ec of all trials from clinicaltrials.gov (into xml local files or a csv file)
# Created by Tony HAO, th2510@columbia.edu
# Modified by Zhe HE, zh2132@columbia.edu

from ..util import file as ufile
from ..util import log as ext_print
import xml.etree.ElementTree as xml_parser
from ..util import web as web
import os,sys, re
import urllib

#=======================================================download xml files
def eligibility_criteria_download_xml (flist, fout):

	# get output data directory
	if fout is None:
		fout = os.path.splitext(flist)[0] + "_all"
	else:
		fout = os.path.splitext(flist)[0]
	if ufile.mkdir (fout) is False:
		print  ('impossible to create the output directory -- interrupting')
		return
	
	print  ('Start to get trial id list...')
	id_nct = _retrieve_clinical_trial_ids (flist)
	print  ('Start to process %d clinical trials in xml format' % len(id_nct))
	
	url = 'http://clinicaltrials.gov/show/%s?displayxml=true'
	n =0
	for id in id_nct:
		if n%100 == 0:
			print  ('processing %d' % n)

		fout_p = fout + "/%s.xml" % id 
		page =urllib.urlretrieve(url % id,fout_p)
		n+=1

	
	print  ('all tasks completed\n')
	return True


# ===========================================================download content into csv
def eligibility_criteria_download (flist, fout):

	# get output data directory
	if fout is None:
		fout = os.path.splitext(flist)[0] + "_all"
	else:
		fout = os.path.splitext(flist)[0]
	if ufile.mkdir (fout) is False:
		print  ('impossible to create the output directory -- interrupting')
		return
		
	id_nct = _retrieve_clinical_trial_ids (flist)
	print  ('processing %d clinical trials in xml format' % len(id_nct))
	
	criterias = download_all_clinical_trials (id_nct)
		
	# save stats result
	fout_save = '%s/alls.csv' % (fout)
	ufile.write_csv (fout_save, criterias)
	print  ('saved result in: %s' % fout_save)

	print  ('all tasks completed\n')
	return True


# retrieve the list of clinical trials
def download_all_clinical_trials (lnct):
	
	criterias = []
	url_trial = 'http://clinicaltrials.gov/show/%s?displayxml=true'
	for nct in lnct:
		ids = nct
		if ids is not None:
			ct_xml = xml_parser.fromstring (web.download_web_data(url_trial % ids))
# 			cnd = ct_xml.findall ('condition')
# 			if len(cnd) > 1:
# 				continue
			
			# define tha structured data types	
			data_type = {'gender':'', 'minimum_age':'', 'maximum_age':''}
			# parse the xml
			ec = ct_xml.find ('eligibility')
			if ec is not None:
				for dt in data_type:
					d = ec.find (dt)
					if d is None:
						continue
					d = d.text.lower().strip()
					if 'n/a' not in d:
						data_type[dt] = d
				# parse to get criteria text
				d = ec.find ('criteria')
				if d is not None:
					txt = d.find ('textblock')
					if txt is not None:
						criterias.append((ids, _process_ec_text(txt.text), data_type['gender'], data_type['minimum_age'], data_type['maximum_age']))
					
	return criterias


# retrieve all the clinical trial ids
def _retrieve_clinical_trial_ids (fname):
	if ufile.file_exist(fname) is True:
		return ufile.read_file(fname)
	url = 'http://clinicaltrials.gov/ct2/crawl'
	html = web.download_web_data (url)
	pages = re.findall (r'href="/ct2/crawl/(\d+)"', html)
	id_nct = set ()
	for p in pages:
		html = web.download_web_data ('%s/%s' % (url, p))
		ct = re.findall (r'href="/ct2/show/(NCT\d+)"', html)
		id_nct |= set(ct)
	# save
	lid_nct = sorted(list(id_nct))
	#ufile.mkdir (fname[:fname.rfind('/')])
	ufile.write_file (fname, lid_nct)
	return lid_nct


# process and clean the eligibility criteria
def _process_ec_text (text):
	# handle special characters
 	text = text.strip().replace('\n\n', '#')
	text = text.replace ('\n', '')
	text = text.replace(u'＝','=').replace(u'＞', '>').replace(u'＜','<').replace(u'≤','<=').replace (u'≥','>=').replace(u'≦','<=').replace(u'≧','>=').replace(u'mm³','mm^3').replace(u'µl','ul').replace(u'µL','ul').replace(u'·','').replace(u'‐','-')
	while '  ' in text:
		text = text.replace('  ',' ')
	return text


# main function	

# # processing the command line options
# import argparse
# def _process_args():
#     parser = argparse.ArgumentParser(description='Downlaod all eligibility criteria from clinicaltrials.gov')
#     parser.add_argument('-i', default=r'/Users/zhehe/Documents/programs/data_preparation/id_list_all_trials.txt', help='file with the list of trials (default "./data/clinical-trials.csv"')
#     parser.add_argument('-of', default=r'/Users/zhehe/Documents/programs/data_preparation/data', help='output directory/folder')
#     return parser.parse_args(sys.argv[1:])
#

# if __name__ == '__main__' :
#     print ''
#     args = _process_args()
#     eligibility_criteria_download_xml (args.i, args.of)
#     print ''
