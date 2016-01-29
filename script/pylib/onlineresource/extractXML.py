# -*- encoding: utf-8 -*-
# Extract elements form XML formatted trials
# Created by Tony HAO, th2510@columbia.edu
from ..util import file as ufile
from ..util import log as ext_print
import xml.etree.ElementTree as xml_parser
from ..util import web as web
import os, sys, re

#=======================================================extract trial information from downloaded xml files

def CT_extractxml (fin, fout):
	# set output data file
	if fout is None:
		fout = os.path.splitext(fin)[0] + "_extractedXML.csv"

	output = []
	
	processed_list = [] # set processed trials into here to avoid redundency
	for root, dir, files in os.walk(fin):
		for f in files:
			if not f.endswith(".xml") or f in processed_list:
				continue
# 	  		if f.startswith('NCT00784511'):
# 	  			pass
# 	  		else:
# 	  			continue

			processed_list.append(f)
			if len(processed_list)%1000 == 0:
				print ('Processing  %d' % len(processed_list))

			# read input data
			fdin = os.path.join(root, f)
			text = ufile.read_file (fdin, 3, False)
			if text is not None:
				(id, brief_title, official_title, conditions, agency, agency_class, source, authority, brief_summary, overall_status, start_date, gender, minimum_age, maximum_age, study_pop, criteria, enrollment, phase, study_type, location, intervention_type, intervention_name, enrollment_type) =  extract_component(text)
				if not criteria.startswith('Please contact site') and criteria.strip() !='':
					# define your required output
#					output.append((id, criteria, gender, minimum_age, maximum_age, conditions, phase))
					output.append((id, brief_title, official_title, location, agency, agency_class, overall_status, start_date, gender, minimum_age, maximum_age, enrollment, criteria, conditions, phase, study_pop, intervention_type, intervention_name, authority, source, study_type, enrollment_type))
# 					output.append((id, brief_title, official_title, conditions, gender, agency, minimum_age, maximum_age, enrollment, phase, study_type, location))
	
	ufile.write_csv (fout, output)
	print 'saved result in: %s' % fout

	print 'all tasks completed\n'
	return True

"""
def CT_extractxml_disease (fin, fout, disease):
	# set output data file
	if fout is None:
		fout = os.path.splitext(fin)[0] + "_extractedXML.csv"

	# extract the id list for the disease ===================
	url = 'http://clinicaltrials.gov/search?cond=' + disease.replace(' ', '+') + '&displayxml=true&count=' 	
	# get the number of studies available (request 0 studies as result)
	xmltree = xml_parser.fromstring (web.download_web_data('%s%s' % (url, '0')))
	nnct = xmltree.get('count')	
	# get the list of clinical studies
	xmltree = xml_parser.fromstring (web.download_web_data('%s%s' % (url, nnct)))
	lnct = xmltree.findall ('clinical_study')
	
	ids = []
	for nct in lnct:
		aid = nct.find ('nct_id')
		if aid is not None:
			ids.append(aid.text)
	print ext_print("get %d trial ids for the disease" % len(ids))
		
	# get all need trial information for the disease
	output = []	
	processed_list = [] # set processed trials into here to avoid redundancy
	for root, dir, files in os.walk(fin):
		for f in files:
			if not f.endswith(".xml") or f in processed_list or f.replace(".xml",'') not in ids:
				continue
			processed_list.append(f)
			
			if len(processed_list)%1000 == 0:
				print ('Processing  %d' % len(processed_list))

			# read input data
			fdin = os.path.join(root, f)
			text = ufile.read_file (fdin, 3, False)
			if text is not None:
				(id, brief_title, official_title, conditions, agency, agency_class, source, authority, brief_summary, overall_status, start_date, gender, minimum_age, maximum_age, study_pop, criteria, enrollment, phase, study_type, location, intervention_type, intervention_name) =  extract_component(text)
				if not criteria.startswith('Please contact site') and criteria.strip() !='':
					# define your required output
					output.append((id, criteria, gender, minimum_age, maximum_age, conditions))
	
	ufile.write_csv (fout, output)
	print ext_print ('saved result in: %s' % fout)	

	print ext_print ('all tasks completed\n')
	return True
"""

def extract_component (text):
	ct_xml = xml_parser.fromstring (text)

	id, brief_title, official_title, conditions, agency, agency_class, source, authority, brief_summary, overall_status, start_date, gender, minimum_age, maximum_age, study_pop, criteria, enrollment, phase, study_type, location, intervention_type, intervention_name, enrollment_type = '', '', '', [], [], [], '', [], '', '', '', '', '', '', '', '', '', '', '', [], '', '', ''

	# find ID
	block = ct_xml.find ('id_info')
	if block is not None:
		d = block.find ('nct_id')
		if d is not None:
			id = d.text.strip()
 
	# find brief_title
	block = ct_xml.find ('brief_title')
	if block is not None:
		brief_title = block.text.encode('ascii', 'ignore').strip()

	# find official_titlebrief_title
	block = ct_xml.find ('official_title')
	if block is not None:
	#official_title = block.text.strip()
		official_title = block.text.encode('ascii', 'ignore').strip()
	# find all conditions
	block = ct_xml.findall ('condition')
	if len(block) > 0:
		for cn in block:
			conditions.append(cn.text.encode('utf-8'))

	# find sponsors
	block = ct_xml.findall ('sponsors')
	if len(block) > 0:
		for cn in block:
			d = cn.find ('lead_sponsor')
			if d is not None:
				d = d.find ('agency')
				if d is not None:
					agency.append(d.text.encode('ascii', 'ignore').strip())

	# find agency class
	block = ct_xml.findall ('sponsors')
	if len(block) > 0:
		for cn in block:
			d = cn.find ('lead_sponsor')
			if d is not None:
				d = d.find ('agency_class')
				if d is not None:
					agency_class.append(d.text.encode('ascii', 'ignore').strip())

	# find source
	block = ct_xml.find ('source')
	if block is not None:
		source = block.text.encode('ascii', 'ignore').strip()

	# find authority
	block = ct_xml.find ('oversight_info')
	if len(block) > 0:
		for cn in block:
			d = block.find ('authority')
			if d is not None:
				authority.append(d.text.encode('ascii', 'ignore').strip())

	# find intervention type
	block = ct_xml.find ('intervention')
	if block is not None:
		d = block.find ('intervention_type')
		if d is not None:
			intervention_type = d.text.encode('ascii', 'ignore').strip()
					
	# find intervention name
	block = ct_xml.find ('intervention')
	if block is not None:
		d = block.find ('intervention_name')
		if d is not None:
			intervention_name = d.text.encode('ascii', 'ignore').strip()


	# find brief_summary
	block = ct_xml.find ('brief_summary')
	if block is not None:
		d = block.find ('textblock')
		if d is not None:
			brief_summary = _process_ec_text(d.text.strip())

	# find overall_status
	block = ct_xml.find ('overall_status')
	if block is not None:
		overall_status = block.text.strip()

	# find start_date
	block = ct_xml.find ('start_date')
	if block is not None:
		start_date = block.text.strip()

	# parse the eligibility
	ec = ct_xml.find ('eligibility')
	if ec is not None:
 
		# extract gender
		d = ec.find ('gender')
		if d is not None:
			d = d.text.lower().strip()
			if 'n/a' not in d:
				gender = d
 
		# extract minimum_age
		d = ec.find ('minimum_age')
		if d is not None:
			d = d.text.lower().strip()
			if 'n/a' not in d:
				minimum_age = d
 
		# extract minimum_age
		d = ec.find ('maximum_age')
		if d is not None:
			d = d.text.lower().strip()
			if 'n/a' not in d:
				maximum_age = d
 
		# extract study population
		d = ec.find ('study_pop')
		if d is not None:
			txt = d.find ('textblock')
			if txt is not None:
				study_pop = _process_ec_text(txt.text)

		# parse to get criteria textstudy_pop
		d = ec.find ('criteria')
		if d is not None:
			txt = d.find ('textblock')
			if txt is not None:
				criteria = _process_ec_text(txt.text)

	# find enrollment
	block = ct_xml.find ('enrollment')
	if block is not None:
		enrollment = block.text.strip()
		enrollment_type = block.get('type')

	# find phase
	block = ct_xml.find ('phase')
	if block is not None:
		phase = block.text.strip()

	# find study_type
	block = ct_xml.find ('study_type')
	if block is not None:
		study_type = block.text.encode('ascii', 'ignore').strip()
			
	# find all location
	block = ct_xml.findall ('location')
	if len(block) > 0:
		for cn in block:
			d = cn.find ('facility')
			if d is not None:
				d = d.find ('name')
				if d is not None:
					location.append(d.text.strip())


	return (id, brief_title, official_title, conditions, agency, agency_class, source, authority, brief_summary, overall_status, start_date, gender, minimum_age, maximum_age, study_pop, criteria, enrollment, phase, study_type, location, intervention_type, intervention_name, enrollment_type)


# process and clean the eligibility criteria
def _process_ec_text (text):
	# handle special characters
	text = text.strip().replace('\n\n', '#')
	text = text.replace ('\n', '')
	text = text.replace(u'＝','=').replace(u'＞', '>').replace(u'＜','<').replace(u'≤','<=').replace (u'≥','>=').replace(u'≦','<=').replace(u'≧','>=').replace(u'mm³','mm^3').replace(u'µl','ul').replace(u'µL','ul').replace(u'·','').replace(u'‐','-')
	while '  ' in text:
		text = text.replace('  ',' ')
	text = text.encode('ascii', 'ignore')
	return text


# main function	

# processing the command line options
import argparse
def _process_args():
	parser = argparse.ArgumentParser(description='Downlaod all eligibility criteria from clinicaltrials.gov')
	parser.add_argument('-i', default=r'/Users/zhehe/Documents/programs/data_preparation/data/Trials_XML', help='file with the list of trials (default "./data/clinical-trials.csv"')
	parser.add_argument('-o', default=r'/Users/zhehe/Documents/programs/data_preparation/data/clinical_trials.csv', help='output file; None: get default output path')
	parser.add_argument('-d', default='Breast-Neoplasms', help='a specific disease')
	return parser.parse_args(sys.argv[1:])


if __name__ == '__main__' :
	print ''
	args = _process_args()
	CT_extractxml (args.i, args.o)
#     CT_extractxml_disease (args.i, args.o, args.d)
	print ''
