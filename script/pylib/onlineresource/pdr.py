__author__ = 'mahandong'


from pylib.analysis.preprocessing import *
from pylib.util.web import *
import time
import codecs
import urlparse
import urllib
from bs4 import BeautifulSoup

#From PDR website crawl all possible drug summaries including Boxed Warning text,
#current format support before year 2011, for 2011 and 2012 data, need further modifications
def crawl_PDR_drugSummary_BBW(target_dir, url):
    affectedFiles = [target_dir+'drugSummarySiteList',target_dir+'drugLabelContent']
    checkFileIfContinue(affectedFiles)
    mkdir(target_dir)
    fhOut_site = codecs.open(target_dir+'drugSummarySiteList','w','utf-8')
    fhOut_content = codecs.open(target_dir+'drugLabelContent','w','utf-8')
    urls = [url]
    visited = [url]
    candi = []
    a = re.compile("http://www.pdr.net/drug-summary/")
    b = re.compile('http://www.pdr.net/browse-by-drug-name')
    errorList = []
    while len(urls) > 0:
        time.sleep(1)
        if urls[0] in errorList:
            continue
        try:
            htmltext = urllib.urlopen(urls[0]).read()
        except:
            print "Fail to open and skip url:"+urls[0]
            errorList.append(urls[0])
            urls.pop(0)
            continue
        soup = BeautifulSoup(htmltext)

        urls.pop(0)
        print len(urls)
        for tag in soup.findAll('a', href=True):
            #tag['href'] = urlparse.urljoin(url, tag['href'])
            if tag['href'] not in visited:
                if a.match(tag['href']) or b.match(tag['href']):
                    urls.append(tag['href'])
                visited.append(tag['href'])
                #print str(tag['href'])
                if a.match(str(tag['href'])):
                    candi.append(tag['href'])
                    fhOut_site.write(tag['href'])

                    ele = str(tag['href']).split('/')[-1]
                    name = ele.split('?')[0]
                    id = ele.split('?')[1]
                    summaryHTML = urllib.urlopen(tag['href']).read()
                    warning = BeautifulSoup(summaryHTML).find('div', {"class" : "boxedWarning"})
                    if warning:
                        bbwText = warning.find('p')
                        try:
                            bbw_unescape = bbwText.find(text=True).strip()
                        except Exception as e:
                            print e
                            print name
                            bbw_unescape = 'NEED MANUAL CORRECTION'
                        try:
                            fhOut_content.write(str(name) + '\t' + str(id) + '\t1\t' + bbw_unescape +'\t' + str(tag['href']) + '\n')
                        except Exception as e:
                            print ' fhOut write error:',e
                            print name
                    else:
                        #print(name + '\t' + id + '\t0\tNone\t' + tag['href'] + '\n')
                        try:
                            fhOut_content.write(str(name) + '\t' + str(id) + '\t0\tNone\t' + str(tag['href']) + '\n')
                        except Exception as e:
                            print ' fhOut write error:',e
                            print name
    fhOut_content.close()
    fhOut_site.close()
    backup(affectedFiles)
#sample run:
#crawl_PDR_drugSummary_BBW(para_pdr_target_dir, para_pdr_source_url)