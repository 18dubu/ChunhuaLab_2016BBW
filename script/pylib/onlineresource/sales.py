__author__ = 'mahandong'

from pylib.analysis.preprocessing import *
import urllib2
from bs4 import BeautifulSoup


#find top sale drugs
#make sure the second col in target file in the drug name
def get_sales_table(target_year_list, target_dir):
    #ref: http://adesquared.wordpress.com/2013/06/16/using-python-beautifulsoup-to-scrape-a-wikipedia-table/
    mkdir(target_dir)
    for year in target_year_list:
        url = year
        stamp = year.split('/')[-1].replace('.','_')
        header = {'User-Agent': 'Mozilla/5.0'} #Needed to prevent 403 error on Wikipedia
        req = urllib2.Request(url,headers=header)
        page = urllib2.urlopen(req)
        soup = BeautifulSoup(page)

        rank = ""
        drugName = ""
        company = ""
        sales = ""
        change = ""
        table = soup.find("table", { "class" : "data-list" })
        #print table
        fhOut_sale = open(target_dir+'topSaleDrug'+str(stamp), 'w')
        for row in table.findAll("tr"):
            cells = row.findAll("td")
            #For each "tr", assign each "td" to a variable.
            if len(cells) == 5:
                rank = cells[0].find(text=True)
                if rank:
                    drugName = str(cells[1].findAll(text=True)[0])
                    company = str(cells[2].find(text=True))
                    sales = str(cells[3].find(text=True))
                    change = str(cells[4].find(text=True))
                    try:
                        write_to_file = rank+'\t'+drugName+'\t'+company+'\t'+sales+"\t"+change+'\n'
                        fhOut_sale.write(write_to_file)
                    except TypeError as e:
                        print "error in: "+ year
                        print rank,drugName,company,sales,change
                        print e
        fhOut_sale.close()
#sample run:
# get_sales_table(para_drugSalesTargetYear_List, para_topSales_target_dir)
##for 2011-2013 different format
# get_sales_table(['http://www.drugs.com/stats/top100/2013/sales'],'../result/topSales/')