__author__ = 'mahandong'


from ..util.file import *
import MySQLdb as mdb
from parameters import *

#from list retrieved from ct.gov, rather than from db
def findCTListByDrug_local(fh_ctgov_drugTrialList_tab, sampleDrug, samplePeriod = 0):
    #the input files are from
    #sample period: 0 is pre, 1 is post
    sampleTrialModeOn = '' #NCT00604695

    Trials = [set(), set()]
    try:
        if file_exist(fh_ctgov_drugTrialList_tab):
            with open(fh_ctgov_drugTrialList_tab) as search:
                for line in search:
                    line = line.rstrip().split('\t')  # remove '\n' at end of line
                    if re.search(sampleDrug,line[0],re.IGNORECASE):
                        for i in line[1].split(','):
                            Trials[0].add(i)
                        for j in line[2].split(','):
                            Trials[1].add(j)

    except Exception:
        print "error in findCTListByDrug_local",Exception

    if sampleTrialModeOn != '':
        target_Trials = [sampleTrialModeOn]
    else:
        target_Trials = list(Trials[samplePeriod])
    return target_Trials

def findDBHeader(ip, usr, passwd, db, table):
    try:
        con = mdb.connect(ip, usr, passwd, db)
        with con:
            cur = con.cursor()
            cur.execute("SELECT * FROM " + table + " LIMIT 5")
            field_names = [i[0] for i in cur.description]
            return field_names
    except:
        print 'can not connect to db and retrieve DB field names'


def findTrialsFromCTlist(ctlist, ip, usr, passwd, db, table):
    cur = ''
    try:
        print 'try connecting to db...'
        con = mdb.connect(ip, usr, passwd, db)
        cur = con.cursor()
    except Exception:
        print 'can not connect to db', Exception

    try:
        if 1:
            print 'connected'
            if type(ctlist) is list:
                candi = '","'.join(ctlist)
            else:
                candi = ctlist
            candi = '("' + candi + '")'
            statement = "SELECT * FROM "+table+" WHERE tid IN " + candi
            print 'forming statement: ', statement
            cur.execute(statement)
            content = []
            #desc = cur.description
            #dbInfo = [i[0] for i in desc]
            for i in range(cur.rowcount):
                row = cur.fetchone()
                content.append(row)
            return content
    except Exception:
        print 'can not connect to db and retrieve trials from list: ', ctlist
        print Exception


def findTrialsFromCTlist_selectPhase(ctlist, phase, ip, usr, passwd, db, table):
    try:
        con = mdb.connect(ip, usr, passwd, db)
        with con:
            cur = con.cursor()
            candi = '","'.join(ctlist)
            candi = '("' + candi + '")'
            statement = "SELECT * FROM "+table+" WHERE tid IN " + candi + " AND phase LIKE '%"+phase+"%'"
            cur.execute(statement)
            content = []
            #desc = cur.description
            #dbInfo = [i[0] for i in desc]
            for i in range(cur.rowcount):
                row = cur.fetchone()
                content.append(row)
            return content
    except:
        print 'can not connect to db and retrieve trials from list %s', ctlist


def findTrialsFromCTlist_selectPeriod(ctlist, period_preORpost,ip, usr, passwd, db, table):
    try:
        if period_preORpost == "pre":
            stat = " AND phase NOT LIKE '%Phase 4%' AND phase NOT LIKE '%N/A%'"
        elif period_preORpost == "post":
            stat = " AND phase LIKE '%Phase 4%'"
        else:
            raise SyntaxError
        con = mdb.connect(ip, usr, passwd, db)
        with con:
            cur = con.cursor()
            candi = '","'.join(ctlist)
            candi = '("' + candi + '")'
            statement = "SELECT * FROM "+table+" WHERE tid IN " + candi + stat
            cur.execute(statement)
            content = []
            #desc = cur.description
            #dbInfo = [i[0] for i in desc]
            for i in range(cur.rowcount):
                row = cur.fetchone()
                content.append(row)
            return content
    except:
        print 'can not connect to db and retrieve trials from list %s', ctlist


def findTrialsFromCTlist_selectSponsor(ctlist, sponsor, ip, usr, passwd, db, table):
    try:
        con = mdb.connect(ip, usr, passwd, db)
        with con:
            cur = con.cursor()
            candi = '","'.join(ctlist)
            candi = '("' + candi + '")'
            statement = "SELECT * FROM "+table+" WHERE tid IN " + candi + " AND agency_type LIKE '%"+sponsor+"%'"
            cur.execute(statement)
            content = []
            #desc = cur.description
            #dbInfo = [i[0] for i in desc]
            for i in range(cur.rowcount):
                row = cur.fetchone()
                content.append(row)
            return content
    except:
        print 'can not connect to db and retrieve trials from list %s', ctlist


def findTrialsFromDrug(drugName, period):
    trialList = findCTListByDrug_local(fh_ctgov_drugTrialList_tab,drugName,period)
    trialContent = findTrialsFromCTlist(trialList, para_mysql_ip, para_mysql_usr, para_mysql_passwd, para_mysql_db, para_mysql_mainTable)
    return trialContent
