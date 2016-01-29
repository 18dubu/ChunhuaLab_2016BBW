__author__ = 'mahandong'
import MySQLdb as mdb

def writeEntry(ori, seg, ip, usr, passwd, db, table):
    con = None
    try:
        con = mdb.connect(ip, usr, passwd, db)
    except:
        print 'error in writeEntry: can not connect to db'
    with con:
        try:
            cur = con.cursor()
            command = "INSERT INTO " + table + " (originalSentence, manualSegment) VALUES (\""+ ori +"\",\""+seg+"\");"
            cur.execute(command)
        except Exception:
            print 'error in writeEntry: can not execute command'

#ori = 'Exclusion Criteria'
#seg = 'NA'
#writeEntry(ori,seg,"127.0.0.1","root",'newpass','drugFDA','trainingPhrase')


def getColFromDB(colName, ip, usr, passwd, db, table):
    try:
        con = mdb.connect(ip, usr, passwd, db)
        with con:
            cur = con.cursor()
            cur.execute("SELECT "+ colName +" FROM " + table + " ;")
            content = []
            for i in range(cur.rowcount):
                row = cur.fetchone()
                content.append(row)
            return content
    except:
        print 'error in getColFromDB: can not connect to db and retrieve DB field names'