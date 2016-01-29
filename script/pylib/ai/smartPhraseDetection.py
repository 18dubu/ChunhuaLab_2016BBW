__author__ = 'mahandong'
'''
The package learns how a human being separate a sentence into phrases of wanted granularity
'''

from pylib.db.writeTrainingDB import *
from pylib.analysis.preprocessing import *
import sys



sampleDrug = para_sampleDrug_pos
samplePeriod = para_samplePeriod


def presentSentence(drugName = sampleDrug):
    trialContent = findTrialsFromDrug()

def manual_train():
    #sample period: 0 is pre, 1 is post
    sentenceInDB = []
    try:
        sentenceInDB = getColFromDB('originalSentence',"127.0.0.1","root",'newpass','drugFDA','trainingPhrase')
        print sentenceInDB
    except Exception:
        print "error 308"


    trialContent = findTrialsFromDrug(sampleDrug,samplePeriod)
    if trialContent:
        chunkedTrialContents = {}
        for singleTrial in trialContent:
            id = singleTrial[0]
            ec = singleTrial[11]
            flaggedText = setFlag(ec) #[[inc],[ex],[other]]
            if len(flaggedText[0])==0 and len(flaggedText[1])==0:
                print 'Warning: Not be able to find standard IN/EX criteria in trial: ', id
            if len(flaggedText[0]) > 0:
                for i in range(len(flaggedText[0])):
                    sentence = flaggedText[0][i]
                    passFlag = 0
                    for i in range(len(sentenceInDB)):
                        if sentence in str(sentenceInDB[i]):
                            passFlag = 1
                            break
                    if passFlag == 1:
                        continue

                    print '------------------------------'
                    print 'current sentence is: ', sentence
                    parsedRules = ';'.join(parse_stat(sentence))
                    print 'Suggested Parsed Rules are: ', parsedRules
                    get = str(raw_input('wait for your modification(-1:discharge sentence; 1: all as one; others: separator ";"): '))
                    if str(get) == "-1":
                        get = 'NA'
                    if str(get) == "1":
                        get = sentence
                    if str(get) == "0":
                        sys.exit()
                    if str(get) == '2':
                        get = parsedRules
                    try:
                        writeEntry(sentence, get, "127.0.0.1","root",'newpass','drugFDA','trainingPhrase')
                    except Exception:
                        print "Warning can not connect to DB and write entry: ", sentence,">>",get
                        continue