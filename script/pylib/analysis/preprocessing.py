__author__ = 'mahandong'
import nltk.data
from ..db.findTrials import *
from stat_parser import Parser
from nltk.tree import *
from nltk.corpus import stopwords
import sys,re

'''
FUNCTION: sentence detection and assign flag to each sentence or concept by indicated lineSplit
INPUT:  linc split symbol that separates different fields in one input text;
        sectionFlagList refer to symbols that change flag [[],[]];
        sectionName, same length as list length of sectionFlagList +1, ADD NA;
        quiet, whether want print line content that changes flag, for debugging purpose;
RETURN: multidimensional list correspond to input sections, with text in each cell
'''
def setFlag(text,lineSplit='#',sectionFlagList = [['Inclusion Criteria','inclusion'],['exclusion criteria','exclusion']], sectionName=['INCLUSION','EXCLUSION','NA'], quiet=True):
    try:
        sent_detector = nltk.data.load('tokenizers/punkt/english.pickle')
    except Exception as e:
        print e
        return -1
    flagText = []
    for i in range(len(sectionName)):
        flagText.append([])
    aspects = text.replace('\n', '#').replace('\t+', ' ').rstrip().split(lineSplit)
    flag = -1
    pre_flag = -1
    for aspect in aspects:
        for sectionNum in range(len(sectionFlagList)):
            section = sectionFlagList[sectionNum]
            for case in range(len(section)):
                compare = section[case]
                if re.search(compare, aspect, re.IGNORECASE) is not None:
                    flag = sectionNum
                    continue
            continue
        if flag != pre_flag and not quiet:
            print "This line(\"" + aspect + "\") changed section from: " + sectionName[pre_flag] + ' to: ' + sectionName[flag]
            pre_flag = flag
            continue

        for sentence in sent_detector.tokenize(aspect.strip()):
            flagText[flag].append(sentence)
    return flagText

def tokenizeAndTagging(text):
    text_token = None
    sentence = None
    try:
        text_token = nltk.word_tokenize(text)
    except Exception:
        print 'nltk word_tokenize error: '
        #print text
    try:
        sentence = nltk.pos_tag(text_token)
        return sentence
    except Exception:
        print 'nltk pos_tag error: '
        #print Exception


def sentenceDetection(text):
    tokenizer = nltk.data.load('tokenizers/punkt/english.pickle')
    return '\n'.join(tokenizer.tokenize(text))


def chunkText(text,grammar):
    text_token = None
    sentence = None
    result = None
    try:
        text_token = nltk.word_tokenize(text)
    except Exception:
        print 'nltk word_tokenize error: '
        #print text
    try:
        sentence = nltk.pos_tag(text_token)
    except Exception:
        print 'nltk pos_tag error: '
        #print Exception
    try:
        cp = nltk.RegexpParser(grammar)
        result = cp.parse(sentence)
    except Exception:
        print 'nltk RegexpParser and parse error: '
        #print Exception
    #print "pos_tagged: ", sentence
    if result:
        chunked = []
        for subtree in result.subtrees(filter=lambda t: t.label() == 'PP' or t.label() == 'VP' or t.label() == 'NP'):
            #print the noun phrase as a list of part-of-speech tagged words
            tagged = subtree.leaves()
            full = ''
            for i in tagged:
                if isinstance(i, list):
                    for j in range(len(i)):
                        full += i[j][0]
                        full += ' '
                else:
                    full += i[0]
                    full += ' '
            all = str(subtree.label())+'_'+str(full)
            chunked.append(all)
            #print ">>>>>>", subtree.label(),full
        return chunked
    else:
        print 'chunkText error 1: result = None'


def chunkByDrug_customizeGrammar(sampleDrug = 'Nexium', samplePeriod = 0):

    #sample period: 0 is pre, 1 is post
    sampleTrialModeOn = '' #NCT00604695

    #pyStatParser, a simple python statistical parser that returns NLTK parse Trees.
    #http://stackoverflow.com/questions/6115677/english-grammar-for-parsing-in-nltk
    #from stat_parser import Parser
    #parser = Parser()

    target_Trials = findCTListByDrug_local(fh_ctgov_drugTrialList_tab,sampleDrug,samplePeriod)

    if len(target_Trials) > 0:
        dbHeader = findDBHeader(para_mysql_ip, para_mysql_usr, para_mysql_passwd, para_mysql_db, para_mysql_mainTable)
        trialContent = findTrialsFromCTlist(target_Trials, para_mysql_ip, para_mysql_usr, para_mysql_passwd, para_mysql_db, para_mysql_mainTable)
        print dbHeader
        chunkedTrialContents = {}
        for singleTrial in trialContent:
            id = singleTrial[0]
            ec = singleTrial[11]
            flaggedText = setFlag(ec) #[[inc],[ex],[other]]
            if len(flaggedText[0])==0 and len(flaggedText[1])==0:
                print 'Warning: Not be able to find standard IN/EX criteria in trial: ', id
                continue
            else:
                phrase = [[],[],[]]
                #print id, flaggedText[0]
                if (len(flaggedText[0])>0):
                    phrase[0].append(chunkText('\n'.join(flaggedText[0]), para_chunking_grammar))
                else:
                    phrase[0].append('')
                if (len(flaggedText[1])>0):
                    #phrase[1].append = parser.parse('\n'.join(flaggedText[1]))
                    phrase[1].append(chunkText('\n'.join(flaggedText[1]), para_chunking_grammar))
                else:
                    phrase[1].append('')
                if (len(flaggedText[2])>0):
                    #phrase[2].append = parser.parse('\n'.join(flaggedText[2]))
                    phrase[2].append(chunkText('\n'.join(flaggedText[2]), para_chunking_grammar))
                else:
                    phrase[2].append('')
                chunkedTrialContents[id] = phrase
        return chunkedTrialContents
#all = chunkByDrug_customizeGrammar()


'''
Tree manipulation, Extract phrases from a parsed (chunked) tree
Phrase = tag for the string phrase (sub-tree) to extract
Returns: List of deep copied trees;  Recursive
'''
def ExtractPhrases( myTree):
    myPhrases = []

    includeKeyWords = ['age','men','women','female','male','old','or','known']
    excludeKeyWords = ['inclusion criteria','exclusion criteria']

    treeString = str(myTree.flatten())
    if (myTree.label() == 'NP' or myTree.label() == "ADJP" or myTree.label() == 'VP' or myTree.label() == 'PP' or myTree.label() =='ADVP' or myTree.label() =='NX+NX' or myTree.label() =='S'):
        #special words that should be excluded
        passFlag1 = 0
        for test in excludeKeyWords:
            if treeString.lower().find(test.lower())>=0 and len(treeString.split(' '))<10:
                passFlag1 = 1
                break
        if passFlag1 ==0:
            myPhrases.append(myTree.copy(True))
        #special words that should be included
    for test in includeKeyWords:
        if treeString.lower().find(test.lower())>=0 and len(treeString.split(' '))<10:
            myPhrases.append(myTree.copy(True))
            break

    for child in myTree:
        if (type(child) is Tree):
            list_of_phrases = ExtractPhrases(child)
            if (len(list_of_phrases) > 0):
                myPhrases.extend(list_of_phrases)
    return myPhrases


'''
pyStatParser, a simple python statistical parser that returns NLTK parse Trees.
http://stackoverflow.com/questions/6115677/english-grammar-for-parsing-in-nltk
Input text can be multiple sentences, split bt \n

###### Jun13 2014, most used currently
'''
def parse_stat(text, interactive=None, quiet=None):
    rules = []
    parser = Parser()
    stop = stopwords.words('english')
    text = sentenceDetection(text)
    sentence = text.split('#')
    for ana in sentence:
        if len(ana)>5:
            parsed = parser.parse(ana) #################################
            if not quiet:
                print "sentence is: ", ana
                print "label is: ", parsed.label()
            maxHeight = 0
            list_of_noun_phrases = ExtractPhrases(parsed)
            #max height of the tree
            for i in list_of_noun_phrases:
                if i.height()> maxHeight:
                    maxHeight = i.height()
            included = []
            findRule = 0
            for phrase in list_of_noun_phrases:
                current = str(phrase.flatten())
                if len(current.split(' '))<2:
                    continue
                try:
                    phraseType = re.search('^\(*\S+\s', current).group(0).rstrip()[1:]
                except Exception:
                    print 'Error 186: ', Exception
                    print current
                    continue
                #print 't: ', phraseType
                #print '---', phrase.height()
                #print '+++', current
                rule = str(current).replace('\n',' ').replace('\s+','\s')
                rule = re.sub('^\(\S+\s', '', rule)
                rule = re.sub('\)$','', rule)
                if len(rule.split(' '))>=2:
                    if rule.split(' ')[0] in stop:
                        rule = rule.split(' ', 1)[1]
                    if rule.split(' ')[-1] in stop:
                        rule = rule.rsplit(' ', 1)[0]
                else:
                    continue
                passFlag = 0
                #print included
                for i in range(len(included)):
                    #print included[i],'   ', rule,'   ', included[i].find(rule)
                    if included[i].find(rule)>=0:
                        passFlag = 1
                        break
                if passFlag == 0:

                    findRule = 1

                    included.append(rule)
                    rules.append(rule)
                    if not quiet:
                        print "Extracted phrases:"
                        print '================================'
                        print "|", rule, "|"
                        print '================================\n'
            if findRule == 0:
                if not quiet:
                    print 'sentence disregarded!'
                    print ''
            if interactive:
                command = raw_input()
                try:
                    c = int(command)
                    if c >= 1:
                        print 'PHRASES: '
                        for phrase in list_of_noun_phrases:
                            print str(phrase)
                            print
                    if c >= 2:
                        print 'PARSED: '
                        print parsed
                        print ''
                    if c == 0:
                        sys.exit()
                except ValueError:
                    continue
    return rules

#parse_stat(text)



def parse_stat_sentence(sentence, interactive=None, quiet=None):
    rules = []
    parser = Parser()
    stop = stopwords.words('english')
    if 1:
        if len(sentence)>5:
            parsed = parser.parse(sentence) #################################
            if not quiet:
                print "sentence is: ", sentence
                print "label is: ", parsed.label()
            maxHeight = 0
            list_of_noun_phrases = ExtractPhrases(parsed)
            #max height of the tree
            for i in list_of_noun_phrases:
                if i.height()> maxHeight:
                    maxHeight = i.height()
            included = []
            findRule = 0
            for phrase in list_of_noun_phrases:
                current = str(phrase.flatten())
                if len(current.split(' '))<2:
                    continue
                try:
                    phraseType = re.search('^\(*\S+\s', current).group(0).rstrip()[1:]
                except Exception:
                    print 'Error 186: ', Exception
                    print current
                    continue
                #print 't: ', phraseType
                #print '---', phrase.height()
                #print '+++', current
                rule = str(current).replace('\n',' ').replace('\s+','\s')
                rule = re.sub('^\(\S+\s', '', rule)
                rule = re.sub('\)$','', rule)
                if len(rule.split(' '))>=2:
                    if rule.split(' ')[0] in stop:
                        rule = rule.split(' ', 1)[1]
                    if rule.split(' ')[-1] in stop:
                        rule = rule.rsplit(' ', 1)[0]
                else:
                    continue
                passFlag = 0
                #print included
                for i in range(len(included)):
                    #print included[i],'   ', rule,'   ', included[i].find(rule)
                    if included[i].find(rule)>=0:
                        passFlag = 1
                        break
                if passFlag == 0:

                    findRule = 1

                    included.append(rule)
                    rules.append(rule)
                    if not quiet:
                        print "Extracted phrases:"
                        print '================================'
                        print "|", rule, "|"
                        print '================================\n'
            if findRule == 0:
                if not quiet:
                    print 'sentence disregarded!'
                    print ''
            if interactive:
                command = raw_input()
                try:
                    c = int(command)
                    if c >= 1:
                        print 'PHRASES: '
                        for phrase in list_of_noun_phrases:
                            print str(phrase)
                            print
                    if c >= 2:
                        print 'PARSED: '
                        print parsed
                        print ''
                    if c == 0:
                        sys.exit()
                except ValueError:
                    pass
    return rules

#parse_stat(text)