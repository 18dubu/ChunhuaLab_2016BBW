__author__ = 'mahandong'
#
# #naive stat parser, no customize, not tested in case reports
# def chunkByDrug_stat(sampleDrug = sampleDrug_pos, samplePeriod = 0, printDBHeader = None):
#
#     #sample period: 0 is pre, 1 is post
#     sampleTrialModeOn = '' #NCT00604695
#
#     #pyStatParser, a simple python statistical parser that returns NLTK parse Trees.
#     #http://stackoverflow.com/questions/6115677/english-grammar-for-parsing-in-nltk
#     #from stat_parser import Parser
#     parser = Parser()
#
#     target_Trials = findCTListByDrug_local(fh_ctgov_drugTrialList_tab,sampleDrug,samplePeriod)
#
#     if len(target_Trials) > 0:
#         trialContent = findTrialsFromCTlist(target_Trials, para_mysql_ip, para_mysql_usr, para_mysql_passwd, para_mysql_db, para_mysql_mainTable)
#         if printDBHeader:
#             dbHeader = findDBHeader(para_mysql_ip, para_mysql_usr, para_mysql_passwd, para_mysql_db, para_mysql_mainTable)
#             print dbHeader
#         chunkedTrialContents = {}
#         for singleTrial in trialContent:
#             id = singleTrial[0]
#             ec = singleTrial[11]
#             flaggedText = setFlag(ec) #[[inc],[ex],[other]]
#             if len(flaggedText[0])==0 and len(flaggedText[1])==0:
#                 print 'Warning: Not be able to find standard IN/EX criteria in trial: ', id
#                 continue
#             else:
#                 phrase = [[],[],[]]
#                 #print id, flaggedText[0]
#                 if (len(flaggedText[0])>0):
#                     print '\n'.join(flaggedText[0])
#                     for i in range(len(flaggedText[0])):
#                         text = flaggedText[0][i]
#                         parsed = parser.parse(text)
#                         phrase[0].append(parsed)
#                     #phrase[0].append = parser.parse('\n'.join(flaggedText[0]))
#                     print parser.parse('\n'.join(flaggedText[0]))
#                     #phrase[0].append(chunkText('\n'.join(flaggedText[0]), para_chunking_grammar))
#                 else:
#                     phrase[0].append('')
#                 if (len(flaggedText[1])>0):
#                     phrase[1].append = parser.parse('\n'.join(flaggedText[1]))
#                     #phrase[1].append(chunkText('\n'.join(flaggedText[1]), para_chunking_grammar))
#                 else:
#                     phrase[1].append('')
#                 if (len(flaggedText[2])>0):
#                     phrase[2].append = parser.parse('\n'.join(flaggedText[2]))
#                     #phrase[2].append(chunkText('\n'.join(flaggedText[2]), para_chunking_grammar))
#                 else:
#                     phrase[2].append('')
#                 chunkedTrialContents[id] = phrase
#         return chunkedTrialContents
# #all = chunkByDrug_stat()
# #print all