__author__ = 'mahandong'


fh_ndc_source = '../data/ndc/product_prescription.csv'

#retrieveOnlineDrugTrialInfo function parameters
fh_ctgov_drugTrialContent_csv = '../result/drug_trial_content.csv'
fh_ctgov_drugTrialContent_tab = '../result/drug_trial_content_tab'
fh_ctgov_drugTrialList_tab = '../result/drug_trial_List'
fh_ctgov_drugTrialTmp = '../result/drug_trial_tmp'

#eligibile drug-trial pairs, by (BBW, robust)*(pre,post)
para_selected_drugTrialList_dir = '../result/selected_drug_trial_List/'


para_topSales_target_dir='../result/topSales/'
para_drugSalesTargetYear_List = ['http://www.drugs.com/top200_2003.html','http://www.drugs.com/top200_2004.html','http://www.drugs.com/top200_2005.html','http://www.drugs.com/top200_2006.html','http://www.drugs.com/top200_2007.html','http://www.drugs.com/top200_2008.html','http://www.drugs.com/top200_2009.html','http://www.drugs.com/top200.html']
#or ['http://www.drugs.com/top200_2003.html','http://www.drugs.com/top200_2004.html','http://www.drugs.com/top200_2005.html','http://www.drugs.com/top200_2006.html','http://www.drugs.com/top200_2007.html','http://www.drugs.com/top200_2008.html','http://www.drugs.com/top200_2009.html','http://www.drugs.com/top200.html', 'http://www.drugs.com/stats/top100/2011/sales','http://www.drugs.com/stats/top100/2012/sales','http://www.drugs.com/stats/top100/2013/sales']


para_pdr_target_dir = "../result/PDR/"
para_pdr_source_url = 'http://www.pdr.net/browse-by-drug-name'

para_mysql_ip = '156.145.134.234'
para_mysql_usr = 'elixr'
para_mysql_passwd = 'columbiaelixr'
para_mysql_db = 'clinicaltrials'
para_mysql_mainTable = 'trials'


para_mysql_ip_drugdata = '127.0.0.1'
para_mysql_usr_root = 'root'
para_mysql_passwd_root = 'newpass'
para_mysql_db_drugFDA = 'drugFDA'
para_mysql_mainTable_trainingPhrase = 'trainingPhrase'


para_sampleDrug_pos = 'Lipitor'
para_sampleDrug_bbw = 'Actos'
para_samplePeriod = 0 # o for pre, 1 for post

para_lowerLimit_CTperPerid = 5

#not used and replaced by stat_parser
para_chunking_grammar = (r'''
    NP:
    {<NNS><CC><NNS>} #MEN OR WOMEN
    {<CD>?<NN.*>?<IN.*>+<JJ>+<NN.*>+}
    {<JJ>?<NN.*>+<TO>?<NN.*>?}
    {<NN.*>?<CC>?<``>?<CD|VBG|JJ.*>+<NN.*>+<JJ.*>?<IN>?<NP|NN>?<PP>?}
    {<NP><PP>}
    {<CD>+<.*><CD>+}
    PP:
    {<JJ.*>?<IN><NP|NN><PP>?}
    {<PP>+<NP><PP>+}
    {<PP>?<IN><JJS><NP><PP>?}
    {<IN><VP|NP|QP>}
    {<TO><NP|QP>}
    {<VBG><PP|NP>}
    {<ADVP><IN><NP>}
    VP:
    {<V.*>+<NP|PP>}
    {<TO><VP>}
    {<VB.*><VP|NP|PP>}
    ADJP:
    {<NP><JJ>}
    {<JJ><PP|VP>*}
    {<JJ><CC>?<JJ>}
    {<RB><JJ|VBN>}
    {<CD><NN>}
    {<ADJP><PP>}
    {<RBR|ADVP><JJ>}
    {<JJR>}
    ADVP:
    {<RB>+}
'''
)


#symbol used for stat_parser
'''
    'S',      # simple declarative clause, i.e. one that is not introduced by a (possible empty) subordinating conjunction or a wh-word and that does not exhibit subject-verb inversion.
    'SBAR',   # Clause introduced by a (possibly empty) subordinating conjunction.
    'SBARQ',  # Direct question introduced by a wh-word or a wh-phrase. Indirect questions and relative clauses should be bracketed as SBAR, not SBARQ.
    'SINV',   # Inverted declarative sentence, i.e. one in which the subject follows the tensed verb or modal.
    'SQ',     # Inverted yes/no question, or main clause of a wh-question, following the wh-phrase in SBARQ.

    'ADJP',   # Adjective Phrase.
    'ADVP',   # Adverb Phrase.
    'CONJP',  # Conjunction Phrase.
    'FRAG',   # Fragment.
    'INTJ',   # Interjection. Corresponds approximately to the part-of-speech tag UH.
    'LST',    # List marker. Includes surrounding punctuation.
    'NAC',    # Not a Constituent; used to show the scope of certain prenominal modifiers within an NP.
    'NP',     # Noun Phrase.
    'NX',     # Used within certain complex NPs to mark the head of the NP. Corresponds very roughly to N-bar level but used quite differently.
    'PP',     # Prepositional Phrase.
    'PRN',    # Parenthetical.
    'PRT',    # Particle. Category for words that should be tagged RP.
    'QP',     # Quantifier Phrase (i.e. complex measure/amount phrase); used within NP.
    'RRC',    # Reduced Relative Clause.
    'UCP',    # Unlike Coordinated Phrase.
    'VP',     # Vereb Phrase.
    'WHADJP', # Wh-adjective Phrase. Adjectival phrase containing a wh-adverb, as in how hot.
    'WHADVP',  # Wh-adverb Phrase. Introduces a clause with an NP gap. May be null (containing the 0 complementizer) or lexical, containing a wh-adverb such as how or why.
    'WHNP',   # Wh-noun Phrase. Introduces a clause with an NP gap. May be null (containing the 0 complementizer) or lexical, containing some wh-word, e.g. who, which book, whose daughter, none of which, or how many leopards.
    'WHPP',   # Wh-prepositional Phrase. Prepositional phrase containing a wh-noun phrase (such as of which or by whose authority) that either introduces a PP gap or is contained by a WHNP

    'CC',     # Coordinating conjunction
    'CD',     # Cardinal number
    'DT',     # Determiner
    'EX',     # Existential there
    'FW',     # Foreign word
    'IN',     # Preposition or subordinating conjunction
    'JJ',     # Adjective
    'JJR',    # Adjective, comparative
    'JJS',    # Adjective, superlative
    'LS',     # List item marker
    'MD',     # Modal
    'NN',     # Noun, singular or mass
    'NNS',    # Noun, plural
    'NNP',    # Proper noun, singular
    'NNPS',   # Proper noun, plural
    'PDT',    # Predeterminer
    'POS',    # Possessive ending
    'PRP',    # Personal pronoun
    'PRP$',   # Possessive pronoun (prolog version PRP-S)
    'RB',     # Adverb
    'RBR',    # Adverb, comparative
    'RBS',    # Adverb, superlative
    'RP',     # Particle
    'SYM',    # Symbol
    'TO',     # to
    'UH',     # Interjection
    'VB',     # Verb, base form
    'VBD',    # Verb, past tense
    'VBG',    # Verb, gerund or present participle
    'VBN',    # Verb, past participle
    'VBP',    # Verb, non-3rd person singular present
    'VBZ',    # Verb, 3rd person singular present
    'WDT',    # Wh-determiner
    'WP',     # Wh-pronoun
    'WP$',    # Possessive wh-pronoun (prolog version WP-S)
    'WRB',    # Wh-adverb

    '.',      # Sentence final puntuation
    ',',      # Comma
    ':',      # Mid sentence punctuation
    '-LRB-',  # Left parenthesis
    '-RRB-',  # Right parenthesis
    '``',     # Start quote
    "''",     # End quote
    '#',      # Pound sign
    '$',      # Dollar sign

    # These will be filtered in the following steps
    '',       # Empty
    '-NONE-', #
    'X'       # Uncertain
'''

text = '''
Criteria
Inclusion Criteria:

Subjects (men or women) at least 18 years and less than 75 years of age and
Ischemic discomfort 20 minutes and 6 hours of duration and
ST elevation 1mm (0.1mV) in two contiguous limb leads OR 2mm (0.2mV) in two contiguous precordial leads and
Occluded infarct-related artery (TIMI Flow Grade 0 or 1) at the time of coronary angiography and
Planned primary PCI within 2 hours of hospital presentation and
Planned or concomitant use of aspirin, clopidogrel, unfractionated heparin, and Glycoprotein IIb/IIIa inhibition with intent to stent the infarct-related artery and
Informed consent able to be obtained
Exclusion Criteria:

CLINICAL

Age >75 years
Maximal systolic blood pressure <80 mmHg AFTER initial fluid and/or pressor resuscitation.
Uncontrolled hypertension (SBP >180 OR DBP >110) at time of enrollment.
Cardiac arrest or arrhythmia requiring chest compressions or cardiopulmonary resuscitation.
Known pregnancy.
BIOCHEMICAL

Known thrombocytopenia (platelet count <100,000)
Known severe renal insufficiency (creatinine >4.0 mg/dL).
INCREASED BLEEDING RISK

Active internal bleeding
Recent (<3 months) gastrointestinal hemorrhage
Recent intracranial or intraspinal surgery, trauma, major surgery, or biopsy of a parenchymal organ (< 1 month)
Known coagulopathy, platelet disorder, or history of thrombocytopenia
Current warfarin therapy
Known neoplasm
Any known history of transient ischemic attack, cerebrovascular accident, or active intracranial pathology including arteriovenous malformation or aneurysm
MEDICATIONS

Administration of a fibrinolytic agent within 72 hours
Known allergy or contraindication to fibrinolytics OR aspirin OR heparin OR clopidogrel
ANGIOGRAPHIC

Left Main Coronary artery culprit lesion
Ostial culprit lesion (ostium of LAD, LCX, or RCA).
Lesion in non-native coronary artery (e.g. saphenous vein graft, arterial conduit graft)
Subjects requiring urgent coronary artery bypass grafting
'''