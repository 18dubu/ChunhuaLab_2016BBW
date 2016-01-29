rm(list=ls())
library(tm)
library(SnowballC)
library(proxy)
library(entropy)
library(MASS)
library(Matching)
library(lsa)
library(gplots)
library(ROCR)
library(ggplot2)
source('./rlib/elitesLib.R')
library(zoo)

#
library(randomForest)
library(lattice)
library(ggplot2)
library(caret)
library(VIM)
library(biglm)
library(allan)
#imputation
library(mi)
library(mice)
library(missForest)
library(party)
library(pROC)
#####################################################


fhIn_dir <- '../result/selected_drug_trial_List/'
bbwDateTableName <- '../result/drug_Date_BBW_table_manual.csv'
integrateTableName <- 'integratedDrugTrialTable.csv'
integrateTableNameUpdate <- 'integratedDrugTrialTableMatched.csv'

getIntegrateTable_switch = 'on'
generateMedParserInputs_switch = 'on'
getMedParserResult_switch = 'on'
getSnowMedResult_switch = 'on'
getIntegratedTableUpdated_switch ='on'
useIntegratedTableUpdated_switch = 'on'
runCoherenceOccurenceTest_switch ='on'
matchPharmGKBName_switch = 'on'
mappingADEOffsides_switch='on'
mappingDDITwosides_switch='on'
save_preprocessed_sets = TRUE

stopWords <- stopwords("en")


#############generate integrate table###########################
#make sure all "comp" files and correspond "list" files are in the fhIn_dir
if (getIntegrateTable_switch == 'on'){
  getIntegrateTable(fhIn_dir,integrateTableName)
}
##########################################
if(getIntegrateTable_switch == 'on' |generateMedParserInputs_switch =='on'|getMedParserResult_switch == 'on'|getSnowMedResult_switch == 'on'|getIntegratedTableUpdated_switch == 'on'){
  mainTable <- read.csv(paste(fhIn_dir,integrateTableName,sep=''),header=T, stringsAsFactors=F)
}
################################################################
#generate files for medParser and drug and dosage detection
ptm <- proc.time()
if (generateMedParserInputs_switch == 'on'){
  #generate MedParser Input files
  generateMedParserInputs_byCT(mainTable)  
  print('Running MedParser From shell...')
  #run MedParser, slow ~min
  setwd('./medex_mac/')
  system('bash run.sh')  #NOTE that once running, it can not be killed 
  setwd('../')
  print('Running MedParser From shell...Done!')
}

if (getMedParserResult_switch == 'on'){
  filesdir<-'./medex_mac/output/'
  file_list <- list.files(filesdir)
  if (length(file_list)>0){
    ECdrug<-getMedParserResult_byCT(file_list,mainTable)
  }else{
    stop('No MedParser Output file detected in directory')
  }
  
  if (length(ECdrug$ECDrug)==length(mainTable[,1])){
    mainTable$ECdrug = ECdrug$ECDrug
  }
  if (length(ECdrug$ECCUI)==length(mainTable[,1])){
    mainTable$ECcui = ECdrug$ECCUI
  }
}
print(paste('Running time of the MedParser Part is: ',(proc.time()-ptm)[3],'seconds')) #Running time of the MedParser Part is:  2565.03200000001 seconds

######################################################################################
#using SNOWMED vocab to find disease names in text; LONG TIME
ptm <- proc.time()
if(getSnowMedResult_switch == 'on'){
  targetFile <- integrateTableName
  snowmed<-read.csv('../data/SNOMEDCT_CORE_SUBSET_201405/SNOMEDCT_CORE_SUBSET_201405.txt',sep='|',header = T)
  snowmed_disease<-tolower(sub('\\s*\\(\\w+\\)','',snowmed$SNOMED_FSN))
  #content<-paste(mainTable$official_title,mainTable$condition,mainTable$brief_summary,mainTable$criteria,sep=';')
  
  #manually exclude some entries in snowmed
  manual_exclude <- c('blind')
  snowmed_disease <- snowmed_disease[!snowmed_disease %in% manual_exclude]

  
  snowmed_disease_refined <- unlist(lapply(snowmed_disease,function(x) x[length(unlist(strsplit(x,'\\s+'))) <= 3]))
  snowmed_disease_refined_pattern <- paste('\\b',snowmed_disease_refined,'\\b',sep='')
  #mainTable_unique <- mainTable[!duplicated(mainTable[,'nct_id']),]
  
  mainTable_DrugChunk <- split(mainTable,mainTable$drug)
  counter = 0
  for (currentChunkID in 1:length(mainTable_DrugChunk)){
    print (paste("analyzing drug ", currentChunkID, ' (total drug count: ',length(mainTable_DrugChunk),')',sep=''))
    cacheFile <- paste('../result/anaCoherenceOccurrence/SNOWMED_mapping/',names(mainTable_DrugChunk[currentChunkID]),'.csv',sep = '')
    
    currentDrugTable <- mainTable_DrugChunk[[currentChunkID]]
    content<-paste(currentDrugTable$official_title,currentDrugTable$condition,currentDrugTable$brief_summary,currentDrugTable$criteria,sep=';')
        
    occur<-sapply(snowmed_disease_refined_pattern, grepl, content, ignore.case=TRUE) #one line is one trial, one col is one instance in snowmed
    
    currentDrugTable$ECdisease <- rep(NA,length(occur[,1]))
    for (i in 1:length(occur[,1])){
      counter = counter + 1
      currentDrugTable$ECdisease[i]<-''
      mainTable$ECdisease[counter] <-''
      currentDrugTable$ECdisease[i]<-paste(gsub('\\\\b','',names(occur[i,][occur[i,]==TRUE])),collapse=';')
      mainTable$ECdisease[counter] <-paste(gsub('\\\\b','',names(occur[i,][occur[i,]==TRUE])),collapse=';')
    }
    
    write.csv(currentDrugTable,cacheFile,row.names=FALSE)
    
  }
}
print(paste('Running time of the SNOWMED Part is: ',(proc.time()-ptm)[3],'seconds')) #Running time of the SNOWMED Part is:  4473.28400000001 seconds
#####################UPDATE INTEGRATED TABLE##################
#ADD SNOWMED DISEASE and MedParser DRUG INFO
if(getIntegratedTableUpdated_switch =='on'){
  write.csv(mainTable,file = paste(fhIn_dir,integrateTableNameUpdate,sep = ''),row.names=FALSE)
  print ('Integrated Table updated with SNOWMED DISEASE and MedParser DRUG INFO!')
}
####################################################
#use the updated version
if (useIntegratedTableUpdated_switch == 'on'){
  mainTable <- read.csv(paste(fhIn_dir,integrateTableNameUpdate,sep=''),header=T, stringsAsFactors=F)
}
else{
   mainTable <- read.csv(paste(fhIn_dir,integrateTableName,sep=''),header=T, stringsAsFactors=F)
}

#refine and reformat
mainTable<- mainTable[mainTable$enrollment>0,]
mainTable<- mainTable[mainTable$start_date>0,]
mainTable$start_date <- as.Date(as.yearmon(mainTable$start_date,"%B %Y"))

####

#filter drug list based on BBW info
if(1){
  bbwManual_ori <- read.csv(bbwDateTableName,header=T,stringsAsFactors=F, na.strings=c("NA", ""))
  bbwManual <- bbwManual_ori[!is.na(bbwManual_ori$DRUG_GROUP),]  
  #robust
  bbwManual_robust <- bbwManual[bbwManual$DRUG_GROUP=='ROBUST',]
  bbwManual_robust <- bbwManual_robust[!is.na(bbwManual_robust$MARKETING_DATE),]
  bbwManual_robust$MARKETING_DATE<-as.Date(as.character(bbwManual_robust$MARKETING_DATE),'%Y%m%d')
  #bbw
  bbwManual_bbw <- bbwManual[bbwManual$DRUG_GROUP=='BBW',]
  bbwManual_bbw <- bbwManual_bbw[!is.na(bbwManual_bbw$MARKETING_DATE),]
  bbwManual_bbw <- bbwManual_bbw[!is.na(bbwManual_bbw$BBW_DATE),]
  bbwManual_bbw$MARKETING_DATE<-as.Date(as.character(bbwManual_bbw$MARKETING_DATE),'%Y%m%d')  #19940624
  bbwManual_bbw$BBW_DATE<-as.Date(as.character(bbwManual_bbw$BBW_DATE),'%Y%m%d')  #19940624
  bbwManual_bbw <- bbwManual_bbw[bbwManual_bbw$MARKETING_DATE<bbwManual_bbw$BBW_DATE,]
  #!!!
  completeBBWManual<-rbind(bbwManual_robust,bbwManual_bbw)
  completeBBWManual$MARKETING_DATE<-as.Date(completeBBWManual$MARKETING_DATE)
  completeBBWManual$BBW_DATE<-as.Date(completeBBWManual$BBW_DATE)
  completeBBWManual$label <- rep(0,length(completeBBWManual[,1]))
  completeBBWManual[completeBBWManual[,1]=='BBW',]$label<-1
  candidate_drug_list <- unlist(list(unique(bbwManual_bbw$Trade_Name),unique(bbwManual_robust$Trade_Name)))
  
  bbwManual_bbw_predict <- '../result/drug_Date_BBW_table_predict.tsv'
  write.table(completeBBWManual,bbwManual_bbw_predict,sep='\t',row.names = FALSE,quote=FALSE)
  #bbwManual_robust_predict <- './result/bbwManual_robust_predict.csv'
  #write.csv(bbwManual_robust,bbwManual_robust_predict,row.names = FALSE, quote = FALSE)
  ###############
  #generate all prediction statistics, used for building model
  #drug group label marketingDate preTrialNum postTrialNum  ECdiseaseFocusDivergenceLevel ECdiseaseFocusShiftLevel  ECdrugFocusDivergenceLevel  ECdrugFocusShiftLevel drugClass
  complete_set<-NA
  complete_set <- completeBBWManual[,c(2,1,10,4,5,9)]
  names(complete_set)<-c('drug','group','label','marketDate','bbwDate','bbwText')
  complete_set$label<-as.factor(complete_set$label)
}
###plot some of the date info
resultDir <- '../result/anaCoherenceOccurrence/'
dir.create(file.path(resultDir), showWarnings = FALSE)
pdf(paste(resultDir,'DateAna','.pdf',sep=''))
#Delay of BBW acquisition Time
market_bbw_diff_week<-apply(data.frame(bbwManual_bbw$BBW_DATE,bbwManual_bbw$MARKETING_DATE),1, function(x) difftime(x[1],x[2],units='weeks'))
market_bbw_diff_year <- market_bbw_diff_week/48
#plot 1
par(mar=c(5,4,4,5)+.1)
hist(market_bbw_diff_year,xlim=c(0,max(market_bbw_diff_year)),breaks=15,freq = NULL,col="gray",main='Delay of BBW acquisition Time',xlab='BBW acquisition time (year)',ylab='Drug Count')
abline(v=mean(market_bbw_diff_year),lty=3,col='green',lwd=2)
par(new=TRUE)
plot(density(market_bbw_diff_year),xlim=c(0,max(market_bbw_diff_year)),type='l',lty="dotted",col='blue',lwd=3, axes=FALSE,ylab=NA,xlab=NA,main=NA)
axis(4)
mtext("Density",side=4,line=3)
par(mar=c(4,4,4,4))
#plot 2
coliflore(bbwManual_bbw[,c(2,4,5)])

dev.off()

############################################################
#filter the main table
mainTable_ori<-mainTable
mainTable<- mainTable[mainTable$drug %in% candidate_drug_list,]

###########################################################
#analysis all words in the corpus
if (0){
  content<-paste(mainTable$official_title,mainTable$condition,mainTable$brief_summary,mainTable$criteria,sep=' ')
  wordNum_content<-unlist(lapply(content,function(x) length(unlist(strsplit(content[1],'\\s+'))[!(unlist(strsplit(x,'\\s+')) %in% stopWords)])))
  wordNum_ECdrug<-unlist(lapply(mainTable$ECdrug,function(x) length(strsplit(x,';')[[1]])))
  wordNum_ECdisease<-unlist(lapply(mainTable$ECdisease,function(x) length(strsplit(x,';')[[1]])))
}

#############################################################
#contain trial number information retrived from CT.gov with only phase information
#will calculate the defined pre/post trial number  
trial_content_BBW <- read.table('../result/drug_trial_content_tab_BBW',header=TRUE,sep='\t')
trial_content_BBW <-trial_content_BBW[complete.cases(trial_content_BBW),]
trial_content_BBW$marketingDate <- as.Date(as.character(trial_content_BBW$marketingDate),format ="%Y%m%d")

trial_content_ROBUST <- read.table('../result/drug_trial_content_tab_ROBUST',header=TRUE,sep='\t')
trial_content_ROBUST <-trial_content_ROBUST[complete.cases(trial_content_ROBUST),]
trial_content_ROBUST$marketingDate <- as.Date(as.character(trial_content_ROBUST$marketingDate),"%Y%m%d")

trial_content_BBW$group = rep('BBW',length(trial_content_BBW$drugName))
trial_content_BBW$ratio = trial_content_BBW$postTrialNum/trial_content_BBW$preTrialNum
trial_content_ROBUST$group = rep('ROBUST',length(trial_content_ROBUST$drugName))
trial_content_ROBUST$ratio = trial_content_ROBUST$postTrialNum/trial_content_ROBUST$preTrialNum

trial_content = rbind(trial_content_BBW,trial_content_ROBUST)

#filter using drugs with complete BBW dates
trial_content <- trial_content[trial_content$drugName %in% candidate_drug_list,]
names(trial_content)[3]<-'prePhaseTrialNum'
names(trial_content)[4]<-'postPhaseTrialNum'

#
complete_set <- merge(x = complete_set, y = trial_content[ , c("drugName", "prePhaseTrialNum",'postPhaseTrialNum')], by.x =  "drug",by.y = 'drugName', all.x=TRUE)

complete_set$preDatedTrialNum <- NA
complete_set$postDatedTrialNum <- NA
complete_set$preMeanEnroll <- NA
complete_set$postMeanEnroll <- NA
#################################################################

drug<-mainTable$drug
drug.unique<-unique(drug)

if(runCoherenceOccurenceTest_switch =='on'){
  
  colNumTesting_list = c(27,29) #27:ECdrug; 29: ECdisease; 7:condition
  
  #change data to corpus
  for(colNumTesting in colNumTesting_list){
    ifTestCoherence = 1
    ifTestOccurence = 1
    varNameTesting = colnames(mainTable)[colNumTesting]
    
    ###change the non-standard list format in cell to ';'seperated/lower format 
    #[\"Parkinson's Disease\"; 'Psychoses']  ==> ParkinsonsDisease;Psychoses
    ###
    if (colNumTesting==7){ #',' as seperater
      mainTable[,colNumTesting] <- tolower(gsub(',',';',gsub( "[^[:alnum:],]", "", mainTable[,colNumTesting])))
    }
    
    occurrence_df<-data.frame()
    coherence_df<- data.frame()
    count=0
    for(j in drug.unique){
      print(j)
      #j='chantix'#for test purpose, "afinitor"
      marketingDate = completeBBWManual[completeBBWManual$Trade_Name==j,]$MARKETING_DATE
      count = count + 1
      drug.all<- mainTable[mainTable$drug==j,]
      j.group <- mainTable[mainTable$drug==j,]$group[1]
      drug.all$testing<-lapply(drug.all[,colNumTesting],function(x) c(strsplit(x,';')[[1]])) # [[],[]]
      drug.all$testing<-lapply(drug.all$testing,function(x) gsub(' ','-',x))
      
      #add debug: Feb 27, 2015: in case corpus instance contain zero length
      #drug.all<-drug.all[unlist(lapply(drug.all$testing, function(x) !is.null(x))),]
      
      drug.all$testing <- Corpus(VectorSource(drug.all$testing)) 
      #data.frame(text=unlist(sapply(corpus_pre, `[`)), stringsAsFactors=F)
      
      #just use phase information to separate PRE/POST
      #corpus_pre <- drug.all[drug.all$period=='PRE',]$testing #all the trials (with disease sep ;) for pre drug; one drug, one doc
      #corpus_post <- drug.all[drug.all$period=='POST',]$testing
      
      #just use DATE information to separate PRE/POST
      #corpus_pre <- drug.all[marketingDate > drug.all$start_date,]$testing #all the trials (with disease sep ;) for pre drug; one drug, one doc
      #corpus_post <- drug.all[marketingDate < drug.all$start_date,]$testing
      
      #use both marketing date and phase to separate PRE/POST
      ct_pre<-drug.all[drug.all$period=='PRE' &  marketingDate > drug.all$start_date,]
      ct_post<-drug.all[drug.all$period=='POST' & marketingDate < drug.all$start_date,]
      corpus_pre <- ct_pre$testing #all the trials (with disease sep ;) for pre drug; one drug, one doc
      corpus_post <- ct_post$testing
      meanEnroll_pre<- mean(ct_pre$enrollment[complete.cases(ct_pre$enrollment)])
      #nan is caused by 0 length ct 
      #if (is.nan(meanEnroll_pre)){print(ct_pre$enrollment)}
      meanEnroll_post<- mean(ct_post$enrollment[complete.cases(ct_post$enrollment)])
      
      #update pre/post-DatedTrialNum in trial_content/completeTrialCount DF
      complete_set[complete_set$drug==j,]$preDatedTrialNum<-length(corpus_pre)
      complete_set[complete_set$drug==j,]$postDatedTrialNum<-length(corpus_post)
      complete_set[complete_set$drug==j,]$preMeanEnroll<-meanEnroll_pre
      complete_set[complete_set$drug==j,]$postMeanEnroll<-meanEnroll_post
      
      #test coherence using JSD
      if (ifTestCoherence == 1){
        jsd_score <- testCoherence(corpus_pre,corpus_post)
        if(!jsd_score[[1]]==FALSE){
          newline1 <- data.frame(count = count,drug = j,group = j.group,jsd_pre = jsd_score$jsd_pre,jsd_post = jsd_score$jsd_post)
          coherence_df<-rbind(coherence_df,newline1)
        }
      }
      #test occurrence
      if (ifTestOccurence == 1){
        occur_score<-testOccurence(corpus_pre,corpus_post)
        #print (c(j,occur_score))
        if (!(occur_score==FALSE)){
          newline2 <- data.frame(count = count, drug=j,group=j.group,newOccureScore = occur_score)
          occurrence_df<-rbind(occurrence_df,newline2)
        }
      }
    }
    #
    A <-coherence_df[complete.cases(coherence_df),]
    A_BBW<-A[A$group=='BBW',]
    A_ROBUST<-A[A$group=='ROBUST',]
    A$change<-A$jsd_post-A$jsd_pre
    
    #
    info <- occurrence_df[complete.cases(occurrence_df),]
    info<-info[order(-as.numeric(info$newOccureScore)),]
    info$label<-rep(0,length(info$group))
    info[info$group=='BBW',]$label<-1
    #
    B<- merge(info,A,by='drug')
    
    name1<-paste('coherence',varNameTesting,'_df',sep='')
    name2<-paste('occurrence',varNameTesting,'_df',sep='')
        
    assign(name1,A)
    assign(name2,info)
    
      
    #####################Draw Pic and Hypothesis Testing###########################
    resultDir <- '../result/anaCoherenceOccurrence/'
    dir.create(file.path(resultDir), showWarnings = FALSE)
    
    pdf(paste(resultDir,varNameTesting,'.pdf',sep=''))
    logFile<-paste(resultDir,varNameTesting,'.log',sep='')
    
    write('Test details from r_main.R: ',logFile)
    write(paste('Time of running: ',Sys.time(),'\n'),logFile,append=T)
    #JSD
    if (ifTestCoherence == 1){
      
      boxplot(as.numeric(A$jsd_pre),as.numeric(A$jsd_post),boxwex=0.5,notch = F,col=c('gray63','gray63'),main='Different Eligibility Coherence Level between Pre/Post-market Trials',names=c('Pre-Market','Post-Market'),ylab='JS-Divergence score (Dissimilarity)')
      ks1<-ks.test(as.numeric(A$jsd_pre),as.numeric(A$jsd_post),alternative = 'greater')
      write('KS test for figure: Coherent Shift of Eligibility Criteria: ',file=logFile,append=T)
      write(as.character(ks1),file=logFile,append=T)
      
      boxplot(as.numeric(A[A$group=='ROBUST',]$change),as.numeric(A[A$group=='BBW',]$change),boxwex=0.5,col=c('darkseagreen1','antiquewhite2'),notch = F,main='Coherence Shift Level for ROBUST/BBW Drugs',names=c('ROBUST Drugs','BBW Drugs'),ylab='JSD_change')
      boxplot(as.numeric(A_ROBUST$jsd_pre),as.numeric(A_ROBUST$jsd_post),as.numeric(A_BBW$jsd_pre),as.numeric(A_BBW$jsd_post),ylim=c(0,0.7),main='Coherent Shift for Robust/BBW Drugs',col=c('darkseagreen1','darkseagreen1','antiquewhite2','antiquewhite2'),names=c('Pre-ROBUST','Post-ROBUST','Pre-BBW','Post-BBW'),notch = F,ylab='JS-Divergence score')
      ks2<-ks.test(as.numeric(A_ROBUST$jsd_pre),as.numeric(A_ROBUST$jsd_post),alternative = 'greater')
      ks3<-ks.test(as.numeric(A_BBW$jsd_pre),as.numeric(A_BBW$jsd_post),alternative = 'greater')
      write('\nKS test for figure: Coherent Shift for Robust/BBW Drugs: ',file=logFile,append=T)
      write(as.character(ks2),file=logFile,append=T)
      write('\n',logFile,append=T)
      write(as.character(ks3),file=logFile,append=T)
      print(c(ks1$p.value,ks2$p.value,ks3$p.value))
    }
    #Count occurence
    if (ifTestOccurence == 1){
      boxplot(as.numeric(info[info$group=='ROBUST',]$newOccureScore),as.numeric(info[info$group=='BBW',]$newOccureScore),boxwex=0.5,col=c('darkseagreen1','antiquewhite2'),notch = F,main='Forcus Shift Level for ROBUST/BBW Drugs',names=c('ROBUST Drugs','BBW Drugs'),ylab='Cosine Similarity')
      #View(info)
      ks4_l<-ks.test(as.numeric(info[info$group=='BBW',]$newOccureScore),as.numeric(info[info$group=='ROBUST',]$newOccureScore),alternative = 'less',exact = NULL)
      ks4_g<-ks.test(as.numeric(info[info$group=='BBW',]$newOccureScore),as.numeric(info[info$group=='ROBUST',]$newOccureScore),alternative = 'greater',exact = NULL)
      ks4<-ks4_l
      if (ks4_g$p.value < ks4_l$p.value){ks4<-ks4_g}
      write('\nKS test for figure: Emerging Criteria Level for ROBUST/BBW Drugs: ',file=logFile,append=T)
      write(as.character(ks4),file=logFile,append=T)
      
      sigLevel <- sum(ks1$p.value,ks2$p.value,ks3$p.value,ks4$p.value)/4.0
      print(paste('KS4: ',ks4$p.value))
      
      pred<-prediction(info$newOccureScore,info$label)
      perf <- performance(pred, "tpr", "fpr")
      plot(perf)
      
      print(qplot(change, newOccureScore, data=B, geom=c("point", "smooth"), method="lm", formula=y~poly(x), color=group.x, main="Regression of MPG on Weight", xlab="Divergence", ylab="New Content"))
      print(qplot(jsd_pre, jsd_post, data=B, geom=c("point", "smooth"), method="lm", formula=y~poly(x), color=group.x, main="Regression of MPG on Weight", xlab="Weight", ylab="Miles per Gallon"))
      
      B$predict<- B$newOccureScore/B$jsd_post
      pred<-prediction(B$predict,B$label)
      perf <- performance(pred, "tpr", "fpr")
      plot(perf)
    }
    dev.off()
  }
}

#########ANCOVA for trial number#####################
#standardize trial count
complete_set_backup1 <- complete_set
standardTrialCount <- complete_set[,c(1,2,3,7,8,9,10)]
colnames(standardTrialCount)<-c('drug','group','label',"prePhaseTrialNum" , "postPhaseTrialNum", "preDatedTrialNum" , "postDatedTrialNum")
#standardTrialCount$preTrialNum<-scale(standardTrialCount$preTrialNum)
#standardTrialCount$postTrialNum<-scale(standardTrialCount$postTrialNum)

#####################################################
manova_output<-paste(statDir,'manova.doc',sep='')

#run manova to determine the relation between trial number and BBW/ROBUST(DRUG GROUP)
res1 = cbind(complete_set$prePhaseTrialNum,complete_set$postPhaseTrialNum)
manova1<-manova(res1~complete_set$marketDate*complete_set$group)
capture.output(c('###########################MANOVA SUMMARY: phase-seperated trials'),file=manova_output)
capture.output(summary(manova1),file=manova_output,append=TRUE)
capture.output(summary.aov(manova1),file=manova_output,append=TRUE)

res2 = cbind(complete_set$preDatedTrialNum,complete_set$postDatedTrialNum)
manova2<-manova(res2~complete_set$marketDate*complete_set$group)
capture.output(c('##########################MANOVA SUMMARY: (phase+date)-seperated trials'),file=manova_output,append=TRUE)
capture.output(summary(manova2),file=manova_output,append=TRUE)
capture.output(summary.aov(manova2),file=manova_output,append=TRUE)

dev.off()
########################################################################################
#standardize ECdisease
completeECdisease<- merge(coherenceECdisease_df,occurrenceECdisease_df,by='drug')
standardECdisease <- completeECdisease[,c(1,3,10,6,9)]
colnames(standardECdisease)<-c('drug','group','label','ECdiseaseFocusDivergenceLevel','ECdiseaseFocusShiftLevel')
complete_set<-complete_set[,!names(complete_set) %in% c('ECdiseaseFocusDivergenceLevel','ECdiseaseFocusShiftLevel')]
complete_set<-merge(complete_set,standardECdisease[,c('drug','ECdiseaseFocusDivergenceLevel','ECdiseaseFocusShiftLevel')],by='drug',all.x = TRUE)

#standardECdisease$FocusDivergenceLevel<-scale(standardECdisease$FocusDivergenceLevel)
#standardECdisease$FocusShiftLevel<-scale(standardECdisease$FocusShiftLevel)
#standardize ECdrug
completeECdrug<- merge(coherenceECdrug_df,occurrenceECdrug_df,by='drug')
standardECdrug <- completeECdrug[,c(1,3,10,6,9)]
colnames(standardECdrug)<-c('drug','group','label','ECdrugFocusDivergenceLevel','ECdrugFocusShiftLevel')
#standardECdrug$FocusDivergenceLevel<-scale(standardECdrug$FocusDivergenceLevel)
#standardECdrug$FocusShiftLevel<-scale(standardECdrug$FocusShiftLevel)
complete_set<-complete_set[,!names(complete_set) %in% c('ECdrugFocusDivergenceLevel','ECdrugFocusShiftLevel')]
complete_set<-merge(complete_set,standardECdrug[,c('drug','ECdrugFocusDivergenceLevel','ECdrugFocusShiftLevel')],by='drug',all.x = TRUE)

#####################
#partition: training, validating and testing sets
statDir = '../result/anaPredictive/'
train_set_file = paste(statDir,'train_set.csv',sep='')
validation_set_file = paste(statDir,'validation_set.csv',sep='')
test_set_file = paste(statDir,'test_set.csv',sep='')

dir.create(statDir, showWarnings = FALSE)
#generate the complete set for prediction
complete_set_calculated<- complete_set[complete.cases(complete_set[,c(-1,-2,-5,-6)]),]
complete_set_drugGroup <- complete_set[complete.cases(complete_set[,c(-1,-2,-5,-6)]),c(1,2,3,4,5,6)]

complete_set_predict<- complete_set[complete.cases(complete_set[,c(-1,-2,-5,-6)]),c(-1,-2,-5,-6)]
complete_set_predict$marketDate<-as.numeric(complete_set_predict$marketDate)
#modify the trialNum/Date vars
complete_set_predict$postPhaseTrialNum <- complete_set_predict$postPhaseTrialNum/(as.integer(Sys.Date())-complete_set_predict$marketDate)
complete_set_predict$postDatedTrialNum <- complete_set_predict$postDatedTrialNum/(as.integer(Sys.Date())-complete_set_predict$marketDate)

###############################MODEL#############################
#linear model

train_index <- createDataPartition(complete_set_predict$label,p = .7,list = FALSE)
train_set <- complete_set_predict[train_index,]
train_set_drugGroup <- complete_set_drugGroup[train_index,]
validation_set <- complete_set_predict[-train_index,]
validation_set_drugGroup <- complete_set_drugGroup[-train_index,]
test_set <- complete_set[!(complete_set$drug %in% complete_set_predict$drug),c(-1,-2,-5,-6)]

if(save_preprocessed_sets == TRUE){
  write.csv(train_set,train_set_file,row.names = FALSE, quote = FALSE)
  write.csv(validation_set,validation_set_file,row.names = FALSE, quote = FALSE)
  write.csv(test_set,test_set_file,row.names = FALSE, quote = FALSE)
  print('###Preprocessed sets successfully saved!')
}
###############################
lm.fit<-glm(as.factor(label)~., data = train_set)
lm.fit_summary <- summary(lm.fit)
confint(lm.fit)

#choose best model, slow
lm.fit.best<-allanVarSelect(lm.fit,train_set_file,validation_set_file,ResponseCol = 1, NumOfSteps = 6, criteria="MSE", silent=FALSE)

test_prediction <- predict(lm.fit.best, newdata=validation_set)
test_prediction_df <- data.frame( drug= validation_set_drugGroup$drug, group = validation_set_drugGroup$group,prediction=test_prediction)

#test_prediction_df <- data.frame( drug= test_set_complete$drug, group = test_set_complete$group,prediction=test_prediction)
print(paste('###Saving test outcome to ',prediction_file))
write.csv(test_prediction_df, prediction_file,row.names = FALSE, quote = FALSE)

lm.prediction<-predict(lm.fit.best, newdata=test_set, make.function = TRUE)

####################################################################
pdf(paste(statDir,'randomForestPredictor.pdf',sep=''))
#random forest

#######predict whether is BBW (label)
bestmtry <- data.frame(tuneRF(y=as.factor(complete_set_predict$label),x=complete_set_predict[,-1], ntreeTry=1000, 
                   stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE))
candimtry <- bestmtry[bestmtry$OOBError==min(bestmtry$OOBError),]$mtry
#SELECT MTRY FOR NEXT STEP
rf.fit <- randomForest(y=as.factor(complete_set_predict$label),x=complete_set_predict[,-1],mtry=candimtry,
                       ntree=1000,importance=TRUE,keep.forest=TRUE)
textplot(varImpPlot(rf.fit))
textplot(importance(rf.fit))
rf.fit.pr = predict(rf.fit,type="prob")[,2]

rf.fit.pred = prediction(rf.fit.pr, complete_set_predict$label)
rf.fit.perf = performance(rf.fit.pred,"tpr","fpr")

auc <- performance(rf.fit.pred,"auc")
auc <- unlist(slot(auc, "y.values"))
print(auc)

# ROC
plot(rf.fit.perf,main="ROC Curve for Random Forest Predictor",col=2,lwd=2,colorize = TRUE)
#plot(rf.fit.perf,add=TRUE,col=2,lwd=2,colorize = TRUE)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

rf.fit.rank<-data.frame(drug=complete_set_drugGroup$drug,label=as.factor(complete_set_drugGroup$label),marketDate=complete_set_drugGroup$marketDate,bbwDate=complete_set_drugGroup$bbwDate,prediction=rf.fit.pr)
rf.fit.rank<-rf.fit.rank[order(rf.fit.rank$prediction,decreasing = TRUE),]

# CI for ROC
rocobj <- plot.roc(rf.fit.rank$label, rf.fit.rank$prediction,  main="ROC Curve for Random Forest Predictor", 
                   percent=TRUE,  ci=TRUE, of="se",print.auc=TRUE,
                   ci.type="bars", ci.col="#1c61b6AA")   
#plot(ci.sp(rocobj, sensitivities=seq(0, 100, 5)), 
#     type="bars") # print this one as bars 

#rocobj <- plot.roc(rf.fit.rank$label, rf.fit.rank$prediction, percent = TRUE, main="Smoothing")  
#lines(smooth(rocobj), col = "#1c61b6")  
lines(smooth(rocobj, method = "density"),  col = "#008600",add=TRUE,lty=3)  
#lines(smooth(rocobj, method = "fitdistr", density = "lognormal"), col = "#840000")  
legend("bottomright", legend = c("Empirical", "Density"),cex=1.0, col = c("black", "gray"),lwd = 2,lty=c(1,3))

#baseline ROC as requested by reviewer

rf.fit_base <- randomForest(y=as.factor(complete_set_predict$label),x=complete_set_predict[,c(-1,-9,-10,-11,-12)],mtry=candimtry,
                       ntree=1000,importance=TRUE,keep.forest=TRUE)
rf.fit.pr_base = predict(rf.fit_base,type="prob")[,2]

rf.fit.pred_base = prediction(rf.fit.pr_base, complete_set_predict$label)
rf.fit.perf_base = performance(rf.fit.pred_base,"tpr","fpr")

auc_base <- performance(rf.fit.pred_base,"auc")
auc_base <- unlist(slot(auc_base, "y.values"))
print(auc_base)

# ROC
#plot(rf.fit.perf_base,add=TRUE,col=8,lwd=1,colorize = FALSE)
lines(rf.fit.perf_base@x.values[[1]],rf.fit.perf_base@y.values[[1]],col=8,lwd=2)


dev.off()


###############################################################
#plot ana trand analysis
if(mappingADEOffsides_switch=='on' & runCoherenceOccurenceTest_switch =='on'){
  library(grid)
  library(quadprog)
  library(directlabels)
  
  pdf('../result/anaCoherenceOccurrence/trendAna.pdf')
  
  #column order matters
  para_dfs <- list(ECdisease=standardECdisease,ECdrug=standardECdrug)
  
  for (i in length(para_dfs)){
    testDF = para_dfs[[i]]
    testVar = names(para_dfs)[i]
    #plot1
    print(qplot(testDF[,4],testDF[,5],data=testDF, 
                geom=c("point", "smooth"), 
                method="lm", formula=y~poly(x), color=testDF[,2], 
                main=paste("Different Eligibility Shift trends for BBW and ROBUST drugs (",testVar,')',sep=''), 
                xlab="Focus Divergence Level", 
                ylab="Focus Shift Level")
    )
    #plot2
    print(ggplot(testDF,aes(x=testDF[,4],y=testDF[,5],label=testDF[,1])) + 
      stat_density2d(geom="tile", aes(fill = testDF[,2], alpha=..density..), contour=FALSE) + 
      scale_fill_manual(name='legendmanual',values=c("BBW"="#FF0000", "ROBUST"="#00FF00")) +
      geom_point(colour="black", size = 3.5,alpha=0.6)+
      geom_point(aes(colour = testDF[,2],shape = testDF[,2]),size=2.3) +
      labs(colour="l1",shape='l2',legendmanual='l3')+
      guides(alpha=FALSE) +
      geom_text(size=3,angle=0,hjust=0.5, vjust=0.5,alpha=0.95,position = position_jitter(width=0.02, height=0.02))+
      theme_minimal() +
      xlim(-0.5,0.5)+ylim(0,1)+
      xlab("Focus Divergence Level (changes in JS-Divergence level)")+ylab("Focus Shift level")+ggtitle(paste("Drug Label and Trial Focus Shifts Level (",testVar,')',sep=''))
    )
  }
  dev.off()
}








#not run
###################################################################
#EPC drug class in candiTable_EPC
in_content <-  c('../result/drug_trial_content.csv')
drug_trial_content <- read.csv(in_content,header = T,stringsAsFactors = F)
content.df <- data.frame(drug_trial_content, stringsAsFactors = F)
#marketing date
content.df$dates <- as.Date(as.character(content.df$marketingDate), "%Y%m%d")
#pharm Class
a<-content.df$pharmClass
b<-strsplit(a,'\\|')
content.df$EPCclass<-sapply(b, function(x) paste(unlist(x)[grepl('\\[EPC\\]',unlist(x))],collapse = ';'))
candiTable_EPC<-content.df[tolower(content.df$drugName) %in% drugName_df_unique$tradeName,]
candiTable_EPC$drugName<-tolower(candiTable_EPC$drugName)

#BBW text 
in_content <-  c('../result/PDR/drugLabelContent')
drug_label_content <- read.delim(in_content,header = F,stringsAsFactors = F)
content.df <- data.frame(drug_label, stringsAsFactors = F)
names(content.df)<-c('drugName','druglabelid','BBW','BBW_Text','REF')
candiTable_BBW<-content.df[tolower(content.df$drugName) %in% drugName_df_unique$tradeName,]

candiTable_EC <-data.frame()
names(candiTable_EC)<-c('drugName','period','ECdrug','ECdisease')
for (currentDrug in drugName_df_unique$tradeName){
  candi<-mainTable[mainTable$drug==currentDrug,]
  ECdrugList_PRE <- candi[candi$period=='PRE',]$ECdrug
  ECdrugList_POST <- candi[candi$period=='POST',]$ECdrug
  ECdiseaseList_PRE <- candi[candi$period=='PRE',]$ECdisease
  ECdiseaseList_POST <- candi[candi$period=='POST',]$ECdisease
  candiTable_EC<-rbind(candiTable_EC,data.frame(drugName = currentDrug, period='PRE', ECdrug=paste(ECdrugList_PRE,collapse = ';'),ECdisease=paste(ECdiseaseList_PRE,collapse = ';')))
  candiTable_EC<-rbind(candiTable_EC,data.frame(drugName = currentDrug, period='POST', ECdrug=paste(ECdrugList_POST,collapse = ';'),ECdisease=paste(ECdiseaseList_POST,collapse = ';')))
  }
}
candiTable_EC<-candiTable_EC[complete.cases(candiTable_EC),]

#histgram of market date and drug count
out_pdf <- c('../result/S1_drug_trial_landscape.pdf')
fhOut_pdf <- pdf(out_pdf)
m <- ggplot(content.df, aes(x=dates))
m + geom_histogram(aes(fill = ..count..))+xlab('Date')+ylab('Drug Count')
#hist(content.df[tolower(content.df$drugName) %in% drugName_df_unique$tradeName,]$dates,breaks=20,xlab='Marketing Date',ylab='Count of Drugs')
#TABLE 1
write.csv(candiTable_EPC,file='../result/anaCoherenceOccurrence/Table1.csv')
dev.off()

#########table 2
bbw_Label<-read.delim('../result/PDR/drugLabelContent',,sep='\t',header=F,stringsAsFactors = F)
bbw_Label$V1 <- gsub('-',' ',bbw_Label$V1)

candi_bbw<-bbw_Label[bbw_Label$V1 %in% occurrenceADE_df$drug,]
names(candi_bbw)<-c('drug','PDR id','BBW','BBW text','reference')
b<-merge(occurrenceADE_df,candi_bbw,by='drug')
write.csv(b,file='../result/anaCoherenceOccurrence/Table2.csv')
