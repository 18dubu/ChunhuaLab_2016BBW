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
library(RColorBrewer)
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
library(grid)
library(quadprog)
library(directlabels)


#####################################################
fhIn_dir <- '../result/selected_drug_trial_List/'
integrateTableNameUpdate <- 'integratedDrugTrialTableMatched_tab'
bbwDateTableName <- '../result/drug_Date_BBW_table_manual_tab'

mainTable <- read.csv(paste(fhIn_dir,integrateTableNameUpdate,sep=''),header=T, stringsAsFactors=F)

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
  
}

############################################################
#filter the main table
mainTable_ori<-mainTable
mainTable_complete<- mainTable[mainTable$drug %in% candidate_drug_list,]

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


#function start
#input para: testDate

rootDir <- '../result/anaTimeSeries/'
dir.create(rootDir, showWarnings = FALSE)

testDates<-c('2006-01-01','2007-01-01','2008-01-01','2009-01-01','2010-01-01','2011-01-01','2012-01-01','2013-01-01','2014-01-01','2015-01-01','2015-04-09')

#OOB means we only limit the data visibility, while we train and test the whole model with the most up-to-date response (all BBW labeled as BBW)
#if useOOB is false, apart from the data visibility, we use the "partially correct" response [from the testDate to the future] to simulate the real-world condition. which means the response might also be false. 
  #For example: testDate = 20060101, then the training data will be 1) all trials start no later than 20060101 2) response with only BBW drugs that acquired label before 20060101
  #and the test set will be the label response for: 20070101,20080101...20160101, which will be different with more drugs acquire BBW. We can then test how many predictions have been realized as time pass by.
useOOB=FALSE
cohOccSubDir<-'anaCoherenceOccurrence/'
statSubDir<-'anaPredictive/'
dateSubDir<-'anaDate/'
benchmarkSubDir = 'benchmarking/'
predictivePowerSubDir <- 'predictivePowerInTime/'

for (testDate in testDates){
  print('#############################')
  print(paste('###Analyzing round: ',testDate))
  print('#############################')
  targetDir <- paste(rootDir,testDate,'/',sep='')
  dir.create(targetDir, showWarnings = FALSE)
  ###############
  #generate all prediction statistics, used for building model
  #drug group label marketingDate preTrialNum postTrialNum  ECdiseaseFocusDivergenceLevel ECdiseaseFocusShiftLevel  ECdrugFocusDivergenceLevel  ECdrugFocusShiftLevel drugClass
  complete_set<-NA
  complete_set <- completeBBWManual[,c(2,1,10,4,5,9)]
  names(complete_set)<-c('drug','group','label','marketDate','bbwDate','bbwText')
  complete_set$label<-as.factor(complete_set$label)
  
  complete_set <- merge(x = complete_set, y = trial_content[ , c("drugName", "prePhaseTrialNum",'postPhaseTrialNum')], by.x =  "drug",by.y = 'drugName', all.x=TRUE)
  
  complete_set$preDatedTrialNum <- NA
  complete_set$postDatedTrialNum <- NA
  complete_set$preMeanEnroll <- NA
  complete_set$postMeanEnroll <- NA
  
  ###############
  #refine mainTable
  mainTable<-mainTable_complete[mainTable_complete$start_date<as.Date(testDate),]
  
  #################################################################
  
  drug<-mainTable$drug
  drug.unique<-unique(drug)
  
  if(1){
    
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
        #print(j)
        #j='chantix'#for test purpose, "afinitor"
        marketingDate = completeBBWManual[completeBBWManual$Trade_Name==j,]$MARKETING_DATE
        count = count + 1
        drug.all<- mainTable[mainTable$drug==j,]
        j.group <- mainTable[mainTable$drug==j,]$group[1]
        drug.all$testing<-lapply(drug.all[,colNumTesting],function(x) c(strsplit(x,';')[[1]])) # [[],[]]
        drug.all$testing<-lapply(drug.all$testing,function(x) gsub(' ','-',x))
        drug.all$testing <- Corpus(VectorSource(drug.all$testing)) 
        
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
      resultDir <- paste(targetDir,cohOccSubDir,sep='')
      dir.create(file.path(resultDir), showWarnings = FALSE)
      
      pdf(paste(resultDir,varNameTesting,'.pdf',sep=''))
      logFile<-paste(resultDir,varNameTesting,'.log',sep='')
      
      write('Test details from r_main.R: ',logFile)
      write(paste('Time of running: ',Sys.time(),'\n'),logFile,append=T)
      #JSD
      if (ifTestCoherence == 1){
        
        boxplot(as.numeric(A$jsd_pre),as.numeric(A$jsd_post),boxwex=0.5,notch = F,col=c('gray63','gray63'),main='Clinical Trial Coherence Level between Pre/Post-market Trials',names=c('Pre-Market','Post-Market'),ylab='JS-Divergence score (Dissimilarity)')
        ks1<-ks.test(as.numeric(A$jsd_pre),as.numeric(A$jsd_post),alternative = 'greater')
        write('KS test for figure: Coherent Shift of Eligibility Criteria: ',file=logFile,append=T)
        write(as.character(ks1),file=logFile,append=T)
        
        boxplot(as.numeric(A[A$group=='ROBUST',]$change),as.numeric(A[A$group=='BBW',]$change),boxwex=0.5,col=c('darkseagreen1','antiquewhite2'),notch = F,main='Coherence Shift Level for ROBUST/BBW Drugs',names=c('ROBUST Drugs','BBW Drugs'),ylab='JSD_change')
        boxplot(as.numeric(A_ROBUST$jsd_pre),as.numeric(A_ROBUST$jsd_post),as.numeric(A_BBW$jsd_pre),as.numeric(A_BBW$jsd_post),ylim=c(0,0.7),main='Coherent Shift for Robust/BBW Drugs',col=c('darkseagreen1','darkseagreen1','antiquewhite2','antiquewhite2'),names=c('Pre-ROBUST','Post-ROBUST','Pre-BBW','Post-BBW'),notch = F,ylab='JS-Divergence score')
        ks2<-ks.test(as.numeric(A_ROBUST$jsd_pre),as.numeric(A_ROBUST$jsd_post),alternative = 'greater')
        ks3<-ks.test(as.numeric(A_BBW$jsd_pre),as.numeric(A_BBW$jsd_post),alternative = 'greater')
        write('\nKS test for figure: Coherent Shift for ROBUST/BBW Drugs: ',file=logFile,append=T)
        write(as.character(ks2),file=logFile,append=T)
        write('\n',logFile,append=T)
        write(as.character(ks3),file=logFile,append=T)
        print(c(ks1$p.value,ks2$p.value,ks3$p.value))
      }
      #Count occurence
      if (ifTestOccurence == 1){
        boxplot(as.numeric(info[info$group=='ROBUST',]$newOccureScore),as.numeric(info[info$group=='BBW',]$newOccureScore),boxwex=0.5,col=c('darkseagreen1','antiquewhite2'),notch = F,main='Focus Shift Level for ROBUST/BBW Drugs',names=c('ROBUST Drugs','BBW Drugs'),ylab='Cosine Distance')
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
  #############################################################
  #####################PLOTS AND ANALYSIS######################
  #############################################################
  ###plot some of the date info
  resultDir<-paste(targetDir,dateSubDir,sep='')
  dir.create(resultDir, showWarnings = FALSE)
  pdf(paste(resultDir,'DateAna.pdf',sep=''))
  #Delay of BBW acquisition Time
  market_bbw_diff_week<-apply(data.frame(bbwManual_bbw$BBW_DATE,bbwManual_bbw$MARKETING_DATE),1, function(x) difftime(x[1],x[2],units='weeks'))
  market_bbw_diff_year <- market_bbw_diff_week/48
  #plot 1
  par(mar=c(5,4,4,5)+.1)
  hist(market_bbw_diff_year,xlim=c(0,max(market_bbw_diff_year)),breaks=15,freq = NULL,col="gray",main='Delay of BBW Acquisition Time',xlab='BBW Acquisition Time (year)',ylab='Drug Count')
  abline(v=mean(market_bbw_diff_year),lty=3,col='green',lwd=2)
  par(new=TRUE)
  plot(density(market_bbw_diff_year),xlim=c(0,max(market_bbw_diff_year)),type='l',lty="dotted",col='blue',lwd=3, axes=FALSE,ylab=NA,xlab=NA,main=NA)
  axis(4)
  mtext("Density",side=4,line=3)
  box()
  par(mar=c(4,4,4,4))
  #plot 2
  coliflore(bbwManual_bbw[,c(2,4,5)],currentDate = as.Date(testDate))
  tmp<-completeBBWManual[,c(2,4,5)]
  tmp[is.na(tmp$BBW_DATE),]$BBW_DATE<-Sys.Date()
  coliflore(tmp,currentDate = as.Date(testDate))
  #plot 3 faster acquisition
  plot(as.Date(bbwManual_bbw$MARKETING_DATE),market_bbw_diff_year,main='Decreased BBW Acquisition Time ',xlab='Drug Marketing Date',ylab='BBW Acquisition Time (year)')
  
  dev.off()
  
  #########ANCOVA for trial number#####################
  #standardize trial count
  complete_set_backup1 <- complete_set
  standardTrialCount <- complete_set[,c(1,2,3,7,8,9,10)]
  colnames(standardTrialCount)<-c('drug','group','label',"prePhaseTrialNum" , "postPhaseTrialNum", "preDatedTrialNum" , "postDatedTrialNum")
  #standardTrialCount$preTrialNum<-scale(standardTrialCount$preTrialNum)
  #standardTrialCount$postTrialNum<-scale(standardTrialCount$postTrialNum)
  
  ################################################
  resultDir<-paste(targetDir,statSubDir,sep='')
  dir.create(resultDir, showWarnings = FALSE)
  manova_output<-paste(resultDir,'manova.doc',sep='')
  
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
  
  ###############play with it
  target_df<- complete_set_calculated
  res3 = cbind(target_df$preMeanEnroll,target_df$postMeanEnroll)
  manova3<-manova(res3~target_df$marketDate*target_df$group)
  #capture.output(c('##########################MANOVA SUMMARY: (phase+date)-seperated trials (target_df)'),file=manova_output,append=TRUE)
  summary.aov(manova3)
  capture.output(summary(manova3),file=manova_output,append=TRUE)
  capture.output(summary.aov(manova3),file=manova_output,append=TRUE)
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
  
  #generate the complete set for prediction
  complete_set_calculated<- complete_set[complete.cases(complete_set[,c(-1,-2,-5,-6)]),]
  complete_set_basic<- complete_set[complete.cases(complete_set[,c(-1,-2,-5,-6,-13,-14,-15,-16)]),]
  complete_set_drugGroup <- complete_set[complete.cases(complete_set[,c(-1,-2,-5,-6)]),c(1,2,3,4,5,6)]
  complete_set_calculated_file = paste(resultDir,'complete_set_calculated.csv',sep='')
  write.table(complete_set_calculated,complete_set_calculated_file,row.names = FALSE, quote = FALSE,sep='\t')
  ##
  complete_set_predict<- complete_set[complete.cases(complete_set[,c(-1,-2,-5,-6)]),c(-1,-2,-5,-6)]
  complete_set_predict$marketDate<-as.numeric(complete_set_predict$marketDate)
  #modify the trialNum/Date vars
  complete_set_predict$postPhaseTrialNum <- complete_set_predict$postPhaseTrialNum/(as.integer(Sys.Date())-complete_set_predict$marketDate)
  complete_set_predict$postDatedTrialNum <- complete_set_predict$postDatedTrialNum/(as.integer(Sys.Date())-complete_set_predict$marketDate)
  
  complete_set_predict_file = paste(resultDir,'complete_set_predict.csv',sep='')
  write.table(complete_set_predict,complete_set_predict_file,row.names = FALSE, quote = FALSE,sep='\t')

  
  #############################contour plot#################
  
  resultDir<-paste(targetDir,cohOccSubDir,sep='')
  dir.create(resultDir, showWarnings = FALSE)
  pdf(paste(resultDir,'trendAna.pdf',sep=''))
  #column order matters
  para_dfs <- list(diseaseLevel=complete_set_calculated[,c(1,2,3,13,14)],drugLevel=complete_set_calculated[,c(1,2,3,15,16)],FocusShiftLevel_labelsNOTRight=complete_set_calculated[,c(1,2,3,14,16)],CoherenceShiftLevel_labelsNOTRight=complete_set_calculated[,c(1,2,3,13,15)])
  
  for (i in 1:length(para_dfs)){
    testDF = para_dfs[[i]]
    testVar = names(para_dfs)[i]
    #plot1
    print(qplot(testDF[,4],testDF[,5],data=testDF, 
                geom=c("point", "smooth"), 
                method="lm", formula=y~poly(x), color=testDF[,2], 
                main=paste("Different Eligibility Shift trends for BBW and ROBUST drugs (",testVar,')',sep=''), 
                xlab="Coherence Shift Level (changes in JS-Divergence)", 
                ylab="Focus Shift Level (Cosine Distance)")
    )
    #plot2
    xmargin = (max(testDF[,4])-min(testDF[,4]))*0.15
    ymargin = (max(testDF[,5])-min(testDF[,5]))*0.15
    print(ggplot(testDF,aes(x=testDF[,4],y=testDF[,5],label=testDF[,1])) + 
            stat_density2d(geom="tile", aes(fill = testDF[,2], alpha=..density..), contour=FALSE) + 
            scale_fill_manual(name='Drug Group',values=c("BBW"="#FF0000", "ROBUST"="#00FF00")) +
            geom_point(name ='Drug Group',colour="black", size = 3.5,alpha=0.6)+
            geom_point(name='Drug Group',aes(colour = testDF[,2],shape = testDF[,2]),size=2.3) +
            labs(colour="Drug Groups",shape='',legendmanual='l3')+
            guides(alpha=FALSE,shape = guide_legend("Drug Group"),color=guide_legend("Drug Group")) +
            geom_text(size=3,angle=0,hjust=0.5, vjust=0.5,alpha=0.95,position = position_jitter(width=0.02, height=0.02))+
            theme_minimal() +
            xlim(min(testDF[,4])-xmargin,max(testDF[,4])+xmargin)+ylim(min(testDF[,5])-ymargin,max(testDF[,5])+ymargin)+
            xlab("Divergence Increase Level(Coherence Shift Level)")+ylab("Focus Shift level (Cosine Distance)")+ggtitle(paste("Clinical Trial Focus Drift Patterns (",testVar,')',sep=''))
    )
  }
  dev.off()
  
  ################################RANDOM FOREST PREDICTION################################
  pdf(paste(resultDir,'randomForestPredictor.pdf',sep=''))
  #random forest
  ######not using OOB, rather, create the "unknown" database to predict

  #BIASED DATA & BIASED OUTCOME
  #Real-world simulation
  
  if(!(useOOB==TRUE)){
    #generate biased training set
    complete_set_unknown<-complete_set_predict[,c(-3,-4,-5,-6,-8)]#marketDate,preMeanEnroll,EC*4
    #all unknown: EXCLUDE EXISTING BBW 
    complete_set_unknownIndex <- (complete_set_predict$label==1 & complete_set_drugGroup$bbwDate>testDate) #true means unknown BBW
        #no unknown BBW
    addTrainBiasFlag=1
    if(table(complete_set_unknownIndex)[['FALSE']]==length(complete_set_predict$label==1)){
      print(paste('No unknown BBWs from',testDate))
      addTrainBiasFlag=0
    }
    if(addTrainBiasFlag==1){
      complete_set_unknown[complete_set_unknownIndex,]$label<-0
    }
    #print out training set conditon
    print(paste('###Test date: ',testDate))
    print(paste('###We include',length(complete_set_unknownIndex),'drugs in the analysis'))
    print(paste('###Out of',length(complete_set_predict[complete_set_predict$label==1,]$label),'BBWs',
                length(complete_set_predict[complete_set_predict$label==1,]$label)-(length(complete_set_unknownIndex)-table(complete_set_unknownIndex)[['FALSE']]),'are known for training!'))
    
    bestmtry <- data.frame(tuneRF(y=as.factor(complete_set_unknown$label),x=complete_set_unknown[,-1], ntreeTry=1000, 
                                  stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE))
    candimtry <- bestmtry[bestmtry$OOBError==min(bestmtry$OOBError),]$mtry
    #SELECT MTRY FOR NEXT STEP
    rf.fit <- randomForest(y=as.factor(complete_set_unknown$label),x=complete_set_unknown[,-1],mtry=candimtry,
                           ntree=1000,importance=TRUE,keep.forest=TRUE)
    textplot(varImpPlot(rf.fit))
    capture.output(importance(rf.fit),file=paste(resultDir,'importance.doc',sep=''),append = FALSE)
    # Extracts variable importance (Mean Decrease in Gini Index)
    # Sorts by variable importance and relevels factors to match ordering
    target_df<-complete_set_unknown
    var_importance <- data.frame(variable=setdiff(colnames(target_df), "label"),
                                 importance=as.vector(importance(rf.fit,type=2)))
    var_importance <- var_importance[order(var_importance$importance,decreasing = TRUE),]
    var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)
    
    print(ggplot(var_importance, aes(x=variable, weight=importance, fill=variable))
          + geom_bar() + ggtitle("Variable Importance for Random Forest")
          + xlab("Variables") + ylab("Variable Importance (Mean Decrease in Gini Index)")
          + scale_fill_discrete(name="Variable Name",labels=c('Focus Shift (Disease)','Coherence Shift (drug)','Mean Enrollment (pre-market)','Marketing Date','Focus Shift (drug)','Coherence Shift (disease)'))
          + theme(axis.text.x=element_blank(),
                  axis.text.y=element_text(size=12),
                  axis.title=element_text(size=14),
                  plot.title=element_text(size=16),
                  legend.title=element_text(size=14),
                  legend.text=element_text(size=12))
    )
    
    textplot(importance(rf.fit))
    rf.fit.pr = predict(rf.fit,type="prob")[,2]
    
    #validate in different time frames
    
    yearsFromNow <- ceiling(as.numeric(difftime(Sys.Date(), testDate, unit="weeks"))/52.25)
    endDate = Sys.Date()
    for (currentValidateDate in seq.Date( as.Date(testDate), length=max(1,yearsFromNow+1), by='1 year' )){
      #generate the partial corrected response
      if(as.Date(currentValidateDate)>endDate){
        print(paste(as.Date(currentValidateDate),'passed! No mroe unknown BBW acquisition event. Refer to above AUC'))
        next
      }
      #if true, should add bias
      complete_set_biasedIndex <- (complete_set_predict$label==1 & complete_set_drugGroup$bbwDate>as.Date(currentValidateDate))
      complete_set_biasedResponse<-complete_set_predict
      #save biased validation matrix
      complete_set_biasedCalculated<-complete_set_calculated  
      #no unknown BBW
      addValidationBiasFlag=1
      if(table(complete_set_biasedIndex)[['FALSE']]==length(complete_set_biasedIndex)){
        print(paste('No unknown BBWs from',as.Date(currentValidateDate)))
        endDate = as.Date(currentValidateDate)
        addValidationBiasFlag=0
      }
      if(addValidationBiasFlag==1){
        complete_set_biasedResponse[complete_set_biasedIndex,]$label<-0
        complete_set_biasedCalculated[complete_set_biasedIndex,]$label<-0
        complete_set_biasedCalculated[complete_set_biasedIndex,]$bbwDate<-NA
        complete_set_biasedCalculated[complete_set_biasedIndex,]$bbwText<-NA
      }
      print(paste('!!!Validation date: ',as.Date(currentValidateDate)))
      print(paste('!!!Out of',length(complete_set_predict[complete_set_predict$label==1,]$label),'BBWs,',
                  length(complete_set_predict[complete_set_predict$label==1,]$label)-(length(complete_set_biasedIndex)-table(complete_set_biasedIndex)[['FALSE']]),'are known for validation set!'))
      possibleError<-tryCatch({
        rf.fit.pred = prediction(rf.fit.pr, complete_set_biasedResponse$label) 
        rf.fit.perf = performance(rf.fit.pred,"tpr","fpr")
        auc <- performance(rf.fit.pred,"auc",fpr.stop=1) #can change for partial AUC here!
      }, error=function(e) e
      )
      if(inherits(possibleError, "error")){
        cat("ERROR 445: @ round",testDate," ;Message",conditionMessage(possibleError), "\n")
        next
      }
      
      auc <- unlist(slot(auc, "y.values"))
      print(paste('!!!!!!!!!!!!!!!!!!Evolving auc:',auc,';TestDate:',testDate,';Validate date:',as.Date(currentValidateDate)))
      # ROC
      plot(rf.fit.perf,main="ROC Curve for Random Forest Predictor",sub=paste('TestDate:',testDate,' Validate date:',as.Date(currentValidateDate)),col=2,lwd=2,colorize = TRUE)
      abline(a=0,b=1,lwd=2,lty=2,col="gray")
      
      #save the biased-validation sets
      rf.fit.rank<-data.frame(drug=complete_set_biasedCalculated$drug,label=as.factor(complete_set_biasedCalculated$label),marketDate=complete_set_biasedCalculated$marketDate,bbwDate=complete_set_biasedCalculated$bbwDate,prediction=rf.fit.pr)
      rf.fit.rank<-rf.fit.rank[order(rf.fit.rank$prediction,decreasing = TRUE),]
      write.csv(rf.fit.rank,paste(resultDir,'rf_fit_',as.Date(currentValidateDate),'.csv',sep=''),row.names = FALSE, quote = FALSE)
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
      legend("bottomright", legend = c("Empirical", "Density",paste('Training Date:',testDate),paste('Validation date:',as.Date(currentValidateDate))),cex=1.0, col = c("black", "#008600",'black','black'),lwd = 2,lty=c(1,3,NA,NA),pch = c(NA, NA, 8,8))
      
    }#validation date
  }else{#BIASED DATA, UNBIASED OUTCOME
    #######predict whether is BBW (label) OOB
    target_df<- complete_set_predict[,c(-3,-4,-5,-6,-8)]#complete_set_basic[,c(3,4,7,8,9,10,11,12)] #
    
    bestmtry <- data.frame(tuneRF(y=as.factor(target_df$label),x=target_df[,-1], ntreeTry=1000, 
                                  stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE))
    candimtry <- bestmtry[bestmtry$OOBError==min(bestmtry$OOBError),]$mtry
    #SELECT MTRY FOR NEXT STEP
    rf.fit <- randomForest(y=as.factor(target_df$label),x=target_df[,-1],mtry=candimtry,
                           ntree=1000,importance=TRUE,keep.forest=TRUE)
    #importance
    textplot(varImpPlot(rf.fit))
    
    
    # Extracts variable importance (Mean Decrease in Gini Index)
    # Sorts by variable importance and relevels factors to match ordering
    var_importance <- data.frame(variable=setdiff(colnames(target_df), "label"),
                                 importance=as.vector(importance(rf.fit,type=1)))
    var_importance <- var_importance[order(var_importance$importance,decreasing = TRUE),]
    var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)
    
    print(ggplot(var_importance, aes(x=variable, weight=importance, fill=variable))
           + geom_bar() + ggtitle("Variable Importance")
           + xlab("Variables") + ylab("Variable Importance (Mean Decrease in Gini Index)")
           + scale_fill_discrete(name="Variable Names",labels=c('Focus Shift (Disease)','Coherence Shift (drug)','Mean Enrollment (pre-market)','Marketing Date','Focus Shift (drug)','Coherence Shift (disease)'))
           + theme(axis.text.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title=element_text(size=16),
              plot.title=element_text(size=18),
              legend.title=element_text(size=14),
              legend.text=element_text(size=10))
    )
    
    #capture.output(importance(rf.fit,type=1),file=paste(resultDir,'importance.doc',sep=''),append = TRUE)
    textplot(importance(rf.fit,type=1))
    capture.output(importance(rf.fit),file=paste(resultDir,'importance.doc',sep=''),append = FALSE)
    rf.fit.pr = predict(rf.fit,type="prob")[,2]
    rf.fit.pred = prediction(rf.fit.pr, target_df$label)
    
    rf.fit.perf = performance(rf.fit.pred,"tpr","fpr")
    auc <- performance(rf.fit.pred,"auc")
    auc <- unlist(slot(auc, "y.values"))
    print(paste('OOB auc:',auc,'testDate:',testDate))
    # ROC
    plot(rf.fit.perf,main="ROC Curve for Random Forest Predictor",col=2,lwd=2,colorize = TRUE)
    abline(a=0,b=1,lwd=2,lty=2,col="gray")
    
    rf.fit.rank<-data.frame(drug=complete_set_drugGroup$drug,label=as.factor(complete_set_drugGroup$label),marketDate=complete_set_drugGroup$marketDate,bbwDate=complete_set_drugGroup$bbwDate,prediction=rf.fit.pr)
    rf.fit.rank<-rf.fit.rank[order(rf.fit.rank$prediction,decreasing = TRUE),]
    write.csv(rf.fit.rank,paste(resultDir,'rf_fit_OOB.csv',sep=''),row.names = FALSE, quote = FALSE)
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
    legend("bottomright", legend = c("Empirical", "Density"),cex=1.0, col = c("black", "#008600"),lwd = 2,lty=c(1,3))
  }
  
  dev.off()
  
}#date seg loop
  
###################################################
###################################################
################MANUAL PART########################
###################################################
###################################################

##############################################FIGURE 1, DATA IN TABLE, WORKFLOW
targetDF <- complete_set_basic

length(targetDF$drug)
#bbw drug num
length(targetDF[targetDF$label==1,]$drug)
#robust drug num
length(targetDF[targetDF$label==0,]$drug)
#pre trial num/bbw
#######
#bbw-post/bbw-pre
sum(targetDF[targetDF$label==1,]$postDatedTrialNum)
sum(targetDF[targetDF$label==1,]$preDatedTrialNum)
#robust-post/robust-pre
sum(targetDF[targetDF$label==0,]$postDatedTrialNum)
sum(targetDF[targetDF$label==0,]$preDatedTrialNum)
#sum bbw/robust
sum(targetDF[targetDF$label==1,]$postDatedTrialNum)+sum(targetDF[targetDF$label==1,]$preDatedTrialNum)
sum(targetDF[targetDF$label==0,]$postDatedTrialNum)+sum(targetDF[targetDF$label==0,]$preDatedTrialNum)
#sum post/pre
sum(targetDF$postDatedTrialNum)
sum(targetDF$preDatedTrialNum)
#sum all
sum(targetDF$postDatedTrialNum)+sum(targetDF$preDatedTrialNum)


mean(targetDF[targetDF$label==1,]$preDatedTrialNum)
#post trial num/bbw
mean(targetDF[targetDF$label==1,]$postDatedTrialNum)
#pre trial num/robust
mean(targetDF[targetDF$label==0,]$preDatedTrialNum)
#post trial num/robust
mean(targetDF[targetDF$label==0,]$postDatedTrialNum)

#pre trial num/bbw
mean(targetDF[targetDF$label==1,]$prePhaseTrialNum)
#post trial num/bbw
mean(targetDF[targetDF$label==1,]$postPhaseTrialNum)
#pre trial num/robust
mean(targetDF[targetDF$label==0,]$prePhaseTrialNum)
#post trial num/robust
mean(targetDF[targetDF$label==0,]$postPhaseTrialNum)



complete_set_predict<- complete_set[complete.cases(complete_set[,c(-1,-2,-5,-6)]),c(-1,-2,-5,-6)]
complete_set_predict$marketDate<-as.numeric(complete_set_predict$marketDate)
#modify the trialNum/Date vars
complete_set_predict$postPhaseTrialNum <- complete_set_predict$postPhaseTrialNum/(as.integer(Sys.Date())-complete_set_predict$marketDate)
complete_set_predict$postDatedTrialNum <- complete_set_predict$postDatedTrialNum/(as.integer(Sys.Date())-complete_set_predict$marketDate)



#############################################benchmark of lower-limit CT

pdf(paste(rootDir,benchmarkSubDir,'benchmark.pdf',sep=''))

#data: created manually
benchmark_data <- read.csv(paste(rootDir,benchmarkSubDir ,'benchmark.csv',sep=''),header=TRUE)
colnames(benchmark_data)<-as.character(c(2:10))
numberofDrugs<-list('2'=c(121,38,83),'3'=c(113,34,79),'4'=c(102,30,72),'5'=c(95,27,68),'6'=c(91,26,65),'7'=c(82,23,59),'8'=c(68,18,50),'9'=c(59,17,42),'10'=c(58,16,42))
numberofDrugs.df<-data.frame(matrix(unlist(numberofDrugs), nrow=9, byrow=T))
colnames(numberofDrugs.df)<-c('TOTAL','BBW','ROBUST')
rownames(numberofDrugs.df)<-as.character(c(2:10))

#plot.new()

plot.window(xlim=range(benchmark_data),ylim=range(length(benchmark_data)))
par(mar=c(5,5,5,5))
barplot(t(numberofDrugs.df[,-1]),ylim=c(0,200),angle = c(30,120),density=20,col = rgb(c(1,0), c(0.65,1), c(0.31,0), rep(0.5, 2)),axes = FALSE, axisnames = FALSE)
axis(4)
mtext("Drug Count",side=4,line=3)
par(new=T)
beanplot(benchmark_data,col = rgb(0,0,1, 0.5),main='Model Performance (AUC) with Different Parameters', xlab='Clinical Trial Lower Limit (parameter)',ylab='AUC Distribution')
#write.table(complete_set_calculated_para2,paste(rootDir,benchmarkSubDir,'complete_set_calculated_para2_tab',sep=''),,row.names = FALSE, quote = FALSE)
dev.off()

  
##############################################PREDICTIVE PORWER IN TIME
pdf(paste(rootDir,predictivePowerSubDir,'predictivePowerInTime.pdf',sep=''))

#data: created manually
powerInTime_data <- read.csv(paste(rootDir,predictivePowerSubDir ,'predictivePowerInTime.csv',sep=''),header=TRUE)
powerInTime.df <- data.frame(powerInTime_data)
#"noice",the BBW acquired between traing and testing
powerInTime.df$BBWAcquisitionCount<-powerInTime.df$knownValidationBBW-powerInTime.df$knownTrainingBBW

#find average BBW acquisition number in a year

ggplot(powerInTime.df, aes(testDate, AUC, group = BBWAcquisitionCount, colour = as.factor(predictiveRange),shape=as.factor(predictiveRange))) +
  geom_line(alpha = 1)+
  geom_point(aes(size = BBWAcquisitionCount)) + 
  scale_colour_manual(name = "Predictive Range (year)",
                      labels = unique(powerInTime.df$predictiveRange),
                      values = brewer.pal(11, 'Paired')) +   
  scale_shape_manual(name = "Predictive Range (year)",
                     labels = unique(powerInTime.df$predictiveRange),
                     values = 1:nlevels(as.factor(powerInTime.df$predictiveRange)))+
  labs(title = "Model Predictive Range Assessment", x="Training Date", y="Model Performance (AUC)") 

#[powerInTime.df$testDate=='1/1/09',]
ggplot(powerInTime.df, aes(as.factor(predictiveRange), AUC, group = testDate, colour = as.factor(testDate),shape=as.factor(predictiveRange))) +
  geom_path(alpha = 1)+
  geom_point(aes(size = BBWAcquisitionCount))+
  scale_colour_manual(name = "Test Date",
                      labels = unique(powerInTime.df$testDate),
                      values = brewer.pal(11, 'Paired')) +   
  scale_shape_manual(name = "Test Date",
                     labels = unique(powerInTime.df$testDate),
                     values = 1:nlevels(as.factor(powerInTime.df$testDate)))+
  labs(title = "Model Predictive Power", x="Predictive range (year)", y="Model Performance (AUC)") 

dev.off()


