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

fhIn_dir <- '../result/selected_drug_trial_List/'
integrateTableName <- 'integratedDrugTrialTable.csv'
integrateTableNameUpdate <- 'integratedDrugTrialTableMatched.csv'

getIntegrateTable_switch = 'off'
generateMedParserInputs_switch = 'off'
getMedParserResult_switch = 'off'
getSnowMedResult_switch = 'off'
getIntegratedTableUpdated_switch ='off'

runCoherenceOccurenceTest_switch ='on'
matchPharmGKBName_switch = 'on'
mappingADEOffsides_switch='on'
mappingDDITwosides_switch='on'
#############generate integrate table###########################
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
  content<-paste(mainTable$official_title,mainTable$condition,mainTable$brief_summary,mainTable$criteria,sep=';')
  
  occur<-sapply(snowmed_disease, grepl, content, ignore.case=TRUE) #one line is one trial, one col is one instance in snowmed
  mainTable$ECdisease <- rep(NA,length(occur[,1]))
  for (i in 1:length(occur[,1])){
    if(length(snowmed_disease[occur[i,]])>0){
      mainTable$ECdisease[i]<-paste(snowmed_disease[occur[i,]],collapse=';')
    }
    else{
      mainTable$ECdisease[i]<-''
    }
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

mainTable <- read.csv(paste(fhIn_dir,integrateTableNameUpdate,sep=''),header=T, stringsAsFactors=F)
#refine
mainTable<- mainTable[!is.na(mainTable$enrollment),]

####

stopWords <- stopwords("en")
content<-paste(mainTable$official_title,mainTable$condition,mainTable$brief_summary,mainTable$criteria,sep=' ')
wordNum_content<-unlist(lapply(content,function(x) length(unlist(strsplit(content[1],'\\s+'))[!(unlist(strsplit(x,'\\s+')) %in% stopWords)])))
wordNum_ECdrug<-unlist(lapply(mainTable$ECdrug,function(x) length(strsplit(x,';')[[1]])))
wordNum_ECdisease<-unlist(lapply(mainTable$ECdisease,function(x) length(strsplit(x,';')[[1]])))



####
drug<-mainTable$drug
drug.unique<-unique(drug)

if(runCoherenceOccurenceTest_switch =='on'){
  
  colNumTesting_list = c(27,29) #27:ECdrug; 29: ECdisease
  
  #change data to corpus
  for(colNumTesting in colNumTesting_list){
    ifTestCoherence = 1
    ifTestOccurence = 1
    varNameTesting = colnames(mainTable)[colNumTesting]
    occurrence_df<-data.frame()
    coherence_df<- data.frame()
    count=0
    for(j in drug.unique){
      #j='chantix'#for test purpose
      count = count + 1
      drug.all<- mainTable[mainTable$drug==j,]
      j.group <- mainTable[mainTable$drug==j,]$group[1]
      drug.all$testing<-lapply(drug.all[,colNumTesting],function(x) c(strsplit(x,';')[[1]])) # [[],[]]
      drug.all$testing<-lapply(drug.all$testing,function(x) gsub(' ','-',x))
      drug.all$testing <- Corpus(VectorSource(drug.all$testing)) 
      
      corpus_pre <- drug.all[drug.all$period=='PRE',]$testing #all the trials (with disease sep ;) for pre drug; one drug, one doc
      corpus_post <- drug.all[drug.all$period=='POST',]$testing
      #test coherence using JSD
      if (ifTestCoherence == 1){
        jsd_score <- testCoherence(corpus_pre,corpus_post)
        #print(c(count,j,j.group,jsd_score$jsd_pre,jsd_score$jsd_post))
        #print(paste(j,jsd_score))
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
    
    A <-coherence_df[complete.cases(coherence_df),]
    A_BBW<-A[A$group=='BBW',]
    A_ROBUST<-A[A$group=='ROBUST',]
    info <- occurrence_df[complete.cases(occurrence_df),]
    info<-info[order(-as.numeric(info$newOccureScore)),]
    #info<-A[order(-A$change),]
    info$label<-rep(1,length(info$group))
    name1<-paste('occurrence',varNameTesting,'_df',sep='')
    name2<-paste('coherence',varNameTesting,'_df',sep='')
    assign(name1,info)
    assign(name2,A)
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
      
      boxplot(as.numeric(A_ROBUST$jsd_pre),as.numeric(A_ROBUST$jsd_post),as.numeric(A_BBW$jsd_pre),as.numeric(A_BBW$jsd_post),ylim=c(0,0.7),main='Coherent Shift for Robust/BBW Drugs',col=c('darkseagreen1','darkseagreen1','antiquewhite2','antiquewhite2'),names=c('Pre-ROBUST','Post-ROBUST','Pre-BBW','Post-BBW'),notch = F,ylab='JS-Divergence score')
      ks2<-ks.test(as.numeric(A_ROBUST$jsd_pre),as.numeric(A_ROBUST$jsd_post),alternative = 'greater')
      ks3<-ks.test(as.numeric(A_BBW$jsd_pre),as.numeric(A_BBW$jsd_post),alternative = 'greater')
      write('\nKS test for figure: Coherent Shift for Robust/BBW Drugs: ',file=logFile,append=T)
      write(as.character(ks2),file=logFile,append=T)
      write('\n',logFile,append=T)
      write(as.character(ks3),file=logFile,append=T)
      print(c(ks1$p.value,ks2$p.value,ks3$p.value))
    }
    #Count
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
      
      
      info$label<-rep(0,length(info$group))
      info[info$group=='BBW',]$label<-1
      A$change<-A$jsd_post-A$jsd_pre
      B<- merge(info,A,by='drug')
      
      pred<-prediction(info$newOccureScore,info$label)
      perf <- performance(pred, "tpr", "fpr")
      #plot(perf)
      
      qplot(change, newOccureScore, data=B, geom=c("point", "smooth"), method="lm", formula=y~poly(x), color=group.x, main="Regression of MPG on Weight", xlab="Divergence", ylab="New Content")
      qplot(jsd_pre, jsd_post, data=B, geom=c("point", "smooth"), method="lm", formula=y~poly(x), color=group.x, main="Regression of MPG on Weight", xlab="Weight", ylab="Miles per Gallon")
      
      B$predict<- B$newOccureScore/B$jsd_post
      pred<-prediction(B$predict,B$label)
      perf <- performance(pred, "tpr", "fpr")
      #plot(perf)
    }
    dev.off()
  }
}
#################################################################################
#mapping brand names and generic names
if(matchPharmGKBName_switch == 'on' | mappingADEOffsides_switch=='on' | mappingDDITwosides_switch=='on'){
#####NOTE THAT SOME DRUGS MAY NOT MATCH!
  drugNameFile <- '../data/PharmGKB/drugs/drugs.tsv'
  gkbDrug<-read.delim(drugNameFile,sep = '\t',header = T,stringsAsFactors = FALSE)
  drugName_df<-data.frame(stringsAsFactors = FALSE)
  for (i in drug.unique){
    line <- gkbDrug[grepl(i,gkbDrug$Trade.Names,ignore.case = TRUE),]
    if(length(line[,1])>0){
      add <- data.frame(name = tolower(line$Name), tradeName = i, genericName = tolower(line$Generic.Names), externalVocab = line$External.Vocabulary)
      drugName_df<-rbind(drugName_df,add)
    }
  }
  #get unique match
  manualCorrect <- list('yaz'='drospirenone','tricor'='fenofibrate','ranexa'='ranolazine','lantus'='insulin glargine')
  drugName_df_unique <- drugName_df
  for (check in unique(drugName_df_unique$tradeName)){
    #print (paste(check,length(which(drugName_df_unique$tradeName == check))))
    if (length(which(drugName_df_unique$tradeName == check))>1){
      drugName_df_unique<- drugName_df_unique[!drugName_df_unique$tradeName==check,]
      if(!is.null(manualCorrect[[check]])){
        manualAdd <- data.frame(name = manualCorrect[[check]],tradeName = check, genericName='',externalVocab='')
        drugName_df_unique<-rbind(drugName_df_unique,manualAdd)
      }
    }
  }
  print (paste('NOTE THAT: Out of',length(drug.unique), 'drugs we analyzed,',length(unique(drugName_df_unique$tradeName)), 'have been found a match in PharmGKB for ADE/DDI analysis!'))
}
###############Find ADE and DDI for each drug##################
if (mappingADEOffsides_switch=='on'){
  
  cache <- '../result/anaCoherenceOccurrence/ADE.cache'
  if(file.exists(cache)){
    ADE_df <- dget(cache)
  }else{
  
    offsides_file <- '../data/PharmGKB/3003377s-offsides.tsv'
    offsides <- read.delim(offsides_file,sep='\t',header=T,stringsAsFactors = FALSE)
    
    ADE_df<-data.frame(stringsAsFactors = FALSE)
    
    for(tradeName in drugName_df_unique$tradeName){
      #tradeName = 'chantix'
      name <- drugName_df_unique[drugName_df_unique$tradeName==tradeName,]$name
      #ADE
      relatedEvent <- offsides[offsides$drug == name,]$event
      relatedEvent <- paste(relatedEvent,collapse = ';')
      if (length(relatedEvent)>0){
        line <- data.frame(name = name, tradeName=tradeName, ADE = relatedEvent)
        ADE_df <- rbind(ADE_df,line)
      }
    }
    rm(offsides)
    dput(ADE_df,cache)
  }

  source('./rlib/elitesLib.R')
  occurrence_df<-data.frame()
  count=0
  for(j in drug.unique){
    #j='chantix'#for test purpose
    count = count + 1
    drug.all<- mainTable[mainTable$drug==j,]
    j.group <- mainTable[mainTable$drug==j,]$group[1]
    drug.all$testing<-lapply(drug.all[,29],function(x) c(strsplit(x,';')[[1]])) # [[],[]]
    drug.all$testing<-lapply(drug.all$testing,function(x) gsub(' ','-',x))
    drug.all$testing <- Corpus(VectorSource(drug.all$testing)) 
    
    corpus_pre <- drug.all[drug.all$period=='PRE',]$testing #all the trials (with disease sep ;) for pre drug; one drug, one doc
    corpus_post <- drug.all[drug.all$period=='POST',]$testing
    
    currentADE <- as.character(ADE_df[ADE_df$tradeName == j,]$ADE)
    currentADE <- unlist(strsplit(currentADE,';'))
    currentADE <- gsub(' ','-',currentADE)
    if(length(currentADE)>0){
      corpus_ADE<-Corpus(VectorSource(currentADE))
      if(0){
        occur_preADE<-testOccurence2(corpus_pre,corpus_ADE)
        occur_postADE<-testOccurence2(corpus_post,corpus_ADE)
        if ((!occur_preADE==FALSE) & (!occur_postADE==FALSE)){
          newline2 <- data.frame(count = count, drug=j,group=j.group,preADE = occur_preADE, postADE = occur_postADE)
          occurrence_df<-rbind(occurrence_df,newline2)
        }
      }
      if(1){
        #print(j)
        newADEScore = testOccurence3(corpus_pre,corpus_post,corpus_ADE)
        if ((!newADEScore == FALSE) & (!is.na(newADEScore))){
          newline2 <- data.frame(count = count, drug=j,group=j.group,newOccureScore = newADEScore)
          occurrence_df<-rbind(occurrence_df,newline2)
        }
      }
    }
  }
  

  info <- occurrence_df[complete.cases(occurrence_df),]
  info<-info[order(-as.numeric(info$newOccureScore)),]
  
  
  logFile <- '../result/anaCoherenceOccurrence/ADE.log'
  pdfFile <- '../result/anaCoherenceOccurrence/ADE.pdf'
  pdf(pdfFile)
  if(0){
    A <-occurrence_df[complete.cases(occurrence_df),]
    boxplot(as.numeric(A$preADE),as.numeric(A$postADE),boxwex=0.5,notch = F,col=c('gray63','gray63'),main='ADE Coherence Level',names=c('Pre-Market','Post-Market'),ylab='JS-Divergence score')
    ks1<-ks.test(as.numeric(A$preADE),as.numeric(A$postADE),alternative = 'greater')
    write('KS test for figure: Coherent Shift of Eligibility Criteria: ',file=logFile,append=T)
    write(as.character(ks1),file=logFile,append=T)
    
    A_BBW<-A[A$group=='BBW',]
    A_ROBUST<-A[A$group=='ROBUST',]
    boxplot(as.numeric(A_ROBUST$preADE),as.numeric(A_ROBUST$postADE),as.numeric(A_BBW$preADE),as.numeric(A_BBW$postADE),ylim=c(0,0.7),main=' ADE occurrence in Eligibility Criteria for Robust/BBW Drugs',col=c('darkseagreen1','darkseagreen1','antiquewhite2','antiquewhite2'),names=c('Pre-ROBUST','Post-ROBUST','Pre-BBW','Post-BBW'),notch = F,ylab='cosine similarity')
    ks2<-ks.test(as.numeric(A_ROBUST$preADE),as.numeric(A_ROBUST$postADE),alternative = 'greater')
    ks3<-ks.test(as.numeric(A_BBW$preADE),as.numeric(A_BBW$postADE),alternative = 'greater')
    
    A$change<-A$postADE-A$preADE
    boxplot(as.numeric(A[A$group=='ROBUST',]$change),as.numeric(A[A$group=='BBW',]$change),boxwex=0.5,col=c('darkseagreen1','antiquewhite2'),notch = F,main='Emerging Criteria Level for ROBUST/BBW Drugs',names=c('ROBUST Drugs','BBW Drugs'),ylab='Emerging Criteria Level')
    ks4<-ks.test(A[A$group=='ROBUST',]$change,A[A$group=='BBW',]$change,alternative='less')
    write(as.character(ks4),file=logFile,append=T)
    
    info<-A[order(-A$change),]
    info$label<-rep(1,length(info$group))
    info[info$group=='BBW',]$label<-0
    pred<-prediction(info$change,info$label)
    perf <- performance(pred, "tpr", "fpr")
    plot(perf)
    ####????
    
    #qplot(preADE, postADE, data=info, geom=c("point", "smooth"), method="lm", formula=y~poly(x), color=group, main="Regression of MPG on Weight", xlab="Divergence", ylab="New Content")
    #qplot(jsd_pre, jsd_post, data=B, geom=c("point", "smooth"), method="lm", formula=y~poly(x), color=group.x, main="Regression of MPG on Weight", xlab="Weight", ylab="Miles per Gallon")
  }
  if(1){
    
    boxplot(as.numeric(info[info$group=='ROBUST',]$newOccureScore),as.numeric(info[info$group=='BBW',]$newOccureScore),boxwex=0.5,col=c('darkseagreen1','antiquewhite2'),notch = F,main='Pre/Post-market Trial ADE Similarity',names=c('ROBUST Drugs','BBW Drugs'),ylab='Cosine Similarity')
    #View(info)
    ks4_l<-ks.test(as.numeric(info[info$group=='BBW',]$newOccureScore),as.numeric(info[info$group=='ROBUST',]$newOccureScore),alternative = 'less',exact = NULL)
    ks4_g<-ks.test(as.numeric(info[info$group=='BBW',]$newOccureScore),as.numeric(info[info$group=='ROBUST',]$newOccureScore),alternative = 'greater',exact = NULL)
    ks4<-ks4_l
    if (ks4_g$p.value < ks4_l$p.value){ks4<-ks4_g}
    write('\nKS test for figure: Emerging Criteria Level for ROBUST/BBW Drugs: ',file=logFile,append=T)
    write(as.character(ks4),file=logFile,append=T)
    info$label<-rep(0,length(info$group))
    info[info$group=='BBW',]$label<-1
    pred<-prediction(info$newOccureScore,info$label)
    perf <- performance(pred, "tpr", "fpr")
    plot(perf)
    #qplot(,newOccureScore, data=info, geom=c("point", "smooth"), method="lm", formula=y~poly(x), color=group, main="Regression of MPG on Weight", xlab="Divergence", ylab="New Content")
    #qplot(jsd_pre, jsd_post, data=B, geom=c("point", "smooth"), method="lm", formula=y~poly(x), color=group.x, main="Regression of MPG on Weight", xlab="Weight", ylab="Miles per Gallon")
    
  }
  assign('occurrenceADE_df',info)
  dev.off()
}
#######################################################

if (mappingDDITwosides_switch=='on'){
  cache <- '../result/anaCoherenceOccurrence/DDI.cache'
  if(file.exists(cache)){
    DDI_df <- dget(cache)
  }else{
    twosides_file <- '../data/PharmGKB/3003377s-twosides.tsv'
    twosides <- read.delim(twosides_file,sep='\t',header=T,stringsAsFactors = FALSE)
    DDI_df<-data.frame(stringsAsFactors = FALSE)
    for(tradeName in drugName_df_unique$tradeName){
      #tradeName = 'chantix'
      name <- drugName_df_unique[drugName_df_unique$tradeName==tradeName,]$name
      #DDI
      relatedDrug<-c(unique(twosides[twosides$drug1==name,]$drug2),unique(twosides[twosides$drug2==name,]$drug1))
      relatedDrug <- paste(relatedDrug,collapse = ';')
      if (length(relatedDrug)>0){
        line <- data.frame(name = name, tradeName=tradeName, DDI = relatedDrug)
        DDI_df <- rbind(DDI_df,line)
      }
    }
    rm(twosides)
    dput(DDI_df,cache)
  }
  
  source('./rlib/elitesLib.R')
  occurrence_df<-data.frame()
  count=0
  for(j in drug.unique){
    #j='chantix'#for test purpose
    count = count + 1
    drug.all<- mainTable[mainTable$drug==j,]
    j.group <- mainTable[mainTable$drug==j,]$group[1]
    drug.all$testing<-lapply(drug.all[,27],function(x) c(strsplit(x,';')[[1]])) # [[],[]]
    drug.all$testing<-lapply(drug.all$testing,function(x) gsub(' ','-',x))
    drug.all$testing <- Corpus(VectorSource(drug.all$testing)) 
    
    corpus_pre <- drug.all[drug.all$period=='PRE',]$testing #all the trials (with disease sep ;) for pre drug; one drug, one doc
    corpus_post <- drug.all[drug.all$period=='POST',]$testing
    
    currentADE <- as.character(DDI_df[DDI_df$tradeName == j,]$DDI)
    currentADE <- unlist(strsplit(currentADE,';'))
    currentADE <- gsub(' ','-',currentADE)
    if(length(currentADE)>0){
      
      corpus_ADE<-Corpus(VectorSource(currentADE))
      if(0){
        #print(paste(j,j.group))
        occur_preADE<-testOccurence2(corpus_pre,corpus_ADE)
        occur_postADE<-testOccurence2(corpus_post,corpus_ADE)
        if ((!occur_preADE==FALSE) & (!occur_postADE==FALSE)){
          newline2 <- data.frame(count = count, drug=j,group=j.group,preADE = occur_preADE, postADE = occur_postADE)
          occurrence_df<-rbind(occurrence_df,newline2)
        }
      }
      if(1){
        newADEScore = testOccurence3(corpus_pre,corpus_post,corpus_ADE)
        if ((!newADEScore == FALSE) & (!is.na(newADEScore))){
          newline2 <- data.frame(count = count, drug=j,group=j.group,newOccureScore = newADEScore)
          occurrence_df<-rbind(occurrence_df,newline2)
        }
      }
    }
  }
  info <- occurrence_df[complete.cases(occurrence_df),]
  info<-info[order(-as.numeric(info$newOccureScore)),]
  
  logFile <- '../result/anaCoherenceOccurrence/DDI.log'
  pdfFile <- '../result/anaCoherenceOccurrence/DDI.pdf'
  pdf(pdfFile)
  
  if(0){
    A <-occurrence_df[complete.cases(occurrence_df),]
    boxplot(as.numeric(A$preADE),as.numeric(A$postADE),boxwex=0.5,notch = T,col=c('gray63','gray63'),main='Coherent Shift of Eligibility Criteria',names=c('Pre-Market','Post-Market'),ylab='JS-Divergence score')
    ks1<-ks.test(as.numeric(A$preADE),as.numeric(A$postADE),alternative = 'greater')
    write('KS test for figure: Coherent Shift of Eligibility Criteria: ',file=logFile,append=T)
    write(as.character(ks1),file=logFile,append=T)
    
    A_BBW<-A[A$group=='BBW',]
    A_ROBUST<-A[A$group=='ROBUST',]
    boxplot(as.numeric(A_ROBUST$preADE),as.numeric(A_ROBUST$postADE),as.numeric(A_BBW$preADE),as.numeric(A_BBW$postADE),ylim=c(0,3),main=' ADE occurrence in Eligibility Criteria for Robust/BBW Drugs',col=c('darkseagreen1','darkseagreen1','antiquewhite2','antiquewhite2'),names=c('Pre-ROBUST','Post-ROBUST','Pre-BBW','Post-BBW'),notch = F,ylab='cosine similarity')
    ks2<-ks.test(as.numeric(A_ROBUST$preADE),as.numeric(A_ROBUST$postADE),alternative = 'greater')
    ks3<-ks.test(as.numeric(A_BBW$preADE),as.numeric(A_BBW$postADE),alternative = 'greater')
    
    A$change<-A$postADE-A$preADE
    boxplot(as.numeric(A[A$group=='ROBUST',]$change),as.numeric(A[A$group=='BBW',]$change),boxwex=0.5,col=c('darkseagreen1','antiquewhite2'),notch = F,main='Emerging Criteria Level for ROBUST/BBW Drugs',names=c('ROBUST Drugs','BBW Drugs'),ylab='Emerging Criteria Level')
    ks4<-ks.test(A[A$group=='ROBUST',]$change,A[A$group=='BBW',]$change,alternative='less')
    write(as.character(ks4),file=logFile,append=T)
    
    info<-A[order(-A$change),]
    info$label<-rep(1,length(info$group))
    info[info$group=='BBW',]$label<-0
    pred<-prediction(info$change,info$label)
    perf <- performance(pred, "tpr", "fpr")
    plot(perf)
  }
  if(1){
    boxplot(as.numeric(info[info$group=='ROBUST',]$newOccureScore),as.numeric(info[info$group=='BBW',]$newOccureScore),boxwex=0.5,col=c('darkseagreen1','antiquewhite2'),notch = F,main='Pre/Post-market Trial DDI Similarity',names=c('ROBUST Drugs','BBW Drugs'),ylab='Cosine Similarity')
    #View(info)
    ks4_l<-ks.test(as.numeric(info[info$group=='BBW',]$newOccureScore),as.numeric(info[info$group=='ROBUST',]$newOccureScore),alternative = 'less',exact = NULL)
    ks4_g<-ks.test(as.numeric(info[info$group=='BBW',]$newOccureScore),as.numeric(info[info$group=='ROBUST',]$newOccureScore),alternative = 'greater',exact = NULL)
    ks4<-ks4_l
    if (ks4_g$p.value < ks4_l$p.value){ks4<-ks4_g}
    write('\nKS test for figure: Emerging Criteria Level for ROBUST/BBW Drugs: ',file=logFile,append=T)
    write(as.character(ks4),file=logFile,append=T)
    info$label<-rep(0,length(info$group))
    info[info$group=='BBW',]$label<-1
    pred<-prediction(info$newOccureScore,info$label)
    perf <- performance(pred, "tpr", "fpr")
    plot(perf)
    #qplot(,newOccureScore, data=info, geom=c("point", "smooth"), method="lm", formula=y~poly(x), color=group, main="Regression of MPG on Weight", xlab="Divergence", ylab="New Content")
    #qplot(jsd_pre, jsd_post, data=B, geom=c("point", "smooth"), method="lm", formula=y~poly(x), color=group.x, main="Regression of MPG on Weight", xlab="Weight", ylab="Miles per Gallon")
    
  }
  assign('occurrenceDDI_df',info)
  dev.off()
  
}
#plot ana trand analysis
if(mappingADEOffsides_switch=='on' & runCoherenceOccurenceTest_switch =='on'){
  pdf('../result/anaCoherenceOccurrence/trendAna.pdf')
  B<- merge(coherenceECdisease_df,occurrenceADE_df,by='drug')
  B$change<-B$jsd_post-B$jsd_pre
  B$DrugGroup = B$group.x
  qplot(change, newOccureScore, data=B, geom=c("point", "smooth"), method="lm", formula=y~poly(x), color=DrugGroup, main="Different Eligibility Shift trends for BBW and ROBUST drugs", xlab="Focus Divergence Level", ylab="Focus Shift Level")
  
  #disease compare#not proper
  B<- merge(coherenceECdisease_df,occurrenceADE_df,by='drug')
  #drug compare#not proper
  B<- merge(coherenceECdrug_df,occurrenceDDI_df,by='drug')
  
  B$change<-B$jsd_post-B$jsd_pre
  B$DrugGroup = B$group.x
  C<-B[,c(1,8,10,3)]
  
  #two occurrence
  B<- merge(occurrenceADE_df,occurrenceDDI_df,by='drug')
  
  C<-B[,c(1,4,8,3)]
  
  colnames(C)<-c('drug','occurADE','occurDDI','drugGroup')
  
  library(grid)
  library(quadprog)
  library(directlabels)
  
  ggplot(C,aes(x=occurADE,y=occurDDI,label=drug)) + 
    stat_density2d(geom="tile", aes(fill = drugGroup, alpha=..density..), contour=FALSE) + 
    scale_fill_manual(name='legendmanual',values=c("BBW"="#FF0000", "ROBUST"="#00FF00")) +
    geom_point(colour="black", size = 3.5,alpha=0.6)+
    geom_point(aes(colour = drugGroup,shape = drugGroup),size=2.3) +
    labs(colour="l1",shape='l2',legendmanual='l3')+
    guides(alpha=FALSE) +
    geom_text(size=3.5,angle=0,hjust=0.5, vjust=0.5,alpha=0.95,position = position_jitter(width=0.02, height=0.02))+
    theme_minimal() +
    xlim(0,1.15)+ylim(-0.2,0.4)+
    xlab("x-lab-here")+ylab("y-lab-here")+ggtitle("title-here")
    
  
  if(0){
    coord_cartesian(xlim = c(0, 1.2), ylim = c(0, 1.2))
    xlim(c(0,1.2))+
    opts(
      panel.background = theme_rect(fill = "transparent",colour = NA), # or theme_blank()
      panel.grid.minor = theme_blank(), 
      panel.grid.major = theme_blank(),
      plot.background = theme_rect(fill = "transparent",colour = NA)
    ) +
  }
  

  dev.off()
}


###################################################################
#EPC drug class in candiTable_EPC
in_content <-  c('../result/drug_trial_content.csv')
content <- read.csv(in_content,header = T,stringsAsFactors = F)
content.df <- data.frame(content, stringsAsFactors = F)
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
content <- read.delim(in_content,header = F,stringsAsFactors = F)
content.df <- data.frame(content, stringsAsFactors = F)
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
