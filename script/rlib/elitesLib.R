library(tm)
library(SnowballC)
library(proxy)
library(entropy)
library(MASS)
library(Matching)
library(lsa)
getIntegrateTable<-function(fhIn_dir,integrateTableName){
  file_list <- list.files(fhIn_dir)
  CTcontent = data.frame()
  CTlist = data.frame()
  for (i in file_list){
    inFile <- paste(fhIn_dir,i,sep = '')
    if (grepl('comp',i)){
      tmp <- read.csv(inFile,header = F,stringsAsFactors=F)
      CTcontent <- rbind(CTcontent,data.frame(tmp))
    }
    if (grepl('list',i)){
      tmp <- read.table(inFile,header = F,sep='\t',stringsAsFactors=F)
      period <- toupper(gsub('ctList_(\\w+)_(\\w+)_list','\\2',i))
      drugGroup <- toupper(gsub('ctList_(\\w+)_(\\w+)_list','\\1',i))
      tmp <- data.frame(tmp)
      tmp$period <- rep(period,length(tmp[,1]))
      tmp$group <- rep(drugGroup,length(tmp[,1]))
      CTlist <- rbind(CTlist,tmp)
    }
  }
  #23 LENGTH
  colName <- c('nct_id',  'brief_title',  'official_title',	'condition',	'sponsors',	'agency_class',	'source',	'authority',	'brief_summary','overall_status',	'start_date',	'gender',	'minimum_age',	'maximum_age',	'study_pop',	'criteria',	'enrollment',	'phase',	'study_type',	'location',	'intervention_type',	'intervention_name',	'enrollment_type')
  colnames(CTlist)<-c('drug','nct_id','period','group')  #run into error: 'names' attribute [4] must be the same length as the vector [0]
  colnames(CTcontent)<-colName
  a <- CTlist[CTlist$nct_id %in% intersect(CTcontent$nct_id,CTlist$nct_id),]
  Integrated <- merge(a,CTcontent,by = 'nct_id') 
  Integrated <- Integrated[order(Integrated$drug,Integrated$period,Integrated$group),]
  # in order to get rid of the special "/" characters in drug names
  Integrated$drug <- gsub("/","-",Integrated$drug)
  
  print ('###############################')
  print (paste('Successfully generated integrated table at',fhIn_dir))
  print ("The integrated Table contains: ")
  print (paste(length(Integrated[,1]),'Entries in total'))
  print (paste(length(unique(Integrated$nct_id)),'unique Clinical Trials'))
  print (paste(length(unique(Integrated$drug)),'unique Drugs'))
  write.csv(Integrated,file = paste(fhIn_dir,integrateTableName,sep = ''),row.names=FALSE)
}


getWordFreqfromTDM<-function(tdm){
  temp <- tdm
  FreqMat <- data.frame(apply(temp, 1, sum))/dim(temp)[2]
  FreqMat <- data.frame(ST = row.names(FreqMat), Freq = FreqMat[, 1])
  FreqMat <- FreqMat[order(FreqMat$Freq, decreasing = T), ]
  row.names(FreqMat) <- NULL
  return(FreqMat)
  #View(FreqMat)
}

JSD4row<-function(matrix_in){
  jsd_out<-list()  
  num =0
  total = 0
  if(!is.vector(matrix_in)){#dim(matrix_in)[1]>0
    for (row in 1:dim(matrix_in)[1]){
      corpus_prob_in <- colSums(matrix_in)/sum(colSums(matrix_in))
      doc_prob_in <- matrix_in[row,]/sum(matrix_in[row,])
      doc_prob_in[is.nan(doc_prob_in)]<-0
      if (sum(doc_prob_in)!=0){
        m=(corpus_prob_in+doc_prob_in)/2.0
        jsd_out[[row]] <- (KL.plugin(corpus_prob_in,m)+KL.plugin(doc_prob_in,m))/2.0
        total = total + (KL.plugin(corpus_prob_in,m)+KL.plugin(doc_prob_in,m))/2.0
        num = num + 1   
      }
    }
  }
  if(num>0){
    return(total/num)
  }
  else{
    return(FALSE)
  }
}


generateMedParserInputs_byCT<-function(mainTable){
  print('Generating MedParser Input files...')
  parser_outdir<-c('./medex_mac/input/')
  parser_outdir2<-c('./medex_mac/output/')
  
  empty<-paste(parser_outdir,list.files(path=parser_outdir),sep='')
  unlink(empty, recursive = T)
  empty<-paste(parser_outdir2,list.files(path=parser_outdir2),sep='')
  unlink(empty, recursive = T)
  
  dir.create(file.path(parser_outdir), showWarnings = FALSE)
  dir.create(file.path(parser_outdir2), showWarnings = FALSE)
  
  for(j in 1:length(mainTable[,1])){
    content<-paste(mainTable$official_title[j],mainTable$condition[j],mainTable$brief_summary[j],mainTable$criteria[j],sep=';')
    write.csv(content,file=paste(parser_outdir,paste(mainTable$group[j],mainTable$drug[j],mainTable$period[j],mainTable$nct_id[j],j,sep='_'),sep='') )
    #./medex_mac/input/NCT00198198_BBW_POST_1
  }
  print(paste('files generated in ',parser_outdir,sep=''))
}


generateMedParserInputs_byDisease<-function(mainTable){
  print('Generating MedParser Input files...')
  parser_outdir<-c('./medex_mac/input/')
  parser_outdir2<-c('./medex_mac/output/')
  
  empty<-paste(parser_outdir,list.files(path=parser_outdir),sep='')
  unlink(empty, recursive = T)
  empty<-paste(parser_outdir2,list.files(path=parser_outdir2),sep='')
  unlink(empty, recursive = T)
  
  dir.create(file.path(parser_outdir), showWarnings = FALSE)
  dir.create(file.path(parser_outdir2), showWarnings = FALSE)
  
  drug<-mainTable$drug
  drug.unique<-unique(drug)
  
  count=0
  for(j in drug.unique){
    #j='intuniv'#for test purpose
    count = count + 1
    drug.all<- mainTable[mainTable$drug==j,]
    
    drug.group = 'ROBUST'
    if(grepl('BBW', drug.all$group[1])){
      drug.group = 'BBW'
    }
    #generate medParser drug-detector inputs
    if(1){
      parser_name<-paste(drug.group,j,sep = '_')
      target_file_pre<-paste(parser_outdir,parser_name,'_pre',sep = '')
      target_file_post<-paste(parser_outdir,parser_name,'_post',sep = '')
      write.csv(drug.pre$criteria,file=target_file_pre)
      write.csv(drug.post$criteria,file=target_file_post)
      print(paste('files generated in ',parser_outdir,sep=''))
    } 
  }
}

getMedParserResult_byCT<-function(file_list,mainTable){
  print('Gathering MedParser Outputs...')
  drugsAll<-c(rep('',length(mainTable[,1])))
  CUIAll <- c(rep('',length(mainTable[,1])))
  for (file in file_list){
    ct = sub('(.+).out','\\1',file)
    filePath<-paste(filesdir,file,sep = '')
    temp_dataset <-readLines(filePath)
    fileSplit <- c(strsplit(strsplit(file,'\\.')[[1]][1],'_')[[1]][1],strsplit(strsplit(file,'\\.')[[1]][1],'_')[[1]][2],strsplit(strsplit(file,'\\.')[[1]][1],'_')[[1]][3],strsplit(strsplit(file,'\\.')[[1]][1],'_')[[1]][4])
    pat<-'.+\\t(C\\d+.+)'
    drug <- ''
    cui <- '' 
    for(i in temp_dataset){
      if(grepl(pat,i)){
        detected <- strsplit(gsub(pat,'\\1',i),'\\t')[[1]]
        if (!detected[3] %in% drug){
          if (drug[1] == ''){
            drug <- detected[3]
            cui <- detected[1]
          }else{
            drug <- c(drug, detected[3])
            cui <- c(cui,detected[1])
          }
        }
      }
    }
    content <- c(fileSplit, paste(drug, collapse = ';'),paste(cui, collapse =';'))
    drugsAll[as.numeric(strsplit(strsplit(file,'\\.')[[1]][1],'_')[[1]][5])]<-content[5]
    CUIAll[as.numeric(strsplit(strsplit(file,'\\.')[[1]][1],'_')[[1]][5])]<-content[6]
  }
  print('Gathering MedParser Outputs...Done!')
  return(list(ECDrug = drugsAll,ECCUI = CUIAll))
}


getMedParserResult_byDisease<-function(){
  print('Gathering MedParser Outputs...')
  filesdir<-'./medex_mac/output/'
  file_list <- list.files(filesdir)
  drugsAll<-data.frame(stringsAsFactors=FALSE)
  count = 0
  for (file in file_list){
    count = count + 1
    if (count>10000000){
      print ('warning: reach upper limit...')
      break
    }
    temp_dataset = ''
    #file = 'BBW_altace_pre.out'
    fileSplit <- data.frame(strsplit(strsplit(file,'\\.')[[1]][1],'_')[[1]][1],strsplit(strsplit(file,'\\.')[[1]][1],'_')[[1]][2],strsplit(strsplit(file,'\\.')[[1]][1],'_')[[1]][3])
    #print(c(strsplit(strsplit(file,'\\.')[[1]][1],'_')[[1]][1],strsplit(strsplit(file,'\\.')[[1]][1],'_')[[1]][2],strsplit(strsplit(file,'\\.')[[1]][1],'_')[[1]][3]))
    filePath<-paste(filesdir,file,sep = '')
    if (!grepl('BBW|ROBUST',file)){
      next
    }
    temp_dataset <-readLines(filePath)
    pat<-'.+\\t(C\\d+.+)'
    for(i in temp_dataset){
      if(grepl(pat,i)){
        detected <- strsplit(gsub(pat,'\\1',i),'\\t')[[1]]
        content <- data.frame(group1=strsplit(strsplit(file,'\\.')[[1]][1],'_')[[1]][1],drug=strsplit(strsplit(file,'\\.')[[1]][1],'_')[[1]][2],group2=strsplit(strsplit(file,'\\.')[[1]][1],'_')[[1]][3],ECdrug=detected[3],CUI=detected[1],stringsAsFactors=FALSE)
        drugsAll<- rbind(drugsAll,content)
      }
    }
  }
  print('Gathering MedParser Outputs...Done!')
  return(drugsAll)
}


testCoherence<-function(corpus_pre,corpus_post){ #for each CT disease
  if (length(corpus_pre)>8 && length(corpus_post)>8){
    tdm_pre <- TermDocumentMatrix(corpus_pre,control = list(removePunctuation = FALSE,removeNumbers = TRUE,stopwords = TRUE))
    tdm_post <- TermDocumentMatrix(corpus_post,control = list(removePunctuation = FALSE,removeNumbers = TRUE,stopwords = TRUE))
    #JS-Divergence test (coherence)
    if(1){
      matrix_post <- t(as.matrix(tdm_post))
      matrix_pre <- t(as.matrix(tdm_pre))
      #colSums(matrix_post)[order(colSums(matrix_post))]
      #colSums(matrix_pre)[order(colSums(matrix_pre))]
      jsd_pre <- JSD4row(matrix_pre)
      jsd_post <- JSD4row(matrix_post)
      #jsd_df[count,]<-c(j,drug.group,jsd_pre,jsd_post)
    }
    return(list(jsd_pre=jsd_pre,jsd_post=jsd_post))
  }else{
    return(FALSE)
  }
}


testCoherence3<-function(corpus_pre,corpus_post,corpus_ADE){ #for each CT disease
  if (length(corpus_pre)>5 && length(corpus_post)>5){
    tdm_pre <- TermDocumentMatrix(corpus_pre,control = list(removePunctuation = FALSE,removeNumbers = TRUE,stopwords = TRUE))
    tdm_post <- TermDocumentMatrix(corpus_post,control = list(removePunctuation = FALSE,removeNumbers = TRUE,stopwords = TRUE))
    tdm_ADE <- TermDocumentMatrix(corpus_ADE,control = list(removePunctuation = FALSE,removeNumbers = TRUE,stopwords = TRUE))
    wordCount_ADE<-data.frame(ST=names(rowSums(as.matrix(tdm_ADE))),Count = rowSums(as.matrix(tdm_ADE)))
    row.names(wordCount_ADE)<-NULL
    #JS-Divergence test (coherence)
    if(1){
      matrix_post <- t(as.matrix(tdm_post))
      matrix_post<-matrix_post[,colnames(matrix_post) %in% wordCount_ADE$ST]
      matrix_pre <- t(as.matrix(tdm_pre))
      matrix_pre<-matrix_pre[,colnames(matrix_pre) %in% wordCount_ADE$ST]
      
      jsd_pre <- JSD4row(matrix_pre)
      jsd_post <- JSD4row(matrix_post)
      #jsd_df[count,]<-c(j,drug.group,jsd_pre,jsd_post)
    }
    return(list(jsd_pre=jsd_pre,jsd_post=jsd_post))
  }else{
    return(FALSE)
  }
}


testOccurence<-function(corpus_pre,corpus_post){
  #Word Frequency Test (Occurrence)
  if(1){
    if (length(corpus_pre)>8 && length(corpus_post)>8){
      tdm_pre <- TermDocumentMatrix(corpus_pre,control = list(removePunctuation = FALSE,removeNumbers = TRUE,stopwords = TRUE))
      tdm_post <- TermDocumentMatrix(corpus_post,control = list(removePunctuation = FALSE,removeNumbers = TRUE,stopwords = TRUE))
      wordFreq_pre<-getWordFreqfromTDM(tdm_pre)
      wordFreq_post<-getWordFreqfromTDM(tdm_post)
      
      #get rid of the words that have the lowest/HIGHEST occurrence (1)
      if(0){
        if (length(wordFreq_post[!(wordFreq_post$Freq == min(wordFreq_post$Freq)),]$Freq)>0 & length(wordFreq_pre[!(wordFreq_pre$Freq == min(wordFreq_pre$Freq)),]$Freq)>0){
          wordFreq_post<-wordFreq_post[!(wordFreq_post$Freq == min(wordFreq_post$Freq)),]
          wordFreq_pre<-wordFreq_pre[!(wordFreq_pre$Freq == min(wordFreq_pre$Freq)),]
        }
      }
      compare<-merge(x = wordFreq_pre, y = wordFreq_post, by = "ST", all = TRUE,suffixes = c("PRE","POST"))
      compare[is.na(compare)]<-0
      compare<-compare[!(compare$ST %in% c('blind','disease','disorder','history-of','drug','abuse')),]
      
      #normalize: each row sums to 1 so that in same scale
      compare$FreqPRE<-compare$FreqPRE/sum(compare$FreqPRE)
      compare$FreqPOST<-compare$FreqPOST/sum(compare$FreqPOST)
      
      change<-compare$FreqPOST-compare$FreqPRE  
      compare<-cbind(compare,change)
      compare<-compare[order(-compare$change),]
      #print(compare)
      #aver<-mean(compare$change[1:15])
      aver <- cosine(compare$FreqPRE,compare$FreqPOST)
      #aver <- KL.plugin(compare$FreqPRE,compare$FreqPOST)
      return(1-as.numeric(aver)) #bug fix!!! do not return cosine similarity, but the distance #return(as.numeric(aver)) 
    }else{
      return(FALSE)
    }
  }
}



testOccurence2<-function(corpus_pre,corpus_ADE){
  #Word Frequency Test (Occurrence)
  if(1){
    if (1){
      tdm_pre <- TermDocumentMatrix(corpus_pre,control = list(removePunctuation = FALSE,removeNumbers = TRUE,stopwords = TRUE))
      #tdm_post <- TermDocumentMatrix(corpus_post,control = list(removePunctuation = FALSE,removeNumbers = TRUE,stopwords = TRUE))
      tdm_ADE <- TermDocumentMatrix(corpus_ADE,control = list(removePunctuation = FALSE,removeNumbers = TRUE,stopwords = TRUE))
      wordCount_pre<-data.frame(ST=names(rowSums(as.matrix(tdm_pre))),Count = rowSums(as.matrix(tdm_pre)))
      row.names(wordCount_pre)<-NULL
      #wordCount_post<-data.frame(ST=names(rowSums(as.matrix(tdm_post))),Count = rowSums(as.matrix(tdm_post)))
      #row.names(wordCount_post)<-NULL
      wordCount_ADE<-data.frame(ST=names(rowSums(as.matrix(tdm_ADE))),Count = rowSums(as.matrix(tdm_ADE)))
      row.names(wordCount_ADE)<-NULL
      compare_pre<-wordCount_pre[wordCount_pre$ST %in% (intersect(wordCount_ADE$ST,wordCount_pre$ST)),]
      #print(compare_pre)
      #compare_post<-wordCount_post[wordCount_post$ST %in% (intersect(wordCount_ADE$ST,wordCount_post$ST)),]
      count_pre<-sum(wordCount_pre[wordCount_pre$ST %in% (intersect(wordCount_ADE$ST,wordCount_pre$ST)),]$Count)/sum(wordCount_pre$Count)
      
      #count_post<-sum(wordCount_post[wordCount_post$ST %in% (intersect(wordCount_ADE$ST,wordCount_post$ST)),]$Count)/length(corpus_post)
      
      #aver <- cosine(compare$FreqPRE,compare$FreqPOST)
      return(as.numeric(count_pre))
    }else{
      return(FALSE)
    }
  }
}

testOccurence3<-function(corpus_pre,corpus_post,corpus_ADE){
  #Word Frequency Test (Occurrence)
  if(1){
    if (length(corpus_pre)>5 && length(corpus_post)>5){
      tdm_pre <- TermDocumentMatrix(corpus_pre,control = list(removePunctuation = FALSE,removeNumbers = TRUE,stopwords = TRUE))
      tdm_post <- TermDocumentMatrix(corpus_post,control = list(removePunctuation = FALSE,removeNumbers = TRUE,stopwords = TRUE))
      tdm_ADE <- TermDocumentMatrix(corpus_ADE,control = list(removePunctuation = FALSE,removeNumbers = TRUE,stopwords = TRUE))
      wordCount_pre<-data.frame(ST=names(rowSums(as.matrix(tdm_pre))),Count = rowSums(as.matrix(tdm_pre)))
      row.names(wordCount_pre)<-NULL
      wordCount_post<-data.frame(ST=names(rowSums(as.matrix(tdm_post))),Count = rowSums(as.matrix(tdm_post)))
      row.names(wordCount_post)<-NULL
      wordCount_ADE<-data.frame(ST=names(rowSums(as.matrix(tdm_ADE))),Count = rowSums(as.matrix(tdm_ADE)))
      row.names(wordCount_ADE)<-NULL
      #!!!1006 debug "change: wordCount_post_ADE<-wordCount_post[!(wordCount_post$ST %in% wordCount_ADE$ST),] to wordCount_post_ADE<-wordCount_post[(wordCount_post$ST %in% wordCount_ADE$ST),],both lines"
      wordCount_post_ADE<-wordCount_post[(wordCount_post$ST %in% wordCount_ADE$ST),]
      #use pmatch
      #wordCount_post_ADE<-wordCount_post[!is.na(pmatch(wordCount_post$ST, wordCount_ADE$ST)),]
      wordCount_pre_ADE<-wordCount_pre[(wordCount_pre$ST %in% wordCount_ADE$ST),]
      #use pmatch
      #wordCount_pre_ADE<-wordCount_pre[!is.na(pmatch(wordCount_pre$ST, wordCount_ADE$ST)),]
      
      compare<-merge(x = wordCount_pre_ADE, y = wordCount_post_ADE, by = "ST", all = TRUE,suffixes = c("PRE","POST"))
      compare[is.na(compare)]<-0
      compare<-compare[!(compare$ST %in% c('blind','disease','disorder','history-of','drug','abuse')),]
      
      #normalize: each row sums to 1 so that in same scale
      compare$FreqPRE<-compare$CountPRE/sum(compare$CountPRE)
      compare$FreqPOST<-compare$CountPOST/sum(compare$CountPOST)
      
      change<-compare$FreqPOST-compare$FreqPRE  
      compare<-cbind(compare,change)
      compare<-compare[order(-compare$change),]
      #print(paste(compare[compare$change>0,]$ST,collapse = '; '))
      aver <- cosine(compare$FreqPRE,compare$FreqPOST)
      
      #disease names per corpus
      #aver <-sum(compare$CountPRE+compare$CountPOST)/(length(corpus_post)+length(corpus_pre))
      #aver <-sum(compare$CountPOST)/length(corpus_post)-sum(compare$CountPRE)/length(corpus_pre)
      #only care about pre-market
      #aver<- length(compare$ST)/length(wordCount_pre$ST)
      return(as.numeric(aver))
    }else{
      return(FALSE)
    }
  }
}

printVarName <- function(v1) {
  return(deparse(substitute(v1)))
}



#print time line
#http://stats.stackexchange.com/questions/20421/plotting-events-on-a-timeline-in-r
zucchini <- function(st, en, allowGap=FALSE, mingap=1)
{
  i <- order(st, en-st);
  st <- st[i];
  en <- en[i];
  last <- r <- 1
  if(allowGap == TRUE){
    while( sum( ok <- (st > (en[last] + mingap)) ) > 0 )
    {
      last <- which(ok)[1];
      r <- append(r, last);
    }
  }
  if( length(r) == length(st) )
    return( list(c = list(st[r], en[r]), n = 1 ));
  
  ne <- zucchini( st[-r], en[-r]);
  return(list( c = c(list(st[r], en[r]), ne$c), n = ne$n+1));
}

coliflore <- function(name_st_en_df, allowGap=FALSE, mingap = 1, currentDate=Sys.Date())
{
  nam<-name_st_en_df[,1]
  st<-name_st_en_df[,2]
  en<-name_st_en_df[,3]
  
  zu <- zucchini(st, en, mingap);
  plot.new();
  plot.window(xlim=c(min(st), max(en)), ylim = c(0, zu$n+1));
  box(); 
  axis(1,at = as.Date(unlist(list(st,en,currentDate))),labels=format(as.Date(unlist(list(st,en,currentDate))),"%Y"));
  #axis(2, at=c(1:length(nam)),labels=nam,las=2);
  
  title(main="Drug Marketing dates and BBW acquisition dates", 
        xlab="Time (Years)", ylab="Drug List",cex.lab=0.7)
  
  legend('topleft',legend=c('Drug Marketing Date','ROBUST period','BBW acquisition Date','BBW period','Current Date'),col=c('darkolivegreen3','slategray1','orange','tan1','skyblue2'),lty=c(NA,1,NA,1,NA),pch=c(0,NA,7,NA,17),lwd = c(NA,4,NA,1,NA),cex=.8)
  for(i in seq(1, 2*zu$n, 2))
  {
    x1 <- zu$c[[i]];
    x2 <- zu$c[[i+1]];
    for(j in 1:length(x1))
      points(x1[j], (i+1)/2+0.25, pch=0,col='darkolivegreen3',cex=0.2)
      #currentDayColor = 'orange'
      #if (x2[j]==Sys.Date()){currentDayColor='green'}
      #points(x2[j], (i+1)/2+0.25, pch=7,col=currentDayColor,cex=0.5)
      points(currentDate,(i+1)/2+0.25, pch=17,col='skyblue2',cex=0.5)
      rect( x1[j], (i+1)/2,  x2[j], (i+1)/2+0.5, col='slategray1', border=NA );##66CD00
      segments(x2[j],(i+1)/2+0.25, Sys.Date(),(i+1)/2+0.25,col='tan1',lty=1)
  }
}
