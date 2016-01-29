#drug_trial landscape
CURRENTSCRIPT = 'S1_ana_drug_trial_landscape.R'

in_content <-  c('../result/drug_trial_content.csv')
out_pdf <- c('../result/S1_drug_trial_landscape.pdf')

content <- read.csv(in_content,header = T,stringsAsFactors = F)
content.df <- data.frame(content, stringsAsFactors = F)
fhOut_pdf <- pdf(out_pdf)

#marketing date
content.df$dates <- as.Date(as.character(content.df$marketingDate), "%Y%m%d")
#pharm Class
a<-content.df$pharmClass
b<-strsplit(a,'\\|')
content.df$EPCclass<-sapply(b, function(x) paste(unlist(x)[grepl('\\[EPC\\]',unlist(x))],collapse = ';'))

candiTable<-content.df[tolower(content.df$drugName) %in% drugName_df_unique$tradeName,]


m <- ggplot(content.df, aes(x=dates))
m + geom_histogram(aes(fill = ..count..))+xlab('Date')+ylab('Drug Count')

#hist(content.df[tolower(content.df$drugName) %in% drugName_df_unique$tradeName,]$dates,breaks=20,xlab='Marketing Date',ylab='Count of Drugs')

boxplot(content.df$preTrialNum,content.df$postTrialNum,ylim=c(0,2000),names=c('pre-Market trials','post-Market trials'),main='Trial count distribution for each drug')

dev.off()


splitCell_CountOccurrence<-function(list, sep){
  count <- list()
  for(cell in list){
    component <- unlist(strsplit(cell,sep))
    if(length(component) != 0){
      for(current in component){
        if(is.null(count[[current]])){
          count[[current]] = 0
        }
        count[[current]] = count[[current]] + 1
      }
    }
  }
  trans <- data.frame(unlist(count))
  trans<-data.frame(rownames(trans),trans)
  colnames(trans)<-c('term','count')
  rownames(trans)<-NULL
  return(trans[order(trans[,2],decreasing=T),])
}

matchPatternInCell_CountOccurrence<-function(list, splitPattern, flag){
  count <- list()
  for(cell in list){
    component <- unlist(strsplit(cell,splitPattern))
    if(length(component) != 0){
      for(current in component){
        if (grepl(flag,current)){
          current = sub(flag,"",current)
          if(is.null(count[[current]])){
            count[[current]] = 0
          }
          count[[current]] = count[[current]] + 1
        }
      }
    }
  }
  trans <- data.frame(unlist(count))
  trans<-data.frame(rownames(trans),trans)
  colnames(trans)<-c('term','count')
  rownames(trans)<-NULL
  return(trans[order(trans[,2],decreasing=T),])
}

substance<-splitCell_CountOccurrence(content.df$substanceName,';')
colnames(substance)<-c('Main Substance','Drug Count')
row.names(substance)<-NULL
class <- matchPatternInCell_CountOccurrence(content.df$pharmClass,"\\|","\\[EPC\\]")
colnames(class)<-c('Drug Class','Drug Count')
row.names(class)<-NULL
substance[1:20,]
class[1:20,]
