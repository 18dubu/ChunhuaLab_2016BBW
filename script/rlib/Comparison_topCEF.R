rm(list=ls())
source("./util.R")
target = 'Comparison_pubmed_ex'
mkdir(paste("../../result/",target,"/topCEF/all/",sep=""))

target1 = 'pubmed'
target2 = 'exclusion'

count_occur<-function(features,restrict){
  ranking <- list()
  for (cde in features){
    if(!(cde %in% restrict)){
      next
    }
    if(is.null(ranking[[cde]])){
      ranking[[cde]] = 0
    }
    ranking[[cde]] = ranking[[cde]] + 1
  }
  trans <- data.frame(unlist(ranking))
  trans<-data.frame(attributes(trans)$row.names,trans$unlist.ranking.)
  return (trans[order(trans[,2],decreasing=T),])#
}


mapST<-function(listCDE,mappingTable){
  mapping <- read.table(mappingTable,sep='\t',header=T,row.names=NULL)
  mapping.cde <- mapping[,1]
  mapping.st <- mapping[,2]
  listST = array()
  j=1
  index = array()
  for (i in listCDE){
    index[j] = which(i == mapping.cde)
    j =j+1
  }
  listST <- mapping.st[index] 
  
  if(length(listST) == length(listCDE)){
    return(listST)
  }
  else{
    print ("input/output length differ\n")
    return(0)
  }
}


#table: Most frequently used CEF (In/Ex/Joint) for all diseases

all.ex<-read.table(paste("../../result/",target2,"/allData_table",sep=''),header=T,sep='\t', row.names=NULL)
all.in<-read.table(paste("../../result/",target1,"/allData_table",sep=''),sep='\t',header=T)
ex.all<-readLines(paste("../../result/",target2,"/allCDE",sep=''))
in.all<-readLines(paste("../../result/",target1,"/allCDE",sep=''))

in.DiseaseName<-all.in[,2]
ex.DiseaseName<-all.ex[,2]
share.disease = intersect(in.DiseaseName,ex.DiseaseName)
all.ex <- all.ex[all.ex[,2] %in% share.disease,]
all.in <- all.in[all.in[,2] %in% share.disease,]

in.CDE<-all.in[,3]
in.Freq<-all.in[,4]
in.st <-all.in[,5]
ex.CDE<-all.ex[,3]
ex.Freq<-all.ex[,4]
ex.st<-all.ex[,5]



inter <- intersect(ex.all,in.all)
ex.unique<-setdiff(ex.all,inter)
in.unique<-setdiff(in.all,inter)


features <- in.CDE
restrict <- in.unique


mappingTable <- paste('../../result/cde_st_table_',target1,'_',target2,sep='') #in

out.rank<- paste("../../result/",target,"/topCEF/all/in_unique",sep='') #out
ranking.df<-count_occur(in.CDE,in.unique)
listCDE<-ranking.df[,1]
mapped.st<- mapST(listCDE,mappingTable)
ranking.df <- data.frame(ranking.df,mapped.st,stringsAsFactors=F)
write.table(ranking.df,out.rank,quote=F,sep='\t',col.names=F,row.names=F)

out.rank<- paste("../../result/",target,"/topCEF/all/ex_unique",sep='') #out
ranking.df<-count_occur(ex.CDE,ex.unique)
listCDE<-ranking.df[,1]
mapped.st<- mapST(listCDE,mappingTable)
ranking.df <- data.frame(ranking.df,mapped.st,stringsAsFactors=F)
write.table(ranking.df,out.rank,quote=F,sep='\t',col.names=F,row.names=F)

out.rank<- paste("../../result/",target,"/topCEF/all/intersection_sig",sep='') #out
ranking.df1<-count_occur(ex.CDE,inter)
ranking.df2<-count_occur(in.CDE,inter)
union.all<-union(ranking.df1[,1],ranking.df2[,1])
ranking.df<-list()
for (current in unique(union.all)){
  if(current %in% ranking.df1[,1] && current %in% ranking.df2[,1]){
    index1<-which(current == ranking.df1[,1])
    index2<-which(current == ranking.df2[,1])
    ranking.df[[current]]<-as.numeric(ranking.df1[index1,2])-as.numeric(ranking.df2[index2,2])
  }
}
ranking.df<-data.frame(unlist(ranking.df))
ranking.df<-data.frame(row.names(ranking.df),ranking.df[,1],stringsAsFactors=F)
listCDE<-ranking.df[,1]
mapped.st<- mapST(listCDE,mappingTable)
ranking.df <- data.frame(ranking.df,mapped.st,stringsAsFactors=F)
write.table(ranking.df[order(as.numeric(ranking.df[,2]),decreasing=T),],out.rank,quote=F,sep='\t',col.names=F,row.names=F)


###########################
#table: Most frequently used CEF (In/Ex/Joint) for each disease 
mkdir(paste("../../result/",target,"/topCEF/each/",sep=""))

out.top5.in <- paste("../../result/",target,"/topCEF/each/in_top5",sep='')
out.top5.ex <- paste("../../result/",target,"/topCEF/each/ex_top5",sep='')
out.top5.sig.in <- paste("../../result/",target,"/topCEF/each/intersection_sig.in",sep='')
out.top5.sig.ex <- paste("../../result/",target,"/topCEF/each/intersection_sig.ex",sep='')
#output ranking list: top 5 cde for each disease
if(file.exists(out.top5.in)){
  file.remove(out.top5.in)
  print(paste("original file removed",out.top5.in))
}
if(file.exists(out.top5.ex)){
  file.remove(out.top5.ex)
  print(paste("original file removed",out.top5.ex))
}
if(file.exists(out.top5.sig.in)){
  file.remove(out.top5.sig.in)
  print(paste("original file removed",out.top5.sig.in))
}
if(file.exists(out.top5.sig.ex)){
  file.remove(out.top5.sig.ex)
  print(paste("original file removed",out.top5.sig.ex))
}
for (disease in share.disease){
  num.per.disease <- 20
  candi.in = all.in[in.DiseaseName==disease,]
  candi.ex = all.ex[ex.DiseaseName==disease,]
  inter <- intersect(candi.in$CDEs,candi.ex$CDEs)
  if (length(inter)==0){next}
  if (length(inter)<5){num.per.disease=length(inter)}
  ex.unique<-setdiff(candi.ex$CDEs,inter)
  in.unique<-setdiff(candi.in$CDEs,inter)
  filtered.in<-candi.in[candi.in$CDEs %in% in.unique,]
  filtered.ex<-candi.ex[candi.ex$CDEs %in% ex.unique,]
  common.in<-candi.in[candi.in$CDEs %in% inter,]
  common.ex<-candi.ex[candi.ex$CDEs %in% inter,]
  common.out <- common.ex
  ########
  for (current  in inter){
    a <- common.ex$Frequency..0.1.normalized.[common.ex$CDEs==current] - common.in$Frequency..0.1.normalized.[common.in$CDEs==current]
    common.out$Frequency..0.1.normalized.[common.out$CDEs==current]<-a
  }
#  if(is.na(common.out[,1])){
#    next
#  }
  write.table(common.out[order(common.out[,4],decreasing=T)[1:num.per.disease],],out.top5.sig.ex,quote = F,row.names=F,col.names=F,append = T,sep='\t')
  write.table(common.out[order(common.out[,4],decreasing=F)[1:num.per.disease],],out.top5.sig.in,quote = F,row.names=F,col.names=F,append = T,sep='\t')
  #########
  for (i in 1:num.per.disease){
    if(is.na(filtered.in[i,1])){
      next
    }
    write.table(filtered.in[order(filtered.in[,4],decreasing=T),][i,],out.top5.in,quote = F,row.names=F,col.names=F,append = T,sep='\t')
  }

  for (i in 1:num.per.disease){
    if(is.na(filtered.ex[i,1])){
      next
    }
    write.table(filtered.ex[order(filtered.ex[,4],decreasing=T),][i,],out.top5.ex,quote = F,row.names=F,col.names=F,append = T,sep='\t')
  }
}


