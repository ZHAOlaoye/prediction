PB<-read.csv2("PB.CSV",sep="\t")


ro=table(PB[,3])


test<-as.character(PB[,1])
test1=matrix(0,length(test),3)
for (i in 1:length(test)) (
  for (j in 1:3)  (
    test1[i,j]=unlist(strsplit(test[i],"/"))[j]                       
  )        
)



ca=paste(test1[,3],test1[,2],test1[,1],sep="/")
ca=as.matrix(ca,length(ca),1)
ca=cbind(ca,PB[,-1])

Ca=aggregate(PB[,c(8:11,16)],by=list(Date=PB[,1],Campagne=PB[,3]),FUN=sum,na.rm=TRUE)



