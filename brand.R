library(vars)
x<-read.csv2("Classeur1.csv",sep="")
attach(x)


xin=x[,c(3,4,5,6,11)]
xin=ts(xin)



level1=function(a,b,c){
Meng=xin[c(a:b),]
qin=matrix(0,1,5)
for( i in 1:40) {
Meng1=VAR(Meng,p=c,type="const")
s1=predict(Meng1,n.ahead=7,ci=0.95)

clics=matrix(s1$fcst$Clics[,1],7,1)
impressions=matrix(s1$fcst$Impressions[,1],7,1)
CTR=matrix(s1$fcst$CTR[,1],7,1)
CPC=matrix(s1$fcst$CPC.moy.[,1],7,1)
TVR=matrix(s1$fcst$Taux.de.conversion.des.clics[,1],7,1)

Brand=cbind(clics,impressions,CTR,CPC,TVR)
colnames(Brand)=c("Clics","Impressions","CTR","CPC","TVR")

qin=rbind(qin,Brand)
ll=length(Meng[,1])
if(ll>500) {
   Meng=rbind(Meng[(ll-280):ll,],Brand)
           }else{
                Meng=rbind(Meng[c(-1:-7),],Brand)}
             }
qin=qin[-1,]
qin
}





fit=lm(Valeur.de.conv..totale ~ Clics+Impressions+CTR+CPC.moy.+Taux.de.conversion.des.clics,data=x)






para=as.matrix(summary(fit)$coefficients[,1])
cons=para[1,]
para=as.matrix(para[-1,])

level2=function(a,b,c){
  qin=level1(a,b,c)
prediction=qin%*%para
# sans CPC  prediction=qin[,-4]%*%para
prediction=prediction+cons
prediction
}

Cli=function(a,b,c){
  qin=level1(a,b,c)
Clics=round(matrix(qin[,1],length(qin[,1]),1),0)
colnames(Clics)="Clics"
Clics
}

Imp=function(a,b,c){
  qin=level1(a,b,c)
Impressions=round(matrix(qin[,2],length(qin[,1]),1),0)
colnames(Impressions)="Impressions"
Impressions
}

CT=function(a,b,c){
  qin=level1(a,b,c)
CTR=round(matrix(qin[,3],length(qin[,1]),1),3)
colnames(CTR)="CTR"
CTR
}


CP=function(a,b,c){
  qin=level1(a,b,c)
CPC.moy.=round(matrix(qin[,4],length(qin[,1]),1),3)
colnames(CPC.moy.)="CPC.moy."
CPC.moy.
}


Taux=function(a,b,c){
  qin=level1(a,b,c)
Taux.de.conversion.des.clics=round(matrix(qin[,5],length(qin[,1]),1),3)
colnames(Taux.de.conversion.des.clics)="Taux.de.conversion.des.clics"
Taux.de.conversion.des.clics
}


Val=function(a,b,c){
  qin=level1(a,b,c)
  prediction=level2(a,b,c)
Valeur.de.conv..totale=round(matrix(prediction[,1],length(qin[,1]),1),3)
colnames(Valeur.de.conv..totale)="Valeur.de.conv..totale"
Valeur.de.conv..totale
}


Day=as.Date(1:752,origin="2012-08-31")


fitt=x[,c(3,4,5,6,11,13)]
total=x[,c(3,4,5,6,11,13)]
total=round(ts(total),3)
total=data.frame(Day,total)



library(vars)
y<-read.csv2("Classeur2.csv",sep="")
attach(y)


yin=y[,c(3,4,5,6,11)]
yin=ts(yin)



ylevel1=function(a,b,c){
  
yMeng=yin[c(a:b),]
yqin=matrix(0,1,5)
for( i in 1:40) {
  yMeng1=VAR(yMeng,p=c,type="const")
  s2=predict(yMeng1,n.ahead=7,ci=0.95)
  
  yclics=matrix(s2$fcst$Clics[,1],7,1)
  yimpressions=matrix(s2$fcst$Impressions[,1],7,1)
  yCTR=matrix(s2$fcst$CTR[,1],7,1)
  yCPC=matrix(s2$fcst$CPC.moy.[,1],7,1)
  yTVR=matrix(s2$fcst$Taux.de.conversion.des.clics[,1],7,1)
  
  yBrand=cbind(yclics,yimpressions,yCTR,yCPC,yTVR)
  colnames(yBrand)=c("yClics","yImpressions","yCTR","yCPC","yTVR")
  
  yqin=rbind(yqin,yBrand)
  l=length(yMeng[,1])
  if(l>500) {
    yMeng=rbind(yMeng[(l-280):l,],yBrand)
  }else{
    yMeng=rbind(yMeng[c(-1:-7),],yBrand)}
}
yqin=yqin[-1,]   
yqin
}




yfit=lm(Valeur.de.conv..totale ~ Clics+Impressions+CTR+CPC.moy.+Taux.de.conversion.des.clics,data=y)





ypara=as.matrix(summary(yfit)$coefficients[,1])
ycons=ypara[1,]
ypara=as.matrix(ypara[-1,])



ylevel2=function(a,b,c){
  yqin=ylevel1(a,b,c)
yprediction=yqin%*%ypara
# sans CPC  prediction=qin[,-4]%*%para
yprediction=yprediction+ycons
}

yCli=function(a,b,c){
  yqin=ylevel1(a,b,c)
  yClics=round(matrix(yqin[,1],length(yqin[,1]),1),0)
  colnames(yClics)="yClics"
  yClics
}

yImp=function(a,b,c){
  yqin=ylevel1(a,b,c)
  yImpressions=round(matrix(yqin[,2],length(yqin[,1]),1),0)
  colnames(yImpressions)="yImpressions"
  yImpressions
}

yCT=function(a,b,c){
  yqin=ylevel1(a,b,c)
  yCTR=round(matrix(yqin[,3],length(yqin[,1]),1),3)
  colnames(yCTR)="yCTR"
  yCTR
}


yCP=function(a,b,c){
  yqin=ylevel1(a,b,c)
  yCPC.moy.=round(matrix(yqin[,4],length(yqin[,1]),1),3)
  colnames(yCPC.moy.)="yCPC.moy."
  yCPC.moy.
}


yTaux=function(a,b,c){
  yqin=ylevel1(a,b,c)
  yTaux.de.conversion.des.clics=round(matrix(yqin[,5],length(yqin[,1]),1),3)
  colnames(yTaux.de.conversion.des.clics)="yTaux.de.conversion.des.clics"
  yTaux.de.conversion.des.clics
}


yVal=function(a,b,c){
  yqin=ylevel1(a,b,c)
  yprediction=ylevel2(a,b,c)
  yValeur.de.conv..totale=round(matrix(yprediction[,1],length(yqin[,1]),1),3)
  colnames(yValeur.de.conv..totale)="yValeur.de.conv..totale"
  yValeur.de.conv..totale
}





yfitt=y[,c(3,4,5,6,11,13)]
ytotal=y[,c(3,4,5,6,11,13)]
ytotal=round(ts(ytotal),3)
colnames(ytotal)=c("yClics","yImpressions","yCTR","yCPC.moy.","yTaux.de.conversion.des.clics","yValeur.de.conv..totale")

ytotal=data.frame(Day,ytotal)








