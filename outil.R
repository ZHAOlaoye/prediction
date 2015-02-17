library(vars)
x<-read.csv2("Classeur1.csv",sep="\t")
attach(x)


xin=x[,c(3,4,5,6,11)]
xin=ts(xin)

VARselect(xin,lag.max=20,type="const")

xin1=VAR(xin,p=6,type="const")


y<-read.csv2("Classeur2.csv",sep="\t")
attach(y)


yin=y[,c(3,4,5,6,11)]
yin=ts(yin)

VARselect(yin,lag.max=20,type="const")

xin2=VAR(yin,p=12,type="const")
