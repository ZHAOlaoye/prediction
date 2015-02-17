library(shiny)
library(datasets)



shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    s=as.Date("2012-09-01")
    f=as.Date("2014-09-22")
    length=as.numeric(f-s)+1
    
    
    ff=length-as.numeric(f-input$dates[2])
    ss=as.numeric(input$dates[1]-s)
    z=input$t
    switch(input$dataset, 
           "Clics"=Cli(ss,ff,z),
           "Impressions"=Imp(ss,ff,z),
           "CTR"=CT(ss,ff,z),
           "CPC.moy."=CP(ss,ff,z),
           "Taux.de.conversion.des.clics"=Taux(ss,ff,z),
           "Valeur.de.conv..totale"=Val(ss,ff,z)
    )
  })
  
  DatasetInput <- reactive({
    s=as.Date("2012-09-01")
    f=as.Date("2014-09-22")
    length=as.numeric(f-s)+1
    
    
    ff=length-as.numeric(f-input$dates[2])
    ss=as.numeric(input$dates[1]-s)
    z=input$t
    
    switch(input$dataset,
           "Clics"=yCli(ss,ff,z),
           "Impressions"=yImp(ss,ff,z),
           "CTR"=yCT(ss,ff,z),
           "CPC.moy."=yCP(ss,ff,z),
           "Taux.de.conversion.des.clics"=yTaux(ss,ff,z),
           "Valeur.de.conv..totale"=yVal(ss,ff,z)
    )
  })
  
  
  
  
  output$summary <- renderPrint({
    xx=head(datasetInput(), n = input$obs)
    yy=head(DatasetInput(), n = input$obs)
    
    net=xx
    ynet=yy
    
    if(input$dataset=="CPC.moy."){
      net=net*(1+input$modcpc/100)      
      ynet=ynet*(1+input$modcpc/100)
    }
    if(input$dataset=="CTR"){
      net=net*(1+input$modctr/100)      
      ynet=ynet*(1+input$modctr/100)
    }
    if(input$dataset=="Impressions"){
      net=net*(1+input$modimp/100)      
      ynet=ynet*(1+input$modimp/100)
    }
    if(input$dataset=="Taux.de.conversion.des.clics"){
      net=net*(1+input$modtaux/100)      
      ynet=ynet*(1+input$modtaux/100)
    }
    
    
    
    
    if(input$Type=="Generic") return(summary(ynet))
    summary(net)
  })
  
  
  output$summary1 <- renderPrint({
    
    s=as.Date("2012-09-01")
    f=as.Date("2014-09-22")
    length=as.numeric(f-s)+1
    
    
    ff=length-as.numeric(f-input$dates[2])
    ss=as.numeric(input$dates[1]-s)
    
    
    
    xxx=fitt[c(ss:ff),which(colnames(fitt)==colnames(datasetInput()))]
    yyy=yfitt[c(ss:ff),which(colnames(yfitt)==colnames(datasetInput()))]
    xx=arima(xxx,order=c(input$p,0,input$q))
    yy=arima(yyy,order=c(input$p,0,input$q))
    xx=round(as.matrix(predict(xx,n.ahead=input$obs)$pred),3)
    yy=round(as.matrix(predict(yy,n.ahead=input$obs)$pred),3)
    colnames(xx)=colnames(datasetInput())
    colnames(yy)=colnames(datasetInput())
    
    
    net=xx
    ynet=yy
    
    if(input$dataset=="CPC.moy."){
      net=net*(1+input$modcpc/100)      
      ynet=ynet*(1+input$modcpc/100)
    }
    if(input$dataset=="CTR"){
      net=net*(1+input$modctr/100)      
      ynet=ynet*(1+input$modctr/100)
    }
    if(input$dataset=="Impressions"){
      net=net*(1+input$modimp/100)      
      ynet=ynet*(1+input$modimp/100)
    }
    if(input$dataset=="Taux.de.conversion.des.clics"){
      net=net*(1+input$modtaux/100)      
      ynet=ynet*(1+input$modtaux/100)
    }
    
    
    
    
    if(input$Type=="Generic") return(summary(ynet))
    summary(net)
  })
  
  
  
  output$view <- renderDataTable({
    date=as.Date(1:input$obs,origin="2014-09-22")
    
    xx=head(datasetInput(),n=input$obs)
    yy=head(DatasetInput(),n=input$obs)
    
    
    net=xx
    ynet=yy
    
    if(input$dataset=="CPC.moy."){
      net=net*(1+input$modcpc/100)      
      ynet=ynet*(1+input$modcpc/100)
    }
    if(input$dataset=="CTR"){
      net=net*(1+input$modctr/100)      
      ynet=ynet*(1+input$modctr/100)
    }
    if(input$dataset=="Impressions"){
      net=net*(1+input$modimp/100)      
      ynet=ynet*(1+input$modimp/100)
    }
    if(input$dataset=="Taux.de.conversion.des.clics"){
      net=net*(1+input$modtaux/100)      
      ynet=ynet*(1+input$modtaux/100)
    }
    
    
    if(input$Type=="Generic") return(data.frame(date,ynet))                          
    return(data.frame(date,net))
    options=list(pageLenth=20)
    
  })
  
  
  
  output$view1 <- renderDataTable({
    date=as.Date(1:input$obs,origin="2014-09-22")
    
    
    s=as.Date("2012-09-01")
    f=as.Date("2014-09-22")
    length=as.numeric(f-s)+1
    
    
    ff=length-as.numeric(f-input$dates[2])
    ss=as.numeric(input$dates[1]-s)
    
    xxx=fitt[c(ss:ff),which(colnames(fitt)==colnames(datasetInput()))]
    yyy=yfitt[c(ss:ff),which(colnames(yfitt)==colnames(datasetInput()))]
    
    xx=arima(xxx,order=c(input$p,0,input$q))
    yy=arima(yyy,order=c(input$p,0,input$q))
    xx=round(as.matrix(predict(xx,n.ahead=input$obs)$pred),3)
    yy=round(as.matrix(predict(yy,n.ahead=input$obs)$pred),3)
    colnames(xx)=colnames(datasetInput())
    colnames(yy)=colnames(datasetInput())
    
    
    net=xx
    ynet=yy
    
    if(input$dataset=="CPC.moy."){
      net=net*(1+input$modcpc/100)      
      ynet=ynet*(1+input$modcpc/100)
    }
    if(input$dataset=="CTR"){
      net=net*(1+input$modctr/100)      
      ynet=ynet*(1+input$modctr/100)
    }
    if(input$dataset=="Impressions"){
      net=net*(1+input$modimp/100)      
      ynet=ynet*(1+input$modimp/100)
    }
    if(input$dataset=="Taux.de.conversion.des.clics"){
      net=net*(1+input$modtaux/100)      
      ynet=ynet*(1+input$modtaux/100)
    }
    
    
    
    
    if(input$Type=="Generic") return(data.frame(date,ynet))                          
    return(data.frame(date,net))
    options=list(pageLenth=10)
    
  })
  
  
  
  
  
  
  
  
  output$P <- renderUI({
    if (input$model != "arma") {return()}else{
      sliderInput("p", "Paramètres de ARMA(p,q),  p:", min=0,max=6,value=3,step=1)
    }
  })
  output$Q <- renderUI({
    if (input$model != "arma") {return()}else{
      sliderInput("q", "q:", min=0,max=6,value=3,step=1)
    }
  })
  output$T <- renderUI({
    if (input$model != "var") {return()}else{
      sliderInput("t", "Paramètre de var(n), n:", min=1,max=20,value=3,step=1)
    }
  })
  output$Per <- renderUI({
    if (input$perdu) {return()}else{
      numericInput("Perdu",label=strong("Taux de perdue:"),
                   value=0.00,min=-1,max=1)    }
  })
  
  
  output$plot <- renderPlot({
    date=as.Date(1:input$obs,origin="2014-09-22")
    xx=head(datasetInput(), n = input$obs)
    yy=head(DatasetInput(), n = input$obs)
    
    xxx=xx
    yyy=yy
    
    if(input$dataset=="CPC.moy."){
      xxx=xxx*(1+input$modcpc/100)      
      yyy=yyy*(1+input$modcpc/100)
    }
    if(input$dataset=="CTR"){
      xxx=xxx*(1+input$modctr/100)      
      yyy=yyy*(1+input$modctr/100)
    }
    if(input$dataset=="Impressions"){
      xxx=xxx*(1+input$modimp/100)      
      yyy=yyy*(1+input$modimp/100)
    }
    if(input$dataset=="Taux.de.conversion.des.clics"){
      xxx=xxx*(1+input$modtaux/100)      
      yyy=yyy*(1+input$modtaux/100)
    }
    
    
    if(input$Type=="Generic") {
      plot(data.frame(date,yy),type="l",col="red")
      lines(data.frame(date,yyy),type="l",col="green")
    }else{
      plot(data.frame(date,xx),type="l",col="red",ylim=c(min(xx[,1])*0.1,max(xx[,1]*2)))
      lines(data.frame(date,xxx),type="l",col="green")
    } 
  }) 
  
  output$plot1 <- renderPlot({
    date=as.Date(1:input$obs,origin="2014-09-22")
    
    
    s=as.Date("2012-09-01")
    f=as.Date("2014-09-22")
    length=as.numeric(f-s)+1
    
    
    ff=length-as.numeric(f-input$dates[2])
    ss=as.numeric(input$dates[1]-s)
    
    xxx=fitt[c(ss:ff),which(colnames(fitt)==colnames(datasetInput()))]
    yyy=yfitt[c(ss:ff),which(colnames(yfitt)==colnames(datasetInput()))]
    xx=arima(xxx,order=c(input$p,0,input$q))
    yy=arima(yyy,order=c(input$p,0,input$q))
    xx=as.matrix(predict(xx,n.ahead=input$obs)$pred)
    yy=as.matrix(predict(yy,n.ahead=input$obs)$pred)
    colnames(xx)=colnames(datasetInput())
    colnames(yy)=colnames(datasetInput())
    
    
    net=xx
    ynet=yy
    
    if(input$dataset=="CPC.moy."){
      net=net*(1+input$modcpc/100)      
      ynet=ynet*(1+input$modcpc/100)
    }
    if(input$dataset=="CTR"){
      net=net*(1+input$modctr/100)      
      ynet=ynet*(1+input$modctr/100)
    }
    if(input$dataset=="Impressions"){
      net=net*(1+input$modimp/100)      
      ynet=ynet*(1+input$modimp/100)
    }
    if(input$dataset=="Taux.de.conversion.des.clics"){
      net=net*(1+input$modtaux/100)      
      ynet=ynet*(1+input$modtaux/100)
    }
    
    
    
    if(input$Type=="Generic") {  
      plot(data.frame(date,yy),type="l",col="red",ylim=c(min(yy[,1])*0.1,max(yy[,1]*2)))
      lines(data.frame(date,ynet),type="l",col="green")
      
    }else{
      plot(data.frame(date,xx),type="l",col="red",ylim=c(min(xx[,1])*0.1,max(xx[,1]*2)))
      lines(data.frame(date,net),type="l",col="green")
      
    }
    
  }) 
  
  
  
  
  output$look <- renderDataTable(
    if(input$Type=="Generic") {return(ytotal)}else{
      return(total)},
    options=list(pageLenth=10)
    
  )
  
  
  output$final <- renderPlot({
    reel=total[,which(colnames(total)==colnames(datasetInput()))]
    reel=matrix(reel,length(reel),1)
    xx=head(datasetInput(), n = input$obs)
    
    
    yreel=ytotal[,which(colnames(ytotal)==colnames(DatasetInput()))]
    yreel=matrix(yreel,length(yreel),1)
    yy=head(DatasetInput(), n = input$obs)
    
    
    
    net=xx
    ynet=yy
    
    if(input$dataset=="CPC.moy."){
      net=net*(1+input$modcpc/100)      
      ynet=ynet*(1+input$modcpc/100)
    }
    if(input$dataset=="CTR"){
      net=net*(1+input$modctr/100)      
      ynet=ynet*(1+input$modctr/100)
    }
    if(input$dataset=="Impressions"){
      net=net*(1+input$modimp/100)      
      ynet=ynet*(1+input$modimp/100)
    }
    if(input$dataset=="Taux.de.conversion.des.clics"){
      net=net*(1+input$modtaux/100)      
      ynet=ynet*(1+input$modtaux/100)
    }
    
    
    
    if(input$Type=="Generic") 
    {plot(ts(rbind(yreel,ynet)),col="red")
     lines(ts(rbind(yreel)),col="blue")}else{
       
       plot(ts(rbind(reel,net)),col="red")
       lines(ts(rbind(reel)),col="blue")}
    
    
    
  }) 
  
  
  
  output$final1 <- renderPlot({
    
    date=as.Date(1:input$obs,origin="2014-09-22")
    
    
    s=as.Date("2012-09-01")
    f=as.Date("2014-09-22")
    length=as.numeric(f-s)+1
    
    
    ff=length-as.numeric(f-input$dates[2])
    ss=as.numeric(input$dates[1]-s)
    
    xxx=fitt[c(ss:ff),which(colnames(fitt)==colnames(datasetInput()))]
    yyy=yfitt[c(ss:ff),which(colnames(yfitt)==colnames(datasetInput()))]
    xx=arima(xxx,order=c(input$p,0,input$q))
    yy=arima(yyy,order=c(input$p,0,input$q))
    xx=as.matrix(predict(xx,n.ahead=input$obs)$pred)
    yy=as.matrix(predict(yy,n.ahead=input$obs)$pred)
    colnames(xx)=colnames(datasetInput())
    colnames(yy)=colnames(datasetInput())
    
    
    
    
    reel=total[,which(colnames(total)==colnames(datasetInput()))]
    reel=matrix(reel,length(reel),1)
    yreel=ytotal[,which(colnames(ytotal)==colnames(DatasetInput()))]
    yreel=matrix(yreel,length(yreel),1)
    
    
    
    net=xx
    ynet=yy
    
    if(input$dataset=="CPC.moy."){
      net=net*(1+input$modcpc/100)      
      ynet=ynet*(1+input$modcpc/100)
    }
    if(input$dataset=="CTR"){
      net=net*(1+input$modctr/100)      
      ynet=ynet*(1+input$modctr/100)
    }
    if(input$dataset=="Impressions"){
      net=net*(1+input$modimp/100)      
      ynet=ynet*(1+input$modimp/100)
    }
    if(input$dataset=="Taux.de.conversion.des.clics"){
      net=net*(1+input$modtaux/100)      
      ynet=ynet*(1+input$modtaux/100)
    }
    
    
    
    if(input$Type=="Generic") 
    {plot(ts(rbind(yreel,ynet)),col="red")
     lines(ts(rbind(yreel)),col="blue")}else{
       
       plot(ts(rbind(reel,net)),col="red")
       lines(ts(rbind(reel)),col="blue")}
    
    
    
    
  }) 
  
  
  
  output$budget <- renderPlot({
    date=as.Date(1:input$obs,origin="2014-09-22")
    
    s=as.Date("2012-09-01")
    f=as.Date("2014-09-22")
    length=as.numeric(f-s)+1
    
    
    ff=length-as.numeric(f-input$dates[2])
    ss=as.numeric(input$dates[1]-s)
    z=input$t
    
    
    
    
    Budget=head(CP(ss,ff,z)*Cli(ss,ff,z),n=input$obs)
    colnames(Budget)="Budget"
    yBudget=head(yCP(ss,ff,z)*yCli(ss,ff,z),n=input$obs)
    colnames(yBudget)="Budget"
    
    xBudget=Budget*(1+input$modcpc/100)
    yyBudget=yBudget*(1+input$modcpc/100)
    
    
    
    
    if(input$Type=="Generic") 
    {plot(data.frame(date,yyBudget),type="l",col="red",ylim=c(min(yBudget[,1])*0.1,max(yBudget[,1]*2)))
     lines(data.frame(date,yBudget),type="l",col="blue")}else{
       
       plot(data.frame(date,xBudget),type="l",col="red",ylim=c(min(Budget[,1])*0.1,max(Budget[,1]*2)))
       lines(data.frame(date,Budget),type="l",col="blue")}
    
    
  })
  
  
  
  
  
  
  output$budget1 <- renderPlot({
    date=as.Date(1:input$obs,origin="2014-09-22")
    
    
    s=as.Date("2012-09-01")
    f=as.Date("2014-09-22")
    length=as.numeric(f-s)+1
    
    
    ff=length-as.numeric(f-input$dates[2])
    ss=as.numeric(input$dates[1]-s)
    
    
    
    xxx=fitt[c(ss:ff),which(colnames(fitt)=="Clics")]
    yyy=yfitt[c(ss:ff),which(colnames(yfitt)=="Clics")]
    
    xxx1=fitt[c(ss:ff),which(colnames(fitt)=="CPC.moy.")]
    yyy1=yfitt[c(ss:ff),which(colnames(yfitt)=="CPC.moy.")]
    
    
    xx=arima(xxx,order=c(input$p,0,input$q))
    yy=arima(yyy,order=c(input$p,0,input$q))
    
    xx1=arima(xxx1,order=c(input$p,0,input$q))
    yy1=arima(yyy1,order=c(input$p,0,input$q))
    
    
    
    xx=as.matrix(predict(xx,n.ahead=input$obs)$pred)
    yy=as.matrix(predict(yy,n.ahead=input$obs)$pred)
    colnames(xx)="Clics"
    colnames(yy)="Clics"
    
    
    xx1=as.matrix(predict(xx1,n.ahead=input$obs)$pred)
    yy1=as.matrix(predict(yy1,n.ahead=input$obs)$pred)
    colnames(xx)="CPC.moy."
    colnames(yy)="CPC.moy."
    
    
    Budget=xx*xx1
    colnames(Budget)="Budget"
    yBudget=yy*yy1
    colnames(yBudget)="Budget"
    
    
    xBudget=Budget*(1+input$modcpc/100)
    yyBudget=yBudget*(1+input$modcpc/100)
    
    
    
    if(input$Type=="Generic") 
    {plot(data.frame(date,yyBudget),type="l",col="red",ylim=c(min(yBudget[,1])*0.1,max(yBudget[,1]*2)))
     lines(data.frame(date,yBudget),type="l",col="blue")}else{
       
       plot(data.frame(date,xBudget),type="l",col="red",ylim=c(min(Budget[,1])*0.1,max(Budget[,1]*2)))
       lines(data.frame(date,Budget),type="l",col="blue")}
    
    
    
    
    
  })
  
  
  
  
  output$Modelplot <- renderPlot({
    
    s=as.Date("2012-09-01")
    f=as.Date("2014-09-22")
    length=as.numeric(f-s)+1
    
    
    ff=length-as.numeric(f-input$dates[2])
    ss=as.numeric(input$dates[1]-s)
    
    
    brand=fitt[c(ss:ff),which(colnames(fitt)==colnames(datasetInput()))]
    generic=yfitt[c(ss:ff),which(colnames(yfitt)==colnames(datasetInput()))]
    if(input$Type=="Generic") return(acf(generic,lag=30))
    acf(brand,lag=30) 
    
  }) 
  
  output$Modelplot1 <- renderPlot({
    
    s=as.Date("2012-09-01")
    f=as.Date("2014-09-22")
    length=as.numeric(f-s)+1
    
    
    ff=length-as.numeric(f-input$dates[2])
    ss=as.numeric(input$dates[1]-s)
    
    
    brand=fitt[c(ss:ff),which(colnames(fitt)==colnames(datasetInput()))]
    generic=yfitt[c(ss:ff),which(colnames(yfitt)==colnames(datasetInput()))]
    if(input$Type=="Generic") return(pacf(generic,lag=30))
    pacf(brand,lag=30)
    
  }) 
  
  
  output$sum3 <- renderPrint({
    
    
    s=as.Date("2012-09-01")
    f=as.Date("2014-09-22")
    length=as.numeric(f-s)+1
    
    
    ff=length-as.numeric(f-input$dates[2])
    ss=as.numeric(input$dates[1]-s)
    
    
    
    if(input$Type=="Generic") return(summary(VAR(yin[c(ss:ff),],p=input$t,type="const")))
    summary(VAR(xin[c(ss:ff),],p=input$t,type="const")) 
    
  })
  
  output$sum1 <- renderPrint({
    if(input$Type=="Generic") return(VARselect(yin,lag.max=20,type="const"))
    VARselect(xin,lag.max=20,type="const")
    
  })
  output$sum2 <- renderPrint({
    
    s=as.Date("2012-09-01")
    f=as.Date("2014-09-22")
    length=as.numeric(f-s)+1
    
    
    ff=length-as.numeric(f-input$dates[2])
    ss=as.numeric(input$dates[1]-s)
    
    
    
    Brand=fitt[c(ss:ff),which(colnames(fitt)==colnames(datasetInput()))]
    Generic=yfitt[c(ss:ff),which(colnames(yfitt)==colnames(datasetInput()))]
    xx=arima(Brand,order=c(input$p,0,input$q))
    yy=arima(Generic,order=c(input$p,0,input$q))
    
    if(input$Type=="Generic") return(yy)
    xx
    
  })
  
  output$sum <- renderPrint({
    if(input$Type=="Generic") return(summary(yfit))
    summary(fit)
    
  })
  output$sum4 <- renderPlot({
    
    s=as.Date("2012-09-01")
    f=as.Date("2014-09-22")
    length=as.numeric(f-s)+1
    
    
    ff=length-as.numeric(f-input$dates[2])
    ss=as.numeric(input$dates[1]-s)
    
    
    xxx=fitt[c(ss:ff),which(colnames(fitt)==colnames(datasetInput()))]
    yyy=yfitt[c(ss:ff),which(colnames(yfitt)==colnames(datasetInput()))]
    xx=arima(xxx,order=c(input$p,0,input$q))
    yy=arima(yyy,order=c(input$p,0,input$q))
    
    
    if(input$Type=="Generic") return(tsdiag(yy))
    tsdiag(xx)
    
  })
  
  output$sum5 <- renderPrint({
    if(input$Type=="Generic") return(stepAIC(yfit,direction="backward"))
    stepAIC(fit,direction="backward")
    
  })
  
  output$cc <- renderPlot({
    d=which(PB[,3]==input$Cam)
    Date=as.Date(ca[d,1])
    
    b=as.matrix(PB[d,8])
    colnames(b)=colnames(PB)[8]
    c=data.frame(Date,b)
    plot(c,type="l",col="blue")
    if(input$perdu) {
      plot(c,type="l",col="blue",ylim=c(min(b)*0.1,max(b)*2))
      b=as.matrix(PB[d,9]*2*PB[d,10])
      lines(data.frame(Date,b),col="red")
    }
    
    if(input$Perdu != 0){
      plot(c,type="l",col="blue",ylim=c(min(b)*0.1,max(b)*2))
      b=as.matrix(PB[d,9]*(1+input$Perdu/100)*PB[d,10])
      lines(data.frame(Date,b),col="black")
      
    }
    
    
  })
  
  
  output$cc1 <- renderPlot({
    d=which(PB[,3]==input$Cam)
    Date=as.Date(ca[d,1])
    
    b=as.matrix(PB[d,9])
    colnames(b)=colnames(PB)[9]
    c=data.frame(Date,b)
    plot(c,type="l",col="blue")
    if(input$perdu) {
      plot(c,type="l",col="blue",ylim=c(min(b)*0.1,max(b)*2))
      b=b*2
      lines(data.frame(Date,b),col="red")
    }
    
    if(input$Perdu != 0){
      plot(c,type="l",col="blue",ylim=c(min(b)*0.1,max(b)*2))
      b=b*(1+input$Perdu/100)
      lines(data.frame(Date,b),col="black")
      
    }
    
    
    
    
  })
  
  output$cc2 <- renderPlot({
    d=which(PB[,3]==input$Cam)
    Date=as.Date(ca[d,1])
    
    b=as.matrix(PB[d,10])
    colnames(b)=colnames(PB)[10]
    c=data.frame(Date,b)
    plot(c,type="l",col="blue")
    
  })
  
  
  output$cc3 <- renderPlot({
    d=which(PB[,3]==input$Cam)
    Date=as.Date(ca[d,1])
    
    b=as.matrix(PB[d,11])
    colnames(b)=colnames(PB)[11]
    c=data.frame(Date,b)
    plot(c,type="l",col="blue")
    if(input$perdu) {
      plot(c,type="l",col="blue",ylim=c(min(b)*0.1,max(b)*2))
      b=b*2
      lines(data.frame(Date,b),col="red")
    }
    
    if(input$Perdu != 0){
      plot(c,type="l",col="blue",ylim=c(min(b)*0.1,max(b)*2))
      b=b*(1+input$Perdu/100)
      lines(data.frame(Date,b),col="black")
      
    }
    
    
  })
  
  
  
  
  output$mois <- renderPlot({
    
    date=as.Date(1:input$obs,origin="2014-09-22")
    xx=head(datasetInput(), n = input$obs)
    yy=head(DatasetInput(), n = input$obs)
    
    xxx=cbind(date,xx)
    xxx[,2]=as.numeric(xxx[,2])
    yyy=cbind(date,yy)
    yyy[,2]=as.numeric(yyy[,2])
    
    
    
    test<-format(date)
    test1=matrix(0,length(test),3)
    for (i in 1:length(test)) (
      for (j in 1:3)  (
        test1[i,j]=unlist(strsplit(test[i],"-"))[j]                       
      )        
    )
    
    
    
    da=paste(test1[,1],test1[,2],sep="/")
    da=as.matrix(da,length(da),1)
    da=cbind(da,xxx[,-1])
    da[,2]=as.numeric(da[,2])
    yda=cbind(da,yyy[,-1])
    yda[,2]=as.numeric(yda[,2])
    
    if(input$dataset=="CPC.moy."){
      
      da=aggregate(as.numeric(da[,2]),by=list(Date=da[,1]),FUN=mean,na.rm=TRUE)      
      yda=aggregate(as.numeric(yda[,2]),by=list(Date=yda[,1]),FUN=mean,na.rm=TRUE)      
      
    }
    if(input$dataset=="CTR"){
      da=aggregate(as.numeric(da[,2]),by=list(Date=da[,1]),FUN=mean,na.rm=TRUE)     
      yda=aggregate(as.numeric(yda[,2]),by=list(Date=yda[,1]),FUN=mean,na.rm=TRUE)      
      
    }
    if(input$dataset=="Impressions"){
      da=aggregate(as.numeric(da[,2]),by=list(Date=da[,1]),FUN=sum,na.rm=TRUE)
      yda=aggregate(as.numeric(yda[,2]),by=list(Date=yda[,1]),FUN=sum,na.rm=TRUE)      
      
    }
    if(input$dataset=="Taux.de.conversion.des.clics"){
      da=aggregate(as.numeric(da[,2]),by=list(Date=da[,1]),FUN=mean,na.rm=TRUE) 
      yda=aggregate(as.numeric(yda[,2]),by=list(Date=yda[,1]),FUN=mean,na.rm=TRUE)      
      
    }
    
    if(input$dataset=="Clics"){
      da=aggregate(as.numeric(da[,2]),by=list(Date=da[,1]),FUN=sum,na.rm=TRUE)  
      yda=aggregate(as.numeric(yda[,2]),by=list(Date=yda[,1]),FUN=sum,na.rm=TRUE)      
      
    }
    
    if(input$dataset=="valeur.de.conv..totale"){
      da=aggregate(as.numeric(da[,2]),by=list(Date=da[,1]),FUN=sum,na.rm=TRUE)  
      yda=aggregate(as.numeric(yda[,2]),by=list(Date=yda[,1]),FUN=sum,na.rm=TRUE)      
      
    }
    
    
    if(input$Type=="Generic") 
    { plot(data.frame(da[,1],da[,2]),type="l",col="red")
    }else{
      
      plot(data.frame(da[,1],da[,2]),type="l",col="red")
    }
    
    
    
    
    
  })
  
})
