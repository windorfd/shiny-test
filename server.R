#    http://shiny.rstudio.com/
library(shiny)
library(ggplot2)
# # Note that these next two lines go together   
library(future.apply)
plan(multisession)
# Also, need to tweak line ~ 158 -> future_sapply(
# library("parallel") # Note that this library is now part of base R
# library("Iso")
library(DT)
library(qvalue)
# rm(list=ls())
library(viridis) #
colsNow=viridis(12)
source("ui.R")
# Define aux f'ns and needed constants not chosen by user ----
# source("auxFns.R") ----
source("auxFns.R")
nWellsPerPlate=308
dfCloseToNormal=41 
# We are considering this as the number of df that is getting close 
# to the normal distribution but note that this is debatable and context dependent
server <- function(input, output, session) {
  output$nWellsPerPlate <- renderText({ nWellsPerPlate })
  # # observe nWellsPerPlateNonControls ----
  # observe({
  #   nWellsPerPlatePositives=input$nWellsPerPlatePositives
  #   nWellsPerPlateNegatives=input$nWellsPerPlateNegatives
  #   setTo=nWellsPerPlate - (nWellsPerPlatePositives + nWellsPerPlateNegatives)
  #   updateSliderInput(session, "nWellsPerPlateNonControls",
  #     value =setTo ,
  #     min =setTo, max = setTo, step =1)
  # })
  # observe metric, adjust sliders accordingly ----
  observe({
    metricNow=input$metricNow
    if(metricNow=="MPDC"){
      updateSliderInput(session, "mu.DMSO", label=paste("Mean, MPDC (um^2/sec)"), value =0 ,
        min =-0.5, max = 0.5, step =0.01)
      updateSliderInput(session,"sdDMSO", label=paste(HTML("SD, MPDC (um^2/sec)")),
        min =0.01, max = 8, value=1, step=0.1)
      updateSliderInput(session,"sd.NonAct", label="SD, MPDC (um^2/sec)",
        min =0.01, max = 8, value=1, step=0.1)
      updateSliderInput(session,"sd.t1", label="SD, MPDC (um^2/sec)",
        min =0.01, max = 8, value=1, step=0.1)
      updateSliderInput(session,"sd.t2", label="SD, MPDC (um^2/sec)",
        min =0.01, max = 8, value=1, step=0.1)
      updateSliderInput(session,"sd.t3", label="SD, MPDC (um^2/sec)",
        min =0.01, max = 8, value=1, step=0.1)
    }else if(metricNow=="Q3JL"){
      updateSliderInput(session, "mu.DMSO", label=paste("Mean, Q3JL (um)"), value =0 ,
        min =-0.1, max = 0.1, step =0.001)
      updateSliderInput(session, "sdDMSO", label="SD, Q3JL (um)",
                        min =0.01, max = 0.2, value=0.1, step=0.01)
      updateSliderInput(session,"sd.NonAct", label="SD, Q3JL (um)",
                        min =0.01, max = 0.2, value=0.1, step=0.01)
      updateSliderInput(session,"sd.t1", label="SD, Q3JL (um)",
                        min =0.01, max = 0.2, value=0.1, step=0.01)
      updateSliderInput(session,"sd.t2", label="SD, Q3JL (um)",
                        min =0.01, max = 0.2, value=0.1, step=0.01)
      updateSliderInput(session,"sd.t3", label="SD, Q3JL (um)",
                        min =0.01, max = 0.2, value=0.1, step=0.01)
    }
  })
  # Graph initial heavy tails ----
  output$howHeavyTails <- renderPlot({
    dfPerFOV=dfCloseToNormal - input$tailWeight
    seqq=seq(-5, 5, length=1e4)
    ys=dt(seqq, dfPerFOV)
    par(mar=c(0, 0, 0, 0))
    plot(seqq, ys, pch=16, xlim=c(-5, 5), ylim=c(0, 0.4), xlab='', ylab='', main='',
         yaxt='n', xaxt='n')
  })
  # Produce table of plates needed ----
  NoPlatesInput<-reactive({
    nWellsPerPlatePositive=input$nWellsPerPlatePositive
    nWellsPerPlateNegative=input$nWellsPerPlateNegative
    nWellsPerPlateNoDye=input$nWellsPerPlateNoDye
    nWellsPlateNonControls=input$nWellsPlateNonControls # Number wells per compound per plate
    nPlatesNonControls=input$nPlatesNonControls 
    # On how many plates does a given non-control appear?
    nCompounds=input$nCompounds # Note that this is clear in the UI that this
    # is the number of non-control compounds
    nWellsReservedNonControls=nWellsPerPlate - 
      (nWellsPerPlatePositive + nWellsPerPlateNegative + nWellsPerPlateNoDye)
    nPlatesNeeded=ceiling((nCompounds*nWellsPlateNonControls*
      nPlatesNonControls)/nWellsReservedNonControls)
    remainder=nPlatesNeeded %% nPlatesNonControls
    if(remainder != 0 ) nPlatesNeeded = nPlatesNeeded + nPlatesNonControls - remainder
    NumberOfPlates=data.frame(nPlatesNeeded=nPlatesNeeded, nWellsPerPlate=nWellsPerPlate,
      nWellsPositive=nWellsPerPlatePositive, nWellsNegative=nWellsPerPlateNegative,
      nWellsPerPlateNoDye=nWellsPerPlateNoDye, nCompounds=nCompounds,
      nWellsPlateNonControls=nWellsPlateNonControls, nPlatesNonControls=nPlatesNonControls
      )
  })
  output$NumberOfPlates <- renderTable(NoPlatesInput())
  # goNow lands here. Start heavy crunching ----
  observeEvent(input$goNow,{
    # Read in needed values ----
    nWellsPerPlatePositive=input$nWellsPerPlatePositive
    nWellsPerPlateNegative=input$nWellsPerPlateNegative
    nWellsPerPlateNoDye=input$nWellsPerPlateNoDye
    nWellsPlateNonControls=input$nWellsPlateNonControls # Number wells per compound per plate
    nPlatesNonControls=input$nPlatesNonControls
    # On how many plates does a given non-control appear?
    nCompounds=input$nCompounds # Note that this is clear in the UI that this
    # is the number of non-control compounds
    nWellsReservedNonControls=nWellsPerPlate - 
      (nWellsPerPlatePositive + nWellsPerPlateNegative + nWellsPerPlateNoDye)
    nPlatesNeeded=ceiling((nCompounds*nWellsPlateNonControls*
      nPlatesNonControls)/nWellsReservedNonControls)
    
    metricNow=input$metricNow
    nCompounds=input$nCompounds
    pi0=as.numeric(input$pi0)
    cat('here\n')
    dfPerFOV=dfCloseToNormal - input$tailWeight
    sdModel=sqrt(dfPerFOV /(dfPerFOV-2)) # Variance of a t-distribution is df/(df-2)
    nFOVsPerWell=input$nFOVsPerWell
    nDMSO=input$nWellsPerPlateNegative*input$nFOVsPerWell*nPlatesNeeded
    SSMD.t1=as.numeric(input$SSMD.t1)
    SSMD.t2=as.numeric(input$SSMD.t2)
    SSMD.t3=as.numeric(input$SSMD.t3)
    cat('here1\n')
    nActives=floor((1-pi0)*nCompounds)
    nNonActives=nCompounds-nActives
    weight.t1=1
    weight.t2=1
    weight.t3=1
    # Read in values for generating SDs and dist'n of DMSO ----
    mu.DMSO=input$mu.DMSO
    sdDMSO=input$sdDMSO
    sd.NonAct=input$sd.NonAct
    sd.t1=input$sd.t1
    sd.t2=input$sd.t2
    sd.t3=input$sd.t3
    cat('hereAA\n')
    # Values are read in, start crunching ----
    # Get n (number of compounds) for each of the three types ----
    nt1=floor((weight.t1/(weight.t1+weight.t2+weight.t3))*nActives)
    nt2=floor((weight.t2/(weight.t1+weight.t2+weight.t3))*nActives)
    nt3=nActives-(nt1+nt2)
    cat('nt1', nt1, 'nt2', nt2, 'nt3', nt3, '\n' )
    # Get DMSO for current screen ----
    # Start from T_dfPerFOV
    cat('hereB\n')
    samp.DMSO=(sdDMSO/sdModel)*rt(nDMSO, dfPerFOV)+mu.DMSO
    cat('mode(samp.DMSO)', mode(samp.DMSO), "\n")
    # Get mu for each of the three types. This is now compound specific ----
    mu.t1=SSMD.t1*sqrt(sdDMSO^2+sd.t1^2)+mu.DMSO
    mu.t2=SSMD.t2*sqrt(sdDMSO^2+sd.t2^2)+mu.DMSO
    mu.t3=SSMD.t3*sqrt(sdDMSO^2+sd.t3^2)+mu.DMSO
    XbarDMSO=mean(samp.DMSO)
    varSampDMSO=var(samp.DMSO)
    cat('hereC\n')
    # Get estimated SSMDs for each compound for non-actives and each of the three types ----
    vecc=1:nNonActives
    SSMDNonActives=t(future_sapply(vecc, getSSMDSingle, muNow=mu.DMSO, # By assumption, mu_nonAct=mu_DMSO
    # SSMDNonActives=t(sapply(vecc, getSSMDSingle, muNow=mu.DMSO, # By assumption, mu_nonAct=mu_DMSO
      nMi=nFOVsPerWell*nWellsPlateNonControls*nPlatesNonControls, dfPerFOVb=dfPerFOV, 
      XbarDMSOb=XbarDMSO, varDMSO=varSampDMSO, sdModelb=sdModel, sdNow=sd.NonAct, nDMSOb=nDMSO))
    # NonActivesXbar=SSMDNonActives[,2]
    # NonActivesSSMD=SSMDNonActives[,1]
    # cat('dim(SSMDNonActives)', dim(SSMDNonActives), '\n')
    cat('hereE\n')
    if(nt1>0){
      vecc=1:nt1
      SSMDt1=t(sapply(vecc, getSSMDSingle, muNow=mu.t1,  
        nMi=nFOVsPerWell*nWellsPlateNonControls*nPlatesNonControls, dfPerFOVb=dfPerFOV, 
        XbarDMSOb=XbarDMSO, varDMSO=varSampDMSO,
        sdModelb=sdModel, sdNow=sd.t1, nDMSOb=nDMSO))
    }
    cat('hereAAA\n')
    if(nt2>0){
    vecc=1:nt2
    SSMDt2=t(sapply(vecc, getSSMDSingle, muNow=mu.t2,  
      nMi=nFOVsPerWell*nWellsPlateNonControls*nPlatesNonControls, dfPerFOVb=dfPerFOV, 
      XbarDMSOb=XbarDMSO, varDMSO=varSampDMSO,
      sdModelb=sdModel, sdNow=sd.t2, nDMSOb=nDMSO))
    }
    if(nt3>0){
    vecc=1:nt3
    SSMDt3=t(sapply(vecc, getSSMDSingle, muNow=mu.t3,  
      nMi=nFOVsPerWell*nWellsPlateNonControls*nPlatesNonControls, dfPerFOVb=dfPerFOV, 
      XbarDMSOb=XbarDMSO, varDMSO=varSampDMSO,
      sdModelb=sdModel, sdNow=sd.t3, nDMSOb=nDMSO)) 
    }
    
    breaksNow=c(-200, 0.25, 0.5, 0.75, 1, 1.28, 1.64, 2, 3, 5, 200)
    catsNonActives=as.numeric(cut(SSMDNonActives[,1],breaksNow))
    catsNonActives=sapply(catsNonActives, getLevels)
    tabsCatsNonActives=table(catsNonActives)
    ysNow=as.numeric(substr(names(tabsCatsNonActives), 1, 2))
    cat("hereD\n")
    if(nt1>0){
      catsT1=as.numeric(cut(SSMDt1[,1],breaksNow))
      catsT1=sapply(catsT1, getLevels)
      tabsCatsT1=table(catsT1)
      ysNowT1=as.numeric(substr(names(tabsCatsT1), 1, 2))
      ssmds=c(SSMDNonActives[,1], SSMDt1[,1])
      Xbars=c(SSMDNonActives[,2], SSMDt1[,2])
      compoundClass=c(rep("NonAct", length(SSMDNonActives[,1])), rep("Class1", length(SSMDt1[,1])))
      allYs=c(ysNow, ysNowT1)
      allNames=c(names(tabsCatsNonActives), names(tabsCatsT1) )
      xAxisTicks=c(3.5, 4.5)
      xAxisLabels=c('NonAct', 'Class1')
    }
    cat("hereCA1\n")
    if(nt2>0){
      catsT2=as.numeric(cut(SSMDt2[,1],breaksNow))
      catsT2=sapply(catsT2, getLevels)
      tabsCatsT2=table(catsT2)
      ysNowT2=as.numeric(substr(names(tabsCatsT2), 1, 2))
      if(nt1==0){
        ssmds=c(SSMDNonActives[,1], SSMDt2[,1])
        Xbars=c(SSMDNonActives[,2], SSMDt2[,2])
        compoundClass=c(rep("NonAct", length(SSMDNonActives[,1])), 
          rep("Class2", length(SSMDt2[,1])))
        allYs=c(ysNow, ysNowT2)
        allNames=c(names(tabsCatsNonActives), names(tabsCatsT2) )
        xAxisTicks=c(3.5, 4.5)
        xAxisLabels=c('NonAct', 'Class2')
      }else{  
        ssmds=c(ssmds, SSMDt2[,1])
        Xbars=c(Xbars, SSMDt2[,2])
        compoundClass=c(compoundClass, rep("Class2", length(SSMDt2[,1])))
        allYs=c(allYs, ysNowT2)
        allNames=c(allNames, names(tabsCatsT2))
        xAxisTicks=c(xAxisTicks, max(xAxisTicks)+1)
        xAxisLabels=c(xAxisLabels, 'Class2')
      }
    }
    if(nt3>0){
      catsT3=as.numeric(cut(SSMDt3[,1],breaksNow))
      catsT3=sapply(catsT3, getLevels)
      tabsCatsT3=table(catsT3)
      ysNowT3=as.numeric(substr(names(tabsCatsT3), 1, 2))
      if(nt2==0 && nt1==0){
        ssmds=c(SSMDNonActives[,1], SSMDt3[,1])
        Xbars=c(SSMDNonActives[,2], SSMDt3[,2])
        compoundClass=c(rep("NonAct", length(SSMDNonActives[,1])), rep("Class3", length(SSMDt3[,1])))
        allYs=c(ysNow, ysNowT3)
        allNames=c(names(tabsCatsNonActives), names(tabsCatsT3) )
        xAxisTicks=c(3.5, 4.5)
        xAxisLabels=c('NonAct', 'Class3')
      }else{
        ssmds=c(ssmds, SSMDt3[,1])
        Xbars=c(Xbars, SSMDt3[,2])
        compoundClass=c(compoundClass, rep("Class3", length(SSMDt3[,1])))
        allYs=c(allYs, ysNowT3)
        allNames=c(allNames, names(tabsCatsT3))
        xAxisTicks=c(xAxisTicks, max(xAxisTicks)+1)
        xAxisLabels=c(xAxisLabels, 'Class3')
      }
    }
  # Generate graph of SSMD by compound type ---- 
  DFNow=data.frame(ssmds=ssmds, compoundClass=compoundClass, Xbars=Xbars)
  DFNow$compoundClass=factor(DFNow$compoundClass, levels=c("NonAct", "Class1", "Class2", "Class3"),
    ordered=TRUE)
  output$SSMDbyClass <- renderPlot({ 
    par(mar=c(5,6,3,1))
    boxplot(ssmds~compoundClass, cex.axis=1.5, data=DFNow, ylab="SSMD (Effect size)", xlab="CompoundClass",
      cex.lab=1.5, main="Boxplots of observed SSMDs by compound class", cex.main=1.5, varwidth=TRUE)
  })
  # Generate text table (as a graph) of count by SSMD category ----
  output$tableSSMDbyClass <- renderPlot({ 
    par(mar=c(5,1,1,1))
    maxXlim=5
    nt2GT0=nt2>0
    nt3GT0=nt3>0
    if(nt2GT0) maxXlim=maxXlim+1
    if(nt3GT0) maxXlim=maxXlim+1
    maxAllYs=max(allYs)
    plot(0, 0, pch="", ylim=c(0, maxAllYs), xlim=c(0.75, maxXlim), xaxt="n", 
      yaxt="n", xlab="", ylab="")
    rect(rep(0, maxAllYs), 0:(maxAllYs-1)+0.5, rep(maxXlim, maxAllYs), 1:maxAllYs+0.5,
      col=c('white', 'ivory2'), border=NA)
    all2=data.frame(allNames=allNames, allYs=allYs)
    all2=unique(all2)
    text(rep(1, nrow(all2)), all2$allYs, all2$allNames, cex=2, pos=4)
    text(rep(3.5, length(tabsCatsNonActives)), ysNow, tabsCatsNonActives, cex=2)
    text(rep(4.5, length(tabsCatsT1)), ysNowT1, tabsCatsT1, cex=2)
    if(nt2GT0>0) text(rep(4.5+nt2GT0, length(tabsCatsT2)), ysNowT2, tabsCatsT2, cex=2)
    if(nt3GT0>0) text(rep(4.5+nt2GT0+nt3GT0, length(tabsCatsT3)), ysNowT3, tabsCatsT3, cex=2)
    axis(1, xAxisTicks, xAxisLabels, cex.axis=2)
  # cat('all2$allYs\n')
  # print(all2$allYs)
  })
  # Generate graph, boxplots of Xbars by compound type  ----
  output$sampleMeans <- renderPlot({ 
    par(mar=c(5,6,3,1))
    boxplot(Xbars~compoundClass, data=DFNow, cex.axis=1.5, ylab="Sample means\n(per compound)", 
      xlab="CompoundClass", varwidth=TRUE,
      cex.lab=1.5, main="Boxplots of observed sample means by compound class", cex.main=1.5)
  })
  # Start analysis ----
  SSMDNonActives=cbind(Class="NonAct",data.frame(SSMDNonActives))
  resMat=SSMDNonActives
  if(nt1>0){
    SSMDt1=cbind(Class="Class1", data.frame(SSMDt1))
    resMat=rbind(resMat, SSMDt1)
  }
  if(nt2>0){
    SSMDt2=cbind(Class="Class2", data.frame(SSMDt2))
    resMat=rbind(resMat, SSMDt2)
  }
  if(nt3>0){
    SSMDt3=cbind(Class="Class3", data.frame(SSMDt3))
    resMat=rbind(resMat, SSMDt3)
  }
  colnames(resMat)=c("Class", "SSMD", "Xbar", "t", "df")
  resMat$pt=2*pt(-1*abs(resMat$t), df=resMat$df)
  
  qvalObj=qvalue(p=resMat$pt)
  resMat$qval=qvalObj$qvalues
  # Do tests to find hits ----
  # 2022-06-23: Currently, we are only testing for superiority
  # Test by 'industry standard' ----
  indStandLimit=XbarDMSO+input$sdIndStand*sqrt(varSampDMSO)
  indStandRes=tapply(resMat$Xbar, resMat$Class, function(x){sum(x>=indStandLimit)})
  # Test by SSMD criteria ----
  SSMDCutoff=input$SSMDCutoff
  SSMDCritRes=tapply(resMat$SSMD, resMat$Class, function(x){sum(x>=SSMDCutoff)})
  # Test by q-value ----
  tmp= resMat$qval<=input$qvalCutoff & resMat$Xbar>0
  qvalRes=tapply(tmp, resMat$Class, function(x){sum(x)})
  cat('hereD\n')
  cat('mode(qvalRes)', mode(qvalRes), '\n')
  print(qvalRes); 
  # cat('mode(SSMD.t1)', mode(SSMD.t1), '\n')
  dataGenSSMD=c("ExtWeak=0", sapply(c(SSMD.t1, SSMD.t2, SSMD.t3), getLevels2))
  cat('hereE\n')
  # testResMat=cbind(c(nNonActives, nt1, nt2, nt3), indStandRes, SSMDCritRes, qvalRes)
  indStandRes=data.frame(Class=names(indStandRes), IndustryStandard=indStandRes)
  SSMDCritRes=data.frame(Class=names(SSMDCritRes), SSMDLimit=SSMDCritRes)
  qvalRes=data.frame(Class=names(qvalRes), FDRLimit=qvalRes)
  ns=c(nNonActives, nt1, nt2, nt3)
  ns=data.frame(Class=c('NonAct', 'Class1', 'Class2', 'Class3'), nInScreenChar=as.character(round(ns)),
     nInScreen=round(ns))
  dataGenSSMD=data.frame(Class=c('NonAct', 'Class1', 'Class2', 'Class3'),
    TrueEffectSize=dataGenSSMD)
  testResDataFrame=merge(dataGenSSMD, merge(ns, merge(indStandRes, 
    merge(SSMDCritRes, qvalRes, all=TRUE), all=TRUE), all=TRUE), all=TRUE)
  colnames(testResDataFrame)[1]="CompoundClass"
  testResDataFrame$CompoundClass=factor(testResDataFrame$CompoundClass, 
    levels=c("NonAct", "Class1", "Class2", "Class3"))
  testResDataFrame=testResDataFrame[order(testResDataFrame$CompoundClass),]
  cat("testResDataFrame:\n")
  print(testResDataFrame)
  # browser()
  tmp=testResDataFrame[,-which(colnames(testResDataFrame)=="nInScreen")]
  colnames(tmp)[which(colnames(tmp)=="nInScreenChar")]="nInScreen"
  output$tableTestResDataFrame <- renderTable(tmp, striped=TRUE)
  # This code from https://www.definitions.net/definition/precision+and+recall
  TPs=apply(testResDataFrame[-1, -c(1:4)], 2, sum)
  TPsPlusFPs=apply(testResDataFrame[, -c(1:4)], 2, sum)
  FNs=apply(testResDataFrame[-1,4]-testResDataFrame[-1, -c(1:4)], 2, sum)
  Precision=TPs/TPsPlusFPs
  Recall=TPs/(TPs+FNs)
  # cat("Precision\n")
  # print(Precision)
  # 
  # cat("Recall\n")
  # print(Recall)
  # 
  Fscore=2/(Precision^(-1)+Recall^(-1))
  # cat("Fscore\n")
  # print(Fscore)
  FscoreTab=round(100*rbind(Precision, Recall, Fscore), 2)
  FscoreTab=cbind(rownames(FscoreTab), FscoreTab)
  colnames(FscoreTab)[1]="Quality\nMeasure"
  # print(FscoreTab)
  output$FscoreTab <- renderTable(FscoreTab, striped=TRUE)
  # Hist of p-values ----
  histNow=hist(resMat$pt,  plot=FALSE)
  numberTrueH0PerBin=(qvalObj$pi0/(length(histNow$breaks)-1))*
    sum(histNow$counts)
  
  output$histPVals <- renderPlot({
    par(mar=c(5,5,6,1))
    histNow=hist(resMat$pt, cex.lab=2, cex.axis=2, cex.main=2,
      main=paste0("Histogram of\np-values from two-sample t-test on",
      "\ntransformed ", metricNow), xlab="p-value", col=colsNow[3])
    text(0.1, 0.1*max(histNow$counts), paste0("N=", nrow(resMat),
      " combinations of MolName x Conc"), cex=1.5, 
      col=colsNow[6], pos=4)
    abline(h=numberTrueH0PerBin, lwd=3, col=colsNow[9])
    text(0.8, 0.3*max(histNow$counts), paste0("y=Level of count\n",
      "of true H0s per bin"), pos=1, cex=1.5, col=colsNow[9])
    text(0.23, numberTrueH0PerBin, bquote(widehat(pi[0]) == 
      .(round(100*qvalObj$pi0, 1))), col=colsNow[9], cex=3, pos=1)
    text(0.48, 0.97*numberTrueH0PerBin, "%", col=colsNow[9], cex=3, pos=1)
  })
  }) # end observeEvent(input$goNow,{ ----
} # end: server
# Run the application 
# shinyApp(ui = ui, server = server)
