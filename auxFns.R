 # This file contains helper f'ns for the Shiny app 'SSMDSampleSize'
 getSSMDSingle=function(x, muNow, nMi, dfPerFOVb, XbarDMSOb, 
   varDMSO, sdModelb, sdNow, nDMSOb){
   sampNow=(sdNow/sdModelb)*rt(nMi, dfPerFOVb)+muNow
   Xbar=mean(sampNow)
   varMi=var(sampNow)
   df=(varMi/nMi+varDMSO/nDMSOb)^2/((((varMi/nMi)^2)/(nMi-1))+
     (((varDMSO/nDMSOb)^2)/(nDMSOb-1)))
   rawDiff=Xbar-XbarDMSOb
   betaHat=rawDiff/(sqrt(varMi + varDMSO))
   t=rawDiff/(sqrt(varMi/nMi + varDMSO/nDMSOb))
   return(c(betaHat, Xbar, t, df))  
 }

 getLevels=function(x) switch(x, "01 ExtWeak", "02 VeryWeak", "03 Weak", "04 FairWeak",
   "05 FairMod", "06 Mod", "07 FairStrong", "08 Strong", "09 VeryStrong",
   "10 ExtStrong")
 # getLevels2 assigns midpoints of the intervals
 getLevels2=function(x){
   if(x==0.375){
     y="VeryWeak=0.375"
   }else if(x==0.625){
     y="Weak=0.625"
   }else if(x==0.875){
     y="FairWeak=0.875"
   }else if(x==1.14){
     y="FairMod=1.14"
   }else if(x==1.46){
     y="Mod=1.46"
   }else if(x==1.82){
     y="FairStrong=1.82"
   }else if(x==2.5){
     y="Strong=2.5"
   }else if(x==4){
     y="VeryStrong=4"
   }else if(x==7.5){
     y="ExtStrong=7.5"
   }else y="StrangeSSMD"
   return(y)
 }
 # getLevels(10)
 # getScreenResults=function(x){
 #   # Available variables are pi0, nDMSO, SSMD.t1, nCompounds, and nFOVs.t1
 #   # How many active compounds do we have?
 #   samp.DMSO=rt(nDMSO, dfPerFOV)
 #   meanDMSO=mean(samp.DMSO)
 #   varDMSO=var(samp.DMSO)
 #   vecc=1:nActives
 #   SSMDActives=sort(sapply(vecc, getSSMDSingle, mu2Use=mu.t1))
 #   SSMDNonActives=sort(sapply(vecc, getSSMDSingle, mu2Use=0))
 #   loLim95Actives=SSMDActives[fivePercentActives]
 #   hiLim95Actives=SSMDActives[ninetyFivePercentActives]
 #   loLim95NonActives=SSMDNonActives[fivePercentNonActives]
 #   hiLim95NonActives=SSMDNonActives[ninetyFivePercentNonActives]
 #   medianActives=median(SSMDActives)
 #   medianNonActives=median(SSMDNonActives)
 #   return(c(loLim95Actives, medianActives, hiLim95Actives, loLim95NonActives,
 #     medianNonActives, hiLim95NonActives))
 # }
 
 
 
 
