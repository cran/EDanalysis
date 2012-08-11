EnrichTandD <-
function(myInterestedGenes,whichOnto="BP",TestMthod="Hyper",DegreeMethod="ED",mapping="org.Hs.eg.db",ID = "symbol",nodeSize = 5){

cat("There are two steps in this process: \n(1) Gene annotation using topGO package.This will take a few minutes, please be patient......")
cat("\n")
AnnResult=AnnGeneToGO(myInterestedGenes=myInterestedGenes,whichOnto=whichOnto,mapping=mapping,ID = ID,nodeSize = nodeSize);
cat("\n\n(2)Calculating enrichment test p-value and enrichment degree indicator.......\n\n")
Size=dim(AnnResult)
if(TestMthod=="Hyper"){
         TestResult=sapply(1:Size[1],function(x) EnrichHyper(AnnResult[x,4],AnnResult[x,5],AnnResult[x,6],AnnResult[x,7]))
         ALLresult=data.frame(AnnResult,hyper_p=TestResult)
   if(DegreeMethod=="ED"){
         DegreeResult=sapply(1:Size[1],function(x) ED(AnnResult[x,4],AnnResult[x,5],AnnResult[x,6],AnnResult[x,7]))
         ALLresult=data.frame(ALLresult,ED=DegreeResult)    
      }
   if(DegreeMethod=="ER"){
         DegreeResult=sapply(1:Size[1],function(x) ER(AnnResult[x,4],AnnResult[x,5],AnnResult[x,6],AnnResult[x,7]))
         ALLresult=data.frame(ALLresult,ER=DegreeResult) 
     }
   if(DegreeMethod=="EP"){
         DegreeResult=sapply(1:Size[1],function(x) EP(AnnResult[x,4],AnnResult[x,5],AnnResult[x,6],AnnResult[x,7]))
         ALLresult=data.frame(ALLresult,EP=DegreeResult) 
     }
   if(DegreeMethod=="RE"){
         DegreeResult=sapply(1:Size[1],function(x) RE(AnnResult[x,4],AnnResult[x,5],AnnResult[x,6],AnnResult[x,7]))
         ALLresult=data.frame(ALLresult,RE=DegreeResult) 
     }
##ALLresult=subset(ALLresult,hyper_p<TestThreshold)
}
if(TestMthod=="Fisher"){
         TestResult=sapply(1:Size[1],function(x) EnrichFisher(AnnResult[x,4],AnnResult[x,5],AnnResult[x,6],AnnResult[x,7]))
         ALLresult=data.frame(AnnResult,Fisher_p=TestResult)
   if(DegreeMethod=="ED"){
         DegreeResult=sapply(1:Size[1],function(x) ED(AnnResult[x,4],AnnResult[x,5],AnnResult[x,6],AnnResult[x,7]))
         ALLresult=data.frame(ALLresult,ED=DegreeResult)    
     }
   if(DegreeMethod=="ER"){
         DegreeResult=sapply(1:Size[1],function(x) ER(AnnResult[x,4],AnnResult[x,5],AnnResult[x,6],AnnResult[x,7]))
         ALLresult=data.frame(ALLresult,ER=DegreeResult) 
     }
   if(DegreeMethod=="EP"){
         DegreeResult=sapply(1:Size[1],function(x) EP(AnnResult[x,4],AnnResult[x,5],AnnResult[x,6],AnnResult[x,7]))
         ALLresult=data.frame(ALLresult,EP=DegreeResult) 
     }
   if(DegreeMethod=="RE"){
         DegreeResult=sapply(1:Size[1],function(x) RE(AnnResult[x,4],AnnResult[x,5],AnnResult[x,6],AnnResult[x,7]))
         ALLresult=data.frame(ALLresult,RE=DegreeResult) 
     }
##ALLresult=subset(ALLresult,Fisher_p<TestThreshold)
}
ALLresult=ALLresult[order(ALLresult[,9],decreasing=TRUE),]
return(ALLresult)
}
