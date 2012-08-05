FilterTandDresults <-
function(rawTandDresults=rawTandDresults,nodeSize = 20,nodeInterestedGeneSize=20,Pvalue=threshold){
rawTandDresults=subset(rawTandDresults,rawTandDresults[,6]>=nodeSize)
rawTandDresults=subset(rawTandDresults,rawTandDresults[,7]>=nodeInterestedGeneSize)
results=subset(rawTandDresults,rawTandDresults[,8]<threshold)
return(results)
}
