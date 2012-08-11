AnnGeneToGO <-
function(myInterestedGenes,whichOnto="BP",mapping="org.Hs.eg.db",ID = "symbol",nodeSize = 5){

cat("Gene annotation start:\n")
MyGetTermsDefinition <- function(whichTerms, ontology, numChar = 150, multipLines = FALSE) {
  qTerms <- paste(paste("'", whichTerms, "'", sep = ""), collapse = ",")
  retVal <- dbGetQuery(GO_dbconn(), paste("SELECT term, go_id FROM go_term WHERE ontology IN",
                                          "('", ontology, "') AND go_id IN (", qTerms, ");", sep = ""))
  termsNames <- retVal$term
  names(termsNames) <- retVal$go_id
  if(!multipLines) 
    shortNames <- paste(substr(termsNames, 1, numChar),
                        ifelse(nchar(termsNames) > numChar, '...', ''), sep = '')
  else
    shortNames <- sapply(termsNames,
                         function(x) {
                           a <- strwrap(x, numChar)
                           return(paste(a, sep = "", collapse = "\\\n"))
                         })
  names(shortNames) <- names(termsNames)
  return(shortNames[whichTerms])
}
myInterestedGenes=myInterestedGenes[,1]
GO_Tree <- annFUN.org(whichOnto=whichOnto,mapping=mapping,ID=ID)
allGenes <- unique(unlist(GO_Tree))
BackGround_num=length(allGenes)
geneList <- factor(as.integer(allGenes %in% myInterestedGenes))
names(geneList) <- allGenes

GOdata <- new("topGOdata",
              ontology = whichOnto,
              allGenes = geneList,
              nodeSize = nodeSize,
              annot = annFUN.org, 
              mapping = mapping,
              ID = ID)

object<-GOdata
nodeLevel <- buildLevels(graph(object), leafs2root = TRUE)
nodeLevel_1 <- unlist(mget(sort(usedGO(GOdata)), envir = nodeLevel$nodes2level))
nN <- c()
nG <- c()
ng <- c()
GOID=sort(usedGO(GOdata))
shortNames <- MyGetTermsDefinition(GOID, topGO::ontology(object), numChar = 150)
GOstatistic<-termStat(GOdata)
result<- data.frame(ID=GOID, Term = shortNames,Level = as.integer(nodeLevel_1),nN=sapply(GOID,function(x)numGenes(GOdata)),nK=sapply(GOID,function(x)numSigGenes(GOdata)),nG=GOstatistic[,1], ng=GOstatistic[,2], stringsAsFactors = FALSE)
#go_gene_numb<- countGenesInTerm(GOdata)
##must be annotated!
Interested_Annoted_result=subset(result,ng!=0)
Interested_Annoted_result=subset(Interested_Annoted_result,nN!=nG)
resultdim=dim(Interested_Annoted_result);
cat("\nGene annotation results: ")
cat(paste("A total of",resultdim[1],"nodes, each node contains at least one interested gene.",sep=" "))
return(Interested_Annoted_result);
}
