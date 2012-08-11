\name{EDanalysis-package}
\alias{EDanalysis-package}
\alias{EDanalysis}
\docType{package}
\title{
An R Package For Gene Enrichment Disequilibrium Analysis 
}
\description{
ED (Enrichment Disequilibrium) is an indicator to measure the degree of enrichment.

Enrichment analysis is important in understanding the biological interpretation of interesting gene set (derived from the results of high-throughput data analysis). 

In the enrichment analysis process, both enrichment test and enrichment degree should be considered. 

They were different types of methods: 

Enrichment test was used to identify whether there is an association between a functional gene set and the interesting gene set, 

while enrichment degree was used to measure the extent of the overlap.
 
A complete enrichment analysis should include two steps: 
 
   (1) the first step is to identify which functional gene sets are significantly associated with the interesting gene set by using enrichment test method, 
 
   (2) the second step is to measure the degree of enrichment and prioritize significant functional gene sets by using enrichment degree indicator. 



The EDanalysis package has the following features:

Calculate the enrichment degree indicators:

   (1)Calculate the Enrichment Disequilibrium coefficient ED. (ED function)

   (2)Calculate the Enrichment Disequilibrium coefficient ER. (ER function)

   (3)Calculate the Enrichment Percentage EP. (EP function)

   (4)Calculate the Relative enrichment factor RE. (RE function)

Carry out the enrichment test:

   (5)Hypergeometric distribution. (EnrichHyper function)

   (6)Fisher's exact test. (EnrichFisher function)

Integrated analysis:

   (7)Annotate Genes To GO. (AnnGeneToGO function)
   
   (8)Calculate Enrichment Test and Enrichment Degree Indicator (EnrichTandD function)
   
   (9)Filter the results from function EnrichTandD (FilterTandDresults function)
   
   (10)Plot a GO Tree. (PlotGOgraph function)
}
\details{
\tabular{ll}{
Package: \tab EDanalysis\cr
Type: \tab Package\cr
Version: \tab 1.0.1\cr
Date: \tab 2012-08-11\cr
License: \tab LGPL\cr
}

}
\author{
Yongshuai Jiang

Maintainer: Yongshuai Jiang <jiangyongshuai@gmail.com>
}
\references{
Y. Jiang, M. Zhang, X. Guo, R. Zhang, Enrichment Disequilibrium: A novel approach for measuring the degree of enrichment after gene enrichment test, Biochem Biophys Res Commun. 2012; 424:563-567.
}

\keyword{ Enrichment Disequilibrium }
\seealso{
\code{\link{ED}}, \code{\link{ER}}, \code{\link{EP}}, \code{\link{RE}}, \code{\link{EnrichHyper}}, \code{\link{EnrichFisher}}, \code{\link{AnnGeneToGO}}, \code{\link{EnrichTandD}}, \code{\link{FilterTandDresults}}, \code{\link{PlotGOgraph}}, \code{\link{EDDataExample}}, \code{\link{EDAnnoDataExample}}
}
\examples{
##---- Example1----
##Import example data
data(EDDataExample)
data<-EDDataExample #Import example data
####

####For a single value
result1<-EP(nN = data[1,1],nK = data[1,2],nG = data[1,3],ng = data[1,4]) #return 0.03824701
result1
#or
result2<-EP(data[1,1],data[1,2],data[1,3],data[1,4]) #return 0.03824701
result2
####

####For a data.frame
result3<-sapply(1:dim(data)[1],function(x)EP(nN = data[x,1],nK = data[x,2],nG = data[x,3],ng = data[x,4]))
result3
#or
result4<-sapply(1:dim(data)[1],function(x)EP(data[x,1],data[x,2],data[x,3],data[x,4]))
result4

########---- Example2 ----
#######In this example, all the codes were commented. To do so, we will save the running time of checking EDanalysis package (R CMD check --timings) when we submit the package to CRAN. 
#######User can run these codes by removing the comment "#" (located in the beginning of each line).
  
#library(graph)  ##need packages  
#library(Biobase)
#library(DBI)
#library(RSQLite)
#library(AnnotationDbi)
#library(GO.db)
#library(SparseM)
#library(topGO)
#library(grid)
#library(Rgraphviz)
#library(Category)
#library(GOstats)
#data(EDAnnoDataExample)   ##Import example data
#mydata<-EDAnnoDataExample ##Import example data
#rawTandDresults<-EnrichTandD(myInterestedGenes=mydata,whichOnto="BP",TestMthod="Hyper",DegreeMethod="ED",mapping="org.Hs.eg.db",ID = "symbol",nodeSize = 5)   ##Annotate genes, calculate enrichment test, calculate enrich degree. 
#threshold=0.05/dim(rawTandDresults)[1]     ##Bonferroni correction threshold
#results<-FilterTandDresults(rawTandDresults=rawTandDresults,nodeSize = 20,nodeInterestedGeneSize=20,Pvalue=threshold)    ##Filter results
#PlotGOgraph(StandFormatData=results,whichOnto="BP",Colorgroup="red",NodeColorBasedOn="EnrichDegree",shape="ellipse",fontsize=12,ShowAnnoGeneNum=TRUE,ShowTestPvalue=TRUE,ShowDegree=TRUE)       ##plot a GO tree / fill color based on the values of ED

########---- Example3 ----
#######In this example, all the codes were commented. To do so, we will save the running time of checking EDanalysis package (R CMD check --timings) when we submit the package to CRAN. 
#######User can run these codes by removing the comment "#" (located in the beginning of each line).

#mydata<-EDAnnoDataExample ##Import example data
#AnnResults<-AnnGeneToGO(myInterestedGenes=mydata,whichOnto="BP",mapping="org.Hs.eg.db",ID = "symbol",nodeSize = 5)     ##Annotate genes
#EDResult<-sapply(1:dim(AnnResults)[1],function(x)ED(nN = AnnResults[x,4],nK = AnnResults[x,5],nG = AnnResults[x,6],ng = AnnResults[x,7]))    ##calculate ED
#EDResult
}
