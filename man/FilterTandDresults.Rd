\name{FilterTandDresults}
\alias{FilterTandDresults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Filter the results from function EnrichTandD
}
\description{
Filter the results from function EnrichTandD.
}
\usage{
FilterTandDresults(rawTandDresults = rawTandDresults, nodeSize = 20, nodeInterestedGeneSize = 20, Pvalue = threshold)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawTandDresults}{
data.frame. Must be 9 columns. More details see "details".
}
  \item{nodeSize}{
numeric, the number of genes in a GO term. For example: nodeSize=20 represents each node contains at least 20 genes.
}
  \item{nodeInterestedGeneSize}{
numeric, the number of interesting genes in a GO term. For example: nodeSize=20 represents each node contains at least 20 interesting genes.
}
  \item{Pvalue}{
numeric, the threshold of P-value. In order to determine the threshold, perhaps you will use the multiple testing correction.  
}
}
\details{
The input data "rawTandDresults" must be 9 columns.

You can use the "function EnrichTandD" to generate the input data "rawTandDresults".

You can also build the input data according to your needs. But it must be 9 columns.

Column 1: GO term ID.

Column 2: GO term name.

Column 3: GO term level.

Column 4: nN-The total number of genes in the background distribution.

Column 5: nK-The number of 'interesting' genes in an 'interesting' gene list.

Column 6: nG-The total number of genes in the known functional gene set.

Column 7: ng-The number of 'interesting' genes in a known functional gene set.

Column 8: P-value of enrichment test (Hypergeometric distribution or Fisher's exact test).

Column 9: value of enrichment degree (ED, ER, EP or RE)
}
\value{
The filtered results also include 9 columns:

Column 1: GO term ID.

Column 2: GO term name.

Column 3: GO term level.

Column 4: nN-The total number of genes in the background distribution.

Column 5: nK-The number of interesting genes in an interesting gene list.

Column 6: nG-The total number of genes in the known functional gene set.

Column 7: ng-The number of interesting genes in a known functional gene set.

Column 8: P-value of enrichment test (Hypergeometric distribution or Fisher's exact test).

Column 9: value of enrichment degree (ED, ER, EP or RE)
}
\references{
ER is a standardized indicator (range from -1 to +1). 
}
\author{
Yongshuai Jiang <jiangyongshuai@gmail.com>
}
\note{
The teturned result can be used to plot a GO tree.

See examples. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ED}}, \code{\link{ER}}, \code{\link{EP}}, \code{\link{RE}}, \code{\link{EnrichHyper}}, \code{\link{EnrichFisher}}, \code{\link{AnnGeneToGO}}, \code{\link{EnrichTandD}}, \code{\link{FilterTandDresults}}, \code{\link{PlotGOgraph}}, \code{\link{EDDataExample}}, \code{\link{EDAnnoDataExample}}
}

\examples{
####---- Examples ----
#######In this example, all the codes were commented. To do so, we will save the running time of checking EDanalysis package (R CMD check --timings) when we submit the package to CRAN. 
#######User can run these codes by removing the comment "#" (located in the beginning of each line).

#library(graph) ##need packages
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
#data(EDAnnoDataExample)  ##Import example data
#mydata<-EDAnnoDataExample #Import example data
#rawTandDresults<-EnrichTandD(myInterestedGenes=mydata,whichOnto="BP",TestMthod="Hyper",DegreeMethod="ED",mapping="org.Hs.eg.db",ID = "symbol",nodeSize = 5)      ##Annotate genes, calculate enrichment test, calculate enrich degree. 
#
##########a filter example for this function###############v
#threshold=0.05/dim(rawTandDresults)[1]   ##Bonferroni correction threshold
#results<-FilterTandDresults(rawTandDresults=rawTandDresults,nodeSize = 20,nodeInterestedGeneSize=20,Pvalue=threshold)         ##Filter results
#########################v
#
#####--------a example for further analysis: plot a GO tree---------
#PlotGOgraph(StandFormatData=results,whichOnto="BP",Colorgroup="red",NodeColorBasedOn="EnrichDegree",shape="ellipse",fontsize=12,ShowAnnoGeneNum=TRUE,ShowTestPvalue=TRUE,ShowDegree=TRUE)          ##plot a GO tree / fill color based on the values of ED
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
