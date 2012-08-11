\name{EnrichTandD}
\alias{EnrichTandD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Enrichment Test and Enrichment Degree Indicator
}
\description{
input a list of genes, calculate enrichment test and enrichment degree indicator
}
\usage{
EnrichTandD(myInterestedGenes, whichOnto = "BP", TestMthod = "Hyper", DegreeMethod = "ED", mapping = "org.Hs.eg.db", ID = "symbol", nodeSize = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{myInterestedGenes}{
should be a data.frame, one column.
}
  \item{whichOnto}{
character string specifying one of the three GO ontologies, namely: "BP", "MF", "CC".
}
  \item{TestMthod}{
character string specifieng gene enrichment test to use. Currently only the following methods can be used: c("Hyper", "Fisher")
}
  \item{DegreeMethod}{
character string specifieng gene enrichment degree to use. Currently only the following methods can be used: c("ED", "ER", "EP", "RE")
}
  \item{mapping}{
character string specifieng the name of the Bioconductor package containing the gene mappings for a specific organism. For example: mapping = "org.Hs.eg.db"
}
  \item{ID}{
character string specifing the gene identifier to use. Currently only the following identifiers can be used: c("entrez", "genbank", "alias", "ensembl", "symbol", "genename", "unigene")
}
  \item{nodeSize}{
numeric, the number of genes in a GO term. For example: nodeSize=5 represents each node contains at least five genes.
}
}
\details{
There are two steps in this process: 
(1) Gene annotation using topGO package.This will take a few minutes.
(2)Calculating enrichment test (such as Hypergeometric distribution and Fisher's exact test) p-value and enrichment degree indicator(such as ED, ER, EP and RE).
}
\value{
The results of annotation will include 9 columns:

Column 1: GO term ID.

Column 2: GO term name.

Column 3: GO term level.

Column 4: nN-The total number of genes in the background distribution.

Column 5: nK-The number of interesting genes in an interesting gene list.

Column 6: nG-The total number of genes in the known functional gene set.

Column 7: ng-The number of interesting genes in a known functional gene set.

Column 8: P-value of enrichment test (Hypergeometric distribution or Fisher's exact test).

Column 9: value of enrichment degree (ED, ER, EP or RE)

The returned results can be used for any further analysis.
}
\references{
Y. Jiang, M. Zhang, X. Guo, R. Zhang, Enrichment Disequilibrium: A novel approach for measuring the degree of enrichment after gene enrichment test, Biochem Biophys Res Commun. 2012; 424:563-567.

The Bioconductor package topGO (available at http://bioconductor.org/packages/release/bioc/html/topGO.html)

The Bioconductor package GOstats (available at http://www.bioconductor.org/packages/release/bioc/html/GOstats.html)
}
\author{
Yongshuai Jiang <jiangyongshuai@gmail.com>
}
\note{
The teturned result can be further filtered by function "FilterTandDresults". 

See examples.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ED}}, \code{\link{ER}}, \code{\link{EP}}, \code{\link{RE}}, \code{\link{EnrichHyper}}, \code{\link{EnrichFisher}}, \code{\link{AnnGeneToGO}}, \code{\link{EnrichTandD}}, \code{\link{FilterTandDresults}}, \code{\link{PlotGOgraph}}, \code{\link{EDDataExample}}, \code{\link{EDAnnoDataExample}}
}

\examples{
####---- Example ----
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
#data(EDAnnoDataExample)   ##Import example data
#mydata<-EDAnnoDataExample #Import example data
#rawTandDresults<-EnrichTandD(myInterestedGenes=mydata,whichOnto="BP",TestMthod="Hyper",DegreeMethod="ED",mapping="org.Hs.eg.db",ID = "symbol",nodeSize = 5)  ##Annotate genes, calculate enrichment test, calculate enrich degree. 
#####--------Further analysis example---------
#threshold=0.05/dim(rawTandDresults)[1]  ##Bonferroni correction threshold
#results<-FilterTandDresults(rawTandDresults=rawTandDresults,nodeSize = 20,nodeInterestedGeneSize=20,Pvalue=threshold)    ##Filter results
#PlotGOgraph(StandFormatData=results,whichOnto="BP",Colorgroup="red",NodeColorBasedOn="EnrichDegree",shape="ellipse",fontsize=12,ShowAnnoGeneNum=TRUE,ShowTestPvalue=TRUE,ShowDegree=TRUE)      ##plot a GO tree / fill color based on the values of ED

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
