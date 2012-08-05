\name{AnnGeneToGO}
\alias{AnnGeneToGO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Annotate Genes To GO 
}
\description{
annotate interesting gene list to GO
}
\usage{
AnnGeneToGO(myInterestedGenes = mydata, whichOnto = "BP", mapping = "org.Hs.eg.db", ID = "symbol", nodeSize = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{myInterestedGenes}{
should be a data.frame, one column.
}
  \item{whichOnto}{
character string specifying one of the three GO ontologies, namely: "BP", "MF", "CC".
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
annotate interesting gene list to a GO ontologie: "BP", "MF" or "CC"
}
\value{
The results of annotation will include 7 columns:

Column 1: GO term ID.

Column 2: GO term name.

Column 3: GO term level.

Column 4: nN-The total number of genes in the background distribution.

Column 5: nK-The number of 'interesting' genes in an 'interesting' gene list.

Column 6: nG-The total number of genes in the known functional gene set.

Column 7: ng-The number of 'interesting' genes in a known functional gene set.

The column 4, 5, 6, 7 (nN, nK, nG, ng) could be used to calculate ED, ER, EP, RE, EnrichHyper and EnrichFisher. 
}
\references{
The Bioconductor package topGO (available at http://bioconductor.org/packages/release/bioc/html/topGO.html)
}
\author{
Yongshuai Jiang <jiangyongshuai@gmail.com>
}
\note{
return annotation results.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ED}}, \code{\link{ER}}, \code{\link{EP}}, \code{\link{RE}}, \code{\link{EnrichHyper}}, \code{\link{EnrichFisher}}, \code{\link{AnnGeneToGO}}, \code{\link{EnrichTandD}}, \code{\link{FilterTandDresults}}, \code{\link{PlotGOgraph}}, \code{\link{EDDataExample}}, \code{\link{EDAnnoDataExample}}
}
\examples{
####---- Examples ----
#######In this example, all the codes were commented. To do so, we will save the running time of checking EDanalysis package (R CMD check --timings) when we submit the package to CRAN. 
#######User can run these codes by removing the comment "#" (located in the beginning of each line).

#library(graph)  ##need packages...
#library(Biobase)
#library(DBI)
#library(RSQLite)
#library(AnnotationDbi)
#library(GO.db)
#library(SparseM)
#library(topGO)
#data(EDAnnoDataExample)   ##Import example data
#mydata<-EDAnnoDataExample #Import example data
#AnnResults<-AnnGeneToGO(myInterestedGenes=mydata,whichOnto="BP",mapping="org.Hs.eg.db",ID = "symbol",nodeSize = 5)    ##Annotate genes
#AnnResults
#EDResult<-sapply(1:dim(AnnResults)[1],function(x)ED(nN = AnnResults[x,4],nK = AnnResults[x,5],nG = AnnResults[x,6],ng = AnnResults[x,7]))   ##calculate ED
#EDResult
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Enrichment Analysis }
\keyword{ gene annotation }% __ONLY ONE__ keyword per line
