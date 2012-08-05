\name{PlotGOgraph}
\alias{PlotGOgraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a GO Tree Graph
}
\description{
plot a GO tree graph
}
\usage{
PlotGOgraph(StandFormatData = myStandData, whichOnto = "BP", Colorgroup = "gray", NodeColorBasedOn = "EnrichDegree", shape = "ellipse", fontsize = 12, ShowAnnoGeneNum = TRUE, ShowTestPvalue = TRUE, ShowDegree = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{StandFormatData}{
data.frame. Must be 9 columns. More details see "details".
}
  \item{whichOnto}{
character string specifying one of the three GO ontologies, namely: "BP", "MF", "CC".
}
  \item{Colorgroup}{
character string specifieng nodes color group to use. Currently only the following color groups can be used: c("gray", "red", "green", "blue", "yellow")
}
  \item{NodeColorBasedOn}{
character string specifieng node color value to use. Currently only the following values can be used: c("EnrichDegree", "EnrichTest").More details see "details".
}
  \item{shape}{
character string specifieng node shape to use.  Currently only the following shapes can be used: c("box", "rectangle", "ellipse", "plaintext", "circle", "triangle")
}
  \item{fontsize}{
numeric,font size in a node.
}
  \item{ShowAnnoGeneNum}{
logical, whether to display the number of interesting genes in the node. Value: TRUE or FALSE.
}
  \item{ShowTestPvalue}{
logical, whether to display the P-value of enrichment test in the node. Value: TRUE or FALSE.
}
  \item{ShowDegree}{
logical, whether to display the value of enrichment degree in the node. Value: TRUE or FALSE.
}
}
\details{
For each color group ("gray", "red", "green", "blue", "yellow"):

(1)If you filled the color based on "EnrichDegree", then: 

The deeper the color, the greater the enrichment degree value.

(2)If you filled the color based on "EnrichTest", then: 

The deeper the color, the smaller the P-value.
}
\value{
return a GO tree
}
\references{
Florian Hahne, Deepayan Sarkar, the R package Rgraphviz.  
}
\author{
Yongshuai Jiang <jiangyongshuai@gmail.com>
}
\note{
A GO slim.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ED}}, \code{\link{ER}}, \code{\link{EP}}, \code{\link{RE}}, \code{\link{EnrichHyper}}, \code{\link{EnrichFisher}}, \code{\link{AnnGeneToGO}}, \code{\link{EnrichTandD}}, \code{\link{FilterTandDresults}}, \code{\link{PlotGOgraph}}, \code{\link{EDDataExample}}, \code{\link{EDAnnoDataExample}}
}

\examples{
####---- Examples1 ----
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
#library(grid)
#library(Rgraphviz)
#library(Category)
#library(GOstats)
#data(EDAnnoDataExample)  ##Import example data
#mydata<-EDAnnoDataExample ##Import example data
#rawTandDresults<-EnrichTandD(myInterestedGenes=mydata,whichOnto="BP",TestMthod="Hyper",TestThreshold=0.05,DegreeMethod="ED",mapping="org.Hs.eg.db",ID = "symbol",nodeSize = 5)         ##Annotate genes, calculate enrichment test, calculate enrich degree. 
#threshold=0.05/dim(rawTandDresults)[1]     ##Bonferroni correction threshold
#results<-FilterTandDresults(rawTandDresults=rawTandDresults,nodeSize = 20,nodeInterestedGeneSize=20,Pvalue=threshold)      ##Filter results
#
##########a example for this function#####
#PlotGOgraph(StandFormatData=results,whichOnto="BP",Colorgroup="red",NodeColorBasedOn="EnrichDegree",shape="ellipse",fontsize=12,ShowAnnoGeneNum=TRUE,ShowTestPvalue=TRUE,ShowDegree=TRUE)     ##plot a GO tree / fill color based on the values of ED
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
