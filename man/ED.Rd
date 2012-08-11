\name{ED}
\alias{ED}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Enrichment Disequilibrium coefficient ED}
\description{
ED (Enrichment Disequilibrium) is an indicator to measure the degree of enrichment.
}
\usage{
ED(nN, nK, nG, ng)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nN}{
The total number of genes in the background distribution.
}
  \item{nK}{
The number of 'interesting' genes in an 'interesting' gene list.
}
  \item{nG}{
The total number of genes in the known functional gene set.
}
  \item{ng}{
The number of 'interesting' genes in a known functional gene set.
}
}
\details{
Enrichment equilibrium means that the interesting gene set K and the known functional gene set G (such as a GO term) are independent (i.e. random association). 

We will get the following expression: P(KG)=P(K)P(G)

If there is non-random association between the interesting gene set K and the known functional gene set G, we define the ed as follow:

ed=P(KG)-P(K)P(G)

We standardize the ed as follows:

ED=ed/ed_max

The detailed algorithm see references.
}
\value{
ED is a standardized indicator (range from -1 to +1). The interval is symmetrical about 0. 

There were three special reference values: 

ED=1 (upper bound) represents the strongest association between the 'interesting' gene set and the known functional gene set; 

ED=0 represents the two sets are independent; 

ED=-1 (lower bound) represents the two sets are incompatible. 

For a given ED, we could know how far it is from the strongest degree of enrichment (ED=1).
}
\references{
Y. Jiang, M. Zhang, X. Guo, R. Zhang, Enrichment Disequilibrium: A novel approach for measuring the degree of enrichment after gene enrichment test, Biochem Biophys Res Commun. 2012; 424:563-567.
}
\author{
Yongshuai Jiang <jiangyongshuai@gmail.com>
}
\note{
ED is a standardized indicator (range from -1 to +1).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ED}}, \code{\link{ER}}, \code{\link{EP}}, \code{\link{RE}}, \code{\link{EnrichHyper}}, \code{\link{EnrichFisher}}, \code{\link{AnnGeneToGO}}, \code{\link{EnrichTandD}}, \code{\link{FilterTandDresults}}, \code{\link{PlotGOgraph}}, \code{\link{EDDataExample}}, \code{\link{EDAnnoDataExample}}
}
\examples{
##---- Examples ----
##Import example data
data(EDDataExample)
data<-EDDataExample #Import example data
####

####For a single value
result1<-ED(nN = data[1,1],nK = data[1,2],nG = data[1,3],ng = data[1,4]) #return 0.4011405
result1
#or
result2<-ED(data[1,1],data[1,2],data[1,3],data[1,4]) #return 0.4011405
result2
####

####For a data.frame
result3<-sapply(1:dim(data)[1],function(x)ED(nN = data[x,1],nK = data[x,2],nG = data[x,3],ng = data[x,4]))
result3
#or
result4<-sapply(1:dim(data)[1],function(x)ED(data[x,1],data[x,2],data[x,3],data[x,4]))
result4
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Enrichment Disequilibrium}
\keyword{Enrichment Analysis}
