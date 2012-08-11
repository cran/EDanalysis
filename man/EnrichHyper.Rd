\name{EnrichHyper}
\alias{EnrichHyper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hypergeometric distribution test in enrichment analysis
}
\description{
Test gene enrichment using the hypergeometric distribution.
}
\usage{
EnrichHyper(nN, nK, nG, ng)
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
The detailed algorithm see references and the R function \code{\link{phyper}}. 
}
\value{
p-value
}
\references{
D.L. Gold, K.R. Coombes, J. Wang, B. Mallick, Enrichment analysis in high-throughput genomics - accounting for dependency in the NULL, Brief Bioinform 8 (2007) 71-77.
}
\author{
Yongshuai Jiang <jiangyongshuai@gmail.com>
}
\note{
Sometimes, multiple testing correction is needed.
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
result1<-EnrichHyper(nN = data[1,1],nK = data[1,2],nG = data[1,3],ng = data[1,4]) #return 0
result1
#or
result2<-EnrichHyper(data[1,1],data[1,2],data[1,3],data[1,4]) #return 0
result2
####

####For a data.frame
result3<-sapply(1:dim(data)[1],function(x)EnrichHyper(nN = data[x,1],nK = data[x,2],nG = data[x,3],ng = data[x,4]))
result3
#or
result4<-sapply(1:dim(data)[1],function(x)EnrichHyper(data[x,1],data[x,2],data[x,3],data[x,4]))
result4
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Enrichment Analylsis}
\keyword{Hypergeometric distribution}% __ONLY ONE__ keyword per line
