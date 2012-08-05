\name{EP}
\alias{EP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Enrichment Percentage)
}
\description{The percentage of the 'interesting' genes in the functional gene set.
}
\usage{
EP(nN = BackgroundGeneNum, nK = InterestedGeneNum, nG = FunctionalgeneNum, ng = AnnotatedGeneNum)
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
The detailed algorithm see references.
}
\value{
The EP range from 0 t0 1.
}
\references{
Y. Jiang, M. Zhang, X. Guo, R. Zhang, Enrichment Disequilibrium: A novel approach for measuring the degree of enrichment after gene enrichment test, Biochem Biophys Res Commun. 2012; 424:563-567.
}
\author{
Yongshuai Jiang <jiangyongshuai@gmail.com>
}
\note{
Enrichment Percentage
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
