\name{EDDataExample}
\alias{EDDataExample}
\docType{data}
\title{
Data Example For ED 
}
\description{
A data example for the calculation of ED. 
}
\usage{data(EDDataExample)}
\format{
  A data frame with 22 observations on the following 4 variables.
  \describe{
    \item{\code{nN}}{a numeric vector}
    \item{\code{nK}}{a numeric vector}
    \item{\code{nG}}{a numeric vector}
    \item{\code{ng}}{a numeric vector}
  }
}
\details{
This data can be used to calculate ED, ER, EP, RE, EnrichHyper and EnrichFisher. 
}
\seealso{
\code{\link{ED}}, \code{\link{ER}}, \code{\link{EP}}, \code{\link{RE}}, \code{\link{EnrichHyper}}, \code{\link{EnrichFisher}}, \code{\link{AnnGeneToGO}}, \code{\link{EnrichTandD}}, \code{\link{FilterTandDresults}}, \code{\link{PlotGOgraph}}, \code{\link{EDDataExample}}, \code{\link{EDAnnoDataExample}}
}
\source{
 Some annotation results of genes in Table 2 in the following reference:

Su H, Hu N, Yang HH, Wang C, Takikita M, et al. (2011) Global gene expression profiling and validation in esophageal squamous cell carcinoma and its association with clinical phenotypes. Clin Cancer Res 17: 2955-2966.

}
\references{
Su H, Hu N, Yang HH, Wang C, Takikita M, et al. (2011) Global gene expression profiling and validation in esophageal squamous cell carcinoma and its association with clinical phenotypes. Clin Cancer Res 17: 2955-2966.
}
\examples{
##---- Example----
##Import example data
data(EDDataExample)
data<-EDDataExample #Import example data
####For a single value
result1<-ED(nN = data[1,1],nK = data[1,2],nG = data[1,3],ng = data[1,4]) #return 0.4011405
result1
result2<-ER(nN = data[1,1],nK = data[1,2],nG = data[1,3],ng = data[1,4]) #return 0.1118446
result2
result3<-EP(nN = data[1,1],nK = data[1,2],nG = data[1,3],ng = data[1,4]) #return 0.03824701
result3
result4<-RE(nN = data[1,1],nK = data[1,2],nG = data[1,3],ng = data[1,4]) #return 5.246335
result4
result5<-EnrichHyper(nN = data[1,1],nK = data[1,2],nG = data[1,3],ng = data[1,4]) #return 0
result5
result6<-EnrichFisher(nN = data[1,1],nK = data[1,2],nG = data[1,3],ng = data[1,4]) #return 1.04423e-23
result6

####For a data.frame
result7<-sapply(1:dim(data)[1],function(x)ED(nN = data[x,1],nK = data[x,2],nG = data[x,3],ng = data[x,4]))
result7
result8<-sapply(1:dim(data)[1],function(x)ER(nN = data[x,1],nK = data[x,2],nG = data[x,3],ng = data[x,4]))
result8
result9<-sapply(1:dim(data)[1],function(x)EP(nN = data[x,1],nK = data[x,2],nG = data[x,3],ng = data[x,4]))
result9
result10<-sapply(1:dim(data)[1],function(x)RE(nN = data[x,1],nK = data[x,2],nG = data[x,3],ng = data[x,4]))
result10
result11<-sapply(1:dim(data)[1],function(x)EnrichHyper(nN = data[x,1],nK = data[x,2],nG = data[x,3],ng = data[x,4]))
result11
result12<-sapply(1:dim(data)[1],function(x)EnrichFisher(nN = data[x,1],nK = data[x,2],nG = data[x,3],ng = data[x,4]))
result12
}
\keyword{datasets}
