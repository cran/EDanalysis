\name{EDAnnoDataExample}
\alias{EDAnnoDataExample}
\docType{data}
\title{
Data Example for Annotation
}
\description{
A data example for gene annotation.
}
\usage{data(EDAnnoDataExample)}
\format{
  A data frame with 116 observations on the following variable.
  \describe{
    \item{\code{rawdata}}{a factor with levels \code{ACTL6A} \code{AGRIN} \code{ANLN} \code{ASPM} \code{ATAD2} \code{ATP6V1C1} \code{ATR} \code{AURKA} \code{AURKB} \code{BIRC5} \code{BUB1} \code{BUB1B} \code{C20orf3} \code{CBX3} \code{CCNB1} \code{CDC2} \code{CDC20} \code{CDH11} \code{CDKN3} \code{CENPA} \code{CENPF} \code{CEP55} \code{CKS1B} \code{CKS2} \code{COL11A1} \code{COL1A1} \code{COL1A2} \code{COL3A1} \code{COL5A2} \code{COL7A1} \code{CSPG2} \code{CTHRC1} \code{DLGAP5} \code{DNMT1} \code{DPY19L4} \code{DTL} \code{ECT2} \code{EFNA1} \code{FANCI} \code{FEN1} \code{FOXM1} \code{FSCN1} \code{FZD6} \code{GINS1} \code{GMNN} \code{GMPS} \code{HMGB3} \code{HOMER3} \code{HPRT1} \code{HSP90AA1} \code{HSPE1} \code{HSPH1} \code{IGF2BP2} \code{ITPR3} \code{KIF14} \code{KIF20A} \code{KIF4A} \code{LAMB3} \code{LAPTM4B} \code{LRRC8D} \code{MAD2L1} \code{MARCKSL1} \code{MCM2} \code{MCM4} \code{MCM5} \code{MCM6} \code{MELK} \code{MEST} \code{MET} \code{MFAP2} \code{MMP1} \code{MSH6} \code{MTHFD1L} \code{MTHFD2} \code{NEK2} \code{NETO2} \code{NUF2} \code{NUP107} \code{NUSAP1} \code{PCNA} \code{PLAU} \code{PLXNA1} \code{POSTIN} \code{PPIL5} \code{PRC1} \code{PRKDC} \code{PTDSS1} \code{PTK7} \code{RAD51AP1} \code{RANBP1} \code{RFC4} \code{SERPINH1} \code{SLC12A4} \code{SLC16A1} \code{SLC20A1} \code{SLC33A1} \code{SMC2} \code{SNAI2} \code{SOX4} \code{SPARC} \code{SPP1} \code{SR140} \code{SULF1} \code{TBL1XR1} \code{TFRC} \code{THBS2} \code{TMEM185B} \code{TOP2A} \code{TOPBP1} \code{TPX2} \code{TRIP13} \code{TTK} \code{UBE2T} \code{UHRF1} \code{VOPP1} \code{ZNF281}}
  }
}
\details{
This data can be used to carry out gene annotation, enrichment test and enrichment degree test, plot a GO tree. 
}
\seealso{
\code{\link{ED}}, \code{\link{ER}}, \code{\link{EP}}, \code{\link{RE}}, \code{\link{EnrichHyper}}, \code{\link{EnrichFisher}}, \code{\link{AnnGeneToGO}}, \code{\link{EnrichTandD}}, \code{\link{FilterTandDresults}}, \code{\link{PlotGOgraph}}, \code{\link{EDDataExample}}, \code{\link{EDAnnoDataExample}}
}
\source{
Table 2 in the following reference:

Su H, Hu N, Yang HH, Wang C, Takikita M, et al. (2011) Global gene expression profiling and validation in esophageal squamous cell carcinoma and its association with clinical phenotypes. Clin Cancer Res 17: 2955-2966.
}
\references{
Su H, Hu N, Yang HH, Wang C, Takikita M, et al. (2011) Global gene expression profiling and validation in esophageal squamous cell carcinoma and its association with clinical phenotypes. Clin Cancer Res 17: 2955-2966.
}
\examples{
####---- Example1 ----
#######In this example, all the codes were commented. To do so, we will save the running time of checking EDanalysis package (R CMD check --timings) when we submit the package to CRAN. 
#######User can run these codes by removing the comment "#" (located in the beginning of each line).

#library(graph)##need packages
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
#rawTandDresults<-EnrichTandD(myInterestedGenes=mydata,whichOnto="BP",TestMthod="Hyper",TestThreshold=0.05,DegreeMethod="ED",mapping="org.Hs.eg.db",ID = "symbol",nodeSize = 5)  ##Annotate genes, calculate enrichment test, calculate enrich degree. 
#threshold=0.05/dim(rawTandDresults)[1]    ##Bonferroni correction threshold
#results<-FilterTandDresults(rawTandDresults=rawTandDresults,nodeSize = 20,nodeInterestedGeneSize=20,Pvalue=threshold)    ##Filter results
#PlotGOgraph(StandFormatData=results,whichOnto="BP",Colorgroup="red",NodeColorBasedOn="EnrichDegree",shape="ellipse",fontsize=12,ShowAnnoGeneNum=TRUE,ShowTestPvalue=TRUE,ShowDegree=TRUE)    ##plot a GO tree / fill color based on the values of ED


####---- Example2 ----
#######In this example, all the codes were commented. To do so, we will save the running time of checking EDanalysis package (R CMD check --timings) when we submit the package to CRAN. 
#######User can run these codes by removing the comment "#" (located in the beginning of each line).

#data(EDAnnoDataExample) ##Import example data
#mydata<-EDAnnoDataExample #Import example data
#AnnResults<-AnnGeneToGO(myInterestedGenes=mydata,whichOnto="BP",mapping="org.Hs.eg.db",ID = "symbol",nodeSize = 5)  ##Annotate genes
#EDResult<-sapply(1:dim(AnnResults)[1],function(x)ED(nN = AnnResults[x,4],nK = AnnResults[x,5],nG = AnnResults[x,6],ng = AnnResults[x,7]))  ##calculate ED
#EDResult
}
\keyword{datasets}
