EnrichFisher <-
function(nN,nK,nG,ng){

  if(ng>nG || ng>nK){
       stop("Error in EnrichFisher(nN=BackgroundGeneNum,nK=InterestedGeneNum,nG=FunctionalgeneNum,ng=AnnotatedGeneNum)! ng should not be greater than nG or nK!")
       }
  if(ng>nN){
       stop("Error in EnrichFisher(nN=BackgroundGeneNum,nK=InterestedGeneNum,nG=FunctionalgeneNum,ng=AnnotatedGeneNum) error! ng should not be greater than nN!")
       }
  if(nK>nN){
       stop("Error in EnrichFisher(nN=BackgroundGeneNum,nK=InterestedGeneNum,nG=FunctionalgeneNum,ng=AnnotatedGeneNum) error! nK should not be greater than nN!")
       }
  if(nG>nN){
       stop("Error in EnrichFisher(nN=BackgroundGeneNum,nK=InterestedGeneNum,nG=FunctionalgeneNum,ng=AnnotatedGeneNum) error! nG should not be greater than nN!")
       }

       a=nK-ng;
       b=ng;
       c=nN-nK-nG+ng;
       d=nG-ng;
       P_value=fisher.test(matrix(c(a,b,c,d),2,2,byrow=T))$p.value;
       return(P_value);
}
