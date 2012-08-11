RE <-
function(nN,nK,nG,ng){

  if(ng>nG || ng>nK){
       stop("Error in RE(nN=BackgroundGeneNum,nK=InterestedGeneNum,nG=FunctionalgeneNum,ng=AnnotatedGeneNum)! ng should not be greater than nG or nK!")
       }
  if(ng>nN){
       stop("Error in RE(nN=BackgroundGeneNum,nK=InterestedGeneNum,nG=FunctionalgeneNum,ng=AnnotatedGeneNum)! ng should not be greater than nN!")
       }
  if(nK>nN){
       stop("Error in RE(nN=BackgroundGeneNum,nK=InterestedGeneNum,nG=FunctionalgeneNum,ng=AnnotatedGeneNum)! nK should not be greater than nN!")
       }
  if(nG>nN){
       stop("Error in RE(nN=BackgroundGeneNum,nK=InterestedGeneNum,nG=FunctionalgeneNum,ng=AnnotatedGeneNum)! nG should not be greater than nN!")
       }

       RE=(ng/nG)/(nK/nN);
       return(RE);
}
