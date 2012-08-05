ER <-
function(nN=BackgroundGeneNum,nK=InterestedGeneNum,nG=FunctionalgeneNum,ng=AnnotatedGeneNum){
  if(ng>nG || ng>nK){
       stop("Error in ER(nN=BackgroundGeneNum,nK=InterestedGeneNum,nG=FunctionalgeneNum,ng=AnnotatedGeneNum)! ng should not be greater than nG or nK!")
       }
  if(ng>nN){
       stop("Error in ER(nN=BackgroundGeneNum,nK=InterestedGeneNum,nG=FunctionalgeneNum,ng=AnnotatedGeneNum)! ng should not be greater than nN!")
       }
  if(nK>nN){
       stop("Error in ER(nN=BackgroundGeneNum,nK=InterestedGeneNum,nG=FunctionalgeneNum,ng=AnnotatedGeneNum)! nK should not be greater than nN!")
       }
  if(nG>nN){
       stop("Error in ER(nN=BackgroundGeneNum,nK=InterestedGeneNum,nG=FunctionalgeneNum,ng=AnnotatedGeneNum)! nG should not be greater than nN!")
       }

       P_K=nK/nN;
       P_G=nG/nN;
       P_KG=ng/nN;
       ed=P_KG-P_K*P_G;
       ER=ed/sqrt(P_K*(1-P_K)*P_G*(1-P_G));
       return(ER);
}
