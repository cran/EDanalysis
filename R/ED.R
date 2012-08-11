ED <-
function(nN,nK,nG,ng){


  if(ng>nG || ng>nK){
       stop("Error in ED(nN=BackgroundGeneNum,nK=InterestedGeneNum,nG=FunctionalgeneNum,ng=AnnotatedGeneNum)! ng should not be greater than nG or nK!")
       }
  if(ng>nN){
       stop("Error in ED(nN=BackgroundGeneNum,nK=InterestedGeneNum,nG=FunctionalgeneNum,ng=AnnotatedGeneNum) error! ng should not be greater than nN!")
       }
  if(nK>nN){
       stop("Error in ED(nN=BackgroundGeneNum,nK=InterestedGeneNum,nG=FunctionalgeneNum,ng=AnnotatedGeneNum) error! nK should not be greater than nN!")
       }
  if(nG>nN){
       stop("Error in ED(nN=BackgroundGeneNum,nK=InterestedGeneNum,nG=FunctionalgeneNum,ng=AnnotatedGeneNum) error! nG should not be greater than nN!")
       }

       P_K=nK/nN;
       P_G=nG/nN;
       P_KG=ng/nN;
       ed=P_KG-P_K*P_G;
       if(ed>0 && nG>=nK){
            ED=ed/(P_K*(1-P_G));
          }
       if(ed>0 && nG<nK){
            ED=ed/((1-P_K)*P_G);
          }
       if(ed<0){
             ED=ed/(P_K*P_G);
          }
       if(ed==0){
              ED=0;
          }
       return(ED);
}
