PlotGOgraph <-
function(StandFormatData=myStandData,whichOnto="BP",Colorgroup="gray",NodeColorBasedOn="EnrichDegree",shape="ellipse",fontsize=12,ShowAnnoGeneNum=TRUE,ShowTestPvalue=TRUE,ShowDegree=TRUE){
rownames(StandFormatData)=StandFormatData[,1]
GOIDlist=StandFormatData[,1]
if(whichOnto=="BP"){g1 <- GOGraph(GOIDlist, GOBPPARENTS)}
if(whichOnto=="MF"){g1 <- GOGraph(GOIDlist, GOMFPARENTS)}
if(whichOnto=="CC"){g1 <- GOGraph(GOIDlist, GOCCPARENTS)}

nodes<-nodes(g1)
nAttrs1<-list()
nAttrs2<-list()
nAttrs1$color=rep("black",length(nodes))
names(nAttrs1$color)<-nodes

nAttrs2$fill=rep("transparent",length(nodes))    ##fill must be for nodeRenderInfo£¬else error!
names(nAttrs2$fill)<-nodes
if(NodeColorBasedOn=="EnrichDegree"){
        ColorValue=StandFormatData[,9]
        if(Colorgroup=="gray"){
               maxvalue=max(ColorValue)
               minvalue=min(ColorValue)
               if(maxvalue==minvalue){nAttrs2$fill[GOIDlist]=Colorgroup}else{
               nAttrs2$fill[GOIDlist]=rgb(sapply(ColorValue,function(x) abs(0.6+0.2*(maxvalue-x)/(maxvalue-minvalue))),sapply(ColorValue,function(x) abs(0.6+0.2*(maxvalue-x)/(maxvalue-minvalue))),sapply(ColorValue,function(x) abs(0.6+0.2*(maxvalue-x)/(maxvalue-minvalue))))
                  }
           }
        if(Colorgroup=="red"){
               maxvalue=max(ColorValue)
               minvalue=min(ColorValue)
               if(maxvalue==minvalue){nAttrs2$fill[GOIDlist]=Colorgroup}else{
               nAttrs2$fill[GOIDlist]=rgb(1,sapply(ColorValue,function(x) abs(0.1+0.5*(maxvalue-x)/(maxvalue-minvalue))),sapply(ColorValue,function(x) abs(0.1+0.5*(maxvalue-x)/(maxvalue-minvalue))))
                  }
           }
        if(Colorgroup=="green"){
               maxvalue=max(ColorValue)
               minvalue=min(ColorValue)
               if(maxvalue==minvalue){nAttrs2$fill[GOIDlist]=Colorgroup}else{
               nAttrs2$fill[GOIDlist]=rgb(0,1,sapply(ColorValue,function(x) abs(0.3+0.3*(maxvalue-x)/(maxvalue-minvalue))))
                  }
           }
        if(Colorgroup=="blue"){
               maxvalue=max(ColorValue)
               minvalue=min(ColorValue)
               if(maxvalue==minvalue){nAttrs2$fill[GOIDlist]=Colorgroup}else{
               nAttrs2$fill[GOIDlist]=rgb(0,sapply(ColorValue,function(x) abs(0.5+0.3*(maxvalue-x)/(maxvalue-minvalue))),1)
                  }
           }
        if(Colorgroup=="yellow"){
               maxvalue=max(ColorValue)
               minvalue=min(ColorValue)
               if(maxvalue==minvalue){nAttrs2$fill[GOIDlist]=Colorgroup}else{
               nAttrs2$fill[GOIDlist]=rgb(1,1,sapply(ColorValue,function(x) abs(0.2+0.5*(maxvalue-x)/(maxvalue-minvalue))))
                  }
           }
}
if(NodeColorBasedOn=="EnrichTest"){
        ColorValue=StandFormatData[,8]
        if(Colorgroup=="gray"){
               maxvalue=max(ColorValue)
               minvalue=min(ColorValue)
               if(maxvalue==minvalue){nAttrs2$fill[GOIDlist]=Colorgroup}else{
               nAttrs2$fill[GOIDlist]=rgb(sapply(ColorValue,function(x) abs(0.6+0.2*(x-minvalue)/(maxvalue-minvalue))),sapply(ColorValue,function(x) abs(0.6+0.2*(x-minvalue)/(maxvalue-minvalue))),sapply(ColorValue,function(x) abs(0.6+0.2*(x-minvalue)/(maxvalue-minvalue))))
                  }
           }
        if(Colorgroup=="red"){
               maxvalue=max(ColorValue)
               minvalue=min(ColorValue)
               if(maxvalue==minvalue){nAttrs2$fill[GOIDlist]=Colorgroup}else{
               nAttrs2$fill[GOIDlist]=rgb(1,sapply(ColorValue,function(x) abs(0.1+0.5*(x-minvalue)/(maxvalue-minvalue))),sapply(ColorValue,function(x) abs(0.1+0.5*(x-minvalue)/(maxvalue-minvalue))))
                  }
           }
        if(Colorgroup=="green"){
               maxvalue=max(ColorValue)
               minvalue=min(ColorValue)
               if(maxvalue==minvalue){nAttrs2$fill[GOIDlist]=Colorgroup}else{
               nAttrs2$fill[GOIDlist]=rgb(0,1,sapply(ColorValue,function(x) abs(0.3+0.3*(x-minvalue)/(maxvalue-minvalue))))
                  }
           }
        if(Colorgroup=="blue"){
               maxvalue=max(ColorValue)
               minvalue=min(ColorValue)
               if(maxvalue==minvalue){nAttrs2$fill[GOIDlist]=Colorgroup}else{
               nAttrs2$fill[GOIDlist]=rgb(0,sapply(ColorValue,function(x) abs(0.5+0.3*(x-minvalue)/(maxvalue-minvalue))),1)
                  }
           }
        if(Colorgroup=="yellow"){
               maxvalue=max(ColorValue)
               minvalue=min(ColorValue)
               if(maxvalue==minvalue){nAttrs2$fill[GOIDlist]=Colorgroup}else{
               nAttrs2$fill[GOIDlist]=rgb(1,1,sapply(ColorValue,function(x) abs(0.2+0.5*(x-minvalue)/(maxvalue-minvalue))))
                  }
           }
}
nAttrs1$fixedsize=rep("FALSE",length(nodes))  ##fixedsize must be for layoutGraph£¬else error!
names(nAttrs1$fixedsize)<-nodes

nAttrs1$shape=rep(shape,length(nodes))    ##shape must be for layoutGraph£¬else error!

names(nAttrs1$shape)<-nodes

for (i in 1:length(nodes)){
nodes[i]=paste(AnnotationDbi::Term(nodes[i]),names(nodes[i]),sep="\n")
}
for (i in 1: length(GOIDlist)){
nodes[GOIDlist[i]]=paste(nodes[GOIDlist[i]]," ","(","nG=",StandFormatData[GOIDlist[i],6],sep="")
}
if(ShowAnnoGeneNum){
   for (i in 1: length(GOIDlist)){
   nodes[GOIDlist[i]]=paste(nodes[GOIDlist[i]],",","ng=",StandFormatData[GOIDlist[i],7],sep="")
    }
}
if(ShowTestPvalue){
   for (i in 1: length(GOIDlist)){         
      if(StandFormatData[GOIDlist[i],8]>0.009999){                #format decimal point
		 PlotPvalue = sprintf("%.2f",StandFormatData[GOIDlist[i],8]);
	     }else{
                 PlotPvalue = sprintf("%.2e",StandFormatData[GOIDlist[i],8]);
	    }   
   nodes[GOIDlist[i]]=paste(nodes[GOIDlist[i]],",","P=",PlotPvalue,sep="")
    }
}
if(ShowDegree){
   for (i in 1: length(GOIDlist)){
      if(StandFormatData[GOIDlist[i],9]>0.009999){                #format decimal point
		 PlotDegree = sprintf("%.2f",StandFormatData[GOIDlist[i],9]);
	     }else{
                 PlotDegree = sprintf("%.2e",StandFormatData[GOIDlist[i],9]);
	    }  
   nodes[GOIDlist[i]]=paste(nodes[GOIDlist[i]],",","D=",PlotDegree,sep="")
    }
}
for (i in 1: length(GOIDlist)){
nodes[GOIDlist[i]]=paste(nodes[GOIDlist[i]],")",sep="")
}

nodes["all"]="all"

nAttrs1$label=nodes                     ##label must be for layoutGraph£¬else error!
nAttrs2$fontsize=rep(fontsize,length(nodes))  ##font must be for nodeRenderInfo£¬else error!
names(nAttrs2$fontsize)<-names(nodes)

x<-layoutGraph(g1,nodeAttrs=nAttrs1,layoutType="dot")
nodeRenderInfo(x)<-nAttrs2
renderGraph(x)
}
