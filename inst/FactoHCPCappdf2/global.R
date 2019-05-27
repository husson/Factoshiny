#global script for HCPC for dataframe2
if(is.data.frame(x)==TRUE){
  quantiHCPCshiny=c()
  qualiHCPCshiny=c()
  posi=c()
  for ( i in 1:dim(x)[2]){
    if (is.numeric(x[,i])==TRUE){
      quantiHCPCshiny=c(quantiHCPCshiny,colnames(x)[i])
    }
    else{
      qualiHCPCshiny=c(qualiHCPCshiny,colnames(x)[i])
    }
  }
  VariableChoices=quantiHCPCshiny
  nom=rownames(x)
  nbindivHCPCshiny=length(nom)
  num=c(1:length(nom))
  QualiChoice=qualiHCPCshiny
  IdChoices=c(1:length(VariableChoices))
  Idqualisup=c(1:length(QualiChoice))
  
  nomDataHCPCshiny=nomDataHCPCshiny
  consolidfHCPCshiny=FALSE
  metricdfHCPCshiny=gettext("Euclidean")
  drawdfHCPCshiny=FALSE
  dfHCPCshiny=FALSE
  centerdfHCPCshiny=FALSE
  numdfHCPCshiny=60
  nb1dfHCPCshiny=1
  nb2dfHCPCshiny=2
  title1HCPCshiny=gettext("Hierarchical tree on the factor map")
  title2HCPCshiny=gettext("Factor map")
  title3HCPCshiny=gettext("Hierarchical tree")
}

if(is.data.frame(x)==FALSE){

if(inherits(x, "HCPCshiny")){
  nomDataHCPCshiny=x$nomDataHCPCshiny
  clustdfHCPCshiny=x$clust
  consolidfHCPCshiny=x$consoli
  metricdfHCPCshiny=x$metric
  drawdfHCPCshiny=x$drawtree
  dfHCPCshiny=x$nom3D
  centerdfHCPCshiny=x$center
  numdfHCPCshiny=x$num
  nb1dfHCPCshiny=x$nb1
  nb2dfHCPCshiny=x$nb2
  x=x$data
  title1HCPCshiny=x$title1HCPCshiny
  title2HCPCshiny=x$title2HCPCshiny
  title3HCPCshiny=x$title3HCPCshiny
}

if(inherits(x, "HCPC")){
  nomDataHCPCshiny=x$call$call[2]
  clustdfHCPCshiny=x$call$t$nb.clust
  consolidfHCPCshiny=FALSE
  if(x$call$t$tree["dist.method"]=="euclidean"){
    metricdfHCPCshiny=gettext("Euclidean")
  }
  if(x$call$t$tree["dist.method"]=="manhattan"){
    metricdfHCPCshiny="Manhattan"
  }
  drawdfHCPCshiny=FALSE
  dfHCPCshiny=FALSE
  centerdfHCPCshiny=FALSE
  numdfHCPCshiny=60
  nb1dfHCPCshiny=1
  nb2dfHCPCshiny=2
  title1HCPCshiny=gettext("Hierarchical tree on the factor map")
  title2HCPCshiny=gettext("Factor map")
  title3HCPCshiny=gettext("Hierarchical tree")
}


quantiHCPCshiny=c()
qualiHCPCshiny=c()
posi=c()
for ( i in 1:dim(x)[2]){
  if (is.numeric(x[,i])==TRUE){
    quantiHCPCshiny=c(quantiHCPCshiny,colnames(x)[i])
  }
  else{
    qualiHCPCshiny=c(qualiHCPCshiny,colnames(x)[i])
  }
}

VariableChoices=quantiHCPCshiny
nom=rownames(x)
nbindivHCPCshiny=length(nom)
num=c(1:length(nom))
QualiChoice=qualiHCPCshiny
IdChoices=c(1:length(VariableChoices))
Idqualisup=c(1:length(QualiChoice))
}
nomDataHCPCshiny=unlist(strsplit(as.character(nomDataHCPCshiny),"\\["))[1]