#global script for HCPC for dataframe2
if(is.data.frame(x)==TRUE){
  quanti=c()
  quali=c()
  posi=c()
  for ( i in 1:dim(x)[2]){
    if (is.numeric(x[,i])==TRUE){
      quanti=c(quanti,colnames(x)[i])
    }
    else{
      quali=c(quali,colnames(x)[i])
    }
  }
  VariableChoices=quanti
  nom=rownames(x)
  nbindiv=length(nom)
  num=c(1:length(nom))
  QualiChoice=quali
  IdChoices=c(1:length(VariableChoices))
  Idqualisup=c(1:length(QualiChoice))
  
  nomData=nomData
  consolidf=FALSE
  metricdf=gettext("Euclidean")
  drawdf=FALSE
  df=FALSE
  centerdf=FALSE
  numdf=60
  nb1df=1
  nb2df=2
  title1=gettext("Hierarchical tree on the factor map")
  title2=gettext("Factor map")
  title3=gettext("Hierarchical tree")
}

if(is.data.frame(x)==FALSE){

if(inherits(x, "HCPCshiny")){
  nomData=x$nomData
  clustdf=x$clust
  consolidf=x$consoli
  metricdf=x$metric
  drawdf=x$drawtree
  df=x$nom3D
  centerdf=x$center
  numdf=x$num
  nb1df=x$nb1
  nb2df=x$nb2
  x=x$data
  title1=x$title1
  title2=x$title2
  title3=x$title3
}

if(inherits(x, "HCPC")){
  nomData=x$call$call[2]
  clustdf=x$call$t$nb.clust
  consolidf=FALSE
  if(x$call$t$tree["dist.method"]=="euclidean"){
    metricdf=gettext("Euclidean")
  }
  if(x$call$t$tree["dist.method"]=="manhattan"){
    metricdf="Manhattan"
  }
  drawdf=FALSE
  df=FALSE
  centerdf=FALSE
  numdf=60
  nb1df=1
  nb2df=2
  title1=gettext("Hierarchical tree on the factor map")
  title2=gettext("Factor map")
  title3=gettext("Hierarchical tree")
}


quanti=c()
quali=c()
posi=c()
for ( i in 1:dim(x)[2]){
  if (is.numeric(x[,i])==TRUE){
    quanti=c(quanti,colnames(x)[i])
  }
  else{
    quali=c(quali,colnames(x)[i])
  }
}

VariableChoices=quanti
nom=rownames(x)
nbindiv=length(nom)
num=c(1:length(nom))
QualiChoice=quali
IdChoices=c(1:length(VariableChoices))
Idqualisup=c(1:length(QualiChoice))
}
nomData=unlist(strsplit(as.character(nomData),"\\["))[1]