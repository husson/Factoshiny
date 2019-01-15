
#global script for MCA
if(is.data.frame(x)==TRUE){
  newdata=x
  nomData=nomData
  axe1=1
  axe2=2
  varsup=c(gettext("Active qualitative variables"),gettext("Supplementary qualitative variables"),gettext("Supplementary quantitative variables"))
#  indvar=c("Ind","Mod","Modsup","Indsup")
  indvar=c(gettext("Individuals"),gettext("Categories"),gettext("Supplementary categories"),gettext("Supplementary individuals"))
  labvar=c()
  habillageind=NULL
  selection=gettext("No selection")
  selection2=NULL
  selection3=gettext("No selection")
  selection4=NULL
  
  quanti=names(which(sapply(newdata,is.numeric)))
  quali=names(which(!(sapply(newdata,is.numeric))))
  supquali=NULL
  quantiS=NULL
#  varsup=c(gettext("Active qualitative variables"))
  # indvar=c("Ind","Mod","Indsup","Modsup")
  # labvar=c("Ind","Mod","Indsup","Modsup")
  # indvar=c(gettext("Individuals"),gettext("Categories"),gettext("Supplementary individuals"),gettext("Supplementary categories"))
  labvar=c(gettext("Individuals"),gettext("Categories"),gettext("Supplementary individuals"),gettext("Supplementary categories"))
  indsupl=NULL
  title1=gettext("MCA factor map")
  title2=gettext("Graph of the variables on the MCA map")
  title3=gettext("Graph of the supplementary quantitative variables")
  color1="blue"
  color2="darkblue"
  color3="red"
  color4="darkgreen"
  color5="red"
  color6="darkgreen"
  color7="blue"
  color8="blue"
  valdef=FALSE
}
if(inherits(x, "MCA")||inherits(x, "MCAshiny")){

if(inherits(x, "MCAshiny")){
  ###
  newdata=x$data
  nomData=x$nomData
  quantiS=x$b
  supquali=x$c
  varsup=x$z
  indvar=x$y
  indsupl=x$d
  axe1=x$e
  axe2=x$f
  habillageind=x$g
  selection=x$h
  selection2=x$i
  selection3=x$j
  selection4=x$k
  title1=x$title1
  title2=x$title2
  title3=x$title3
  color1=x$color1
  color2=x$color2
  color3=x$color3
  color4=x$color4
  color5=x$color5
  color6=x$color6
  color7=x$color7
  color8=x$color8
  valdel=x$valdef
}
if(inherits(x, "MCA")){
  nomData=as.character(x$call$call[2])
#  nomData=unlist(strsplit(nomData, split='[', fixed=TRUE))[1]
  newdata=x$call$X
  quantiS=rownames(x$quanti.sup$coord)
  supquali=rownames(x$quali.sup$coord)
  indsupl=rownames(x$ind.sup$coord)
  axe1=1
  axe2=2
  varsup=c(gettext("Active qualitative variables"),gettext("Supplementary qualitative variables"),gettext("Supplementary quantitative variables"))
  indvar=c(gettext("Individuals"),gettext("Categories"),gettext("Supplementary categories"),gettext("Supplementary individuals"))
  labvar=c(gettext("Individuals"),gettext("Categories"),gettext("Supplementary individuals"),gettext("Supplementary categories"))
  # indvar=c("Ind","Mod","Modsup","Indsup")
  # labvar=c("Ind","Mod","Indsup","Modsup")
  habillageind=NULL
  selection=gettext("No selection")
  selection2=NULL
  selection3=gettext("No selection")
  selection4=NULL
  title1=gettext("MCA factor map")
  title2=gettext("Graph of the variables on the MCA map")
  title3=gettext("Graph of the supplementary quantitative variables")
  color1="blue"
  color2="darkblue"
  color3="red"
  color4="darkgreen"
  color5="red"
  color6="darkgreen"
  color7="blue"
  color8="blue"
  valdef=FALSE
}  }


###
quanti=names(which(sapply(newdata,is.numeric)))
quali=names(which(!(sapply(newdata,is.numeric))))
VariableChoices=quali
nom=rownames(newdata)
num=c(1:length(nom))
QuantiChoice=quanti
IdChoices=c(1:length(VariableChoices))
Idquantisup=c(1:length(QuantiChoice))
nomData=unlist(strsplit(as.character(nomData),"\\["))[1]