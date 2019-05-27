#global script for MCA
if(is.data.frame(x)==TRUE){
  newdataMCAshiny=x
  axe1MCAshiny=1
  axe2MCAshiny=2
  varsupMCAshiny=c(gettext("Active qualitative variables"),gettext("Supplementary qualitative variables"),gettext("Supplementary quantitative variables"))
  indvarMCAshiny=c(gettext("Individuals"),gettext("Categories"),gettext("Supplementary categories"),gettext("Supplementary individuals"))
  labvarMCAshiny=c()
  habillageindMCAshiny=NULL
  selectionMCAshiny=gettext("No selection")
  selection2MCAshiny=NULL
  selection3MCAshiny=gettext("No selection")
  selection4MCAshiny=NULL
  nbdimclustMCAshiny <- 5
  hcpcparaMCAshiny <- FALSE

  quantiMCAshiny=names(which(sapply(newdataMCAshiny,is.numeric)))
  qualiMCAshiny=names(which(!(sapply(newdataMCAshiny,is.numeric))))
  supqualiMCAshiny=NULL
  quantiSMCAshiny=NULL
  labvarMCAshiny=c(gettext("Individuals"),gettext("Categories"),gettext("Supplementary individuals"),gettext("Supplementary categories"))
  indsuplMCAshiny=NULL
  title1MCAshiny=gettext("MCA factor map")
  title2MCAshiny=gettext("Graph of the variables on the MCA map")
  title3MCAshiny=gettext("Graph of the supplementary quantitative variables")
  color1MCAshiny="blue"
  color2MCAshiny="darkblue"
  color3MCAshiny="red"
  color4MCAshiny="darkgreen"
  color5MCAshiny="red"
  color6MCAshiny="darkgreen"
  color7MCAshiny="blue"
  color8MCAshiny="blue"
  valdefMCAshiny=FALSE
}
if(inherits(x, "MCA")||inherits(x, "MCAshiny")){

if(inherits(x, "MCAshiny")){
  newdataMCAshiny=x$data
  nomDataMCAshiny=x$nomDataMCAshiny
  quantiSMCAshiny=x$b
  supqualiMCAshiny=x$c
  varsupMCAshiny=x$z
  indvarMCAshiny=x$y
  indsuplMCAshiny=x$d
  axe1MCAshiny=x$e
  axe2MCAshiny=x$f
  habillageindMCAshiny=x$g
  selectionMCAshiny=x$h
  selection2MCAshiny=x$i
  selection3MCAshiny=x$j
  selection4MCAshiny=x$k
  title1MCAshiny=x$title1MCAshiny
  title2MCAshiny=x$title2MCAshiny
  title3MCAshiny=x$title3MCAshiny
  color1MCAshiny=x$color1MCAshiny
  color2MCAshiny=x$color2MCAshiny
  color3MCAshiny=x$color3MCAshiny
  color4MCAshiny=x$color4MCAshiny
  color5MCAshiny=x$color5MCAshiny
  color6MCAshiny=x$color6MCAshiny
  color7MCAshiny=x$color7MCAshiny
  color8MCAshiny=x$color8MCAshiny
  valdefMCAshiny=x$valdefMCAshiny
  hcpcparaMCAshiny <- x$hcpcparam
  nbdimclustMCAshiny <- x$nbdimclustMCAshiny
}
if(inherits(x, "MCA")){
  nomDataMCAshiny=as.character(x$call$call[2])
#  nomDataMCAshiny=unlist(strsplit(nomDataMCAshiny, split='[', fixed=TRUE))[1]
  newdataMCAshiny=x$call$X
  quantiSMCAshiny=rownames(x$quanti.sup$coord)
  supqualiMCAshiny=rownames(x$quali.sup$coord)
  indsuplMCAshiny=rownames(x$ind.sup$coord)
  axe1MCAshiny=1
  axe2MCAshiny=2
  varsupMCAshiny=c(gettext("Active qualitative variables"),gettext("Supplementary qualitative variables"),gettext("Supplementary quantitative variables"))
  indvarMCAshiny=c(gettext("Individuals"),gettext("Categories"),gettext("Supplementary categories"),gettext("Supplementary individuals"))
  labvarMCAshiny=c(gettext("Individuals"),gettext("Categories"),gettext("Supplementary individuals"),gettext("Supplementary categories"))
  habillageindMCAshiny=NULL
  nbdimclustMCAshiny <- 5
  hcpcparaMCAshiny <- FALSE
  selectionMCAshiny=gettext("No selection")
  selection2MCAshiny=NULL
  selection3MCAshiny=gettext("No selection")
  selection4MCAshiny=NULL
  title1MCAshiny=gettext("MCA factor map")
  title2MCAshiny=gettext("Graph of the variables on the MCA map")
  title3MCAshiny=gettext("Graph of the supplementary quantitative variables")
  color1MCAshiny="blue"
  color2MCAshiny="darkblue"
  color3MCAshiny="red"
  color4MCAshiny="darkgreen"
  color5MCAshiny="red"
  color6MCAshiny="darkgreen"
  color7MCAshiny="blue"
  color8MCAshiny="blue"
  valdefMCAshiny=FALSE
}  }


###
quantiMCAshiny=names(which(sapply(newdataMCAshiny,is.numeric)))
qualiMCAshiny=names(which(!(sapply(newdataMCAshiny,is.numeric))))
VariableChoicesMCAshiny=qualiMCAshiny
nomMCAshiny=rownames(newdataMCAshiny)
num=c(1:length(nomMCAshiny))
QuantiChoiceMCAshiny=quantiMCAshiny
IdChoices=c(1:length(VariableChoicesMCAshiny))
Idquantisup=c(1:length(QuantiChoiceMCAshiny))
nomDataMCAshiny=unlist(strsplit(as.character(nomDataMCAshiny),"\\["))[1]