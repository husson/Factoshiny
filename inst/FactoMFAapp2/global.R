#global AFM2
if(inherits(x,"MFA")){
  ligne=x$call$call
  anafact=x
  newdataMFAshiny=x$call$X
  axe1=1
  axe2=2
  sizeInd <- 1
  sizeVar <- 1
  sizeGroup <- 1
  sizePartial <- 1
  sizeFreq <- 1
  ind1=TRUE
  ind2=TRUE
  ind3=TRUE
  ind4=TRUE
  drawing=gettext("group")
  drawing2=NULL
  partial=gettext("None")
  partial2=NULL
  partial3=FALSE
  selectvar2=NULL
  selectionMFAshiny <- gettext("No selection")
  selection2MFAshiny <- NULL
  hide=gettext("Nothing")
  colorvar=TRUE
  freq1=TRUE
  freq2=TRUE
  partaxe=TRUE
  nbdimclustMFAshiny <- 5
  hcpcparaMFAshiny <- FALSE
  indvarMFAshiny=c(gettext("Individuals"),gettext("Categories"),gettext("Supplementary categories"),gettext("Supplementary individuals"))
titleGroup=gettext("Groups representation")
titleInd=gettext("Individual factor map")
titleVar=gettext("Correlation circle")
titlePartial=gettext("Graph of the partial axes")
titleFreq=gettext("Graph of the frequencies")
}
if(inherits(x,"MFAshiny")){
  nomObjectMFA <- x$nomObjectMFA
  ligne=x$codeMFA
  anafact=x$anafact
  newdataMFAshiny=x$data
  nomData=x$nomData
  axe1=x$axe1
  axe2=x$axe2
  ind1=x$ind1
  ind2=x$ind2
  ind3=x$ind3
  ind4=x$ind4
  sizeInd <- x$sizeInd
  sizeVar <- x$sizeVar
  sizeGroup <- x$sizeGroup
  sizePartial <- x$sizePartial
  sizeFreq <- x$sizeFreq
  drawing=x$drawing
  drawing2=x$drawing2
  partial=x$partial
  partial2=x$partial2
  partial3=x$partial3
  selectvar2=x$selectvar2
  hide=x$hide
  colorvar=x$colorvar
  indvarMFAshiny=x$ind_var
  freq1=x$freq1
  freq2=x$freq2
  partaxe=x$partaxe
  selectionMFAshiny <- x$selectionMFAshiny
  selection2MFAshiny <- x$selection2MFAshiny

  titlePartial=x$titlePartial
  titleInd=x$titleInd
  titleVar=x$titleVar
  titleGroup=x$titleGroup
  titleFreq=x$titleFreq
  hcpcparaMFAshiny <- x$hcpcparam
  nbdimclustMFAshiny <- x$nbdimclustMFAshiny
}
nameJDD=nomData
# nom=rownames(anafact$ind$coord)
# num=c(1:nrow(anafact$ind$coord))
data=anafact$global.pca$call$X
nomMFAshiny <- rownames(data)
### Recherche des variables quali
quanti=names(which(sapply(data,is.numeric)))
quali=names(which(!(sapply(data,is.numeric))))
VariableChoices=quanti
# num=c(1:length(nom))
QualiChoice=quali
nomDatacourt=unlist(strsplit(as.character(nomData),"\\["))[1]
