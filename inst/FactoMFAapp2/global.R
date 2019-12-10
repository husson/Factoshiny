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
  drawing=gettext("group",domain="R-Factoshiny")
  drawing2=NULL
  partial=gettext("None",domain="R-Factoshiny")
  partial2=NULL
  partial3=FALSE
  selectvar2=NULL
  selectionMFAshiny <- gettext("No selection",domain="R-Factoshiny")
  selection2MFAshiny <- NULL
  hide=gettext("Nothing",domain="R-Factoshiny")
  colorvar=TRUE
  freq1=TRUE
  freq2=TRUE
  partaxe=TRUE
  nbdimclustMFAshiny <- 5
  nbDimPartialAxes <- 2
  hcpcparaMFAshiny <- FALSE
data=anafact$global.pca$call$X
nomMFAshiny <- rownames(data)
quanti=names(which(sapply(data,is.numeric)))
quali=names(which(!(sapply(data,is.numeric))))
indvarMFAshiny=c(gettext("Individuals",domain="R-Factoshiny"),gettext("Supplementary individuals",domain="R-Factoshiny"))
if (length(quali)>0) indvarMFAshiny <- c(indvarMFAshiny,gettext("Categories",domain="R-Factoshiny"),gettext("Supplementary categories",domain="R-Factoshiny"))
if (length(quanti)>0) indvarMFAshinyfreq <- c(indvarMFAshiny,gettext("Frequencies",domain="R-Factoshiny"),gettext("Supplementary frequencies",domain="R-Factoshiny"))
titleGroup=gettext("Groups representation",domain="R-Factoshiny")
titleInd=gettext("Individual factor map",domain="R-Factoshiny")
titleVar=gettext("Correlation circle",domain="R-Factoshiny")
titlePartial=gettext("Graph of the partial axes",domain="R-Factoshiny")
titleFreq=gettext("Graph of the frequencies",domain="R-Factoshiny")
}
if(inherits(x,"MFAshiny")){
  nomObjectMFA <- x$nomObjectMFA
  ligne=x$codeMFA
  anafact=x$anafact
  res.MFA <- x$anafact
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
  indvarMFAshinyfreq=x$ind_varfreq
  freq1=x$freq1
  freq2=x$freq2
  partaxe=x$partaxe
  selectionMFAshiny <- x$selectionMFAshiny
  selection2MFAshiny <- x$selection2MFAshiny
  nbDimPartialAxes <- x$nbDimPartialAxes

  titlePartial=x$titlePartial
  titleInd=x$titleInd
  titleVar=x$titleVar
  titleGroup=x$titleGroup
  titleFreq=x$titleFreq
  hcpcparaMFAshiny <- x$hcpcparam
  nbdimclustMFAshiny <- x$nbdimclustMFAshiny
  data=anafact$global.pca$call$X
  nomMFAshiny <- rownames(data)
### Recherche des variables quali
  quanti=names(which(sapply(data,is.numeric)))
  quali=names(which(!(sapply(data,is.numeric))))
}
nameJDD=nomData
# nom=rownames(anafact$ind$coord)
# num=c(1:nrow(anafact$ind$coord))
VariableChoices=quanti
# num=c(1:length(nom))
QualiChoice=quali
nomDatacourt=unlist(strsplit(as.character(nomData),"\\["))[1]
