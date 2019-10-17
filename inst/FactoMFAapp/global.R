data=x
  newdataMFAshiny=x
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
nameJDD=nomData
nomMFAshiny <- rownames(data)
quanti=names(which(sapply(data,is.numeric)))
quali=names(which(!(sapply(data,is.numeric))))
VariableChoices=quanti
QualiChoice=quali
nomDatacourt=unlist(strsplit(as.character(nomData),"\\["))[1]
