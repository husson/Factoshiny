data=x
  newdataMFAshiny=x
  axe1=1
  axe2=2
  sizeInd <- 1
  sizeVar <- 1
  sizeGroup <- 1
  sizePartial <- 1
  sizeFreq <- 1
  indsupl <- NULL
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
  hcpcparaMFAshiny <- FALSE
titleGroup=gettext("Groups representation",domain="R-Factoshiny")
titleInd=gettext("Individual factor map",domain="R-Factoshiny")
titleVar=gettext("Correlation circle",domain="R-Factoshiny")
titlePartial=gettext("Graph of the partial axes",domain="R-Factoshiny")
titleFreq=gettext("Graph of the frequencies",domain="R-Factoshiny")
nameJDD=nomData
nomMFAshiny <- rownames(data)
quanti=names(which(sapply(data,is.numeric)))
quali=names(which(!(sapply(data,is.numeric))))
VariableChoices=quanti
QualiChoice=quali
indvarMFAshiny=c(gettext("Individuals",domain="R-Factoshiny"),gettext("Supplementary individuals",domain="R-Factoshiny"))
if (length(quali)>0) indvarMFAshiny <- c(indvarMFAshiny,gettext("Categories",domain="R-Factoshiny"),gettext("Supplementary categories",domain="R-Factoshiny"))
if (length(quanti)>0) indvarMFAshinyfreq <- c(indvarMFAshiny,gettext("Frequencies",domain="R-Factoshiny"),gettext("Supplementary frequencies",domain="R-Factoshiny"))
listeType <- c()
if (length(quanti)>0) listeType <- gettext("Quantitative",domain="R-Factoshiny")
if (length(quali)>0) listeType <- c(listeType,gettext("Qualitative",domain="R-Factoshiny"))
if (length(quanti)>0) listeType <- c(listeType,gettext("Frequencies",domain="R-Factoshiny"))
nomDatacourt=unlist(strsplit(as.character(nomData),"\\["))[1]
