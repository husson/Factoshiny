#global script for MCA
if(is.data.frame(x)==TRUE){
  newdataMCAshiny=x
  axe1MCAshiny=1
  axe2MCAshiny=2
  varsupMCAshiny=c(gettext("Active qualitative variables",domain="R-Factoshiny"),gettext("Supplementary qualitative variables",domain="R-Factoshiny"),gettext("Supplementary quantitative variables",domain="R-Factoshiny"))
  indvarMCAshiny=c(gettext("Individuals",domain="R-Factoshiny"),gettext("Categories",domain="R-Factoshiny"),gettext("Supplementary categories",domain="R-Factoshiny"),gettext("Supplementary individuals",domain="R-Factoshiny"))
  labvarMCAshiny=c()
  habillageindMCAshiny <- NULL
  selectionMCAshiny=gettext("No selection",domain="R-Factoshiny")
  selection2MCAshiny=NULL
  selection3MCAshiny=gettext("No selection",domain="R-Factoshiny")
  selection4MCAshiny=NULL
  nbdimclustMCAshiny <- 5
  hcpcparaMCAshiny <- FALSE
  poids1MCAshiny <- NULL

  quantiMCAshiny=names(which(sapply(newdataMCAshiny,is.numeric)))
  qualiMCAshiny=names(which(!(sapply(newdataMCAshiny,is.numeric))))
  supqualiMCAshiny=NULL
  quantiSMCAshiny=NULL
  labvarMCAshiny=c(gettext("Individuals",domain="R-Factoshiny"),gettext("Categories",domain="R-Factoshiny"),gettext("Supplementary individuals",domain="R-Factoshiny"),gettext("Supplementary categories",domain="R-Factoshiny"))
  indsuplMCAshiny=NULL
  title1MCAshiny=gettext("MCA factor map",domain="R-Factoshiny")
  title2MCAshiny=gettext("Variables representation",domain="R-Factoshiny")
  title3MCAshiny=gettext("Supplementary quantitatives variables",domain="R-Factoshiny")
  color1MCAshiny="black"
  color2MCAshiny="blue" #   #0C2B94
  color3MCAshiny="red"
  color4MCAshiny="darkgreen"
  color5MCAshiny="red"
  color6MCAshiny="darkgreen"
  color7MCAshiny="blue"
  color8MCAshiny="blue"
  valdefMCAshiny=FALSE
  drawconfInit <- FALSE
  color_pointInit <- gettext("active/supplementary",domain="R-Factoshiny")
  color_ModInit <- gettext("active/supplementary",domain="R-Factoshiny")
  pvalueDimdescInit <- 0.05
  choixLabelInit <- c(gettext("Individuals",domain="R-Factoshiny"),gettext("Categories",domain="R-Factoshiny"),gettext("Supplementary individuals",domain="R-Factoshiny"),gettext("Supplementary categories",domain="R-Factoshiny"))
}

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
  selectionMCAshiny=x$h
  selection2MCAshiny=x$i
  selection3MCAshiny=x$j
  selection4MCAshiny=x$k
  habillageindMCAshiny <- x$habillageindMCAshiny
  poids1MCAshiny <- x$poids1MCAshiny
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
  color_pointInit <- x$color_point
  color_ModInit <- x$color_Mod
  drawconfInit <- x$drawconf
  valdefMCA <- x$valdefMCA
  pvalueDimdescInit <- x$pvalueDimdescInit
  choixLabelInit <- x$choixLabelInit
}
if(inherits(x, "MCA")){
  nomDataMCAshiny=as.character(x$call$call[2])
#  nomDataMCAshiny=unlist(strsplit(nomDataMCAshiny, split='[', fixed=TRUE))[1]
  newdataMCAshiny=x$call$X
  quantiSMCAshiny=rownames(x$quanti.sup$coord)
  supqualiMCAshiny=rownames(x$quali.sup$eta2)
  indsuplMCAshiny=rownames(x$ind.sup$coord)
  drawconfInit <- FALSE
  axe1MCAshiny=1
  axe2MCAshiny=2
  varsupMCAshiny=c(gettext("Active qualitative variables",domain="R-Factoshiny"),gettext("Supplementary qualitative variables",domain="R-Factoshiny"),gettext("Supplementary quantitative variables",domain="R-Factoshiny"))
  indvarMCAshiny=c(gettext("Individuals",domain="R-Factoshiny"),gettext("Categories",domain="R-Factoshiny"),gettext("Supplementary categories",domain="R-Factoshiny"),gettext("Supplementary individuals",domain="R-Factoshiny"))
  labvarMCAshiny=c(gettext("Individuals",domain="R-Factoshiny"),gettext("Categories",domain="R-Factoshiny"),gettext("Supplementary individuals",domain="R-Factoshiny"),gettext("Supplementary categories",domain="R-Factoshiny"))
  nbdimclustMCAshiny <- 5
  hcpcparaMCAshiny <- FALSE
  habillageindMCAshiny <- NULL
  selectionMCAshiny=gettext("No selection",domain="R-Factoshiny")
  selection2MCAshiny=NULL
  selection3MCAshiny=gettext("No selection",domain="R-Factoshiny")
  selection4MCAshiny=NULL
  title1MCAshiny=gettext("MCA factor map",domain="R-Factoshiny")
  title2MCAshiny=gettext("Variables representation",domain="R-Factoshiny")
  title3MCAshiny=gettext("Supplementary quantitatives variables",domain="R-Factoshiny")
  color1MCAshiny="black"
  color2MCAshiny="blue" #   #0C2B94
  color3MCAshiny="red"
  color4MCAshiny="darkgreen"
  color5MCAshiny="red"
  color6MCAshiny="darkgreen"
  color7MCAshiny="blue"
  color8MCAshiny="blue"
  valdefMCAshiny=FALSE
  poids1MCAshiny <- x$call$row.w
  pvalueDimdescInit <- 0.05
  color_pointInit <- gettext("active/supplementary",domain="R-Factoshiny")
  color_ModInit <- gettext("active/supplementary",domain="R-Factoshiny")
  choixLabelInit <- c(gettext("Individuals",domain="R-Factoshiny"),gettext("Categories",domain="R-Factoshiny"),gettext("Supplementary individuals",domain="R-Factoshiny"),gettext("Supplementary categories",domain="R-Factoshiny"))
  if(!is.null(poids1MCAshiny)){
    if(sum(poids1MCAshiny!=rep(1,length(poids1MCAshiny)))==0){
      poids1MCAshiny <- NULL
    }}
} 


###
quantiMCAshiny=names(which(sapply(newdataMCAshiny,is.numeric)))
qualiMCAshiny=names(which(!(sapply(newdataMCAshiny,is.numeric))))
VariableChoicesMCAshiny=qualiMCAshiny
nomMCAshiny=rownames(newdataMCAshiny)
QuantiChoiceMCAshiny=quantiMCAshiny
# nomDataMCAshiny=unlist(strsplit(as.character(nomDataMCAshiny),"\\["))[1]
nomDataMCAshinycourt=unlist(strsplit(as.character(nomDataMCAshiny),"\\["))[1]