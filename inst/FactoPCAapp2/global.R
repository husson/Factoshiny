#global script for PCA2
if(inherits(x, "data.frame")){
  newdataPCAshiny <- x
  quantisupPCAshiny <- NULL
  qualisupPCAshiny <- NULL
  indsuplPCAshiny <- NULL
  labmodPCAshiny <- c(gettext("Individuals",domain="R-Factoshiny"),gettext("Supplementary individuals",domain="R-Factoshiny"),gettext("Supplementary categories",domain="R-Factoshiny"))
  indmodPCAshiny <- c(gettext("Individuals",domain="R-Factoshiny"),gettext("Supplementary individuals",domain="R-Factoshiny"),gettext("Supplementary categories",domain="R-Factoshiny"))
  axe1PCAshiny <- 1
  axe2PCAshiny <- 2
  nbdimclustPCAshiny <- 5
  hcpcparaPCAshiny <- FALSE
  habillageindPCAshiny2 <- habillageindPCAshiny <- NULL
  selectionPCAshiny <- gettext("No selection",domain="R-Factoshiny")
  selection2PCAshiny <- NULL
  selection3PCAshiny <- gettext("No selection",domain="R-Factoshiny")
  selection4PCAshiny <- NULL
  sizePCAshiny <- 1
  size2PCAshiny <- 1
  color_pointInit <- gettext("active/supplementary",domain="R-Factoshiny")
  color_arrowInit <- gettext("active/supplementary",domain="R-Factoshiny")
  titre1PCAshiny <- gettext("PCA graph of individuals",domain="R-Factoshiny")
  titre2PCAshiny <- gettext("PCA graph of variables",domain="R-Factoshiny")
  ellipsesPCAshiny <- FALSE
  activeindPCAshiny <- "black"
  supindPCAshiny <- "blue"
  categPCAshiny <- "magenta"
  coloractvarPCAshiny <- "black"
  colorsupvarPCAshiny <- "blue"
  imputeInit <- gettext("Impute by the mean (fast but not recommended)",domain="R-Factoshiny")
  normePCAshiny <- TRUE
  poids1PCAshiny <- NULL
  poids2PCAshiny <- NULL
  pvalueDimdescInit <- 0.05
}

if(inherits(x, "PCAshiny")){
  nomDataPCAshiny <- x$nomDataPCAshiny
  newdataPCAshiny <- x$newdataPCAshiny
  quantisupPCAshiny <- x$c
  qualisupPCAshiny <- x$b
  indsuplPCAshiny <- x$d
  indmodPCAshiny <- x$y
  axe1PCAshiny <- x$nb1
  axe2PCAshiny <- x$nb2
  habillageindPCAshiny <- x$habiller
  habillageindPCAshiny2 <- x$habiller2
  selectionPCAshiny <- x$selectionPCAshiny
  selection2PCAshiny <- x$selection2PCAshiny
  selection3PCAshiny <- x$j
  selection4PCAshiny <- x$k
  sizePCAshiny <- x$l
  size2PCAshiny <- x$m
  titre1PCAshiny <- x$title1
  titre2PCAshiny <- x$title2
  color_pointInit <- x$color_point
  color_arrowInit <- x$color_arrow
  ellipsesPCAshiny <- x$ellipsesPCAshiny
  activeindPCAshiny <- if (!is.null(x$activeindPCAshiny)) {x$activeindPCAshiny} else {"black"}
  supindPCAshiny <- if (!is.null(x$supin)) {x$supin} else {"blue"}
  categPCAshiny <- if (!is.null(x$categPCAshiny)) {x$categPCAshiny} else {"magenta"}
  coloractvarPCAshiny <- if (!is.null(x$coloractvarPCAshiny)) {x$coloractvarPCAshiny} else {"black"}
  colorsupvarPCAshiny <- if (!is.null(x$colorsupvarPCAshiny)) {x$colorsupvarPCAshiny} else {"blue"}
  normePCAshiny <- x$normePCAshiny
  poids1PCAshiny <- x$poids1PCAshiny
  poids2PCAshiny <- x$poids2PCAshiny
  hcpcparaPCAshiny <- x$hcpcparam
  nbdimclustPCAshiny <- x$nbdimclustPCAshiny
  if (!is.null(x$impute)) imputeInit <- x$imputeInit
  else imputeInit <- gettext("Impute by the mean (fast but not recommended)",domain="R-Factoshiny")
  pvalueDimdescInit <- x$pvalueDimdescInit
  labmodPCAshiny <- c(gettext("Individuals",domain="R-Factoshiny"),gettext("Supplementary individuals",domain="R-Factoshiny"),gettext("Supplementary categories",domain="R-Factoshiny"))
}
if(inherits(x, "PCA")){
  nomDataPCAshiny <- as.character(x$call$call[2])
  newdataPCAshiny <- x$call$X
  quantisupPCAshiny <- colnames(x$call$quanti.sup)
  qualisupPCAshiny <- colnames(x$call$quali.sup$quali.sup)
  indsuplPCAshiny <- rownames(x$ind.sup$coord)
  labmodPCAshiny <- c(gettext("Individuals",domain="R-Factoshiny"),gettext("Supplementary individuals",domain="R-Factoshiny"),gettext("Supplementary categories",domain="R-Factoshiny"))
  indmodPCAshiny <- c(gettext("Individuals",domain="R-Factoshiny"),gettext("Supplementary individuals",domain="R-Factoshiny"),gettext("Supplementary categories",domain="R-Factoshiny"))
  axe1PCAshiny <- 1
  axe2PCAshiny <- 2
  nbdimclustPCAshiny <- 5
  hcpcparaPCAshiny <- FALSE
  habillageindPCAshiny2 <- habillageindPCAshiny <- NULL
  selectionPCAshiny <- gettext("No selection",domain="R-Factoshiny")
  selection2PCAshiny <- NULL
  selection3PCAshiny <- gettext("No selection",domain="R-Factoshiny")
  selection4PCAshiny <- NULL
  sizePCAshiny <- 1
  size2PCAshiny <- 1
  color_arrowInit <- gettext("active/supplementary",domain="R-Factoshiny")
  color_pointInit <- gettext("active/supplementary",domain="R-Factoshiny")
  titre1PCAshiny <- gettext("PCA graph of individuals",domain="R-Factoshiny")
  titre2PCAshiny <- gettext("PCA graph of variables",domain="R-Factoshiny")
  ellipsesPCAshiny <- FALSE
  activeindPCAshiny <- "black"
  supindPCAshiny <- "blue"
  categPCAshiny <- "magenta"
  coloractvarPCAshiny <- "black"
  colorsupvarPCAshiny <- "blue"
  normePCAshiny <- x$call$scale.unit
  poids1PCAshiny <- x$call$row.w.init
  imputeInit <- gettext("Impute by the mean (fast but not recommended)",domain="R-Factoshiny")
  pvalueDimdescInit <- 0.05
  if(!is.null(poids1PCAshiny)){
    if(sum(poids1PCAshiny!=rep(1,length(poids1PCAshiny)))==0){
      poids1PCAshiny <- NULL
    }}
  poids2PCAshiny <- x$call$col.w
  if(!is.null(poids2PCAshiny)){
    if(sum(poids2PCAshiny!=rep(1,length(poids2PCAshiny)))==0){
      poids2PCAshiny <- NULL
    }}
}  

nomPCAshiny <- rownames(newdataPCAshiny)
numPCAshiny <- c(1:length(nomPCAshiny))
quantiPCAshiny <- names(which(sapply(newdataPCAshiny,is.numeric)))
qualiPCAshiny <- names(which(!(sapply(newdataPCAshiny,is.numeric))))
VariableChoicesPCAshiny <- quantiPCAshiny
QualiChoicePCAshiny <- qualiPCAshiny
nomDataPCAshinycourt <- unlist(strsplit(as.character(nomDataPCAshiny),"\\["))[1]
if(inherits(x, "data.frame")) qualisupPCAshiny <- QualiChoicePCAshiny
