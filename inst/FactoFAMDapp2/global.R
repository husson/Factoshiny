if(inherits(x, "data.frame")){
  newdata <- x
  quantisup <- NULL
  qualisup <- NULL
  indsupl <- NULL
  axe1 <- 1
  axe2 <- 2
  indmodFAMDshiny <- c(gettext("Individuals",domain="R-Factoshiny"),gettext("Active categories",domain="R-Factoshiny"),gettext("Supplementary individuals",domain="R-Factoshiny"),gettext("Supplementary categories",domain="R-Factoshiny"))
  nbdimclustFAMDshiny <- 5
  hcpcparaFAMDshiny <- FALSE
  labind <- TRUE
  labvar <- TRUE
  habillageind <- NULL
  selection <- "NONE"
  selection2 <- NULL
  selection3 <- "NONE"
  selection4 <- NULL
  selection5 <- "NONE"
  selection6 <- NULL
  size <- 1
  size2 <- 1
  size3 <- 1
  title1 <- gettext("Graph of individuals and categories",domain="R-Factoshiny")
  title2 <- gettext("Graph of the variables",domain="R-Factoshiny")
  title3 <- gettext("Correlation circle",domain="R-Factoshiny")
  pvalueDimdescInit <- 0.05
}

if(inherits(x, "FAMDshiny")){
  nomData <- x$nomData
  newdata <- x$data
  quantisup <- x$b
  qualisup <- x$c
  indsupl <- x$d
  axe1 <- x$e
  axe2 <- x$f
  habillageind <- x$g
  selection <- x$h
  selection2 <- x$i
  selection3 <- x$j
  selection4 <- x$k
  selection5 <- x$o
  selection6 <- x$p
  size <- x$l
  size2 <- x$m
  size3 <- x$n
  title1 <- x$title1
  title2 <- x$title2
  title3 <- x$title3
  labind <- x$labind
  labvar <- x$labvar
  indmodFAMDshiny <- x$indmodFAMDshiny
  hcpcparaFAMDshiny <- x$hcpcparam
  nbdimclustFAMDshiny <- x$nbdimclustFAMDshiny
  pvalueDimdescInit <- x$pvalueDimdescInit

}
if(inherits(x, "FAMD")){
  nomData <- as.character(x$call$call[2])
  newdata <- x$call$X
  quantisup <- qualisup <- NULL
  if (sum(x$call$nature.var=="quanti.sup")>0) quantisup <- which(x$call$nature.var=="quanti.sup")
  if (sum(x$call$nature.var=="quali.sup")>0) qualisup <- which(x$call$nature.var=="quali.sup")
  indsupl <- rownames(x$ind.sup$coord)
  axe1 <- 1
  axe2 <- 2
  habillageind <- NULL
  selection <- "NONE"
  selection2 <- NULL
  selection3 <- "NONE"
  selection4 <- NULL
  selection5 <- "NONE"
  selection6 <- NULL
  nbdimclustFAMDshiny <- 5
  hcpcparaFAMDshiny <- FALSE
  indmodFAMDshiny <- c(gettext("Individuals",domain="R-Factoshiny"),gettext("Active categories",domain="R-Factoshiny"),gettext("Supplementary individuals",domain="R-Factoshiny"),gettext("Supplementary categories",domain="R-Factoshiny"))
  size <- 1
  size2 <- 1
  size3 <- 1
  title1 <- gettext("Graph of individuals and categories",domain="R-Factoshiny")
  title2 <- gettext("Graph of the variables",domain="R-Factoshiny")
  title3 <- gettext("Correlation circle",domain="R-Factoshiny")
  labind <- TRUE
  labvar <- TRUE
  pvalueDimdescInit <- 0.05
}  
allVariables <- colnames(newdata)
quanti <- names(which(sapply(newdata,is.numeric)))
quali <- names(which(!(sapply(newdata,is.numeric))))
VariableChoices <- quanti
nom <- rownames(newdata)
QualiChoice <- quali
nomDatacourt <- unlist(strsplit(as.character(nomData),"\\["))[1]
