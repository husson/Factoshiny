#global script for FAMD
if(inherits(x, "data.frame")){
  newdata <- x
  quantisup <- NULL
  qualisup <- NULL
  indsupl <- NULL
  axe1 <- 1
  axe2 <- 2
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
  title1 <- gettext("Graph of individuals and categories")
  title2 <- gettext("Graph of the variables")
  title3 <- gettext("Correlation circle")
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
  hcpcparaFAMDshiny <- x$hcpcparam
  nbdimclustFAMDshiny <- x$nbdimclustFAMDshiny
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
  size <- 1
  size2 <- 1
  size3 <- 1
  title1 <- gettext("Graph of individuals and categories")
  title2 <- gettext("Graph of the variables")
  title3 <- gettext("Correlation circle")
  labind <- TRUE
  labvar <- TRUE
}  
all <- colnames(newdata)
quanti <- names(which(sapply(newdata,is.numeric)))
quali <- names(which(!(sapply(newdata,is.numeric))))
VariableChoices <- quanti
nom <- rownames(newdata)
num <- c(1:length(nom))
QualiChoice <- quali
IdChoices <- c(1:length(VariableChoices))
Idqualisup <- c(1:length(QualiChoice))
Idall <- c(1:length(all))
nomData <- unlist(strsplit(as.character(nomData),"\\["))[1]
