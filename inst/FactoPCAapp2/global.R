#global script for PCA2
if(inherits(x, "data.frame")){
  nomData <- nomData
  newdata <- x
  quantisup <- NULL
  qualisup <- NULL
  indsupl <- NULL
  labmod <- c()
  indmod <- c(gettext("Individuals"),gettext("Supplementary individuals"),gettext("Supplementary categories"))
  axe1 <- 1
  axe2 <- 2
  habillageind <- NULL
  selection <- gettext("No selection")
  selection2 <- NULL
  selection3 <- gettext("No selection")
  selection4 <- NULL
  size <- 1
  size2 <- 1
  titre1 <- gettext("Individuals factor map (PCA)")
  titre2 <- gettext("Variables factor map (PCA)")
  ellipses <- FALSE
  activeind <- "black"
  supind <- "blue"
  categ <- "magenta"
  coloractvar <- "black"
  colorsupvar <- "blue"
  norme <- TRUE
  poids1 <- NULL
  poids2 <- NULL
}

if(inherits(x, "PCAshiny")){
  nomData <- x$nomData
  newdata <- x$data
  quantisup <- x$c
  qualisup <- x$b
  indsupl <- x$d
  indmod <- x$y
  axe1 <- x$e
  axe2 <- x$f
  habillageind <- x$g
  selection <- x$h
  selection2 <- x$i
  selection3 <- x$j
  selection4 <- x$k
  size <- x$l
  size2 <- x$m
  titre1 <- x$title1
  titre2 <- x$title2
  ellipses <- x$ellipses
  activeind <- x$activeind
  supind <- x$supin
  categ <- x$categ
  coloractvar <- x$coloractvar
  colorsupvar <- x$colorsupvar
  norme <- x$norme
  poids1 <- x$poids1
  poids2 <- x$poids2
}
if(inherits(x, "PCA")){
  nomData <- as.character(x$call$call[2])
  newdata <- x$call$X
  quantisup <- colnames(x$call$quanti.sup)
  qualisup <- colnames(x$call$quali.sup$quali.sup)
  indsupl <- rownames(x$ind.sup$coord)
  labmod <- c()
  indmod <- c(gettext("Individuals"),gettext("Supplementary individuals"),gettext("Supplementary categories"))
  axe1 <- 1
  axe2 <- 2
  habillageind <- NULL
  selection <- gettext("No selection")
  selection2 <- NULL
  selection3 <- gettext("No selection")
  selection4 <- NULL
  size <- 1
  size2 <- 1
  titre1 <- gettext("Individuals factor map (PCA)")
  titre2 <- gettext("Variables factor map (PCA)")
  ellipses <- FALSE
  activeind <- "black"
  supind <- "blue"
  categ <- "magenta"
  coloractvar <- "black"
  colorsupvar <- "blue"
  norme <- x$call$scale.unit
  poids1 <- x$call$row.w.init
  if(!is.null(poids1)){
    if(sum(poids1!=rep(1,length(poids1)))==0){
    poids1 <- NULL
  }}
  poids2 <- x$call$col.w
  if(!is.null(poids2)){
    if(sum(poids2!=rep(1,length(poids2)))==0){
      poids2 <- NULL
    }}
}  

quanti <- names(which(sapply(newdata,is.numeric)))
quali <- names(which(!(sapply(newdata,is.numeric))))
VariableChoices <- quanti
nom <- rownames(newdata)
num <- c(1:length(nom))
QualiChoice <- quali
IdChoices <- c(1:length(VariableChoices))
Idqualisup <- c(1:length(QualiChoice))
nomData <- unlist(strsplit(as.character(nomData),"\\["))[1]
