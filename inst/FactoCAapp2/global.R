# global script for CA2
if(inherits(x, "data.frame")){
  nomDataCAshiny <- nomDataCAshiny
  newdataCAshiny <- x
  colonnesupCAshiny <- NULL
  lignesupCAshiny <- NULL
  catsupCAshiny <- NULL
  axe1CAshiny <- 1
  axe2CAshiny <- 2
  InvisibleCAshiny <- NULL
  selec1CAshiny <- gettext("No selection")
  selec2CAshiny <- gettext("No selection")
  valueselec1CAshiny <- NULL
  valueselec2CAshiny <- NULL
  sizeCAshiny <- 1
  title1CAshiny <- gettext("CA factor map")
  color_pointInit <- gettext("row/column")
  col1CAshiny <- "blue"
  col2CAshiny <- "red"
  col3CAshiny <- "#0C2B94"
  col4CAshiny <- "darkred"
  col5CAshiny  <- "magenta"
  ellipsesCAshiny <- NULL
  nbdimclustCAshiny <- 5
  hcpcparaCAshiny <- FALSE
}

if(inherits(x, "CAshiny")){
nomDataCAshiny <- x$nomDataCAshiny
newdataCAshiny <- x$data
colonnesupCAshiny <- x$a
lignesupCAshiny <- x$b
catsupCAshiny <- x$c
axe1CAshiny <- x$d
axe2CAshiny <- x$e
InvisibleCAshiny <- x$f
selec1CAshiny <- x$type1
selec2CAshiny <- x$type2
valueselec1CAshiny <- x$selec1CAshiny
valueselec2CAshiny <- x$selec2CAshiny
sizeCAshiny <- x$taille
title1CAshiny <- x$title1CAshiny
col1CAshiny <- x$col1CAshiny
col2CAshiny <- x$col2CAshiny
col3CAshiny <- x$col3CAshiny
col4CAshiny <- x$col4CAshiny
col5CAshiny  <- x$col5CAshiny
ellipsesCAshiny <- x$ellip
hcpcparaCAshiny <- x$hcpcparam
nbdimclustCAshiny <- x$nbdimclustCAshiny
}

if(inherits(x, "CA")){
  nomDataCAshiny <- as.character(x$call$call[2])
#  nomDataCAshiny <- unlist(strsplit(nomDataCAshiny, split='[', fixed=TRUE))[1]
  newdataCAshiny <- x$call$Xtot
  colonnesupCAshiny <- rownames(x$col.sup$coord)
  lignesupCAshiny <- rownames(x$row.sup$coord)
  catsupCAshiny <- NULL
  axe1CAshiny <- 1
  axe2CAshiny <- 2
  nbdimclustCAshiny <- 5
  hcpcparaCAshiny <- FALSE
  color_pointInit <- gettext("row/column")
  InvisibleCAshiny <- NULL
  selec1CAshiny <- gettext("No selection")
  selec2CAshiny <- gettext("No selection")
  valueselec1CAshiny <- NULL
  valueselec2CAshiny <- NULL
  sizeCAshiny <- 1
  title1CAshiny <- gettext("CA factor map")
  col1CAshiny <- "blue"
  col2CAshiny <- "red"
  col3CAshiny <- "darkblue"
  col4CAshiny <- "darkred"
  col5CAshiny  <- "magenta"
  ellipsesCAshiny <- NULL
}

withnaCAshiny <- c()
rownaCAshiny <- c()
nomrowCAshiny <- c()
for (i in 1:dim(newdataCAshiny)[2]){
  if(any(is.na(newdataCAshiny[,i])==TRUE)){
    if(is.numeric(newdataCAshiny[,i])==TRUE){
      withnaCAshiny <- c(withnaCAshiny,colnames(newdataCAshiny)[i])
    }
  }
}

for (i in 1:dim(newdataCAshiny)[1]){
  if(any(is.na(newdataCAshiny[i,])==TRUE)){
      rownaCAshiny <- c(rownaCAshiny,i)
      nomrowCAshiny <- c(nomrowCAshiny,rownames(newdataCAshiny)[i])
  }
}

VariableChoiceCAshiny <- names(which(sapply(newdataCAshiny,is.numeric)))
nomsCAshiny <- rownames(newdataCAshiny)
QualiChoiceCAshiny <- names(which(!(sapply(newdataCAshiny,is.numeric))))
supCAshiny <- which( VariableChoiceCAshiny%in%withnaCAshiny)
if (length(supCAshiny)==0) supCAshiny <- NULL
 
if(!(is.null(supCAshiny))){
  VariableChoicesCAshiny <-  VariableChoiceCAshiny[-supCAshiny]
}
if(is.null(supCAshiny)){
  VariableChoicesCAshiny <-  VariableChoiceCAshiny
}
sup2CAshiny <- which(nomsCAshiny%in%nomrowCAshiny)
if (length(sup2CAshiny)==0) sup2CAshiny <- NULL
if(!(is.null(sup2CAshiny))){
  nomCAshiny <- nomsCAshiny[-sup2CAshiny]
}
if(is.null(sup2CAshiny)){
  nomCAshiny <- nomsCAshiny
}
nomDataCAshiny <- unlist(strsplit(as.character(nomDataCAshiny),"\\["))[1]
