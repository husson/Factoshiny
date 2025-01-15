utils::globalVariables(c("objcatdesshiny","qualicatdesshiny","myListOfThingscatdesshiny"))
catdesshiny <- function(X){
  G <- .GlobalEnv
  assign("objcatdesshiny",ls(all.names=TRUE, envir=G),envir=G)
  if (inherits(X, "data.table") | inherits(X, "matrix")) 	X <- as.data.frame(X)
  if (!(inherits(X, "data.frame") | inherits(X, "catdes")| inherits(X, "catdesshiny"))){
    stop(gettext('X is not a dataframe, a matrix, the results of the catdesshiny function or a catdes result',domain="R-Factoshiny"))
  }
  assign("jdd",X, envir=G)
  assign("nomDatacatdesshiny",sys.calls()[[1]][2], envir=G)
  if(is.data.frame(X)==TRUE){
    for (i in 1:ncol(X)) colnames(X)[i] <- gsub(" ",".",colnames(X)[i])
    assign("qualicatdesshiny",names(which(!sapply(X,is.numeric))),envir=G)
    if(length(qualicatdesshiny)==0)
      stop(gettext('No qualitative variable in your dataset',domain="R-Factoshiny"))
  }
 outShiny <- shiny::runApp(system.file("Factocatdesapp", package="Factoshiny"),launch.browser = TRUE)
#  outShiny <- shiny::runApp('/home/husson/Site_Git/Factoshiny/inst/Factocatdesapp')
# outShiny <- shiny::runApp('C:/Users/husson/AOBox/Travail/huss/Divers/Site_Github/Factoshiny/inst/Factocatdesapp')
  assign("myListOfThingscatdesshiny",setdiff(ls(all.names=TRUE,envir=G),c("outShiny",objcatdesshiny)),envir=G)  ## on met "a" pour ne pas le supprimer
  rm(list=myListOfThingscatdesshiny, envir=G)
  rm(list=c("myListOfThingscatdesshiny"),envir=G)
  return(invisible(outShiny))
}
