utils::globalVariables(c("objcondesshiny","quanticondesshiny","myListOfThingscondesshiny"))
condesshiny <- function(X){
  G <- .GlobalEnv
  assign("objcondesshiny",ls(all.names=TRUE, envir=G),envir=G)
  if (inherits(X, "data.table") | inherits(X, "matrix")) 	X <- as.data.frame(X)
  if (!(inherits(X, "data.frame")  | inherits(X, "condes"))){
    stop(gettext('X is not a dataframe, a matrix, the results of the condesshiny function or a condes result',domain="R-Factoshiny"))
  }
  assign("jdd",X, envir=G)
  assign("nomDatacondesshiny",sys.calls()[[1]][2], envir=G)
  if(is.data.frame(X)==TRUE){
    assign("quanticondesshiny",names(which(sapply(X,is.numeric))),envir=G)
    if(length(quanticondesshiny)==0)
      stop(gettext('No quantitative variable in your dataset',domain="R-Factoshiny"))
  }
 outShiny <- shiny::runApp(system.file("Factocondesapp", package="Factoshiny"),launch.browser = TRUE)
#  outShiny <- shiny::runApp('/home/husson/Site_Git/Factoshiny/inst/Factocondesapp')
# outShiny <- shiny::runApp('C:/Users/husson/AOBox/Travail/huss/Divers/Site_Github/Factoshiny/inst/Factocondesapp')
  assign("myListOfThingscondesshiny",setdiff(ls(all.names=TRUE,envir=G),c("outShiny",objcondesshiny)),envir=G)  ## on met "a" pour ne pas le supprimer
  rm(list=myListOfThingscondesshiny, envir=G)
  rm(list=c("myListOfThingscondesshiny"),envir=G)
  return(invisible(outShiny))
}
