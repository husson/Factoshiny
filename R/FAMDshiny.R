utils::globalVariables(c("objFAMDshiny","myListOfThingsFAMDshiny"))
FAMDshiny <- function(X){
  G <- .GlobalEnv
  assign("objFAMDshiny",ls(all.names=TRUE, envir=G),envir=G)
  assign("x",X, envir=G)
  assign("nomData",sys.calls()[[1]][2], envir=G)
  if (!(inherits(X, "FAMDshiny") | inherits(X, "data.frame") | inherits(X, "matrix") | inherits(X, "FAMD"))){
    stop(gettext('X is not a dataframe, a matrix, the results of the FAMDshiny function or a FAMD result'))
  }
if (is.matrix(X)==TRUE) 	X <- as.data.frame(X)
  if(is.data.frame(X)==TRUE){
    quanti=names(which(sapply(X,is.numeric)))
    quali=names(which(!(sapply(X,is.numeric))))
    if(length(quanti)==0 || length(quali)==0) stop(gettext('Your dataset is not mixed'))
  }
  outShiny <-shiny::runApp(system.file("FactoFAMDapp2", package="Factoshiny"),launch.browser = TRUE)
#  outShiny <- shiny::runApp('/home/husson/Site_Git/Factoshiny/inst/FactoFAMDapp2')
#  outShiny <- shiny::runApp("C:/Users/husson/AOBox/Travail/huss/Divers/Site_Github/Factoshiny/inst/FactoFAMDapp2")
  assign("myListOfThingsFAMDshiny",setdiff(ls(all.names=TRUE,envir=G),c("outShiny",objFAMDshiny)),envir=G)
  rm(list=myListOfThingsFAMDshiny, envir=G)
  rm(list=c("myListOfThingsFAMDshiny"),envir=G)
  if (outShiny$hcpcparam==TRUE) resHCPC <- HCPCshiny(outShiny)
  return(invisible(outShiny))
}



