utils::globalVariables(c("objCAshiny","myListOfThingsCAshiny"))
CAshiny <- function(X){
  G <- .GlobalEnv
  assign("objCAshiny",ls(all.names=TRUE, envir=G),envir=G)
  assign("x", X, envir=G)
  assign("nomDataCAshiny",sys.calls()[[1]][2], envir=G)
  if (is.table(X)) X <- as.data.frame(X)
  if (!(inherits(X, "CAshiny") | inherits(X, "data.frame") | inherits(X, "matrix") | inherits(X, "CA"))){
    stop(gettext('X is not a dataframe, a matrix, a table, the results of the CAshiny function or a CA result'))
  }
  if (is.matrix(X)==TRUE) 	X <- as.data.frame(X)
  if(is.data.frame(X)==TRUE){
    if(dim(X)[1]<3 && dim(X)[2]<3)
      stop(gettext('not enough row/column'))
  }
  assign("pathsaveCAshiny",getwd(),envir=G)
 outShiny=shiny::runApp(system.file("FactoCAapp2", package="Factoshiny"),launch.browser = TRUE)
#  outShiny=shiny::runApp('/home/husson/Site_Git/Factoshiny/inst/FactoCAapp2')
#   outShiny <- shiny::runApp('C:/Users/husson/AOBox/Travail/huss/Divers/Site_Github/Factoshiny/inst/FactoCAapp2')
  assign("myListOfThingsCAshiny",setdiff(ls(all.names=TRUE,envir=G),c("outShiny",objCAshiny)),envir=G)  ## on met "a" pour ne pas le supprimer
  rm(list=myListOfThingsCAshiny, envir=G)
  rm(list=c("myListOfThingsCAshiny"),envir=G)
  if (outShiny$hcpcparam==TRUE) {
    resHCPC <- HCPCshiny(outShiny)
    return(list(invisible(outShiny),resHCPC))
  } else {
    return(invisible(outShiny))
  }
}



