utils::globalVariables(c("objPCAshiny","quantiPCAshiny","myListOfThingsPCAshiny"))
PCAshiny <- function(X){
  G <- .GlobalEnv
  assign("objPCAshiny",ls(all.names=TRUE, envir=G),envir=G)
  assign("x",X, envir=G)
  assign("nomDataPCAshiny",sys.calls()[[1]][2], envir=G)
  if (!(inherits(X, "PCAshiny") | inherits(X, "data.frame") | inherits(X, "matrix") | inherits(X, "PCA"))){
    stop(gettext('X is not a dataframe, a matrix, the results of the PCAshiny function or a PCA result'))
  }
  if (is.matrix(X)==TRUE) 	X <- as.data.frame(X)
  if(is.data.frame(X)==TRUE){
    assign("quantiPCAshiny",names(which(sapply(X,is.numeric))),envir=G)
    if(length(quantiPCAshiny)<=2)
      stop(gettext('not enough quantitative variables in your dataset'))
  }
  assign("pathsavePCAshiny",getwd(),envir=G)
  outShiny <- shiny::runApp(system.file("FactoPCAapp2", package="Factoshiny"),launch.browser = TRUE)
#  outShiny <- shiny::runApp('/home/husson/Site_Git/Factoshiny/inst/FactoPCAapp2')
#  outShiny <- shiny::runApp('C:/Users/husson/AOBox/Travail/huss/Divers/Site_Github/Factoshiny/inst/FactoPCAapp2')
  assign("myListOfThingsPCAshiny",setdiff(ls(all.names=TRUE,envir=G),c("outShiny",objPCAshiny)),envir=G)  ## on met "a" pour ne pas le supprimer
  rm(list=myListOfThingsPCAshiny, envir=G)
  rm(list=c("myListOfThingsPCAshiny"),envir=G)
  if (outShiny$hcpcparam==TRUE) {
    resHCPC <- HCPCshiny(outShiny)
    return(list(invisible(outShiny),resHCPC))
  } else {
    return(invisible(outShiny))
  }
}
