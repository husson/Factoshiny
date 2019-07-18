utils::globalVariables(c("objMCAshiny","myListOfThingsMCAshiny"))
MCAshiny <-
  function(X){
    G <- .GlobalEnv
    assign("objMCAshiny",ls(all.names=TRUE, envir=G),envir=G)
    assign("x", X, envir=G)
    assign("nomDataMCAshiny",sys.calls()[[1]][2], envir=G)
    
    if (!(inherits(X, "MCAshiny") | inherits(X, "data.frame") | inherits(X, "matrix") | inherits(X, "MCA"))){
        stop(gettext('X is not a dataframe, a matrix, the results of the MCAshiny function or a MCA result'))
    }
    
      if (is.matrix(X)==TRUE) 	X <- as.data.frame(X)
      if(is.data.frame(X)==TRUE){
        qualiMCAshiny=names(which(!(sapply(X,is.numeric))))
      if(length(qualiMCAshiny)<=2)
        stop(gettext('not enough qualitative variables in your dataset'))
      }
      ###

  assign("pathsaveMCAshiny",getwd(),envir=G)
  outShiny=shiny::runApp(system.file("FactoMCAapp2", package="Factoshiny"),launch.browser = TRUE)
#  outShiny=shiny::runApp('/home/husson/Site_Git/Factoshiny/inst/FactoMCAapp2')
#  outShiny <- shiny::runApp('C:/Users/husson/AOBox/Travail/huss/Divers/Site_Github/Factoshiny/inst/FactoMCAapp2')
  assign("myListOfThingsMCAshiny",setdiff(ls(all.names=TRUE,envir=G),c("outShiny",objMCAshiny)),envir=G)  ## on met "outShiny" pour ne pas le supprimer
  rm(list=myListOfThingsMCAshiny, envir=G)
  rm(list=c("myListOfThingsMCAshiny"),envir=G)
   if (outShiny$hcpcparam==TRUE) {
    resHCPC <- HCPCshiny(outShiny)
    print(list(invisible(outShiny),resHCPC))
  }
  return(invisible(outShiny))
}
