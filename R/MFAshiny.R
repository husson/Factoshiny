utils::globalVariables(c("objMFAshiny","myListOfThingsMFAshiny"))
MFAshiny <- function(X){
    G <- .GlobalEnv
    assign("objMFAshiny",ls(all.names=TRUE, envir=G),envir=G)
    assign("x", X, envir=G)

    if (!(inherits(X, "MFA") | inherits(X, "data.frame") | inherits(X, "matrix") | inherits(X, "MFAshiny"))){
      stop(gettext('X is not a dataframe or the result of the MFA function',domain="R-Factoshiny"))
    }
    if (is.matrix(X)==TRUE) 	X <- as.data.frame(X)
	booldf <- FALSE
    if(is.data.frame(X)==TRUE){
	  booldf <- TRUE
      assign("nomData",sys.calls()[[1]][2], envir=G)
      if(ncol(X)<=2) stop(gettext('not enough variables in your dataset',domain="R-Factoshiny"))
   # outShiny <- shiny::runApp(system.file("FactoMFAapp", package="Factoshiny"),launch.browser = TRUE)
      outShiny <- shiny::runApp('C:/Users/husson/AOBox/Travail/huss/Divers/Site_Github/Factoshiny/inst/FactoMFAapp')
    } else{
      assign("nomData",as.character(X$call$call)[2], envir=G)
      if (!booldf) assign("nomObjectMFA",strsplit(as.character(sys.calls()[[1]][2]),split="[(]")[[1]][1], envir=G)
  #  outShiny <- shiny::runApp(system.file("FactoMFAapp2", package="Factoshiny"),launch.browser = TRUE)
    outShiny <- shiny::runApp('C:/Users/husson/AOBox/Travail/huss/Divers/Site_Github/Factoshiny/inst/FactoMFAapp2')
    }
  assign("myListOfThingsMFAshiny",setdiff(ls(all.names=TRUE,envir=G),c("outShiny",objMFAshiny)),envir=G)  ## on met "outShiny" pour ne pas le supprimer
  rm(list=myListOfThingsMFAshiny, envir=G)
  rm(list=c("myListOfThingsMFAshiny"),envir=G)
  if (outShiny$hcpcparam==TRUE)resHCPC <- HCPCshiny(outShiny)
  return(invisible(outShiny))
}
