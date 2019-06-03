MFAshiny <-
function(X){
#    gassign("x", X)
    G <- .GlobalEnv
    assign("x", X, envir=G)

    if (!(inherits(X, "MFA") | inherits(X, "data.frame") | inherits(X, "matrix") | inherits(X, "MFAshiny"))){
      stop(gettext('X is not a dataframe or the result of the MFA function'))
    }
    if (is.matrix(X)==TRUE) 	X <- as.data.frame(X)
    if(is.data.frame(X)==TRUE){
      assign("nomData",sys.calls()[[1]][2], envir=G)
      if(dim(X)[2]<=2)
        stop(gettext('not enough variables in your dataset'))
#    outShiny=shiny::runApp(system.file("FactoMFAapp", package="Factoshiny"),launch.browser = TRUE)
    outShiny <- shiny::runApp('C:/Users/husson/AOBox/Travail/huss/Divers/Site_Github/Factoshiny/inst/FactoMFAapp')
    }   else{
      nom=as.character(X$call$call)[2] 
#      gassign("nomData",nom)
      assign("nomData",nom, envir=G)
# outShiny=shiny::runApp(system.file("FactoMFAapp2", package="Factoshiny"),launch.browser = TRUE)
    outShiny <- shiny::runApp('C:/Users/husson/AOBox/Travail/huss/Divers/Site_Github/Factoshiny/inst/FactoMFAapp2')
    }
  if (outShiny$hcpcparam==TRUE) {
    resHCPC <- HCPCshiny(outShiny)
    print(list(invisible(outShiny),resHCPC))
  }
  return(invisible(outShiny))

  }
