MCAshiny <-
  function(X){
#    gassign("x", X)
    G <- .GlobalEnv
    assign("x", X, envir=G)
    nom=sys.calls()[[1]]
    nameJDD=nom[2]
#    gassign("nomData", nameJDD)
    assign("nomData", nameJDD, envir=G)
    
    if (!(inherits(X, "MCAshiny") | inherits(X, "data.frame") | inherits(X, "matrix") | inherits(X, "MCA"))){
        stop(gettext('X is not a dataframe, a matrix, the results of the MCAshiny function or a MCA result'))
    }
    
      if (is.matrix(X)==TRUE) 	X <- as.data.frame(X)
      if(is.data.frame(X)==TRUE){
        quali=names(which(!(sapply(X,is.numeric))))
      if(length(quali)<=2)
        stop(gettext('not enough qualitative variables in your dataset'))
      }
      ###
      a=shiny::runApp(system.file("FactoMCAapp2",package="Factoshiny"),launch.browser = TRUE)
      return(invisible(a))
  }
