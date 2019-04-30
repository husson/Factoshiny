FAMDshiny <-
function(X){
#   gassign("x", X)
   G <- .GlobalEnv
   assign("x", X, envir=G)
  nom=sys.calls()[[1]]
  nameJDD=nom[2]
#  gassign("nomData",nameJDD)
  assign("nomData",nameJDD, envir=G)
  if (!(inherits(X, "FAMDshiny") | inherits(X, "data.frame") | inherits(X, "matrix") | inherits(X, "FAMD"))){
    stop(gettext('X is not a dataframe, a matrix, the results of the FAMDshiny function or a FAMD result'))
  }
  if (is.matrix(X)==TRUE) 	X <- as.data.frame(X)
  if(is.data.frame(X)==TRUE){
    quanti=names(which(sapply(X,is.numeric)))
    quali=names(which(!(sapply(X,is.numeric))))
    if(length(quanti)==0 || length(quali)==0)
      stop(gettext('Your dataset is not mixed'))
  }
  a=shiny::runApp(system.file("FactoFAMDapp2", package="Factoshiny"),launch.browser = TRUE)
  return(invisible(a))
}



