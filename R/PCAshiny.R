PCAshiny <-
function(X){
#   gassign("x", X)
   G <- .GlobalEnv
   assign("x", X, envir=G)
  nom=sys.calls()[[1]]
  nameJDD=nom[2]
#  gassign("nomData",nameJDD)
  assign("nomData",nameJDD, envir=G)
  if (!(inherits(X, "PCAshiny") | inherits(X, "data.frame") | inherits(X, "PCA"))){
    stop(gettext('df is not a dataframe, the results of the PCAshiny function or a PCA result'))
  }
  if(is.data.frame(X)==TRUE){
    quanti=names(which(sapply(X,is.numeric)))
    quali=names(which(!(sapply(X,is.numeric))))
    if(length(quanti)<=2)
      stop(gettext('not enough quantitative variables in your dataset'))
  }
  a=shiny::runApp(system.file("FactoPCAapp2", package="Factoshiny"),launch.browser = TRUE)
  #a=shiny::runApp('C:/Users/Ordinateur/Dropbox/Factoshiny/Factoshiny/inst/FactoPCAapp2')
  return(invisible(a))
}



