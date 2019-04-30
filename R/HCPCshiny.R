HCPCshiny <-
function(res){
#  gassign("x", res)
  G <- .GlobalEnv
  assign("x", res, envir=G)
  nom=sys.calls()[[1]]
  nameJDD=nom[2]
  #    gassign("nomData",nameJDD)
  assign("nomData",nameJDD, envir=G)
  if (!(inherits(res, "PCA") | inherits(res,"HCPCshiny") | inherits(res, "MCA") | inherits(res, "CA") | inherits(res, "FAMD") | inherits(res, "MFA")| inherits(res, "data.frame") | inherits(res, "matrix") | inherits(res, "MCAshiny") | inherits(res, "PCAshiny")| inherits(res, "FAMDshiny")| inherits(res, "MFAshiny") | inherits(res, "CAshiny"))){
    stop(gettext('res is not the result of a factorial analysis or a dataframe or a matrix'))
  }
  if (is.matrix(res)==TRUE) 	res <- as.data.frame(res)
  if(inherits(res,"data.frame")||(class(res)=="HCPCshiny")&&(res$classx=="data.frame")){
    a=shiny::runApp(system.file("FactoHCPCappdf2", package="Factoshiny"),launch.browser = TRUE)
    return(invisible(a))
  }
  else{
    li=res$call$call
    assign("lignecode",li, envir=G)
    a=shiny::runApp(system.file("FactoHCPCapp2", package="Factoshiny"),launch.browser = TRUE)
    return(invisible(a))
  }
}
