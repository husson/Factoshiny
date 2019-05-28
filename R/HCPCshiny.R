utils::globalVariables(c("objHCPCshiny","myListOfThingsHCPCshiny"))
HCPCshiny <-
function(res){
  G <- .GlobalEnv
  assign("objHCPCshiny",ls(all.names=TRUE, envir=G),envir=G)
  assign("x", res, envir=G)
  assign("nomDataHCPCshiny",sys.calls()[[1]][2], envir=G)
  if (!(inherits(res, "PCA") | inherits(res,"HCPC") | inherits(res, "MCA") | inherits(res, "CA") | inherits(res, "FAMD") | inherits(res, "MFA")| inherits(res, "data.frame") | inherits(res, "matrix") | inherits(res, "MCAshiny") | inherits(res, "PCAshiny")| inherits(res, "HCPCshiny")| inherits(res, "FAMDshiny")| inherits(res, "MFAshiny") | inherits(res, "CAshiny"))){
    stop(gettext('res is not the result of a factorial analysis or a dataframe or a matrix'))
  }
  if (is.matrix(res)==TRUE) 	res <- as.data.frame(res)
  assign("pathsaveHCPCshiny",getwd(),envir=G)
  if(inherits(res,"data.frame")||(class(res)=="HCPCshiny")&&(res$classx=="data.frame")){
    outShiny=shiny::runApp(system.file("FactoHCPCappdf2", package="Factoshiny"),launch.browser = TRUE)
#    outShiny <- shiny::runApp('/home/husson/Site_Git/Factoshiny/inst/FactoHCPCappdf2')
#    outShiny <- shiny::runApp('C:/Users/husson/AOBox/Travail/huss/Divers/Site_Github/Factoshiny/inst/FactoHCPCappdf2')
  }  else{
    li <- res$call$call
    assign("lignecodeHCPCshiny",li, envir=G)
    outShiny <- shiny::runApp(system.file("FactoHCPCapp2", package="Factoshiny"),launch.browser = TRUE)
#    outShiny <- shiny::runApp('/home/husson/Site_Git/Factoshiny/inst/FactoHCPCapp2')
#    outShiny <- shiny::runApp('C:/Users/husson/AOBox/Travail/huss/Divers/Site_Github/Factoshiny/inst/FactoHCPCapp2')
  }
  assign("myListOfThingsHCPCshiny",setdiff(ls(all.names=TRUE,envir=G),c("outShiny",objHCPCshiny)),envir=G)  ## on met "a" pour ne pas le supprimer
  rm(list=myListOfThingsHCPCshiny, envir=G)
  rm(list=c("myListOfThingsHCPCshiny"),envir=G)
  return(invisible(outShiny))
}
