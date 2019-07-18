utils::globalVariables(c("objFactoshiny","myListOfThingsFactoshiny"))
Factoshiny <- function(res, language="auto"){
  G <- .GlobalEnv
  foo <- Sys.getenv("LANG")
  language <- tolower(language)
  if (language=="auto") Sys.setenv(LANG = tolower(substr(Sys.getlocale("LC_COLLATE"),1,2)))
  if ((language=="fr") | (language=="french")) Sys.setenv(LANG ="fr")
  if ((language=="en") | (language=="english")) Sys.setenv(LANG = "en")
  assign("objFactoshiny",ls(all.names=TRUE, envir=G),envir=G)
  assign("x", res, envir=G)
  assign("nomDatashiny",sys.calls()[[1]][2], envir=G)
  if(inherits(res, "catdes") |inherits(res, "condes") |inherits(res, "PCA") | inherits(res, "MCA") | inherits(res, "CA")| inherits(res, "FAMD")| inherits(res, "MFA") | inherits(res,"HCPC") | inherits(res, "PCAshiny") | inherits(res, "MCAshiny") | inherits(res, "CAshiny")| inherits(res, "FAMDshiny")| inherits(res, "MFAshiny") | inherits(res,"HCPCshiny")){
    switch(class(res)[1],
           condes = {condesshiny(res)},
           catdes = {catdesshiny(res)},
           PCA = {PCAshiny(res)},
           PCAshiny = {PCAshiny(res)},
           CA = {CAshiny(res)},
           CAshiny = {CAshiny(res)},
           MCA = {MCAshiny(res)},
           MCAshiny = {MCAshiny(res)},
           FAMD = {FAMDshiny(res)},
           FAMDshiny = {FAMDshiny(res)},
           MFA = {MFAshiny(res)},
           MFAshiny = {MFAshiny(res)},
           HCPC = {HCPCshiny(res)},
           HCPCshiny = {HCPCshiny(res)}			 
      )
    } else {
     if (!(inherits(res, "data.frame") | inherits(res, "matrix"))) stop("res should be a data frame, a matrix or a result object of FactoMineR")
    outShiny <- shiny::runApp(system.file("FactoApp", package="Factoshiny"),launch.browser = TRUE)
#    outShiny <- shiny::runApp('/home/husson/Site_Git/Factoshiny/inst/FactoApp',launch.browser = TRUE)
#    outShiny <- shiny::runApp('C:/Users/husson/AOBox/Travail/huss/Divers/Site_Github/Factoshiny/inst/FactoApp')
    if (!is.null(outShiny)){
    switch(outShiny,
           condes = {result <- condesshiny(res)},
           catdes = {result <- catdesshiny(res)},
           PCA ={ result <- PCAshiny(res)},
           CA ={ result <- CAshiny(res)},
           MCA ={ result <- MCAshiny(res)},
           MFA ={ result <- MFAshiny(res)},
           FAMD ={ result <- FAMDshiny(res)},
           HCPC ={ result <- HCPCshiny(res)},
          )
    }
  }
  Sys.setenv(LANG=foo)
  assign("myListOfThingsFactoshiny",setdiff(ls(all.names=TRUE,envir=G),c("outShiny",objFactoshiny)),envir=G)  ## on met "a" pour ne pas le supprimer
  rm(list=myListOfThingsFactoshiny, envir=G)
  rm(list=c("myListOfThingsFactoshiny"),envir=G)
  return(invisible(result))
}
