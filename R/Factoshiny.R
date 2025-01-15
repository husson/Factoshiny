utils::globalVariables(c("objFactoshiny","myListOfThingsFactoshiny"))
Factoshiny <- function(X, language="auto"){
  G <- .GlobalEnv
  foo <- Sys.getenv("LANG")
  language <- tolower(language)
  if (language=="auto") Sys.setenv(LANG = tolower(substr(Sys.getlocale("LC_COLLATE"),1,2)))
  if ((language=="fr") | (language=="french")) Sys.setenv(LANG ="fr")
  if ((language=="en") | (language=="english")) Sys.setenv(LANG = "en")
  assign("objFactoshiny",ls(all.names=TRUE, envir=G),envir=G)
  assign("x", X, envir=G)
  assign("nomDatashiny",sys.calls()[[1]][2], envir=G)
  if(inherits(X, "catdes") |inherits(X, "condes") |inherits(X, "PCA") | inherits(X, "MCA") | inherits(X, "CA")| inherits(X, "FAMD")| inherits(X, "MFA") | inherits(X,"HCPC") | inherits(X, "PCAshiny") | inherits(X, "MCAshiny") | inherits(X, "CAshiny")| inherits(X, "FAMDshiny")| inherits(X, "MFAshiny") | inherits(X,"HCPCshiny")){
    switch(class(X)[1],
           condes = {result <-condesshiny(X)},
           catdes = {result <-catdesshiny(X)},
           PCA = {result <-PCAshiny(X)},
           PCAshiny = {result <-PCAshiny(X)},
           CA = {result <-CAshiny(X)},
           CAshiny = {result <-CAshiny(X)},
           MCA = {result <-MCAshiny(X)},
           MCAshiny = {result <-MCAshiny(X)},
           FAMD = {result <-FAMDshiny(X)},
           FAMDshiny = {result <-FAMDshiny(X)},
           MFA = {result <-MFAshiny(X)},
           MFAshiny = {result <-MFAshiny(X)},
           HCPC = {result <-HCPCshiny(X)},
           HCPCshiny = {result <-HCPCshiny(X)}			 
      )
    } else {
     if (!(inherits(X, "data.frame") | inherits(X, "matrix"))) stop("X should be a data frame, a matrix or a result object of FactoMineR")
    outShiny <- shiny::runApp(system.file("FactoApp", package="Factoshiny"),launch.browser = TRUE)
#    outShiny <- shiny::runApp('/home/husson/Site_Git/Factoshiny/inst/FactoApp',launch.browser = TRUE)
#    outShiny <- shiny::runApp('C:/Users/husson/AOBox/Travail/huss/Divers/Site_Github/Factoshiny/inst/FactoApp')
    if (!is.null(outShiny)){
    switch(outShiny,
           condes = {result <- condesshiny(X)},
           catdes = {result <- catdesshiny(X)},
           PCA ={ result <- PCAshiny(X)},
           CA ={ result <- CAshiny(X)},
           MCA ={ result <- MCAshiny(X)},
           MFA ={ result <- MFAshiny(X)},
           FAMD ={ result <- FAMDshiny(X)},
           HCPC ={ result <- HCPCshiny(X)},
          )
    }
  }
  Sys.setenv(LANG=foo)
  assign("myListOfThingsFactoshiny",setdiff(ls(all.names=TRUE,envir=G),c("outShiny",objFactoshiny)),envir=G)  ## on met "a" pour ne pas le supprimer
  rm(list=myListOfThingsFactoshiny, envir=G)
  rm(list=c("myListOfThingsFactoshiny"),envir=G)
  return(invisible(result))
}
