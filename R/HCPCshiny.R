utils::globalVariables(c("objHCPCshiny","myListOfThingsHCPCshiny","nomDataHCPCshiny"))
HCPCshiny <-
function(res){
  G <- .GlobalEnv
  assign("objHCPCshiny",ls(all.names=TRUE, envir=G),envir=G)

  if (!(inherits(res, "PCA") | inherits(res,"HCPC") | inherits(res, "MCA") | inherits(res, "CA") | inherits(res, "FAMD") | inherits(res, "MFA")| inherits(res, "data.frame") | inherits(res, "matrix") | inherits(res, "MCAshiny") | inherits(res, "PCAshiny")| inherits(res, "HCPCshiny")| inherits(res, "FAMDshiny")| inherits(res, "MFAshiny") | inherits(res, "CAshiny"))){
    stop(gettext('res is not the result of a factorial analysis or a dataframe or a matrix'))
  }
  booldf <- FALSE
  if (is.matrix(res)==TRUE) 	res <- as.data.frame(res)
  # if(inherits(res,"data.frame")||(class(res)=="HCPCshiny")&&(res$classx=="data.frame")){
  if(inherits(res,"data.frame")){
    booldf <- TRUE
    # assign("nomDataHCPCshiny",sys.calls()[[1]][2], envir=G)
    quantiHCPCshiny=which(sapply(res,is.numeric))
    qualiHCPCshiny=which(!(1:ncol(res)%in%which(sapply(res,is.numeric))))
    if (length(quantiHCPCshiny)==0) {
      res <- MCA(res, ncp=Inf, graph=FALSE)
	  res$call$call <- paste0("res <- MCA(",sys.calls()[[1]][2],",ncp=Inf ,graph=FALSE)")
	  assign("nomDataHCPCshiny","res.MCA", envir=G)
    } else {
      res <- PCA(res, ncp=Inf, scale.unit=FALSE, quali.sup=if (length(qualiHCPCshiny)>0){qualiHCPCshiny} else NULL, graph=FALSE)
	  res$call$call <- paste0("res <- PCA(",sys.calls()[[1]][2],",ncp=Inf, scale.unit=FALSE",if(length(qualiHCPCshiny)>0) paste0(",quali.sup=c(",paste0(qualiHCPCshiny,collapse=","),")"),",graph=FALSE)")
	  assign("nomDataHCPCshiny","res.PCA", envir=G)
    }
  }

  assign("x", res, envir=G)
  assign("pathsaveHCPCshiny",getwd(),envir=G)
  if (inherits(res, "PCA") | inherits(res, "MCA") | inherits(res, "CA") | inherits(res, "FAMD") | inherits(res, "MFA")){
      if (is.null(nomDataHCPCshiny)) assign("nomDataHCPCshiny",strsplit(as.character(sys.calls()[[1]][2]),split="[(]")[[1]][1], envir=G)
	  li <- str2lang(paste0(nomDataHCPCshiny,"<-",as.character(as.expression(res$call$call))))
  }
  if (inherits(res, "PCAshiny")){
    li <- str2lang(res$codePCA)
	assign("nomDataHCPCshiny","res.PCA", envir=G)
  }
  if (inherits(res, "MCAshiny")){
    li <- str2lang(res$codeMCA)
	assign("nomDataHCPCshiny","res.MCA", envir=G)
  }
  if (inherits(res, "CAshiny")){
    li <- str2lang(res$codeCA)
	assign("nomDataHCPCshiny","res.CA", envir=G)
  }
  if (inherits(res, "FAMDshiny")){
    li <- str2lang(res$codeFAMD)
	assign("nomDataHCPCshiny","res.FAMD", envir=G)
  }
  if (inherits(res, "MFAshiny")){
    li <- str2lang(res$codeMFA)
	assign("nomDataHCPCshiny","res.MFA", envir=G)
  }

  if (inherits(res, "HCPCshiny")){
    li <- str2lang(res$Code)
	assign("nomDataHCPCshiny",res$nomDataHCPCshiny, envir=G)
  }
    assign("lignecodeHCPCshiny",li, envir=G)
#     outShiny <- shiny::runApp(system.file("FactoHCPCapp2", package="Factoshiny"),launch.browser = TRUE)
#    outShiny <- shiny::runApp('/home/husson/Site_Git/Factoshiny/inst/FactoHCPCapp2')
   outShiny <- shiny::runApp('C:/Users/husson/AOBox/Travail/huss/Divers/Site_Github/Factoshiny/inst/FactoHCPCapp2')
  assign("myListOfThingsHCPCshiny",setdiff(ls(all.names=TRUE,envir=G),c("outShiny",objHCPCshiny)),envir=G)  ## on met "a" pour ne pas le supprimer
  rm(list=myListOfThingsHCPCshiny, envir=G)
  rm(list=c("myListOfThingsHCPCshiny"),envir=G)
  return(invisible(outShiny))
}
