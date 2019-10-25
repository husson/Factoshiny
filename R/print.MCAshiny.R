print.MCAshiny<-
  function(x,...){
    if(!inherits(x,"MCAshiny"))
      stop(gettext("non convenient data"))
  cat(paste0(gettext("To fine your app the way you left it, use:"),"\n"))
  cat(paste0("MCAshiny(",sys.calls()[[1]][2],")\n"))
  cat("\n")
  cat(paste0(gettext("Or use the corresponding script:"),"\n"))
  cat("\n")
    cat(x$codeMCA,"\n")
    cat("\n")
    cat(x$codeGraphInd,"\n")
    cat(x$codeGraphVar,"\n")
    if(!is.null(x$codeGraphQuanti)) cat(x$codeGraphQuanti,"\n")
  }