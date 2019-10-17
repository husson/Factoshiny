print.FAMDshiny<-
  function(x,...){
    if(!inherits(x,"FAMDshiny")) stop(gettext("non convenient data"))
  cat(paste0(gettext("To fine your app the way you left it, use:"),"\n"))
  cat(paste0("FAMDshiny(",sys.calls()[[1]][2],")\n"))
  cat("\n")
  cat(paste0(gettext("Or use the corresponding script:"),"\n"))
  cat("\n")
    cat(x$codeFAMD,"\n")
    cat("\n")
    cat(x$codeGraphInd,"\n")
    cat(x$codeGraphVar,"\n")
    cat(x$codeGraphQuanti,"\n")
  }