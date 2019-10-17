print.MFAshiny<-
  function(x,...){
    if(!inherits(x,"MFAshiny"))
      stop(gettext("non convenient data"))
  cat(paste0(gettext("To fine your app the way you left it, use:"),"\n"))
  cat(paste0("MFAshiny(",sys.calls()[[1]][2],")\n"))
  cat("\n")
  cat(paste0(gettext("Or use the corresponding script:"),"\n"))
  cat("\n")
    cat(x$codeMFA)
    cat("\n")
    cat(x$CodeGraphInd,"\n")
    cat(x$CodeGraphGroup,"\n")
    if(!is.null(x$CodeGraphVar)) cat(x$CodeGraphVar,"\n")
    cat(x$CodeGraphPartial,"\n")
    if(!is.null(x$CodeGraphFreq)) cat(x$CodeGraphFreq,"\n")
  }
  