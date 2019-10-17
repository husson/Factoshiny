print.CAshiny<-
  function(x,...){
    if(!inherits(x,"CAshiny"))
      stop(gettext("non convenient data"))
  cat(paste0(gettext("To fine your app the way you left it, use:"),"\n"))
  cat(paste0("CAshiny(",sys.calls()[[1]][2],")\n"))
  cat("\n")
  cat(paste0(gettext("Or use the corresponding script:"),"\n"))
  cat("\n")
  cat(x$codeCA,"\n")
  cat("\n")
  cat(x$codeGraph,"\n")
  }