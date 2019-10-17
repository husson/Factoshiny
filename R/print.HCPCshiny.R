print.HCPCshiny<-
  function(x,...){
    if(!inherits(x,"HCPCshiny"))
      stop("non convenient data")
  cat(paste0(gettext("To fine your app the way you left it, use:"),"\n"))
  cat(paste0("HCPCshiny(",sys.calls()[[1]][2],")\n"))
  cat("\n")
  cat(paste0(gettext("Or use the corresponding script:"),"\n"))
    cat(x$anafact)
    cat(x$Code,"\n")
    cat("\n")
    cat(x$CodeTree,"\n")
    cat(x$Code2Dmap,"\n")
    cat(x$Code3D,"\n")
  }