print.FAMDshiny<-
  function(x,...){
    if(!inherits(x,"FAMDshiny"))
      stop(gettext("non convenient data"))
  cat(gettext("Results for the FAMD with Factoshiny"))
  cat("\n")
  cat(gettext("You can use it to fine your app the way you left it"))
  cat("\n")
  cat("\n")
  cat(gettext("Corresponding script:"))
  cat("\n")
    cat(x$code1,"\n")
    cat("\n")
    cat(x$code2,"\n")
    cat(x$code3,"\n")
    cat(x$code4,"\n")
  }