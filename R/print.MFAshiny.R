print.MFAshiny<-
  function(x,...){
    if(!inherits(x,"MFAshiny"))
      stop(gettext("non convenient data"))
  cat(gettext("Results for the MFA with Factoshiny"))
  cat("\n")
  cat(gettext("You can use it to fine your app the way you left it"))
  cat("\n")
  cat("\n")
  cat(gettext("Corresponding script:"))
  cat("\n")
    print(x$ligne)
    cat("\n")
    cat(x$code1,"\n")
    cat(x$code2,"\n")
    if(!is.null(x$code3)) cat(x$code3,"\n")
    cat(x$code4,"\n")
    if(!is.null(x$code5)) cat(x$code5,"\n")
  }