print.MCAshiny<-
  function(x,...){
    if(!inherits(x,"MCAshiny"))
      stop(gettext("non convenient data"))
  cat(gettext("Results for the MCA with Factoshiny"))
  cat("\n")
  cat(gettext("You can use it to fine your app the way you left it"))
  cat("\n")
  cat("\n")
  cat(gettext("Corresponding script:"))
  cat("\n")
    cat(x$codeMCA,"\n")
    cat("\n")
    cat(x$codeGraphInd,"\n")
    cat(x$codeGraphVar,"\n")
    if(!is.null(x$codeGraphQuanti)) cat(x$codeGraphQuanti,"\n")
  }