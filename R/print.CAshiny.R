print.CAshiny<-
  function(x,...){
    if(!inherits(x,"CAshiny"))
      stop(gettext("non convenient data"))
  cat(gettext("Results for the CA with Factoshiny"))
  cat("\n")
  cat(gettext("You can use it to fine your app the way you left it"))
  cat("\n")
  cat("\n")
  cat(gettext("Corresponding script:"))
  cat("\n")
  cat(x$codeCA,"\n")
  cat("\n")
  cat(x$codeGraph,"\n")
  }