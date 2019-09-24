print.CAshiny<-
  function(x,...){
    res.shinyca=x
    if(!inherits(res.shinyca,"CAshiny"))
      stop(gettext("non convenient data"))
  cat(gettext("Results for the CA with Factoshiny"))
  cat("\n")
  cat(gettext("You can use it to fine your app the way you left it"))
  cat("\n")
  cat("\n")
  cat(gettext("Corresponding script:"))
  cat("\n")
    cat(res.shinyca$code1,"\n")
    cat("\n")
    cat(res.shinyca$code2,"\n")
  }