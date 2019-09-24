print.MCAshiny<-
  function(x,...){
    res.shinymca=x
    if(!inherits(res.shinymca,"MCAshiny"))
      stop(gettext("non convenient data"))
  cat(gettext("Results for the MCA with Factoshiny"))
  cat("\n")
  cat(gettext("You can use it to fine your app the way you left it"))
  cat("\n")
  cat("\n")
  cat(gettext("Corresponding script:"))
  cat("\n")
    cat(res.shinymca$code1,"\n")
    cat("\n")
    cat(res.shinymca$code2,"\n")
    cat(res.shinymca$code3,"\n")
    if(!is.null(res.shinymca$code4)){
      cat(res.shinymca$code4,"\n")
    }
  }