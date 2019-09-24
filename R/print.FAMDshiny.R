print.FAMDshiny<-
  function(x,...){
    res.shinyfamd=x
    if(!inherits(res.shinyfamd,"FAMDshiny"))
      stop(gettext("non convenient data"))
  cat(gettext("Results for the FAMD with Factoshiny"))
  cat("\n")
  cat(gettext("You can use it to fine your app the way you left it"))
  cat("\n")
  cat("\n")
  cat(gettext("Corresponding script:"))
  cat("\n")
    cat(res.shinyfamd$code1,"\n")
    cat("\n")
    cat(res.shinyfamd$code2,"\n")
    cat(res.shinyfamd$code3,"\n")
    cat(res.shinyfamd$code4,"\n")
  }