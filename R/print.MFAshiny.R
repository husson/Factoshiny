print.MFAshiny<-
  function(x,...){
    res.shinymfa=x
    if(!inherits(res.shinymfa,"MFAshiny"))
      stop(gettext("non convenient data"))
  cat(gettext("Results for the MFA with Factoshiny"))
  cat("\n")
  cat(gettext("You can use it to fine your app the way you left it"))
  cat("\n")
  cat("\n")
  cat(gettext("Corresponding script:"))
  cat("\n")
    print(res.shinymfa$ligne)
    cat("\n")
    cat(res.shinymfa$code1,"\n")
    cat(res.shinymfa$code2,"\n")
    if(!is.null(res.shinymfa$code3)){
      cat(res.shinymfa$code3,"\n")
    }
    cat(res.shinymfa$code4,"\n")
    if(!is.null(res.shinymfa$code5)){
      cat(res.shinymfa$code5,"\n")
    }
  }