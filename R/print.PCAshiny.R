print.PCAshiny<-
function(x,...){
  if(!inherits(x,"PCAshiny")) stop(gettext("non convenient data"))
  cat(gettext("Results for the PCA with Factoshiny"))
  cat("\n")
  cat(gettext("You can use it to fine your app the way you left it"))
  cat("\n")
  cat("\n")
  cat(gettext("Corresponding script:"))
  cat("\n")
  cat(x$codePCA,"\n")
  cat("\n")
  cat(x$codeGraphInd,"\n")
  cat(x$codeGraphVar,"\n")
}