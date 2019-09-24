print.PCAshiny<-
function(x,...){
  res.shinypca=x
  if(!inherits(res.shinypca,"PCAshiny")) stop(gettext("non convenient data"))
  cat(gettext("Results for the PCA with Factoshiny"))
  cat("\n")
  cat(gettext("You can use it to fine your app the way you left it"))
  cat("\n")
  cat("\n")
  cat(gettext("Corresponding script:"))
  cat("\n")
  cat(res.shinypca$codePCA,"\n")
  cat("\n")
  cat(res.shinypca$codeGraphInd,"\n")
  cat(res.shinypca$codeGraphVar,"\n")
}