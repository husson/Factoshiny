print.PCAshiny <- function(x,...){
  if(!inherits(x,"PCAshiny")) stop(gettext("non convenient data"))
  cat(paste0(gettext("To fine your app the way you left it, use:"),"\n"))
  cat(paste0("PCAshiny(",sys.calls()[[1]][2],")\n"))
  cat("\n")
  cat(paste0(gettext("Or use the corresponding script:"),"\n"))
  if (!is.null(x$codePCAp)) cat(x$codePCAp,"\n")
  cat(x$codePCA,"\n")
  cat("\n")
  cat(x$codeGraphInd,"\n")
  cat(x$codeGraphVar,"\n")
}