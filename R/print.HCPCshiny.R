print.HCPCshiny<-
  function(x,...){
    if(!inherits(x,"HCPCshiny"))
      stop("non convenient data")
    cat("Results for the HCPC with Factoshiny\n")
    cat("You can use it to fine your app the way you left it\n")
    cat("\n")
    cat("Corresponding script : \n")
    print(x$anafact)
    cat(x$Code,"\n")
    cat("\n")
    cat(x$CodeTree,"\n")
    cat(x$Code2Dmap,"\n")
    cat(x$Code3D,"\n")
  }