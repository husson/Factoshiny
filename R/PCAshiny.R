PCAshiny <-
function(X){
  .obj <<- ls(all.names=TRUE)
  .x <<- X
  # nom <- sys.calls()[[1]]
  # nameJDD <- nom[2]
  # .nomData <<- nameJDD
  .nomData <<- sys.calls()[[1]][2]
  if (!(inherits(X, "PCAshiny") | inherits(X, "data.frame") | inherits(X, "matrix") | inherits(X, "PCA"))){
    stop(gettext('X is not a dataframe, a matrix, the results of the PCAshiny function or a PCA result'))
  }
  if (is.matrix(X)==TRUE) 	X <- as.data.frame(X)
  if(is.data.frame(X)==TRUE){
    .quanti <<- names(which(sapply(X,is.numeric)))
    .quali <<- names(which(!(sapply(X,is.numeric))))
    if(length(.quanti)<=2)
      stop(gettext('not enough quantitative variables in your dataset'))
  }
  .pathsave <<- getwd()
#  a <- shiny::runApp(system.file("FactoPCAapp2", package="Factoshiny"),launch.browser = TRUE)
   a <- shiny::runApp('/home/husson/Site_Git/Factoshiny/inst/FactoPCAapp2')
   .myListOfThings <<- setdiff(ls(all.names=TRUE),c("a",.obj))  ## on met "a" pour ne pas le supprimer
   cat(.myListOfThings)
   rm(list=.myListOfThings,envir=.GlobalEnv)
   rm(.myListOfThings,envir=.GlobalEnv)
   rm(.obj,envir=.GlobalEnv)
   if (a$hcpcparam==TRUE) {
    resHCPC <- HCPCshiny(a)
    print(list(invisible(a),resHCPC))
  }
  return(invisible(a))
}



