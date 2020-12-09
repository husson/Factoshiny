#global script for HCPC2

if((inherits(x, "PCA") | inherits(x, "MCA") | inherits(x, "CA")| inherits(x, "FAMD")| inherits(x, "MFA"))){
  resultsHCPCshiny <- x
  anafact <- lignecodeHCPCshiny
  if (inherits(x, "CA")){
    nbindivHCPCshiny <- nrow(resultsHCPCshiny$row$coord)
    nbcolHCPCshiny <- ncol(resultsHCPCshiny$row$coord)
    clusterOnCA <- gettext("Rows",domain="R-Factoshiny")
  } else {
    nbindivHCPCshiny <- nrow(resultsHCPCshiny$ind$coord)
    nbcolHCPCshiny <- ncol(resultsHCPCshiny$ind$coord)
    clusterOnCA <- NULL
  }
  if (nbindivHCPCshiny>10000) resClusHCPCshiny <- HCPC(resultsHCPCshiny,kk=100,nb.clust=-1,graph=FALSE)$call$t$nb.clust
  else resClusHCPCshiny <- HCPC(resultsHCPCshiny,nb.clust=-1,graph=FALSE)$call$t$nb.clust
  clustdfHCPCshiny <- resClusHCPCshiny
  consolidfHCPCshiny <- FALSE
  metricdfHCPCshiny <- gettext("Euclidean",domain="R-Factoshiny")
  drawdfHCPCshiny <- FALSE
  dfHCPCshiny <- FALSE
  centerdfHCPCshiny <- FALSE
  if (nbindivHCPCshiny>10000) kkparamInit <- TRUE
  else kkparamInit <- FALSE
  kkInit <- min(100,nbindivHCPCshiny-1)
  numdfHCPCshiny <- 60
  nb1dfHCPCshiny <- 1
  nb2dfHCPCshiny <- 2
  title1HCPCshiny <- gettext("Hierarchical tree on the factor map",domain="R-Factoshiny")
  title2HCPCshiny <- gettext("Factor map",domain="R-Factoshiny")
  title3HCPCshiny <- gettext("Hierarchical tree",domain="R-Factoshiny")
} else {
if(inherits(x, "HCPCshiny")){
resultsHCPCshiny <- x$resultsHCPCshiny$call$t$res
anafact <- x$anafact
nomDataHCPCshiny <- x$nomDataHCPCshiny
clustdfHCPCshiny <- x$clust
consolidfHCPCshiny <- x$consoli
metricdfHCPCshiny <- x$metric
drawdfHCPCshiny <- x$drawtree
dfHCPCshiny <- x$nom3D
resClusHCPCshiny <- x$resClusHCPCshiny
centerdfHCPCshiny <- x$center
numdfHCPCshiny <- x$num
nb1dfHCPCshiny <- x$nb1
nb2dfHCPCshiny <- x$nb2
title1HCPCshiny <- x$title1HCPCshiny
title2HCPCshiny <- x$title2HCPCshiny
title3HCPCshiny <- x$title3HCPCshiny
clusterOnCA <- x$clusterOnCA
kkInit <- x$kk
kkparamInit <- x$kkparam
}

if(inherits(x, "PCAshiny") | inherits(x, "CAshiny") | inherits(x, "MCAshiny") | inherits(x, "FAMDshiny")| inherits(x, "MFAshiny")){
  resultsHCPCshiny=x$anafact
  if (inherits(x, "CAshiny")) {
    nbindivHCPCshiny=nrow(resultsHCPCshiny$row$coord)
    nbcolHCPCshiny <- ncol(resultsHCPCshiny$row$coord)
    clusterOnCA <- gettext("Rows",domain="R-Factoshiny")
  } else {
    nbindivHCPCshiny=nrow(resultsHCPCshiny$ind$coord)
    nbcolHCPCshiny <- ncol(resultsHCPCshiny$ind$coord)
    clusterOnCA <- NULL
  }
  if (nbindivHCPCshiny>10000) resClusHCPCshiny <- HCPC(resultsHCPCshiny,kk=100,nb.clust=-1,graph=FALSE)$call$t$nb.clust
  else resClusHCPCshiny <- HCPC(resultsHCPCshiny,nb.clust=-1,graph=FALSE)$call$t$nb.clust
  if (nbindivHCPCshiny>10000) kkparamInit <- TRUE
  else kkparamInit <- FALSE
  kkInit <- min(100,nbindivHCPCshiny-1)
  clustdfHCPCshiny=resClusHCPCshiny
  consolidfHCPCshiny=FALSE
  metricdfHCPCshiny=gettext("Euclidean",domain="R-Factoshiny")
  drawdfHCPCshiny=FALSE
  dfHCPCshiny=FALSE
  centerdfHCPCshiny=FALSE
  numdfHCPCshiny=60
  nb1dfHCPCshiny=1
  nb2dfHCPCshiny=2
  title1HCPCshiny=gettext("Hierarchical tree on the factor map",domain="R-Factoshiny")
  title2HCPCshiny=gettext("Factor map",domain="R-Factoshiny")
  title3HCPCshiny=gettext("Hierarchical tree",domain="R-Factoshiny")
}

if(inherits(x, "HCPC")){
  resClusHCPCshiny <- x$call$t$nb.clust
  anafact <- x$call$t$res
  resultsHCPCshiny <- anafact
  nomDataHCPCshiny <- x$call$call[2]
  clustdfHCPCshiny <- x$call$t$nb.clust
  clusterOnCA <- x$clusterOnCA
  consolidfHCPCshiny <-FALSE
  if(x$call$t$tree["dist.method"]=="euclidean"){
    metricdfHCPCshiny=gettext("Euclidean",domain="R-Factoshiny")
  }
  if(x$call$t$tree["dist.method"]=="manhattan"){
    metricdfHCPCshiny="Manhattan"
  }
  drawdfHCPCshiny <- FALSE
  dfHCPCshiny <- FALSE
  centerdfHCPCshiny <- FALSE
  numdfHCPCshiny <- 60
  nb1dfHCPCshiny <- 1
  nb2dfHCPCshiny <- 2
  title1HCPCshiny <- gettext("Hierarchical tree on the factor map",domain="R-Factoshiny")
  title2HCPCshiny <- gettext("Factor map",domain="R-Factoshiny")
  title3HCPCshiny <- gettext("Hierarchical tree",domain="R-Factoshiny")
}
# if(!(inherits(x, "HCPC") | inherits(x, "HCPCshiny"))) resClusHCPCshiny <- HCPC(resultsHCPCshiny,nb.clust=-1,graph=FALSE)$call$t$nb.clust
# if (inherits(x, "CA")) nbindivHCPCshiny=nrow(resultsHCPCshiny$row$coord)
# else nbindivHCPCshiny=nrow(resultsHCPCshiny$ind$coord)
# if (inherits(x, "CA")) nbcolHCPCshiny <- ncol(resultsHCPCshiny$row$coord)
# else nbcolHCPCshiny <- ncol(resultsHCPCshiny$ind$coord)
}
pvalueDimdescInit <- 0.05
