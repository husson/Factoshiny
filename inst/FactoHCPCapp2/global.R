#global script for HCPC2
if((inherits(x, "PCA") | inherits(x, "MCA") | inherits(x, "CA")| inherits(x, "FAMD")| inherits(x, "MFA"))){
  resultsHCPCshiny <- x
  anafact <- lignecodeHCPCshiny
  resClusHCPCshiny <- HCPC(resultsHCPCshiny,nb.clust=-1,graph=FALSE)$call$t$nb.clust
  if (inherits(x, "CA")) nbindivHCPCshiny <- nrow(resultsHCPCshiny$row$coord)
  else nbindivHCPCshiny <- nrow(resultsHCPCshiny$ind$coord)
  nomDataHCPCshiny <- nomDataHCPCshiny
  clustdfHCPCshiny <- resClusHCPCshiny
  consolidfHCPCshiny <- FALSE
  metricdfHCPCshiny <- gettext("Euclidean")
  drawdfHCPCshiny <- FALSE
  dfHCPCshiny <- FALSE
  centerdfHCPCshiny <- FALSE
  numdfHCPCshiny <- 60
  nb1dfHCPCshiny <- 1
  nb2dfHCPCshiny <- 2
  title1HCPCshiny <- gettext("Hierarchical tree on the factor map")
  title2HCPCshiny <- gettext("Factor map")
  title3HCPCshiny <- gettext("Hierarchical tree")
}

if(!((inherits(x, "PCA") | inherits(x, "MCA") | inherits(x, "CA")| inherits(x, "FAMD")| inherits(x, "MFA")))){
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
}

if(inherits(x, "PCAshiny") | inherits(x, "CAshiny") | inherits(x, "MCAshiny") | inherits(x, "FAMDshiny")| inherits(x, "MFAshiny")){
  resultsHCPCshiny=x$anafact
  anafact=x$code1
  resClusHCPCshiny=HCPC(resultsHCPCshiny,nb.clust=-1,graph=FALSE)$call$t$nb.clust
  if (inherits(x, "CAshiny")) nbindivHCPCshiny=dim(resultsHCPCshiny$row$coord)[1]
  else nbindivHCPCshiny=dim(resultsHCPCshiny$ind$coord)[1]
  nomDataHCPCshiny=nomDataHCPCshiny
  clustdfHCPCshiny=resClusHCPCshiny
  consolidfHCPCshiny=FALSE
  metricdfHCPCshiny=gettext("Euclidean")
  drawdfHCPCshiny=FALSE
  dfHCPCshiny=FALSE
  centerdfHCPCshiny=FALSE
  numdfHCPCshiny=60
  nb1dfHCPCshiny=1
  nb2dfHCPCshiny=2
  title1HCPCshiny=gettext("Hierarchical tree on the factor map")
  title2HCPCshiny=gettext("Factor map")
  title3HCPCshiny=gettext("Hierarchical tree")
}

if(inherits(x, "HCPC")){
  resClusHCPCshiny <- x$call$t$nb.clust
  anafact <- x$call$t$res
  resultsHCPCshiny <- anafact
  nomDataHCPCshiny <- x$call$call[2]
  clustdfHCPCshiny <- x$call$t$nb.clust
  consolidfHCPCshiny <-FALSE
  if(x$call$t$tree["dist.method"]=="euclidean"){
    metricdfHCPCshiny=gettext("Euclidean")
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
  title1HCPCshiny <- gettext("Hierarchical tree on the factor map")
  title2HCPCshiny <- gettext("Factor map")
  title3HCPCshiny <- gettext("Hierarchical tree")
}

if(!(inherits(x, "HCPC") | inherits(x, "HCPCshiny"))) resClusHCPCshiny <- HCPC(resultsHCPCshiny,nb.clust=-1,graph=FALSE)$call$t$nb.clust
if (inherits(x, "CA")) nbindivHCPCshiny=nrow(resultsHCPCshiny$row$coord)
else nbindivHCPCshiny=nrow(resultsHCPCshiny$ind$coord)
}