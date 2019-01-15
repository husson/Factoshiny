\name{print.MFAshiny}

\alias{print.MFAshiny}

\title{Print the MFAshiny results}

\description{
Print the MFAshiny results i.e the R corresponding script
}

\usage{
\method{print}{MFAshiny}(x,...)
}

\arguments{
  \item{x}{an object of class MFAshiny}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{Pauline Vaissie, Astrid Monge, Francois Husson \email{paulinevaissie@gmail.com}}

\seealso{ \code{\link{MFAshiny}}}

\examples{
\dontrun{
data(wine)
res<- MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
    ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
    num.group.sup=c(1,6),graph=FALSE)
res.shiny=MFAshiny(res)

#click on the "Quit the app" button
print(res.shiny)
}
}
\keyword{print}
