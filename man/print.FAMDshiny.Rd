\name{print.FAMDshiny}

\alias{print.FAMDshiny}

\title{Print the FAMDshiny results}

\description{
Print the FAMDshiny results i.e the R corresponding script
}

\usage{
\method{print}{FAMDshiny}(x,...)
}

\arguments{
  \item{x}{an object of class FAMDshiny}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{Pauline Vaissie, Astrid Monge, Francois Husson \email{paulinevaissie@gmail.com}}

\seealso{ \code{\link{FAMDshiny}}}

\examples{
\dontrun{
data(geomorphology)
res.shiny = FAMDshiny(geomorphology)
#click on the "Quit the app" button
print(res.shiny)
}
}
\keyword{print}
