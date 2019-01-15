\name{print.PCAshiny}

\alias{print.PCAshiny}

\title{Print the PCAshiny results}

\description{
Print the PCAshiny results i.e the R corresponding script
}

\usage{
\method{print}{PCAshiny}(x,...)
}

\arguments{
  \item{x}{an object of class PCAshiny}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{Pauline Vaissie, Astrid Monge, Francois Husson \email{paulinevaissie@gmail.com}}

\seealso{ \code{\link{PCAshiny}}}

\examples{
\dontrun{
data(decathlon)
res.shiny = PCAshiny(decathlon)
#click on the "Quit the app" button
print(res.shiny)
}
}
\keyword{print}
