\name{print.MCAshiny}

\alias{print.MCAshiny}

\title{Print the MCAshiny results}

\description{
Print the MCAshiny results i.e the R corresponding script
}

\usage{
\method{print}{MCAshiny}(x,...)
}

\arguments{
  \item{x}{an object of class MCAshiny}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{Pauline Vaissie, Astrid Monge, Francois Husson \email{paulinevaissie@gmail.com}}

\seealso{ \code{\link{MCAshiny}}}

\examples{
\dontrun{
data(poison)
res.shiny = MCAshiny(poison)
#click on the "Quit the app" button
print(res.shiny)
}
}
\keyword{print}
