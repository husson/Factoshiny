\name{print.CAshiny}

\alias{print.CAshiny}

\title{Print the CAshiny results}

\description{
Print the CAshiny results i.e the R corresponding script
}

\usage{
\method{print}{CAshiny}(x,...)
}

\arguments{
  \item{x}{an object of class CAshiny}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{Pauline Vaissie, Astrid Monge, Francois Husson}

\seealso{ \code{\link{CAshiny}}}

\examples{
\dontrun{
data(children)
res.shiny = CAshiny(children)
#click on the "Quit the app" button
print(res.shiny)
}
}
\keyword{print}
