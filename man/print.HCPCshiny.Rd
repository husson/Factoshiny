\name{print.HCPCshiny}

\alias{print.HCPCshiny}

\title{Print the HCPCshiny results}

\description{
Print the HCPCshiny results i.e the R corresponding script
}

\usage{
\method{print}{HCPCshiny}(x,...)
}

\arguments{
  \item{x}{an object of class HCPCshiny}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{Pauline Vaissie, Astrid Monge, Francois Husson}

\seealso{ \code{\link{HCPCshiny}}}

\examples{
\dontrun{
data(decathlon)
res.shiny = HCPCshiny(decathlon)
#click on the "Quit the app" button
print(res.shiny)
}
}
\keyword{print}
