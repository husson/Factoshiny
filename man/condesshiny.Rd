\name{condesshiny}

\alias{condesshiny}

\title{Continuous variable description}

\description{
Description continuous by quantitative variables and/or by categorical variables
}

\usage{
condesshiny(X)}

\arguments{
  \item{X}{a data frame made up of at least one quantitative variable and a set of quantitative variables and/or categorical variables}
}

\value{
Returns a list including:
  \item{quanti}{the description of the \code{num.var} variable by the quantitative variables. The variables are sorted in ascending order  (from the one which characterized the most to the one which significantly characterized with the proba \code{proba})}
  \item{quali}{The categorical variables which characterized the continuous variables are listed in ascending order}
  \item{category}{description of the continuous variable \code{num.var} by each category of all the categorical variables}
}

\author{Etienne Lancelle, Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link[FactoMineR]{condes}}}

\examples{
\dontrun{
require(FactoMineR)
data(decathlon)
condesshiny(decathlon)
}
}
\keyword{multivariate}
