\name{catdesshiny}

\alias{catdesshiny}

\title{Categories description}

\description{
Description of the categories of one factor by categorical variables and/or by quantitative variables
}

\usage{
catdesshiny(X)}

\arguments{
  \item{X}{a data frame made up of at least one categorical variables and a set of quantitative variables and/or categorical variables}
}

\value{
Returns a list including:
  \item{test.chi}{The categorical variables which characterized the factor are listed in ascending order (from the one which characterized the most
  the factor to the one which significantly characterized with the proba \code{proba}}
  \item{category}{description of each category of the \code{num.var} by each category of all the categorical variables}
  \item{quanti.var}{the global description of the \code{num.var} variable by the quantitative variables with the square correlation coefficient and the p-value of the F-test in a one-way analysis of variance (assuming the hypothesis of homoscedsticity)}
  \item{quanti}{the description of each category of the \code{num.var} variable by the quantitative variables.}
}

\author{Etienne Lancelle, Francois Husson \email{francois.husson@institut-agro.fr}}


\seealso{ \code{\link[FactoMineR]{catdes}}}

\references{
Husson, F., Le, S. and Pages, J. (2010). Exploratory Multivariate Analysis by Example Using R, \emph{Chapman and Hall}.
Lebart, L., Morineau, A. and Piron, M. (1995) Statistique exploratoire multidimensionnelle, \emph{Dunod}.\cr
}

\examples{
\dontrun{
require(FactoMineR)
data(wine)
catdesshiny(wine)
}
}

\keyword{multivariate}
