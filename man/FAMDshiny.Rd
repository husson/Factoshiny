\name{FAMDshiny}

\alias{FAMDshiny}

\title{Factor Analysis for Mixed Data with Factoshiny}

\description{
Performs Factor Analysis for Mixed Data (FAMD) with supplementary individuals, supplementary quantitative variables and supplementary categorical variables on a Shiny application.\cr
Allows to change FAMD parameters and graphical parmeters.\cr
Graphics can be downloaded in png, jpg and pdf.
}

\usage{
FAMDshiny(X)}

\arguments{
  \item{X}{a data frame with \emph{n} rows (individuals) and \emph{p} columns (numeric variables and categorical variables), or a FAMDshiny result or a FAMD result}
}

\value{
Returns several tabs:
  \item{Graphs}{a tab containing the individuals factor map, the variables factor (quantitative and categorical) map and the correlation circle.}
  \item{Values}{a tab containing the summary of the FAMD performed, the eigenvalues, the results for the variables, the results for the individuals, the results for the supplementary variables and the results for the supplementary individuals.}
  \item{Summary of dataset}{a tab containing the summary of the dataset and a boxplot and histogram for quantitative variables.}
  \item{Data}{a tab containing the dataset with a nice display.}

The left part of the application allows to change all the elements of the FAMDA and the graphs (axes,variables,colors...)
}


\author{Pauline Vaissie, Astrid Monge, Francois Husson \email{paulinevaissie@gmail.com}}

\seealso{ \code{\link{FAMD}},\code{\link{plot.FAMD}}\cr
\href{https://shiny.rstudio.com}{shiny website}}

\examples{
\dontrun{
require(FactoMineR)
data(geomorphology)
# FAMD with Factoshiny:
res.shiny=FAMDshiny(geomorphology)

# Find your app the way you left it (by clicking on the "Quit the app" button)
res.shiny2=FAMDshiny(res.shiny)

}
}
