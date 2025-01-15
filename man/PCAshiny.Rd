\name{PCAshiny}

\alias{PCAshiny}

\title{Principal Component Analysis (PCA) with FactoShiny}

\description{
Performs Principal Component Analysis (PCA) with supplementary individuals, supplementary quantitative 
variables and supplementary categorical variables on a Shiny application.\cr
Allows to change PCA parameters and graphical parmeters.\cr
Graphics can be downloaded in png, jpg, pdf and emf.
}

\usage{
PCAshiny(X)}

\arguments{
  \item{X}{a data frame with \emph{n} rows (individuals) and \emph{p} columns (numeric variables) or a PCAshiny result, or a PCA result}
}

\value{
Returns several tabs:
  \item{Graphs}{a tab containing the individuals factor map and the variables factor map.}
  \item{Values}{a tab containing the summary of the PCA performed, the eigenvalues, the results for the variables, the results for the individuals, the results for the supplementary variables and the results for the categorical variables.}
   \item{Automatic description of axes}{a tab containing the output of the dimdesc function. This function is designed to point out the variables and the categories that are the most characteristic according to each dimension obtained by a Factor Analysis.}
  \item{Summary of dataset}{a tab containing the summary of the dataset and a boxplot and histogram for quantitative variables.}
  \item{Data}{a tab containing the dataset with a nice display.}

The left part of the application allows to change all the elements of the PCA and the graphs (axes,variables,colors...).
The "Get your code" button print the PCA and the plots code in your RStudio session. 
}


\author{Pauline Vaissie, Astrid Monge, Francois Husson \email{paulinevaissie@gmail.com}}

\seealso{ \code{\link{PCA}}, \code{\link{plot.PCA}}\cr
\href{https://shiny.posit.co}{shiny website}}

\examples{
\dontrun{
require(FactoMineR)
data(decathlon)
# Principal Component Analysis with Factoshiny:
res.shiny=PCAshiny(decathlon)

# Find your app the way you left it (by clicking on the "Quit the app" button)
res.shiny2=PCAshiny(res.shiny)

#PCAshiny on a result of a PCA
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13)
res.shiny=PCAshiny(res.pca)
}
}
