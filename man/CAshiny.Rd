\name{CAshiny}

\alias{CAshiny}

\title{Correspondance Analysis (CA) with Factoshiny}

\description{
Performs Correspondance Analysis (CA) including supplementary row and/or column points on a Shiny application.\cr
Columns or rows with NA are considered as supplementary in the analysis. \cr
Allows to change CA parameters and graphical parmeters.\cr
Graphics can be downloaded in png, jpg and pdf.
}

\usage{
CAshiny(X)}

\arguments{
  \item{X}{a data frame or a table with \emph{n} rows and \emph{p} columns, i.e. a contingency table or a CAshiny result or a CA result}
}

\value{
Returns several tabs:
  \item{Graphs}{a tab containing the row and column points factor map (with supplementary columns and supplementary rows)}
  \item{Values}{a tab containing the summary of the CA performed, the eigenvalues, the results for the columns, for the rows, for the supplementary columns and for the supplementary rows.}
  \item{Summary of dataset}{a tab containing the summary of the dataset and a boxplot and histogram for quantitative variables.}
  \item{Data}{a tab containing the dataset with a nice display.}

The left part of the application allows to change all the elements of the CA and the graphs (axes,variables,colors...)
}


\author{Pauline Vaissie, Astrid Monge, Francois Husson}

\seealso{ \code{\link[FactoMineR]{CA}}, \code{\link[FactoMineR]{plot.CA}}\cr
\href{https://shiny.posit.co/}{shiny website},\cr
\href{https://youtu.be/6y0NDDQcIXo?si=eo8c9nMUiWPjI_xH}{Video showing how to perform CA with Factoshiny}}

\examples{
\dontrun{
require(FactoMineR)
data(children)
# Correspondance Analysis with Factoshiny:
res.shiny=CAshiny(children)

# Find your app the way you left it (by clicking on the "Quit the app" button)
res.shiny2=CAshiny(res.shiny)

#CAshiny on a result of a CA
data(children)
res.ca <- CA (children, row.sup = 15:18, col.sup = 6:8)
res.shiny=CAshiny(res.ca)
}
}


