\name{MCAshiny}

\alias{MCAshiny}

\title{Multiple Correspondence Analysis (MCA) with Factoshiny}

\description{
Performs Multiple Correspondence Analysis (MCA) with supplementary individuals, supplementary quantitative variables and supplementary categorical variables on a Shiny application.\cr
Allows to change MCA parameters and graphical parmeters.\cr
Graphics can be downloaded in png, jpg and pdf.
}

\usage{
MCAshiny(X)}

\arguments{
  \item{X}{a data frame with \emph{n} rows (individuals) and \emph{p} columns (numeric variables), or a MCAshiny result or a MCA result}
}

\value{
Returns several tabs:
  \item{Graphs}{a tab containing the individuals factor map and the variables factor map.}
  \item{Values}{a tab containing the summary of the MCA performed, the eigenvalues, the results for the variables, the results for the individuals, the results for the supplementary variables and the results for the numerical variables.}
    \item{Automatic description of axes}{a tab containing the output of the dimdesc function. This function is designed to point out the variables and the categories that are the most characteristic according to each dimension obtained by a Factor Analysis.}
  \item{Summary of dataset}{a tab containing the summary of the dataset and a boxplot and histogram for quantitative variables.}
  \item{Data}{a tab containing the dataset with a nice display.}

The left part of the application allows to change all the elements of the MCA and the graphs (axes,variables,colors...)
}


\author{Pauline Vaissie, Astrid Monge, Francois Husson}

\seealso{ \code{\link[FactoMineR]{MCA}},\code{\link[FactoMineR]{dimdesc}},\code{\link[FactoMineR]{plot.MCA}}\cr
\href{https://shiny.posit.co}{shiny website},\cr
\href{https://youtu.be/0z30RDikYaw?si=Rwl2YSqyQvM57Zza}{Video showing how to perform MCA with Factoshiny}}

\examples{
\dontrun{
require(FactoMineR)
data(poison)
# Multiple Correspondance Analysis with Factoshiny:
res.shiny=MCAshiny(poison)

# Find your app the way you left it (by clicking on the "Quit the app" button)
res.shiny2=MCAshiny(res.shiny)

#MCAshiny on a result of a MCA
data(hobbies)
res.mca <- MCA(hobbies,quali.sup=19:22,quanti.sup=23)
res.shiny=MCAshiny(res.mca)
}
}
