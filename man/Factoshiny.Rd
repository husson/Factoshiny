\name{Factoshiny}

\alias{Factoshiny}

\title{Choose the Factoshiny function to perform the analysis}

\description{
Choose betwwen all the available functions of the package}

\usage{
Factoshiny(X, language="auto")}

\arguments{
  \item{X}{a data frame with \emph{n} rows (individuals) and \emph{p} columns (numeric variables) or a Factoshiny result}
  \item{language}{string corresponding to the language you want for the interface. "french" or "english" are available; 
      by default "auto" will automatically find the language of your computer}
}

\value{
Open a web interface that allows you to choose the method you want to use on your dataset
}


\author{Francois Husson}

\examples{
\dontrun{
require(FactoMineR)
data(decathlon)
# Principal Component Analysis with Factoshiny:
res.shiny <- Factoshiny(decathlon)

# Find your app the way you left it (by clicking on the "Quit the app" button)
res.shiny2=Factoshiny(res.shiny)

#PCAshiny on a result of a PCA
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13)
res.shiny=Factoshiny(res.pca)
}
}
