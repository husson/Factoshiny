\name{MFAshiny}

\alias{MFAshiny}

\title{Multiple Factor Analysis (MFA) with Factoshiny}

\description{
Performs Multiple Factor Analysis (MFA) with supplementary individuals and supplementary groups of variables on a Shiny application.\cr
Groups of variables can be quantitative, categorical or contingency tables.\cr
Allows to change MFA parameters and graphical parmeters. A maximum of 10 groups can be created\cr
Graphics can be downloaded in png, jpg, pdf and emf.
}

\usage{
MFAshiny(X)}

\arguments{
  \item{X}{a data frame with \emph{n} rows (individuals) and \emph{p} columns or a result of the MFA function}
}

\value{
Returns several tabs:
  \item{Graphs}{a tab containing the individuals factor map, the variables factor map, the groups factor map, the axes factor map and the frequencies factor map (if there is at least one frequency group)}
  \item{Values}{a tab containing the summary of the MFA performed, the eigenvalues, the results for the variables, the results for the individuals and the results of the groups}
  \item{Summary of dataset}{a tab containing the summary of the dataset and a boxplot and histogram for quantitative variables.}
  \item{Data}{a tab containing the dataset with a nice display.}

}


\author{Pauline Vaissie, Astrid Monge, Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{PCA}}, \code{\link{plot.PCA}}\cr
\href{http://shiny.rstudio.com}{shiny website}}

\examples{
\dontrun{
require(FactoMineR)
data(wine)
# Multiple Factor Analysis with Factoshiny:
res.shiny=MFAshiny(wine)

#If you want to create your groups first and customize your graphs
res.mfa= MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
    ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
    num.group.sup=c(1,6))
res.shiny=MFAshiny(res.mfa)
}
}
