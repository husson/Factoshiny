\name{HCPCshiny}

\alias{HCPCshiny}

\title{Hierarchical Clustering on Principal Components (HCPC) with Factoshiny}

\description{
Performs Hierarchical Clustering on Principal Components (HCPC) o results from a factor analysis on a Shiny application.\cr
Allows to change HCPC parameters and graphical parmeters.\cr
Graphics can be downloaded in png, jpg and pdf.
}

\usage{
HCPCshiny(res)}

\arguments{
  \item{res}{Either the result of a factor analysis or a dataframe, the result of a HCPCshiny app or an HCPC result}
}

\value{
Returns several tabs:
  \item{Graphs}{a tab containing the factor map,individuals colores by cluster, the 3D map (same graph but with tree above) and the tree map}
  \item{Values}{a tab containing the description of classes (by variables and by axes) and the parangons of each cluster}
  \item{Summary of dataset}{a tab containing the summary of the dataset and a boxplot and histogram for quantitative variables.}
  \item{Data}{a tab containing the dataset with a nice display.}

The left part of the application allows to change the number of cluster very easily. Some parameters from the graphs can also be changed.
By default, the numeric value of the number of clusters is set to the value advised by the HCPC function.
}


\author{Pauline Vaissie, Astrid Monge, Francois Husson}

\seealso{ \code{\link[FactoMineR]{HCPC}}, \code{\link[FactoMineR]{plot.HCPC}}\cr
\href{https://shiny.posit.co}{shiny website},\cr
\href{https://youtu.be/pmFCosCzRN8?si=76y3JXUhup4dwnXW}{Video showing how to perform clustering with Factoshiny}}

\examples{
\dontrun{
require(FactoMineR)
data(decathlon)
# HCPC with Factoshiny on a result of a factor analysis:
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13)
res.shiny=HCPCshiny(res.pca)

# HCPC with Factoshiny on a dataframe:
data(wine)
res.shiny2=HCPCshiny(wine)

# Find your app the way you left it (by clicking on the "Quit the app" button)
res.shiny3=HCPCshiny(res.shiny2)

}
}


