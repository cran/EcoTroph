\name{check.table}
\alias{check.table}
%- Also NEED an '\alias' for EACH other topic documented here. ????
\title{
check Ecopath Table Function
}
\description{
This function enables the verification of the entry table for all the EcoTroph routine. A model is available in the provided example : data(ecopath_guinee).
}
\usage{
check.table(ecopath)
}
%- maybe also 'usage' for other objects documented here. ????
\arguments{
\item{ecopath}{
is the table returned by the Ecopath routine. The different variables are the group name, its trophic level, biomass, production and consumption ratio, catches, omnivory index and accessibility (fraction of the group that can be catch supposing an infinite fishing effort).
}
}
\details{
The user have the possibility to check its entry table with the headings and the values (no NA)
}
\value{
The check.table function returns notifications related to the ecopath entry table and its viability in the EcoTroph routine (problems in the headings or values) 
}
\references{
Gascuel et al. (2009) EcoTroph (ET): a trophic level based software for assessing the impacts of fishing on aquatic ecosystems.
}
\author{
Colleter Mathieu and Guitton Jerome.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
data(ecopath_guinee)
ecopath_guinee
check.table(ecopath_guinee)
}