\name{read.ecopath.model}
\alias{read.ecopath.model}
%- Also NEED an '\alias' for EACH other topic documented here.????
\title{
read.ecopath.model Function
}
\description{
This function read inputs datas from an xml file exported from Ecopath with the Ecotroph plugin or from a web service providing
 inputs for EcopTroph coming from a database populate with parameters of several Ecopath's models.
}
\usage{
read.ecopath.model(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
is the name of the file exported by ecopath.
}
}
\value{
This function return a data frame containing all the column needed by Ecotroph as input.
}
\references{
Gascuel et al. (2009) EcoTroph (ET): a trophic level based software for assessing the impacts of fishing on aquatic ecosystems.
}
\author{
Colleter Mathieu and Guitton Jerome.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
create.smooth function to create the Smooth, plot_smooth to plot the smooth function, Transpose to calculate the data transposition into trophic spectra, plot_Transpose to plot the trophic spectra.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }

