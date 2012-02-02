\name{plot_ETmain}
\alias{plot_ETmain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot ET Main
}
\description{
This function enables the creation of the principle graphics resulting from the create.ETmain function.  
}
\usage{
plot_ETmain(m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
is the list of tables returned by the create_ET_Main function.
}
}
\value{
The function returns the principal graphics of the global ET-Transpose routine: the Biomass Trophic Spectra, the Accessible Biomass Trophic Spectra and other graphics notably the Catch Trophic Spectra for the fisheries in input.
}
\references{
Gascuel et al. (2009) EcoTroph (ET): a trophic level based software for assessing the impacts of fishing on aquatic ecosystems.
}
\author{
Colleter Mathieu and Guitton Jerome.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
create.smooth function to create the Smooth, plot_smooth to plot the smooth function, Transpose to calculate the data transposition into trophic spectra, create.ETmain to create a list of table including the ET_Main table.
}
\examples{
data(ecopath_guinee)
plot_ETmain(create.ETmain(ecopath_guinee))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~Transpose }
