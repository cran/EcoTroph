\name{plot.ETmain}
\alias{plot.ETmain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot ET Main
}
\description{
This function enables the display of the principle plots resulting from the create.ETmain function: Biomass Trophic Spectra, Accessible Biomass Trophic Spectra, Catch by fleet Trophic spectra, Total Catch Trophic Spectra and two summary plots.  
}
\usage{
\method{plot}{ETmain}(x,scale1=NULL,scale2=NULL,scale3=NULL,ask=interactive(),...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
is the list of tables returned by the create_ET_Main function.
}

  \item{scale1}{
defines the scale of the Biomass plots y axis: can be log or not.
}
  \item{scale2}{
defines the scale of the Accessible Biomass plots y axis: can be log or not.
}
  \item{scale3}{
defines the scale of the Catch by fleet plots y axis: can be log or not.
}
\item{ask}{
default value is interactive. Parameter to wait for the user to plot each graph.
}

\item{\dots}{plot other arguments}
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
plot(create.ETmain(ecopath_guinee),scale1=log)
plot(create.ETmain(ecopath_guinee),scale1=log,scale3=log)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~Transpose }
