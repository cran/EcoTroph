\name{plot.ETdiagnosis}
\alias{plot.ETdiagnosis}
%- Also NEED an '\alias' for EACH other topic documented here.???
\title{
Plot Diagnosis Function
}
\description{
This function enables the creation of the principle graphics resulting from the create.ETdiagnosis function.  
}
\usage{
\method{plot}{ETdiagnosis}(x,scale=NULL,maxrange=NULL,...)
}
%- maybe also 'usage' for other objects documented here.???
\arguments{
  \item{x}{
is the list returned by the create.ETdiagnosis function.
}
  \item{scale}{
is the scale parameter of the Biomass Trophic Spectra, can be log or by default the standard scale of the results.
}
\item{maxrange}{
is the maximum TL (Trophic Level) for the x-axis of the plots. By default maxrange = 5.5.
}
\item{\dots}{plot other arguments}
}
\details{
The scale parameter controls the scale of the BTS y-axis, it correspond by default to the scale of results, but it's usually more practical to use a log scale.
}
\value{
The function returns the principal graphics of the global ET-Diagnosis routine: the graphics of the biomass, accessible biomass...rates for the different effort multipliers, the Biomass Trophic Spectra (BTS) for the different effort multipliers, the B/Bref(mE=1) and Y/Yref graphs for the main TL classes and the the catch trophic spectra (CTS) (global and per fleet).
}
\references{
Gascuel et al. (2009) EcoTroph (ET): a trophic level based software for assessing the impacts of fishing on aquatic ecosystems.
Gascuel et al. (2011) The trophic-level-based ecosystem modelling approach: theoretical overview and pratical uses.
Gasche et Gascuel (unpublished) EcoTroph: a simplified ecosystem model to assess fisheries interactions.
}
\author{
Colleter Mathieu, Guitton Jerome and Gatti Paul.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
data(ecopath_guinee)
diagn.list<-create.ETdiagnosis(create.ETmain(ecopath_guinee),same.mE=TRUE)
plot(diagn.list)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~Diagnosis }
