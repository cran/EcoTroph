\name{plot_ETdiagnosis}
\alias{plot_ETdiagnosis}
%- Also NEED an '\alias' for EACH other topic documented here.???
\title{
Plot Diagnosis Function
}
\description{
This function enables the creation of the principle graphics resulting from the create.ETdiagnosis function.  
}
\usage{
plot_ETdiagnosis(m,scale1,scale2,maxrange)
}
%- maybe also 'usage' for other objects documented here.???
\arguments{
  \item{m}{
is the list of tables returned by the create.ETdiagnosis function.
}
  \item{scale1}{
is the scale parameter of the Biomass Trophic Spectra, can be log or by default the standart scale of the results.
}
  \item{scale2}{
is the scale parameter of the Flow Trophic Spectra, can be log or by default the standart scale of the results.
}
\item{maxrange}{
is the max Trophic Level for the x-axis of the plots.
}
}
\details{
The scale1 parameter controls the scale of the Yaxis for the BTS, it's by default the scale of the result but it's normally more practical to use a log scale. It's the same thing for the scale2 parameter controlling the Yaxis of the FTS.
}
\value{
The function returns the principal graphics of the global ET-Diagnosis routine: the graphics of the biomass, accessible biomass...rates for the different effort multipliers, the Biomass Trophic Spectra (BTS)for the different effort multipliers, the B/Bref(mE=1) graph for the main TL classes, the CTS and FTS.
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
plot_ETdiagnosis(create.ETdiagnosis(create.ETmain(ecopath_guinee)$ET_Main))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~Diagnosis }
