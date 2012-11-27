\name{plot.smooth}
\alias{plot.smooth}
%- Also NEED an '\alias' for EACH other topic documented here.???
\title{
Plot Smooth Function
}
\description{
plot.smooth is used to plot a smooth function. This function enables the user to see the TL distribution around its mean trophic level.
}
\usage{
\method{plot}{smooth}(x,...)
}
%- maybe also 'usage' for other objects documented here.???
\arguments{
  \item{x}{
is the table returned by the create.smooth function.
}
\item{\dots}{plot other arguments}
}
\value{
The function returns a plot of the Smooth with the TL classes in absisse.
}
\references{
Gascuel et al. (2009) EcoTroph (ET): a trophic level based software for assessing the impacts of fishing on aquatic ecosystems.
}
\author{
Colleter Mathieu and Guitton Jerome.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
create.smooth function to create the Smooth, Transpose to calculate the data transposition into trophic spectra, plot_Transpose to plot the trophic spectra.
}
\examples{
data(ecopath_guinee)
plot(create.smooth(ecopath_guinee))
plot(create.smooth(ecopath_guinee,smooth_type=2))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~Smooth }
