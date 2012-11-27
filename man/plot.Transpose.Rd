\name{plot.Transpose}
\alias{plot.Transpose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Transpose Function Results
}
\description{
This function returns the two principal plots of the Transpose function : a plot by group and the associated Trophic Spectra (CTS, BTS...).
}
\usage{
\method{plot}{Transpose}(x,title = NULL,scale = NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
is the table returned by the Transpose function.
}
  \item{title}{
defines the title of the graph.
}
  \item{scale}{
defines the scale of the graphic y axis: can be log or not.
}
\item{\dots}{plot other arguments}
}
\value{
The function returns the principal graphics of the Transpose function : the different Trophic Spectra (CTS, BTS...) and other graphic representations.
}
\references{
Gascuel et al. (2009) EcoTroph (ET): a trophic level based software for assessing the impacts of fishing on aquatic ecosystems.
}
\author{
Colleter Mathieu and Guitton Jerome.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
create.smooth function to create the Smooth, plot_smooth to plot the smooth function, Transpose to calculate the data transposition into trophic spectra.
}
\examples{
data(ecopath_guinee)
plot(Transpose(create.smooth(ecopath_guinee),ecopath_guinee,"biomass"),scale=log)
plot(Transpose(create.smooth(ecopath_guinee),ecopath_guinee,"catch.1"),title="Small Scale Fishery Catch")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }

