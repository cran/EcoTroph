\name{Transpose}
\alias{Transpose}
%- Also NEED an '\alias' for EACH other topic documented here.????
\title{
ET Transpose Function
}
\description{
Transpose enables the conversion of data pertaining to specific taxa or functionnal groupings into data by trophic classes. Data can be catches, biomasses or production in order to produce continuous distributions of those variables over trophic levels.
}
\usage{
Transpose(tab_smooth, ecopath, column)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab_smooth}{
is the table returned by the create.smooth function.
}
  \item{ecopath}{
is the table returned by the Ecopath routine. The different variables are the group name, its trophic level, biomass, production and consumption ratio, catches, omnivory index and accessibility (fraction of the group that can be catch supposing an infinite fishing effort).
}
  \item{column}{
is the table ecopath column name of the variable you want to transpose (for example "biomass" or "catch").
}
}
\value{
The function returns a table of the choosen variable split into the defined trophic classes per ecopath group. 
}
\references{
Gascuel et al. (2009) EcoTroph (ET): a trophic level based software for assessing the impacts of fishing on aquatic ecosystems.
}
\author{
Colleter Mathieu and Guitton Jerome.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
create.smooth function to create the Smooth, plot_smooth to plot the smooth function, plot_Transpose to plot the trophic spectra.
}
\examples{
data(ecopath_guinee)
Transpose(create.smooth(ecopath_guinee),ecopath_guinee,"biomass")
Transpose(create.smooth(ecopath_guinee),ecopath_guinee,"catch.1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }

