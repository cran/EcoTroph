\name{create.ETmain}
\alias{create.ETmain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create ET Main 
}
\description{
This function enables the creation of the ET Main table (summarizing the principal results in function of the TL).
}
\usage{
create.ETmain(ecopath,smooth_type=NULL,sigmaLN_cst=NULL,pas=NULL,
shift=NULL,smooth_param=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ecopath}{
is the table returned by the Ecopath routine. The different varaiables are the group name, its trophic level, biomass, production and consumption ratio, catches, omnivory index and accessibility (fraction of the group that can be catch supposing an infinite fishing effort).
}
  \item{smooth_type}{
is a parameter of the create.smooth function. It defines the type of sigma calculation for the lognormal distribution. The value for this parameter is 1, 2 or 3. By default smooth_type=1, this defines a constant sigma. By putting smooth_type=2, the user has the possibility to put a sigmaLN=smooth_param*ln(TL-0.05), with the parameter smooth_param=0.07 and shift=0.95 by default. Smooth_type=3 corresponds to the use of the calculated Omnivory Index (OI) as the sigmaLN.
}
  \item{sigmaLN_cst}{
is a parameter of the create.smooth function. It defines the value of the constant sigma of the lognormal distribution in case of smooth_type=1. By default, sigmaLN_cst=0.12.
}
  \item{pas}{
is a parameter of the create.smooth function. It defines the splitting of the TL classes. 
}
  \item{shift}{
is a parameter of the create.smooth function. It defines the beginning of the smooth function and allows the substraction of 0.05 in the sigma calculation accounting for the half interval range of the trophic class.
}
  \item{smooth_param}{
is a parameter of the create.smooth function. It defines the slope of the log-linearly increase of the TL variability with the mean trophic level of the group for smooth_type=2. SigmaLN(TL) is thus defined as sigmaLN(TL)=smooth_param*ln(TL-0.05).
}
}
\value{
This function returns the ET Main table and some intermediate matrix : biomass, accessible biomass, flowP and a list of matrix corresponding to the catch of the different fisheries.
}
\references{
Gascuel et al. (2009) EcoTroph (ET): a trophic level based software for assessing the impacts of fishing on aquatic ecosystems.
}
\author{
Colleter Mathieu and Guitton Jerome.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
plot_ETmain to create the principle graphics resulting from the create.ETmain function, create.smooth function to create the Smooth, plot_smooth to plot the smooth function, Transpose to calculate the data transposition into trophic spectra, plot_Transpose to plot the trophic spectra.
}
\examples{
data(ecopath_guinee)
create.ETmain(ecopath_guinee)
create.ETmain(ecopath_guinee,smooth_type=2)
create.ETmain(ecopath_guinee,smooth_type=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~Transpose }
