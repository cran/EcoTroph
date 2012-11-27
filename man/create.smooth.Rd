\name{create.smooth}
\alias{create.smooth}
%- Also NEED an '\alias' for EACH other topic documented here. ????
\title{
Create Smooth Function
}
\description{
create.smooth is used to create a smooth function. This function enables the conversion of data pertaining to specific taxa or functional groupings into data by trophic classes. The main assumption in this Smooth function is that the distribution of the biomass or catch of a trophic group around its mean trophic level follows a lognormal curve. The curve is defined by a mean (the mean TL of a trophic group) and a standart deviation (sigma), which is a measure of the trophic level variability within the group. The distribution are then defined by the lognormal function LN(mean TL, sigma).
}
\usage{
create.smooth(ecopath,smooth_type=NULL,sigmaLN_cst=NULL,pas=NULL,
shift=NULL,smooth_param=NULL)
}
%- maybe also 'usage' for other objects documented here. ????
\arguments{
  \item{ecopath}{
is the table returned by the Ecopath routine. The different varaiables are the group name, its trophic level, biomass, production and consumption ratio, catches, omnivory index and accessibility (fraction of the group that can be catch supposing an infinite fishing effort).
}
  \item{smooth_type}{
defines the type of sigma calculation for the lognormal distribution. The value for this parameter is 1, 2 or 3. By default smooth_type=1, this defines a constant sigma. By putting smooth_type=2, the user has the possibility to put a sigmaLN=smooth_param*ln(TL-0.05), with the parameter smooth_param=0.07 and shift=0.95 by default. Smooth_type=3 corresponds to the use of Omnivory Index (OI) in sigmaLN calculation (sigmaLN=OI/TL). 
}
  \item{sigmaLN_cst}{
defines the value of the constant sigma of the lognormal distribution in case of smooth_type=1. By default, sigmaLN_cst=0.12.  
}
  \item{pas}{
defines the splitting of the TL classes. By default, pas=0.1 
}
  \item{shift}{
defines the beginning of the smooth function and allows the substraction of 0.05 in the sigma calculation accounting for the half interval range of the trophic class. By default, with a constant sigmaLN (smooth_type=1), shift=1.8; with a function defined sigmaLN (smooth_type=2), shift=0.95; with sigmaLN=OI (smooth_type=3), shift=0.
}
  \item{smooth_param}{
defines the slope of the log-linearly increase of the TL variability with the mean trophic level of the group. SigmaLN(TL) is thus defined as 
sigmaLN(TL)=smooth_param*ln(TL-0.05).
}
}
\details{
The user have the possibility to define sigma for each trophic group and also adjust the LN distribution with the smooth_type, sigmaLN_cst, smooth_param, shift and pas parameters. Different choises are available : a constant sigma, a function defined sigma , or the sigma is equal to omnivory index divided by associated TL.
}
\value{
create.smooth returns a table of the TL distribution within a trophic class. This table enables the calculation of Trophic Spectra. 
}
\references{
Gascuel et al. (2009) EcoTroph (ET): a trophic level based software for assessing the impacts of fishing on aquatic ecosystems.
}
\author{
Colleter Mathieu and Guitton Jerome.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
plot_smooth for plotting the smooth function, Transpose to calculate the data transposition into trophic spectra, plot_Transpose to plot the trophic spectra.
}
\examples{
data(ecopath_guinee)
create.smooth(ecopath_guinee)
create.smooth(ecopath_guinee,sigmaLN_cst=0.11)
create.smooth(ecopath_guinee,smooth_type=2)
create.smooth(ecopath_guinee,smooth_type=2, pas=0.2)
create.smooth(ecopath_guinee,smooth_type=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~Smooth }
