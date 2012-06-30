\name{create.ETdiagnosis}
\alias{create.ETdiagnosis}
\title{
ET Diagnosis
}
\description{
ET-Transpose allows to obtain a picture of an ecosystem under a given fishing mortality. ET-Diagnosis is a routine that can simulate how this baseline ecosystem would be impacted by increasing or decreasing fishing effort. Ecosystem-wide effects of altering fishing effort include potential changes of biomass, accessible biomass, production, kinetics and catch trophic spectra as well as impacts on the mean trophic level of the catch and biomass. Additionally, ET-Diagnosis can be a useful exploratory tool for ecosystem-based management by simulating how reducing or increasing fishing effort and/or preferentially targeting different trophic levels could improve yield at the ecosystem scale. Lastly, ET-Diagnosis allows to view how different assumptions about ecosystem functioning (extent recycling, top-down effect) can have effects on both trophic level specific and ecosystem-wide properties in relation to fishing.
}
\usage{
create.ETdiagnosis(ET_Main, Mul_eff = NULL, Beta = NULL, 
TopD = NULL, FormD = NULL, TLpred = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ET_Main}{
is the ET_Main table returned by the create_ET_Main function in its list of table. 
}
  \item{Mul_eff}{
is a vector of the different effort multiplier you want to test.
}
  \item{Beta}{
is a coefficient expressing the extent of biomass input-control. Beta=0 refers to an ecosystem where all secondary production originate from grazing on primary producers and Beta=1 to an ecosystem where detritus and/or recruitment contribute a major part of the biomass input.
}
  \item{TopD}{
is a coefficient expressing the top-down control, the fraction of the natural mortality which depends on predator abundance. It may vary between 0 and 1.
}
  \item{FormD}{
is a shape parameter, varying between 0 and 1, which defines the functional relationship between prey and predators. The value 1 relates to a situation where predators abundance has a linear effect on the speed of the flow of their preys.
}
  \item{TLpred}{
TLpred, is the trophic level at which you consider the predator trophic classes start. The default value is 3.5.
}

}

\value{
The function provides two types of results for each different fishing effort simulated: summary statistics for the whole ecosystem, including absolute and relative values compared to the baseline scenario; catches, total and accessible biomass and production, kinetics for all trophic levels.
}
\references{
Gascuel et al. (2009) EcoTroph (ET): a trophic level based software for assessing the impacts of fishing on aquatic ecosystems.
}
\author{
Colleter Mathieu and Guitton Jerome.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
plot_ETdiagnosis to plot the principle graphics resulting from the create.ETdiagnosis function, create.smooth function to create the Smooth, plot_smooth to plot the smooth function, Transpose to calculate the data transposition into trophic spectra, create.ETmain to create a list of table including the ET_Main table.
}
\examples{
data(ecopath_guinee)
create.ETdiagnosis(create.ETmain(ecopath_guinee)$ET_Main)
create.ETdiagnosis(create.ETmain(ecopath_guinee)$ET_Main,TopD=0.6)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~Diagnosis }
