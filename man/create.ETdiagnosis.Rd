\name{create.ETdiagnosis}
\alias{create.ETdiagnosis}
\title{
ET Diagnosis
}
\description{
ET-Transpose provides a picture of an ecosystem under a given fishing mortality. ET-Diagnosis is a routine simulating how this baseline ecosystem would be impacted by increasing or decreasing fishing effort. Fishing effort can be modified per fleet and/or trophic group. Ecosystem-wide effects of altering fishing effort include potential changes in biomass, accessible biomass, production, kinetics and catch trophic spectra, as well as impacts on the mean trophic level of the catch and biomass. Additionally, ET-Diagnosis constitutes a useful exploratory tool for ecosystem-based management. It simulates how reducing or increasing fishing effort and/or preferentially targeting different trophic levels could improve yield at the ecosystem scale. Lastly, ET-Diagnosis allows to view how different assumptions about ecosystem functioning (biomass input control, top-down effect, foraging arena theory) affecting both trophic level specific and ecosystem-wide properties in relation to fishing.
}
\usage{
create.ETdiagnosis(data, Mul_eff = NULL, Group = NULL, fleet.of.interest = NULL ,same.mE = NULL, B.Input=NULL,
Beta = NULL, TopD = NULL, FormD = NULL, TLpred = NULL, Forag.A = NULL , Kfeed = NULL ,Ponto = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
is the list returned by the create.ETmain function. 
}
  \item{Mul_eff}{
is a vector of fishing effort multipliers that the user wants to test. Mul_eff must contain the value 1 (reference state). By default, the function simulates a range of fishing multipliers from 0 to 5 for each fleet.
}
  \item{Group}{
is a character vector of trophic groups that the user wants to specifically impact by changing associated fishing efforts. By default, all the trophic groups are equally impacted.
}
  \item{fleet.of.interest}{
is a character vector of fleet(s) for which the wants to study specifically the impact of changing associated fishing efforts (default =NULL). This argument is particularly useful if there is more fleet because it limits the mE combination to test and thus reduce the computation time.
}
  \item{same.mE}{
is a logical argument (default=F), if TRUE same effort multipliers are applied to all fleets simultaneously.
}
  \item{B.Input}{
is a logical argument (default=F), if TRUE "Biomass input control" function is implemented in EcoTroph equations.
}
  \item{Beta}{
is a coefficient expressing the extent of biomass input-control. Beta=0 refers to an ecosystem where all secondary production originate from grazing on primary producers, and Beta=1 to an ecosystem where detritus and/or recruitment contribute a major part of the biomass input. (default=0.2)
}
  \item{TopD}{
is a coefficient expressing the top-down control, i.e. the fraction of the natural mortality depending on predator abundance. It varies between 0 and 1. The user could specify a numeric value, which is applied to each trophic level, or a numeric vector (of the same length as trophic levels), i.e. a value for each TL. (default=0.4)
}
  \item{FormD}{
is a shape parameter varying between 0 and 1. It defines the functional relationship between prey and predators. The value 1 relates to a situation where predators abundance has a linear effect on the speed of the flow of their preys. The user could specify a numeric value, which is applied to each trophic level, or a numeric vector (of the same length as trophic levels), i.e. a value for each TL. (default=0.5)
}
  \item{TLpred}{
is the trophic level at which you consider the "predator" trophic classes start. The default value is 3.5.
}
  \item{Forag.A}{
is a logical argument (default=FALSE), if TRUE "Foraging Arena Theory" is implemented in EcoTroph equations. "Biomass Input Control" & "Foraging Arena Theory" cannot be implemented at the same time : if B.Input is TRUE then Forag.A is FALSE.
}
  \item{Kfeed}{
is a coefficient of "Foraging Arena Theory" : the "Goodyear compensation ratio", expressing the maximum possible relative increase in Q/Bpred when Bpred decreases. The user could specify a numeric value, which is applied to each trophic level, or a numeric vector (of the same length as trophic levels), i.e. a value for each TL.
}
  \item{Ponto}{
is a coefficient of "Foraging Arena Theory", expressing the part of biomass flow from one trophic level to the next one due to ontogenic changes in diet. The user could specify a numeric value, which is applied to each trophic level, or a numeric vector (of the same length as trophic levels), i.e. a value for each TL.
}
}
\details{
Fleets' names used in the argument 'fleet.of.interest' are the catch column names of ecopath input dataframe (e.g. 'catch.1' or 'catch.ind').
}

\value{
This function returns a list of element referring to each simulated combination of fishing effort multipliers. Each element is a list of two types of results:
-	Variables characterizing the state and functioning of the ecosystem: biomass, flow, kinetic, catches (total and per fleet) and fishing mortality per trophic level.
-	Summary statistics (contained in the ET_Main_diagnose): absolute and relative (in comparison with reference state) total biomass, flow, catches 
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

\seealso{
plot_ETdiagnosis and isopleth.plot_ETdiagnosis to plot the principle graphics resulting from the create.ETdiagnosis function, create.smooth function to create the Smooth, plot_smooth to plot the smooth function, Transpose to calculate the data transposition into trophic spectra, create.ETmain to create a list of table including the ET_Main table.
}
\examples{
data(ecopath_guinee)
# test impacts of global changes in fishing efforts multipliers (in the range 0-5)
create.ETdiagnosis(create.ETmain(ecopath_guinee),same.mE=TRUE) 

# test of all the combination of fishing efforts multipliers per fleet (in the range 0-5)
\donttest{create.ETdiagnosis(create.ETmain(ecopath_guinee))}
# with biomass input control and without foraging arena theory
\donttest{create.ETdiagnosis(create.ETmain(ecopath_guinee),B.Input=TRUE)}

# test impacts of changing fishing effort against Barracudas+ and Carangids groups
\donttest{create.ETdiagnosis(create.ETmain(ecopath_guinee),Mul_eff=(seq(0,5,.1)),Group=c('Barracudas+','Carangids'))}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~Diagnosis }
