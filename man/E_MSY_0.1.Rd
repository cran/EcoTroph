\name{E_MSY_0.1}
\alias{E_MSY_0.1}
\title{
E_MSY_0.1
}
\description{
E_MSY_0.1 computes two indicators of explotation : Emsy or Fmsy (maximum sustainable yield), and E0.1 or F0.1 ("start" of full exploitation) per trophic level.
}
\usage{
E_MSY_0.1(data,Mul_eff=NULL,B.Input=NULL,Beta=NULL,TopD=NULL,FormD=NULL,TLpred=NULL,Forag.A=NULL,Kfeed=NULL,Ponto=NULL,maxTL=NULL)
}
\arguments{
  \item{data}{
is the list returned by the create.ETmain function. 
}
  \item{Mul_eff}{
is a numeric vector, specifying the value of tested effort multipliers to compute Fmsy and F0.1. By default, Mul_eff=seq(0,10,.1).
}
  \item{B.Input}{
is a logical argument (default=F), if TRUE "Biomass input control" equation is implemented ET-Diagnosis routine.
}
  \item{Beta}{
is a coefficient expressing the extent of biomass input-control. Beta=0 refers to an ecosystem where all secondary production originate from grazing on primary producers and Beta=1 to an ecosystem where detritus and/or recruitment contribute a major part of the biomass input.
}
  \item{TopD}{
is a coefficient expressing the top-down control, the fraction of the natural mortality which depends on predator abundance. It may vary between 0 and 1. The user could specify a numeric value, which is applied to each trophic level, or a numeric vector (of the same length as trophic levels), i.e. a value for each TL.
}
  \item{FormD}{
is a shape parameter, varying between 0 and 1, which defines the functional relationship between prey and predators. The value 1 relates to a situation where predators abundance has a linear effect on the speed of the flow of their preys. The user could specify a numeric value, which is applied to each trophic level, or a numeric vector (of the same length as trophic levels), i.e. a value for each TL.
}
  \item{TLpred}{
TLpred, is the trophic level at which you consider the predator trophic classes start. The default value is 3.5.
}
  \item{Forag.A}{
is a logical argument (default=FALSE), if TRUE "Foraging Arena Theory" is implemented in Ecotroph predictions. "Biomass Input Control" & "Foraging Arena Theory" cannot be implemented at the same time : if B.Input is TRUE then Forag.A is FALSE.
}
  \item{Kfeed}{
is a a coefficient of "Foraging Arena Theory" : the "Goodyear compensation ratio", expressing the maximum possible relative increase in Q/Bpred when Bpred decrease. The user could specify a numeric value, which is applied to each trophic level, or a numeric vector (of the same length as trophic levels), i.e. a value for each TL.
}
  \item{Ponto}{
is a coefficient of "Foraging Arena Theory", expressing the part of biomass flow from one trophic level to the next one due to ontogenic changes in diet. The user could specify a numeric value, which is applied to each trophic level, or a numeric vector (of the same length as trophic levels), i.e. a value for each TL.
}
  \item{maxTL}{
is a numeric string indicating the maximum TL for which indicators are computed.
}
}
\details{
For any trophic level, if E0.1 and/or Emsy value(s) computed by E_MSY_0.1 is(are) equal to the maximum effort multiplier tested (max(Mul_eff)) E/F0.1 and/or E/Fmsy are set to NA.
}
\value{
The function returns a dataframe containing Fmsy, Emsy, F0.1 and E0.1 per trophic level.
}
\references{
Gascuel et al. (2009) EcoTroph (ET): a trophic level based software for assessing the impacts of fishing on aquatic ecosystems.
Gascuel et al. (2011) The trophic-level-based ecosystem modelling approach: theoretical overview and pratical uses.
Gasche et al. (2012) Global assessment of the fishing impacts on the Southern Benguela ecosystem using an EcoTroph modelling approach.
}

\author{
Colleter Mathieu, Guitton Jerome and Gatti Paul.
}

\examples{
data(ecopath_guinee)
\donttest{E_MSY_0.1(create.ETmain(ecopath_guinee))}
}

\keyword{ ~EcoTroph }
\keyword{ ~MSY }