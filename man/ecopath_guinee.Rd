\name{ecopath_guinee}
\alias{ecopath_guinee}
\docType{data}
\title{
EcoTroph guinean data
}
\description{
This dataset is extract from the 2004 Guinean Ecopath model (Gascuel et al., 2009). It gives a model for the variables wanted names and table format, and the different possibilities of this package (used in function's examples). 
}
\usage{data(ecopath_guinee)}
\format{
  A data frame with 35 observations on the following 8 variables.
  \describe{
    \item{\code{group_name}}{name of the trophic groups used in Ecopath model. Has obligatory to be written group_name.}
    \item{\code{TL}}{a numeric vector corresponding to the trophic level of the associated trophic groups. Has obligatory to be written TL.}
    \item{\code{biomass}}{a numeric vector corresponding to the biomass of the associated of the trophic groups. Has obligatory to be written biomass.}
    \item{\code{prod}}{a numeric vector corresponding to the production/biomass rate. For the Detritus groups (no P/B value needed in Ecopath), put 0 as a value. Has obligatory to be written prod.}
    \item{\code{catch.1}}{a numeric vector corresponding to the catch of the traditional fleet. For all the groups a value must be entered, 0 if no catch are maid}
    \item{\code{catch.2}}{a numeric vector corresponding to the catch of the industrial fleet. For all the groups a value must be entered, 0 if no catch are maid}
    \item{\code{accessibility}}{a numeric vector corresponding to the fraction of the trophic group that can be catch supposing an infinite fishing effort. Has obligatory to be written accessibility.}
    \item{\code{OI}}{a numeric vector corresponding to the omnivory index calculated by Ecopath for each trophic groups. Has obligatory to be written OI.}
  }
}
\details{
No NA are possible in the dataset (0 for the P/B of the detritus groups, 0 for the catch...). Follow the instructions developed in the variables descriptions. Different fleets can be entered in the model using the following system: catch.1, catch.2... If there is only one, you just have to put catch as a variable name.
}
\source{
Gascuel et al. (2009) Impact de la peche sur l'ecosysteme marin de Guinee - Modelisation EwE 1985/2005 - 
}
\references{
Gascuel et al. (2009) Impact de la peche sur l'ecosysteme marin de Guinee - Modelisation EwE 1985/2005 - 
Gascuel et al. (2009) EcoTroph (ET): a trophic level based software for assessing the impacts of fishing on aquatic ecosystems.
}
\examples{
data(ecopath_guinee)
ecopath_guinee
names(ecopath_guinee)
}
\keyword{datasets}
