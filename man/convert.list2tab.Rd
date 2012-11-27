\name{convert.list2tab}
\alias{convert.list2tab}
\title{
convert.list2tab
}
\description{
convert.list2tab enables to create a liste of data.frames starting from the list returned by create.ETdiagnosis. These data.frames display variables by trophic level and combination of effort multipliers.
}
\usage{
convert.list2tab(diagn.list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{diagn.list}{
is the list returned by the create.ETdiagnosis function. 
}

}
\value{
This function returns a list of data.frame : B, B_acc, P, P_acc, Kin, Kin_acc, Y, Fish_mort, Fish_mort_acc and ET_Main_diagnose.
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
create_ETdiagnosis 
}
\examples{
data(ecopath_guinee)
\donttest{Liste=create.ETdiagnosis(create.ETmain(ecopath_guinee))
Tab=convert.list2tab(Liste)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~Diagnosis }