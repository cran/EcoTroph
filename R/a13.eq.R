#'  function used within the multiplier effort analysis
#' @param compteur counter
#' @param ET_Main output of the create.ETmain function
#' @param biom.mf Effort multiplier applied on biomass
#' @param fish.m Effort multiplier applied on natural mortality
#' @param TopD Parameters of the formula
#' @param FormD Parameters of the formula
#' @param range.TLpred Range of predators trophic level
#' @export
a13.eq=function(compteur,ET_Main,biom.mf,fish.m,TopD,FormD,range.TLpred){
  kin=(ET_Main$Kin[compteur] - ET_Main$Fish_mort[compteur]) * (1 + TopD[compteur] * (sum(biom.mf[(compteur + range.TLpred[1]):(compteur + range.TLpred[2])])^(FormD[compteur]) - sum(ET_Main$B[(compteur+ range.TLpred[1]):(compteur + range.TLpred[2])])^(FormD[compteur]))/(sum(ET_Main$B[(compteur + range.TLpred[1]):(compteur + range.TLpred[2])])^(FormD[compteur]))) +  fish.m[compteur]
  return(kin)
}
