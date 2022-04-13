#'  function used to compute pB for the higest trophic levels and accessible biomass
#' @param compteur counter
#' @param pb.mf.ac Effort multiplier
#' @param TL_out Trophic level out of the scope
#' @param range.highTL Range of high trophic level
#' @export
regPB.ac=function(compteur,pb.mf.ac,TL_out,range.highTL){
  x. <- TL_out[(range.highTL[1]):(range.highTL[2])]
  y <- log(pb.mf.ac[(range.highTL[1]):(range.highTL[2])])
  #if(fast){reg <- coef(fastLm(y ~ x.))}else{
  reg <- coef(lm(y ~ x.))
  reg.ac<- exp(reg[1] + reg[2] * TL_out[compteur])
  return(reg.ac)
}
