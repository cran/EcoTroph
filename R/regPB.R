#'  function used to compute pB for the higest trophic levels
#' @param compteur counter
#' @param pb.mf Effort multiplier vector
#' @param TL_out Trophic level out of the scope
#' @param range.highTL Range of High Trophic level
#' @importFrom stats lm
#' @importFrom stats coef
#' @export
regPB=function(compteur,pb.mf,TL_out,range.highTL){
  x. <- TL_out[(range.highTL[1]):(range.highTL[2])]
  y <- log(pb.mf[(range.highTL[1]):(range.highTL[2])])
  reg <- coef(lm(y ~ x.))
  reg. <- exp(reg[1] + reg[2] * TL_out[compteur])
  return(reg.)
}
