Transpose <-
function(tab_smooth,ecopath,column)
{
if(missing(tab_smooth))
cat("tab_smooth is missing\n")
if(missing(ecopath))
cat("ecopath is missing\n")
if(missing(column))
cat("column is missing\n")

tab_Trans <- array(dim=c(length(rownames(tab_smooth)),length(ecopath$group_name)),dimnames=list(rownames(tab_smooth),ecopath$group_name))  
pas <- round(0.00+(as.real(rownames(tab_smooth)[length(rownames(tab_smooth))])-as.real(rownames(tab_smooth)[length(rownames(tab_smooth))-1])),3)       ## recalculation of the 'pas', argument of the create.smooth function

for(groupe in ecopath$group_name)
{
tmp_tl=ecopath[ecopath$group_name==groupe,]$TL
tmp_tl <- seq(0,7,pas)[as.numeric(cut(tmp_tl+0.0000000000001,seq(0,7,pas),right=FALSE))]  ## assignment of a trophic class to the trophic groups
tab_Trans[,groupe]=ecopath[ecopath$group_name==groupe,column]*tab_smooth[,as.character(tmp_tl)]
}

return (tab_Trans)
}

