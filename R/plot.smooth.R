plot.smooth <-
function(x,...)
{
tab_smooth <- x[-1,]                           
plot(rownames(tab_smooth),tab_smooth[,"2"],type="l",ylab="Smooth",xlab="TL",col="blue")
for (compteur in 3:ncol(tab_smooth))
lines(rownames(tab_smooth),tab_smooth[,compteur],type="l",col="blue")    
}

