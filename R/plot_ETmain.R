plot_ETmain <-
function(m){
options(warn=-1)
#the scale parameter can be log or not for the y axis
#library(RColorBrewer)

plot_Transpose(m$biomass,log,title="Biomasses")
readline()
plot_Transpose(m$biomass_acc,log,title="Accessible Biomasses")

for (pecheries in names(m$Y))
{
readline()
plot_Transpose(m$Y[[paste(pecheries)]],log,title=paste(pecheries,": Catch per fishing fleet"))
}
readline()
opar=par(no.readonly=TRUE)
par(mfrow=c(2,1))

par(mar=c(5,5,1,10))

m1<-m$ET_Main[,c("B","B_acc","Y_tot","FL_P","Kin")]
plot(rownames(m1),m1[,"B"],log='y',col=2,type='l',ylim=c(0.0000004,max(m1)),xlim=c(2,5),xlab="TL",ylab="")
lines(rownames(m1),m1[,"B_acc"],type='l',col=3)
lines(rownames(m1),m1[,"Y_tot"],type='l',col=1)
lines(rownames(m1),m1[,"FL_P"],col=4,type='l')
lines(rownames(m1),m1[,"Kin"],type='l',col=5)
legend(5.5,15.5,legend = colnames(m1), bg = 'gray90',col=c(2,3,1,4,5),pch=1,xpd=NA)

readline()
 if (!is.na(sum(m$ET_Main[,"Fish_mort_acc"])))
{
plot(rownames(m$ET_Main),m$ET_Main[,"Fish_mort_acc"],col=1,pch=2,type='l',xlim=c(2,5),ylim=c(0,max(m$ET_Main[,"Fish_mort_acc"])+0.1),xlab="TL",ylab="")
legend(5.5,0.1,legend ="Fish_mort_acc" , bg = 'gray90',col=c(1),pch=c(1),xpd=NA)

readline()
}
m2<-m$ET_Main[,c("F_loss","Fish_mort")]
plot(rownames(m2),m2[,"F_loss"],col=2,pch=2,type='l',ylim=c(0.0000004,max(m2)),xlim=c(2,6),xlab="TL",ylab="")
lines(rownames(m2),m2[,"Fish_mort"],type='l',col=5,pch=3)
legend(6.5,0,legend = colnames(m2), bg = 'gray90',col=c(2,5),pch=c(2,3),xpd=NA)

options(warn=-1)
par(opar)
}

