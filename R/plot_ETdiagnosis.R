plot_ETdiagnosis <-
function(m,scale1=NULL,scale2=NULL,maxrange=NULL){

scale1 <- if(is.null(scale1)) '' else 'y' #the scale parameter can be log or not for the y axis of the BTS
scale2 <- if(is.null(scale2)) '' else 'y' #the scale parameter can be log or not for the y axis of the PTS
if(is.null(maxrange)) maxrange<-5.5

#library(RColorBrewer)

opar=par(no.readonly=TRUE)

par(mar=c(5,5,1,10))

m1<-m$ET_Main_diagnose[,c("R_TOT_biomass","R_TOT_biomass_acc","R_Predat_biom","R_Y","R_Pred_Y")]
plot(rownames(m1),m1[,"R_TOT_biomass"],col=2,type='l',ylim=c(0,max(m1)),xlab="mE",ylab="")
lines(rownames(m1),m1[,"R_TOT_biomass_acc"],type='l',col=3)
lines(rownames(m1),m1[,"R_Predat_biom"],type='l',col=1)
lines(rownames(m1),m1[,"R_Y"],col=4,type='l')
lines(rownames(m1),m1[,"R_Pred_Y"],type='l',col=5)
legend(5.1,0.9,legend = colnames(m1), bg = 'gray90',col=c(2,3,1,4,5),pch=1,xpd=NA)

readline()

m2<-m$ET_Main_diagnose[,c("TL_TOT_biomass","TL_TOT_biomass_acc","TL_Catches")]

plot(rownames(m2),m2[,"TL_TOT_biomass"],col=2,type='l',ylim=c(1,4),xlab="mE",ylab="TL")
lines(rownames(m2),m2[,"TL_TOT_biomass_acc"],type='l',col=5)
lines(rownames(m2),m2[,"TL_Catches"],type='l',col=8)
legend(5.1,1.5,legend = colnames(m2), bg = 'gray90',col=c(2,5),pch=1,xpd=NA)

readline()
par(mar=c(5,5,1,1))

m3 <- m$BIOM_MF

plot(rownames(m3),m3[,1],log=scale1,col=1,type='l',bg='gray',axes=T,ylab="Biomasses",xlab="TL",ylim=c(0.010,max(m3[2:length(rownames(m3)),1])),xlim=c(2,maxrange))
for (i in 2:length(colnames(m3)))
lines(rownames(m3),m3[,i],col=i,type='l')
legend(5,35,legend = colnames(m3), bg = 'gray90',col=c(1:length(colnames(m3))),pch=1,xpd=NA)

readline()

TL_2.5 <- m3['2.5',]/m3['2.5','1']
TL_3 <- m3['3',]/m3['3','1']
TL_3.5 <- m3['3.5',]/m3['3.5','1']
TL_4 <- m3['4',]/m3['4','1']
TL_4.5 <- m3['4.5',]/m3['4.5','1']
TL_5 <- m3['5',]/m3['5','1']
mm <- rbind(TL_2.5,TL_3,TL_3.5,TL_4,TL_4.5,TL_5)

plot(colnames(mm),mm[1,],col=1,type='l',ylim=c(0,max(mm)),xlab="mE",ylab="B/Bref")
for (i in 2:length(rownames(mm)))
lines(colnames(mm),mm[i,],col=i,type='l')
legend(4,2,legend = rownames(mm), bg = 'gray90',col=c(1:length(rownames(mm))),pch=1,xpd=NA)


readline()

m4 <- m$Catches
plot(rownames(m4),m4[,length(colnames(m4))],col=length(colnames(m4)),type='l',bg='gray',axes=T,ylab="Catches",xlab="TL",ylim=c(0,max(m4)),xlim=c(2,maxrange))
for (i in 1:(length(colnames(m4))-1))
lines(rownames(m4),m4[,i],col=i,type='l')
legend(5,0.3,legend = colnames(m3), bg = 'gray90',col=c(1:length(colnames(m4))),xpd=NA,pch=1)

readline()

m5 <- m$FLOW_MF
plot(rownames(m5),m5[,1],log=scale2,col=1,type='l',bg='gray',axes=T,ylab="Flow",xlab="TL",ylim=c(0.01,max(m5[5:length(rownames(m5)),])),xlim=c(2,maxrange))  ##on peut changer le 5 dans ylim par autre chiffre selon besoin?
for (i in 2:length(colnames(m5)))
lines(rownames(m5),m5[,i],col=i,type='l')
legend(4.8,30,legend = colnames(m5), bg = 'gray90',col=c(1:length(colnames(m5))),xpd=NA,pch=1)
}
