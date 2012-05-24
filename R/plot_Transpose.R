plot_Transpose <-
function(tab_Trans,scale=NULL,title=NULL)    #the scale parameter can be log or not for the y axis
{                                                               

if(is.null(scale))
{
scale <- ''
opar=par(no.readonly=TRUE)
par(mfrow=c(3,1))
m <- (tab_Trans[-1,])
m <- m[-c(32:51),]
tmp_vecteur=m[,1]

y_max<-max(apply(m,1,sum))
y_min <- 0

par(mar=c(5,5,1,15))

plot(rownames(m),apply(m,1,sum),log=scale,col=2,type='l',axes=F,ylab="Biomasses",xlab="TL",main=title,ylim=c(y_min,y_max),xaxs = "i",xlim=c(2,5))

m_cum <- apply(m,1,cumsum)

for(i in length(rownames(m_cum)):1) 
{
polygon(c(2,as.numeric(colnames(m_cum)),5),c(y_min,m_cum[i,],y_min),col=i,xlim=c(2,5),border='NA')
polygon(c(2,2,5,5),c(min(m_cum[i,]),y_min,y_min,min(m_cum[i,])),col='white',border='NA')
}
axis(4)
legend(5.4,0.1,legend = colnames(m), bg = 'gray90',col=seq(1:ncol(m)),pch=1,xpd=NA)

plot(rownames(m),tmp_vecteur,log=scale,col=1,type='l',axes=F,ylab="Biomasses",xlab="TL",ylim=c(y_min,y_max),xlim=c(2,5))
axis(4)
for (compteur in 2:ncol(m))
{
tmp_vecteur=tmp_vecteur+m[,compteur]
lines(rownames(m),tmp_vecteur,col=compteur)
}
   
plot(rownames(m),apply(m,1,sum),log=scale,col=2,type='l',bg='gray',axes=F,ylab="Biomasses",xlab="TL",ylim=c(y_min,y_max),xlim=c(2,5))
axis(1)
axis(4)
par(opar)

}

else
{
scale <- 'y'   
opar=par(no.readonly=TRUE)
par(mfrow=c(3,1))
m <- (tab_Trans[-1,])
m <- m[-c(32:51),]
tmp_vecteur=m[,1]

y_max<-max(apply(m,1,sum))
y_min <- sum(apply(m, 1, sum))*0.0001

par(mar=c(5,5,1,15))

plot(rownames(m),apply(m,1,sum),log=scale,col=2,type='l',axes=F,ylab="Biomasses",xlab="TL",main=title,ylim=c(y_min,y_max),xaxs = "i",xlim=c(2,5))

m_cum <- apply(m,1,cumsum)

for(i in length(rownames(m_cum)):1) 
{
polygon(c(2,as.numeric(colnames(m_cum)),5),c(y_min,m_cum[i,],y_min),col=i,xlim=c(2,5),border='NA')
polygon(c(2,2,5,5),c(min(m_cum[i,]),y_min,y_min,min(m_cum[i,])),col='white',border='NA')
}
axis(4)
legend(5.4,0.1,legend = colnames(m), bg = 'gray90',col=seq(1:ncol(m)),pch=1,xpd=NA)

plot(rownames(m),tmp_vecteur,log=scale,col=1,type='l',axes=F,ylab="Biomasses",xlab="TL",ylim=c(y_min,y_max),xlim=c(2,5))
axis(4)
for (compteur in 2:ncol(m))
{
tmp_vecteur=tmp_vecteur+m[,compteur]
lines(rownames(m),tmp_vecteur,col=compteur)
}
   
plot(rownames(m),apply(m,1,sum),log=scale,col=2,type='l',bg='gray',axes=F,ylab="Biomasses",xlab="TL",ylim=c(y_min,y_max),xlim=c(2,5))
axis(1)
axis(4)
par(opar)  
}
}


