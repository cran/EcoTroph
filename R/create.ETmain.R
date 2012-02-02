create.ETmain <-
function(ecopath,smooth_type=NULL,sigmaLN_cst=NULL,pas=NULL,shift=NULL,smooth_param=NULL)
{

B <- apply(biomass<-Transpose(tab_smooth<-create.smooth(ecopath=ecopath,smooth_type,sigmaLN_cst,pas,shift,smooth_param),ecopath,'biomass'),1,sum)
B_acc <- apply(biomass_acc<-sweep(biomass,2,ecopath$accessibility,FUN="*"),1,sum)
FL_P <- apply(flowP<-sweep(biomass,2,ecopath$prod,FUN="*"),1,sum)
FL_P_acc <- apply(flowP_acc<-sweep(flowP,2,ecopath$accessibility,FUN="*"),1,sum)

if (B_acc[[1]]==0) B_acc[[1]]<-0.0001
if (FL_P_acc[[1]]==0) FL_P_acc[[1]] <- 0.0001
Kin <- FL_P/B
Kin_acc <- FL_P_acc/B_acc

Y <- list()                            #Handling of the Y in case of multiple fisheries
somme_pecheries<-biomass
somme_pecheries[]<-0
for (pecheries in colnames(ecopath)[grep ("catch",colnames(ecopath))])
{
Y[[paste(pecheries)]] <- Transpose(tab_smooth,ecopath,pecheries)
somme_pecheries <- somme_pecheries+Y[[paste(pecheries)]]
}
Y_tot <- apply(somme_pecheries,1,sum)

F_loss <- Y_tot/FL_P
F_loss_acc <- Y_tot/FL_P_acc
N_loss <- c(log(FL_P[-length(FL_P)]/FL_P[-1])/((V<-as.numeric(rownames(biomass)))[-1]-V[-length(V)])-F_loss[-length(F_loss)],NA)
Fish_mort <- Y_tot/B
Fish_mort_acc <- Fish_mort/(B_acc/B)
Selec <- B_acc/B
Time <- cumsum(c(0,((V <- as.numeric(rownames(biomass)))[-1] - V[-length(V)])/Kin[-length(Kin)]))

ET_Main <- cbind(B,B_acc,FL_P,FL_P_acc,Kin,Kin_acc,Y_tot,F_loss,F_loss_acc,N_loss,Fish_mort,Fish_mort_acc,Selec,Time)
return(list(ET_Main=as.data.frame(ET_Main),biomass=biomass,biomass_acc=biomass_acc,flowP=flowP,flowP_acc=flowP_acc,Y=Y,tab_smooth=tab_smooth))
}

