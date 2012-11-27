create.ETdiagnosis <- function(data, Mul_eff = NULL, Group = NULL, fleet.of.interest = NULL, same.mE = NULL, B.Input=NULL,
                      Beta = NULL, TopD = NULL, FormD = NULL, TLpred = NULL, Forag.A = NULL, Kfeed = NULL, Ponto = NULL) 
{ 
#Group=NULL;Mul_eff = NULL;same.mE=T;B.Input=NULL; Beta = NULL; TopD = NULL; FormD = NULL; TLpred = NULL;data=create.ETmain(ecopath_guinee);Kfeed = NULL; Ponto = NULL; Forag.A = F
#Group=c('Barracudas+','Carangids')
  #data=create.ETmain(ecopath_guinee,pas=0.1);fleet.of.interest='catch.1'
  #data=create.ETmain(ecopath_guinee);fleet.of.interest=NULL
  ET_Main=data$ET_Main
  
  TL_out <- as.numeric(rownames(ET_Main))
  names(TL_out)=1:length(TL_out)
  n.TL=length(TL_out)
  
  #Initialization
  
  # determinate the number of fleets
  fleet=names(data$Y) ; n.fleet=length(fleet)
  
  if (is.null(same.mE)){same.mE <- FALSE}
  if(!is.null(fleet.of.interest)){same.mE <- FALSE}
  if (is.null(Mul_eff)){Mul_eff.=list()
    for(i in 1:n.fleet){Mul_eff.[[i]]=c(0, 0.2, 0.4, 0.7, 1, 1.5, 2, 2.5, 3, 4, 5)}
    #names(Mul_eff)=paste('mF',1:n.fleet,sep='')
    if(same.mE){Mul_eff.=c(0, 0.2, 0.4, 0.7, 1, 1.5, 2, 2.5, 3, 4, 5)}
  }else{Mul_eff.=list()
      for(i in 1:n.fleet){Mul_eff.[[i]]=Mul_eff}
      #names(Mul_eff)=paste('mF',1:n.fleet,sep='')
      if(same.mE){Mul_eff.=Mul_eff} 
  }
  if (is.null(B.Input)){B.Input <- FALSE}
  if (B.Input){Forag.A <- FALSE}# if biomass input control is implemented Foraging arena theory is not.
  if (is.null(Beta)){Beta <- .2}
  if (is.null(TopD)){TopD <- rep(.4,n.TL)}else{if(length(TopD)==1){TopD=rep(TopD,n.TL)}}
  if (is.null(FormD)){FormD <- rep(.5,n.TL)}else{if(length(FormD)==1){FormD=rep(FormD,n.TL)}}
  if (is.null(TLpred)){TLpred <- 3.5}
  if (is.null(Forag.A)){Forag.A <- FALSE}
  if (is.null(Kfeed)){Kfeed <- rep(5,n.TL)}else{if(length(Kfeed)==1){Kfeed=rep(Kfeed,n.TL)}}
  if (is.null(Ponto)){Ponto <- rep(.3,n.TL)}else{if(length(Ponto)==1){Ponto=rep(Ponto,n.TL)}}
    
  if(B.Input & Forag.A){Forag.A=FALSE
    cat('Foraging Arena Theory & Biomass input control cannot be implemented at the same time, Forag.A is repalced by F.')}
    
  # Compute reference (initial state) fishing mortality coefficients per fleet and per TL levels
  Fish_mort_ref=list()
  Fish_mort_acc_ref=list()
  for(i in 1:n.fleet){
    Fish_mort_ref[[fleet[i]]]=apply(data[['Y']][[fleet[i]]],1,sum)/ET_Main$B
    Fish_mort_acc_ref[[fleet[i]]]=apply(data[['Y']][[fleet[i]]],1,sum)/ET_Main$B_acc
  }
  for(i in 1:n.fleet){Fish_mort_acc_ref[[i]][is.nan(Fish_mort_acc_ref[[i]])]=0}
  
  # Compute reference (initial state) fishing mortality coefficients per fleet,per TL levels and TL groups
  if(!is.null(Group)){
    Fish_mort_gp_ref=list()
    Fish_mort_acc_gp_ref=list()
    for(i in 1:n.fleet){
      # Gasche et Gascuel (unpublished) (7) Fg,i,t=Yg,i,t/Bt
      Fish_mort_gp_ref[[fleet[i]]]=data$Y[[fleet[i]]]/ET_Main$B
      Fish_mort_gp_ref[[fleet[i]]][is.nan(Fish_mort_gp_ref[[fleet[i]]])]=0
      Fish_mort_acc_gp_ref[[fleet[i]]]=data$Y[[fleet[i]]]/ET_Main$B_acc
      Fish_mort_acc_gp_ref[[fleet[i]]][is.nan(Fish_mort_acc_gp_ref[[fleet[i]]])]=0
    }
  }
  
# create a list with for each combination of effort multipliers containing all variables (F,Flow,Biom,Kin)
 
if(!same.mE){
  ff=expand.grid(Mul_eff.)
  if(is.null(fleet.of.interest)){
    for(n in 1:n.fleet){
      if(n==1){FF=ff[,n]}else{FF=paste(FF,'_',ff[,n],sep='')}}
  }else{
    colnames(ff)=c('interest','other')
      for(n in 1:n.fleet){
        if(fleet[n]%in%fleet.of.interest){ff.=ff[,'interest']}else{ff.=ff[,'other']}
        if(n==1){FF=ff.}else{FF=paste(FF,'_',ff.,sep='')}}
  }
}else{
  for(n in 1:n.fleet){
    if(n==1){FF=Mul_eff.}else{FF=paste(FF,'_',Mul_eff.,sep='')}} 
}
 FF=unique(FF)
 comb=as.list(FF)
 names(comb)=FF
  
  for(i in 1:length(comb)){
    
    comb[[i]]=list()
    # 
    comb[[i]][['mf']]=as.numeric(unlist(strsplit(names(comb)[i],'_')))
    names(comb[[i]][['mf']])=paste('mf',1:n.fleet,sep='')
    # F & F_acc
    mf=as.numeric(comb[[i]][['mf']])
    if(is.null(Group)){
      for(j in 1:n.fleet){
        ff=mf[j]*Fish_mort_ref[[j]]
        if(j==1){ff.=ff}else{ff.=ff.+ff}
      }
    }else{
      for(j in 1:n.fleet){
        ff=Fish_mort_gp_ref[[fleet[j]]]
        if(j==1){f.=ff}else{f.=f.+ff}
      }
      for(g in Group){
        for(j in 1:n.fleet){
          ff=mf[j]*Fish_mort_gp_ref[[fleet[j]]][,g]
          if(j==1){f..=ff}else{f..=f..+ff}
        }
        f.[,g]=f..
      }
      ff.=apply(f.,1,sum)
    }
    
    comb[[i]][['Fish_mort']]=ff.
    comb[[i]][['Fish_mort_acc']]=ff./ET_Main$Selec
    
    # Kin & Kin_acc
    comb[[i]][['TEMP_Kin']]=ET_Main[,'Kin']-ET_Main[,'Fish_mort']+comb[[i]][['Fish_mort']]
    comb[[i]][['TEMP_Kin_acc']]=ET_Main[,'Kin_acc']-ET_Main[,'Fish_mort_acc']+comb[[i]][['Fish_mort_acc']]
    comb[[i]][['Kin_MF']]=comb[[i]][['TEMP_Kin']]
    comb[[i]][['Kin_MF_acc']]=comb[[i]][['TEMP_Kin_acc']]
    
    # FLOW_MF & FLOW_MF_acc
    comb[[i]][['Prod_MF']]=ET_Main[,'P']
    comb[[i]][['Prod_MF_acc']]=ET_Main[,'P_acc']
    
    # BIOM_MF & BIOM_MF_acc
    comb[[i]][['BIOM_MF']]=ET_Main[,'B']
    comb[[i]][['BIOM_MF_acc']]=ET_Main[,'B_acc']
  }
  
  # other arguments of mf.diagnosis
  tll=names(TL_out[TL_out>=2.8 & TL_out<=3.3])
  range.TLpred=as.numeric(c(tll[1],tll[length(tll)]))-2
  high.tl=abs(TL_out-5.6)
  lim.high.TL=as.numeric(names(high.tl[high.tl==min(high.tl)[1]]))
  range.highTL=abs(as.numeric(names(TL_out[TL_out %in% 
     range(TL_out[TL_out>=(TL_out[lim.high.TL]-.9) & TL_out<=round(TL_out[lim.high.TL]-.2,1)])
   ]))-lim.high.TL)
  
  # computation runed on each list element
  diagn.list=lapply(comb,mf.diagnosis,ET_Main,data$Y,TL_out,fleet,n.fleet,Fish_mort_ref,Fish_mort_acc_ref,B.Input,
                    Beta,TopD,FormD,TLpred,n.TL,range.TLpred,lim.high.TL,range.highTL,Forag.A,Kfeed,Ponto)
  # mf.diagnosis(comb[[10]],ET_Main,TL_out,fleet,n.fleet,Fish_mort_ref,Fish_mort_acc_ref,Beta,TopD,FormD,TLpred)
  names(diagn.list)=names(comb)
  diagn.list[['fleet.of.interest']]=fleet.of.interest
  class(diagn.list)<-"ETdiagnosis"
  return(diagn.list)
}