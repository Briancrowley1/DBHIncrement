
#stourhead
stourhead_model<-data.frame(cohort_name=unique(stourhead_cycle_123_AFI$cohort_name),coeff_a=NA,coeff_b=NA)

  for (i in unique(stourhead_cycle_123_AFI$cohort_name)){
    if (NROW(stourhead_cycle_123_AFI[stourhead_cycle_123_AFI$cohort_name==i & !is.na(stourhead_cycle_123_AFI$Height_m),])>=5){
      coeeffs_model<-nls(Height_m ~ 1.3+ exp(a+b/dbhcm), data =stourhead_cycle_123_AFI[stourhead_cycle_123_AFI$cohort_name==i & !is.na(stourhead_cycle_123_AFI$Height_m),],start = c(a=3,b=-12))  # solve eq parameters
      stourhead_model[stourhead_model$cohort_name==i,]$coeff_a<-summary(coeeffs_model)$coef["a","Estimate"]
      stourhead_model[stourhead_model$cohort_name==i,]$coeff_b<-summary(coeeffs_model)$coef["b","Estimate"]
    } else{
      coeffs_model<-data.frame(Cohort_name=i,coeffs_a=NA,coeffs_b=NA)  #store parameters

    }
  }

summary(coeeffs_model)$coef["a","Estimate"]
summary(coeeffs_model)$coef["b","Estimate"]
#monivea
Monivea_cycle_1_PRE

Monivea_model<-data.frame(cohort_name=unique(Monivea_cycle_1_PRE$cohort_name),coeff_a=NA,coeff_b=NA)

for (i in unique(Monivea_cycle_1_PRE$cohort_name)){
  if (NROW(Monivea_cycle_1_PRE[Monivea_cycle_1_PRE$cohort_name==i & !is.na(Monivea_cycle_1_PRE$Height_m),])>=5){
    coeeffs_model<-nls(Height_m ~ 1.3+ exp(a+b/dbhcm), data =Monivea_cycle_1_PRE[Monivea_cycle_1_PRE$cohort_name==i & !is.na(Monivea_cycle_1_PRE$Height_m),],start = c(a=3,b=-12))  # solve eq parameters
    Monivea_model[Monivea_model$cohort_name==i,]$coeff_a<-summary(coeeffs_model)$coef["a","Estimate"]
    Monivea_model[Monivea_model$cohort_name==i,]$coeff_b<-summary(coeeffs_model)$coef["b","Estimate"]
  } else{
    coeffs_model<-data.frame(Cohort_name=i,coeffs_a=NA,coeffs_b=NA)  #store parameters
    
  }
}

# Mellory_cycle_1_PRE
Mellory_cycle_1_PRE


Mellory_model<-data.frame(cohort_name=unique(Mellory_cycle_1_PRE$cohort_name),coeff_a=NA,coeff_b=NA)

for (i in unique(Mellory_cycle_1_PRE$cohort_name)){
  if (NROW(Mellory_cycle_1_PRE[Mellory_cycle_1_PRE$cohort_name==i & !is.na(Mellory_cycle_1_PRE$Height_m),])>=5){
    coeeffs_model<-nls(Height_m ~ 1.3+ exp(a+b/(dbhcm)), data =Mellory_cycle_1_PRE[Mellory_cycle_1_PRE$cohort_name==i & !is.na(Mellory_cycle_1_PRE$Height_m),],start = c(a=1,b=1))  # solve eq parameters
    Mellory_model[Mellory_model$cohort_name==i,]$coeff_a<-summary(coeeffs_model)$coef["a","Estimate"]
    Mellory_model[Mellory_model$cohort_name==i,]$coeff_b<-summary(coeeffs_model)$coef["b","Estimate"]
  } else{
    coeffs_model<-data.frame(Cohort_name=i,coeffs_a=NA,coeffs_b=NA)  #store parameters
    
  }
}


#Knockrath_cycle_1_PRE

Knockrath_cycle_1_PRE


Knockrath_model<-data.frame(cohort_name=unique(Knockrath_cycle_1_PRE$cohort_name),coeff_a=NA,coeff_b=NA)

for (i in unique(Knockrath_cycle_1_PRE$cohort_name)){
  if (NROW(Knockrath_cycle_1_PRE[Knockrath_cycle_1_PRE$cohort_name==i & !is.na(Knockrath_cycle_1_PRE$Height_m),])>=5){
    coeeffs_model<-nls(Height_m ~ 1.3+ exp(a+b/dbhcm), data =Knockrath_cycle_1_PRE[Knockrath_cycle_1_PRE$cohort_name==i & !is.na(Knockrath_cycle_1_PRE$Height_m),],start = c(a=3,b=-12))  # solve eq parameters
    Knockrath_model[Knockrath_model$cohort_name==i,]$coeff_a<-summary(coeeffs_model)$coef["a","Estimate"]
    Knockrath_model[Knockrath_model$cohort_name==i,]$coeff_b<-summary(coeeffs_model)$coef["b","Estimate"]
  } else{
    coeffs_model<-data.frame(Cohort_name=i,coeffs_a=NA,coeffs_b=NA)  #store parameters
    
  }
}
#Knockrath_model[Knockrath_model$cohort_name=="Slow-growing broadleaves",c("coeff_a"  ,  "coeff_b")]<-c(3.422 ,-15.365)

Knockrath_cycle_1_PRE[Knockrath_cycle_1_PRE$cohort_name=="Fast-growing broadleaves",]$species


Tsuga<-AFI_ISN_Data[AFI_ISN_Data$species=="Tsuga",]


table(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Fast-growing broadleaves",]$species)

Knockrath_model[Knockrath_model$cohort_name=="Fast-growing broadleaves",c("coeff_a"  ,  "coeff_b")]<-c(3.494, -12.145 )
summary(Fast_growing_model)
plot(Fast_growing$dbhcm,Fast_growing$Height_m)
curve(predict(Fast_growing_model, newdata = data.frame(dbhcm=x)), add=TRUE)
#Rushmore_AFI

Rushmore_AFI


Rushmore_model<-data.frame(cohort_name=unique(Rushmore_AFI$cohort_name),coeff_a=NA,coeff_b=NA)

for (i in unique(Rushmore_AFI$cohort_name)){
  if (NROW(Rushmore_AFI[Rushmore_AFI$cohort_name==i & !is.na(Rushmore_AFI$Height_m),])>=5){
    coeeffs_model<-nls(Height_m ~ 1.3+ exp(a+b/dbhcm), data =Rushmore_AFI[Rushmore_AFI$cohort_name==i & !is.na(Rushmore_AFI$Height_m),],start = c(a=3,b=-12))  # solve eq parameters
    Rushmore_model[Rushmore_model$cohort_name==i,]$coeff_a<-summary(coeeffs_model)$coef["a","Estimate"]
    Rushmore_model[Rushmore_model$cohort_name==i,]$coeff_b<-summary(coeeffs_model)$coef["b","Estimate"]
  } else{
    coeffs_model<-data.frame(Cohort_name=i,coeffs_a=NA,coeffs_b=NA)  #store parameters
    
  }
}

# Berth_Ddu_AFI
Berth_Ddu_AFI

Berth_Ddu_model<-data.frame(cohort_name=unique(Berth_Ddu_AFI$cohort_name),coeff_a=NA,coeff_b=NA)

for (i in unique(Berth_Ddu_AFI$cohort_name)){
  if (NROW(Berth_Ddu_AFI[Berth_Ddu_AFI$cohort_name==i & !is.na(Berth_Ddu_AFI$Height_m),])>=5){
    coeeffs_model<-nls(Height_m ~ 1.3+ exp(a+b/dbhcm), data =Berth_Ddu_AFI[Berth_Ddu_AFI$cohort_name==i & !is.na(Berth_Ddu_AFI$Height_m),],start = c(a=3,b=-12))  # solve eq parameters
    Berth_Ddu_model[Berth_Ddu_model$cohort_name==i,]$coeff_a<-summary(coeeffs_model)$coef["a","Estimate"]
    Berth_Ddu_model[Berth_Ddu_model$cohort_name==i,]$coeff_b<-summary(coeeffs_model)$coef["b","Estimate"]
  } else{
    coeffs_model<-data.frame(Cohort_name=i,coeffs_a=NA,coeffs_b=NA)  #store parameters
    
  }
}



slow_broadleaves<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Slow-growing broadleaves",]

slow_broadleaves_model<-nls(Height_m ~ 1.3+ exp(a+b/(dbhcm)), data =slow_broadleaves[ !is.na(slow_broadleaves$Height_m),],start = c(a=1,b=1)) 

plot(slow_broadleaves$dbhcm,slow_broadleaves$Height_m)
curve(predict(slow_broadleaves_model, newdata = data.frame(dbhcm=x)), add=TRUE)

Berth_Ddu_model[Berth_Ddu_model$cohort_name=="Slow-growing broadleaves",c("coeff_a"  ,  "coeff_b")]<-c(3.422 ,-15.365)

Other_conifers<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Other conifers",]

Otherconifers_model<-nls(Height_m ~ 1.3+ exp(a+b/(dbhcm)), data =Other_conifers[ !is.na(Other_conifers$Height_m),],start = c(a=1,b=1)) 

plot(Other_conifers$dbhcm,Other_conifers$Height_m)
curve(predict(Otherconifers_model, newdata = data.frame(dbhcm=x)), add=TRUE)

Berth_Ddu_model[Berth_Ddu_model$cohort_name=="Other conifers",c("coeff_a"  ,  "coeff_b")]<-c(3.77, -21.50)
###############################################################################################################################################

Preproc$Estim_Ht <- NA

height_data<-Preproc[!is.na( Preproc$dbhcm) &!is.na( Preproc$Height_m), ]
#do some loop where i go through each plot and calculate the model
#if too little amount of trees then use overall model

# for i in species
coeffs<- list()  
coeffs_plot<-list()
coeffs_plot<-data.frame(Cohort_name=NA,idplot=NA,coeffs_a=NA,coeffs_b=NA)
n<-1
s<-1
for ( i in unique(Preproc$cohort_name)){
  if (NROW(Preproc[Preproc$cohort_name==i & !is.na(Preproc$Height_m),])>=5){
    coeeffs_model<-nls(Height_m ~ 1.3+ exp(a+b/dbhcm), data =Preproc[Preproc$cohort_name==i & !is.na(Preproc$Height_m),],start = c(a=3,b=-12))  # solve eq parameters
    coeffs[[n]]<-data.frame(Cohort_name=i,coeffs_a=summary(coeeffs_model)$coef["a","Estimate"],coeffs_b=summary(coeeffs_model)$coef["b","Estimate"])  #store parameters
    n<-n+1
  } else{
    coeffs[[n]]<-data.frame(Cohort_name=i,coeffs_a=NA,coeffs_b=NA)  #store parameters
    n<-n+1
  }
  for(idp in unique(Preproc$idplots)){ # for i in id plots
    if (NROW(Preproc[Preproc$cohort_name==i & !is.na(Preproc$Height_m) & Preproc$idplots == idp ,])>5){
      coeeffs_model_plot<-nls(Height_m ~ 1.3+ exp(a+b/dbhcm), data =Preproc[Preproc$cohort_name==i & !is.na(Preproc$Height_m) & Preproc$idplots == idp ,],start = c(a=3,b=-12))  # solve eq parameters
      coeffs_plot[s,]<-data.frame(Cohort_name=i,idplot=idp,coeffs_a=summary(coeeffs_model_plot)$coef["a","Estimate"],coeffs_b=summary(coeeffs_model_plot)$coef["b","Estimate"])  #store parameters
      s<-s+1
      
    } else {
      coeffs_plot[s,"Cohort_name"]<-  coeffs[sapply(coeffs, "[[", "Cohort_name") == i][[1]]$Cohort_name #store parameters
      coeffs_plot[s,"idplot"]<-  idp #store parameters
      coeffs_plot[s,"coeffs_a"]<-  coeffs[sapply(coeffs, "[[", "Cohort_name") == i][[1]]$coeffs_a #store parameters
      coeffs_plot[s,"coeffs_b"]<-  coeffs[sapply(coeffs, "[[", "Cohort_name") == i][[1]]$coeffs_b #store parameters
      
      s<-s+1
    }
  }
}
if(is_empty(Preproc$coeffs_a)){
  Preproc$coeffs_a<-NA}
if(is_empty(Preproc$coeffs_b)){
  Preproc$coeffs_b<-NA
}

for ( i in unique(Preproc$cohort_name)){
  for ( idp in unique(Preproc$idplots)){
    if(!is_empty(Preproc[Preproc$cohort_name==i & Preproc$idplots == idp,]$coeffs_a)){
      if(!is.na(coeffs_plot[coeffs_plot$Cohort_name==i & coeffs_plot$idplot == idp,"coeffs_a"])){
        Preproc[Preproc$cohort_name==i & Preproc$idplots == idp,]$coeffs_a<-coeffs_plot[coeffs_plot$Cohort_name==i & coeffs_plot$idplot == idp,"coeffs_a"]
        Preproc[Preproc$cohort_name==i & Preproc$idplots == idp,]$coeffs_b<-coeffs_plot[coeffs_plot$Cohort_name==i & coeffs_plot$idplot == idp,"coeffs_b"]
      }
    }
  }
}

Preproc$Estim_Ht<-1.3+exp(Preproc$coeffs_a+Preproc$coeffs_b/Preproc$dbhcm)


(33.6931-0.274*Preproc[Preproc$cohort_name=="Spruce",]$BAL_ties_adjusted_3 +
    0.1603*Preproc[Preproc$cohort_name=="Spruce",]$plotba_m2_ha)*
  (1-exp(-0.02401*Preproc[Preproc$cohort_name=="Spruce",]$dbhcm^(0.8846+0.006412*
                                                                   Preproc[Preproc$cohort_name=="Spruce",]$BAL_ties_adjusted_3)))


-nls(Height_m ~ 1.3+ exp(a+b/dbhcm), data =Preproc[Preproc$cohort_name==i & !is.na(Preproc$Height_m),],start = c(a=3,b=-12))  # solve eq parameters


nls((Height_m~a-b*BAL_ties_adjusted_3 +c*plotba_m2_ha)*
      (d-exp(i*dbhcm^(f+s*BAL_ties_adjusted_3))),
    start = c(a=33,b=0.2,c=0.16,d=1,i=-0.024,f=0.8846,s=0.006))





Height_m<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Douglas fir"& AFI_ISN_Data$Height_m<200 & !is.na(AFI_ISN_Data$Height_m) & !is.na(AFI_ISN_Data$BAL),]$Height_m
BAL<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Douglas fir"& AFI_ISN_Data$Height_m<200 & !is.na(AFI_ISN_Data$Height_m) & !is.na(AFI_ISN_Data$BAL),]$BAL_ties_adjusted_3
plotba_m2_ha<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Douglas fir"& AFI_ISN_Data$Height_m<200 & !is.na(AFI_ISN_Data$Height_m) & !is.na(AFI_ISN_Data$BAL),]$plotba_m2_ha
dbhcm<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Douglas fir" & AFI_ISN_Data$Height_m<200 & !is.na(AFI_ISN_Data$Height_m) & !is.na(AFI_ISN_Data$BAL),]$dbhcm
Density.plot<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Douglas fir" & AFI_ISN_Data$Height_m<200 & !is.na(AFI_ISN_Data$Height_m) & !is.na(AFI_ISN_Data$BAL),]$Density_ha

fit<-nls(Height_m~(a+b*BAL + c*plotba_m2_ha)*(d-exp(e*dbhcm^(f+g*BAL))),
    start = c(a=33,b=-0.2,c=0.16,d=1,e=-0.024,f=0.88,g=0.006))
summary(fit)

plot(dbhcm,Height_m)
lines(dbhcm,predict(fit))

AIC(fit)
BIC(fit)

impht<-ImputeHeights(Height_m,dbhcm,1)


lines(dbhcm,(101.074129-0.655223*BAL -0.213237*plotba_m2_ha)*(0.807457-exp( -0.073676*dbhcm^( 0.591465+0.003423*BAL))))
abline(coef=c(0,1))

summary(impht$model)



data(spati)

fithd(spati$d,spati$h,spati$plot)

# cohort  and specie specific models
fit <-nls(Height_m~(a+b*plotba_m2_ha+c*Density.plot+d*BAL)*(1-exp(e*dbhcm^(f+g*BAL))),
          start = c(a=12.7, b= 0.26,c=-0.002,d=-0.003,e=-0.07,f=0.8,g=0))
AIC(fit)
AIC(slow_broadleaves_model)
    AIC(Otherconifers_model)
        AIC(Fast_growing_model)
        
        
 # slow growing broadleaves       
        slow_broadleaves      
  sbl_fit<-     nls(Height_m~(a+b*plotba_m2_ha+c*Density_ha+d*BAL)*(1-exp(e*dbhcm^(f+g*BAL))),data = 
                      slow_broadleaves,
                  start = c(a=34.5, b= 0.33,c=-0.003,d=0,e=-0.07,f=0.6,g=0))
        
  Tsuga<- AFI_ISN_Data[AFI_ISN_Data$species=="Tsuga",]  
  
  
  [!is.na(Tsuga$plotba_m2_ha) & !is.na(Tsuga$Density_ha),]
  
  Douglasfir<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Douglas fir",]
 AIC( nls(Height_m ~ 1.3+ exp(a+b/(dbhcm)), data =Douglasfir,start = c(a=1,b=1)) )
 
 
 34.5 0.33 -0.003 e=-0.07 0.6
 AIC(sbl_fit)
 
 
 # Douglas Fir
   Douglasfir<-AFI_Final_Data[AFI_Final_Data$cohort_name=="Douglas fir",]
   Douglasfir<-Douglasfir[!(Douglasfir$dbhcm<20 & Douglasfir$Height_m>40),]
   
   Douglasfir_model<-nls(Height_m ~ 1.3+ exp(a+b/(dbhcm)), data =Douglasfir[ !is.na(Douglasfir$Height_m),],start = c(a=1,b=1)) 
   AIC(Douglasfir_model)
   summary(Douglasfir_model)
   
   fit <-nls(Height_m~(a+b*plotba_m2_ha+c*Density_ha+d*BAL_ties_adjusted_3)*(1-exp(e*dbhcm^(f+g*BAL_ties_adjusted_3))),control = nls.control(maxiter = 1000),
           start = c(a=61.872928, b= -0.163052,c=0.00275,d=-0.424075,e=-0.012714 ,f=1.047755,g=0.006553 ),data = Douglasfir)
   AIC(fit)
   summary(fit)
   Douglasfir$NEW_height<- (78.755716-0.384251*Douglasfir$plotba_m2_ha+0.011092*Douglasfir$Density_ha-0.625095*Douglasfir$BAL_ties_adjusted_3)*
     (1-exp(-0.009568*Douglasfir$dbhcm^(1.024247+0.007481*Douglasfir$BAL_ties_adjusted_3)))
   hist(Douglasfir$dbhcm,breaks = 30)
   # fast growing broadleaves
   
   FGbroadleaves<-AFI_Final_Data[AFI_Final_Data$cohort_name=="Fast-growing broadleaves",]
   
   fit <-nls(Height_m~(a+b*plotba_m2_ha+c*Density_ha+d*BAL_ties_adjusted_3)*(1-exp(e*dbhcm^(f+g*BAL_ties_adjusted_3))),
             control = nls.control(maxiter = 1000),
             start = c(a=61.872928, b= -0.163052,c=0.00275,d=-0.424075,e=-0.012714 ,f=1.047755,g=0.006553 ),
             data = FGbroadleaves)
   AIC(fit)
   summary(fit)
   
   fit <-nls(Height_m~(a+b*plotba_m2_ha+d*BAL_ties_adjusted_3)*(1-exp(e*dbhcm)),
             control = nls.control(maxiter = 1000),
             start = c(a=61.872928, b= -0.163052,d=-0.424075,e=-0.012714 ),
             data = FGbroadleaves)
   AIC(fit)
   summary(fit)
   
   fit <-nls(Height_m~ (a+b*BAL_ties_adjusted_3+
                          c*plotba_m2_ha)*
               (1-exp(-d*dbhcm)),
             control = nls.control(maxiter = 1000),
             start = c(a=14.6661, b= 0.1167,c=0.01872,d=-0.0761624 ),
             data = FGbroadleaves)

   AIC(fit)
   summary(fit)
   
   fit <-nls(Height_m~(a+b*plotba_m2_ha)*(1-exp(e*dbhcm^(f))),
             control = nls.control(maxiter = 1000),
             start = c(a=61.872928, b= -0.163052,e=-0.012714 ,f=1.047755 ),
             data = FGbroadleaves)
   AIC(fit)
   summary(fit)

   AIC( nls(Height_m ~ 1.3+ exp(a+b/(dbhcm)), data =FGbroadleaves,start = c(a=1,b=1)) ) 
   
   FGbroadleaves$NEW_height<-(24.518617+0.027237*FGbroadleaves$plotba_m2_ha)*(1-exp(-0.011482*FGbroadleaves$dbhcm^(1.652651)))
     
     
     
     
     
   Fast_growing<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Fast-growing broadleaves",]
   Fast_growing_model<-nls(Height_m ~ 1.3+ exp(a+b/(dbhcm)), data =Fast_growing[ !is.na(Fast_growing$Height_m),],start = c(a=1,b=1)) 
   AIC(Fast_growing_model)
   hist(Fast_growing$dbhcm,breaks = 30)
   
   # Slow growing broadleaves
   
   
   slow_broadleaves<-AFI_Final_Data[AFI_Final_Data$cohort_name=="Slow-growing broadleaves",]
   slow_broadleaves<-slow_broadleaves[!(slow_broadleaves$dbhcm>60 & slow_broadleaves$Height_m<10),]
   
   slow_broadleaves_model<-nls(Height_m ~ 1.3+ exp(a+b/(dbhcm)), data =slow_broadleaves[ !is.na(slow_broadleaves$Height_m),],start = c(a=1,b=1)) 
   AIC(slow_broadleaves_model)
   summary(slow_broadleaves_model)
   
   
   fit <-nls(Height_m~(a+b*plotba_m2_ha+c*Density_ha+d*BAL_ties_adjusted_3)*(1-exp(e*dbhcm^(f+g*BAL_ties_adjusted_3))),
             control = nls.control(maxiter = 1000),
             start = c(a=61.872928, b= -0.163052,c=0.00275,d=-0.424075,e=-0.012714 ,f=1.047755,g=0.006553 ),
             data = slow_broadleaves)
   AIC(fit)
   summary(fit)
   
   fit <-nls(Height_m~(a+c*Density_ha+d*BAL_ties_adjusted_3)*(1-exp(e*dbhcm^(f+g*BAL_ties_adjusted_3))),
             control = nls.control(maxiter = 1000),
             start = c(a=61.872928,c=0.00275,d=-0.424075,e=-0.012714 ,f=1.047755,g=0.006553 ),
             data = slow_broadleaves)
   AIC(fit)
   summary(fit)
   fit <-nls(Height_m~   (a+b*Density_ha )*
               (1-exp(-c*dbhcm^d)),
             control = nls.control(maxiter = 1000),
             start = c(a=27.770117,b=-0.004689,c=0.054666,d=0.943400 ),
             data = slow_broadleaves)
   AIC(fit)
   summary(fit)
   


   
   table(slow_broadleaves$species)

   hist(slow_broadleaves$dbhcm,breaks = 30)
   
   slow_broadleaves$NEW_height<-(30.894449-0.005030*slow_broadleaves$Density_ha)*(1-exp(-0.070699*slow_broadleaves$dbhcm^(0.794897   )))
   
   
   
   #larch
   
   Larch<-AFI_Final_Data[AFI_Final_Data$cohort_name=="Larch",]
   
   Larch_model<-nls(Height_m ~ 1.3+ exp(a+b/(dbhcm)), data =Larch[ !is.na(Larch$Height_m),],start = c(a=1,b=1)) 
   AIC(Larch_model)
   summary(Larch_model)
   
   table(Larch$species)
   fit <-nls(Height_m~(a+b*plotba_m2_ha+c*Density_ha+d*BAL_ties_adjusted_3)*(1-exp(e*dbhcm^(f+g*BAL_ties_adjusted_3))),
             control = nls.control(maxiter = 1000),
             start = c(a=61.872928, b= -0.163052,c=0.00275,d=-0.424075,e=-0.012714 ,f=1.047755,g=0.006553 ),
             data = Larch)
   AIC(fit)
   summary(fit)
   
   fit <-nls(Height_m~(a+b*plotba_m2_ha+c*Density_ha)*(1-exp(e*dbhcm^(f+g*BAL_ties_adjusted_3))),
             control = nls.control(maxiter = 1000),
             start = c(a=61.872928, b= -0.163052,c=0.00275,e=-0.012714 ,f=1.047755,g=0.006553 ),
             data = Larch)
   AIC(fit)
   summary(fit)
   


   
   Larch$NEW_height<-(30.417416+0.223216*Larch$plotba_m2_ha-0.020314*Larch$Density_ha)*(1-exp(-0.007544*Larch$dbhcm^(1.409916+0.005455*Larch$BAL_ties_adjusted_3)))
   
   plot(Larch$Height_m,Larch$NEW_height,xlim=c(0,60),ylim=c(0,60))
   abline(coef = c(0,1))
   
   hist(Larch$dbhcm,breaks = 30)
   
   mean(Larch$NEW_height-Larch$Height_m,na.rm=T)
   median(Larch$NEW_height-Larch$Height_m,na.rm=T)
   sd(Larch$NEW_height-Larch$Height_m,na.rm=T)
   var(Larch$NEW_height-Larch$Height_m,na.rm=T)
   range(Larch$NEW_height-Larch$Height_m,na.rm=T)
   
   
   mean(Larch$Height_m-Larch$Estim_Ht,na.rm=T)
   median(Larch$Height_m-Larch$Estim_Ht,na.rm=T)
   sd(Larch$Height_m-Larch$Estim_Ht,na.rm=T)
   var(Larch$Height_m-Larch$Estim_Ht,na.rm=T)
   range(Larch$Height_m-Larch$Estim_Ht,na.rm=T)
   
  # Other conifers # "Other conifers"
   Other_conifers<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Other conifers",]
   Other_conifers<-Other_conifers[!(Other_conifers$TREE_NAME == "7 5" & Other_conifers$Site_code=="102"),]
   Other_conifers_model<-nls(Height_m ~ 1.3+ exp(a+b/(dbhcm)), data =Other_conifers[ !is.na(Other_conifers$Height_m),],start = c(a=1,b=1)) 
   AIC(Other_conifers_model)
   summary(Other_conifers_model)
   

   fit <-nls(Height_m~(a+b*plotba_m2_ha+c*Density_ha+d*BAL_ties_adjusted_3)*(1-exp(e*dbhcm^(f+g*BAL_ties_adjusted_3))),
             control = nls.control(maxiter = 1000),
             start = c(a=61.872928, b= -0.163052,c=0.00275,d=-0.424075,e=-0.012714 ,f=1.047755,g=0.006553 ),
             data = Other_conifers)
   AIC(fit)
   summary(fit)
   

   
   Other_conifers$NEW_height<-(77.995880-1.097861*Other_conifers$plotba_m2_ha+0.020173*Other_conifers$Density_ha-0.642245*Other_conifers$BAL_ties_adjusted_3)*
                              (1-exp(-0.003564*Other_conifers$dbhcm^(1.325656+0.014722*Other_conifers$BAL_ties_adjusted_3)))

   
   plot(Other_conifers$Height_m,Other_conifers$NEW_height,xlim=c(0,60),ylim=c(0,60))
   abline(coef = c(0,1))
   
   
   
   mean(Other_conifers$NEW_height-Other_conifers$Height_m,na.rm=T)
   median(Other_conifers$NEW_height-Other_conifers$Height_m,na.rm=T)
   sd(Other_conifers$NEW_height-Other_conifers$Height_m,na.rm=T)
   var(Other_conifers$NEW_height-Other_conifers$Height_m,na.rm=T)
   range(Other_conifers$NEW_height-Other_conifers$Height_m,na.rm=T)
   
   
   mean(Other_conifers$Height_m-Other_conifers$Estim_Ht,na.rm=T)
   median(Other_conifers$Height_m-Other_conifers$Estim_Ht,na.rm=T)
   sd(Other_conifers$Height_m-Other_conifers$Estim_Ht,na.rm=T)
   var(Other_conifers$Height_m-Other_conifers$Estim_Ht,na.rm=T)
   range(Other_conifers$Height_m-Other_conifers$Estim_Ht,na.rm=T)
   
   hist(Other_conifers$dbhcm,breaks = 30)
   
   
   # Pine

   Pine<-   Baronscourt_AFI[Baronscourt_AFI$cohort_name=="Pine",]
   Pine<- Pine[!is.na(Pine$plotba_m2_ha) & !is.na(Pine$BAL_ties_adjusted_3) & !is.na(Pine$Density_ha),]
   
   plot(Pine$dbhcm,Pine$Height_m)
   
   Pine_model<-nls(Height_m ~ 1.3+ exp(a+b/(dbhcm)), data =Pine[ !is.na(Pine$Height_m),],start = c(a=1,b=1)) 
   AIC(Pine_model)
   
   
   table(Pine$species)
   fit <-nls(Height_m~(a+b*plotba_m2_ha+c*Density_ha+d*BAL_ties_adjusted_3)*(1-exp(e*dbhcm^(f+g*BAL_ties_adjusted_3))),
             control = nls.control(maxiter = 1000),
             start = c(a=61.872928, b= -0.163052,c=0.00275,d=-0.424075,e=-0.012714 ,f=1.047755,g=0.006553 ),
             data = Pine[ !is.na(Pine$Height_m),])
   AIC(fit) 
   summary(fit)
   
   
   Pine$plotba_m2_ha
   Pine$Density_ha
   Pine$BAL_ties_adjusted_3
   Pine$dbhcm
   

  
  #Spruce   
   Spruce<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]

   Spruce_model<-nls(Height_m ~ 1.3+ exp(a+b/(dbhcm)), data =Spruce[ !is.na(Spruce$Height_m),],start = c(a=1,b=1)) 
   AIC(Spruce_model)
   summary(Spruce_model)
   
   table(Spruce$species)
   fit <-nls(Height_m~(a+b*plotba_m2_ha+c*Density_ha+d*BAL_ties_adjusted_3)*(1-exp(e*dbhcm^(f+g*BAL_ties_adjusted_3))),
             control = nls.control(maxiter = 1000),
             start = c(a=61.872928, b= -0.163052,c=0.00275,d=-0.424075,e=-0.012714 ,f=1.047755,g=0.006553 ),
             data = Spruce )
   summary(fit) 
   AIC(fit)
   

   
   fit <-nls(Height_m~(a+b*plotba_m2_ha+c*Density_ha+d*BAL_ties_adjusted_3)*(1-exp(e*dbhcm^(f))),
             control = nls.control(maxiter = 1000),
             start = c(a=61.872928, b= -0.163052,c=0.00275,d=-0.424075,e=-0.012714 ,f=1.047755),
             data = Spruce )
   summary(fit) 
   AIC(fit)
   
 
   
   
   Spruce$NEW_height<- (112.614046 -0.341039*Spruce$plotba_m2_ha+
                          0.008297*Spruce$Density_ha+0.550804*Spruce$BAL_ties_adjusted_3)*
     (1-exp(-0.013665*Spruce$dbhcm^(0.794101)))
   
   hist(Spruce$dbhcm,breaks = 30)
   
   plot(Spruce$Height_m,Spruce$NEW_height,xlim=c(0,60),ylim=c(0,60))
   abline(coef = c(0,1))
   
   
   mean(Spruce$NEW_height-Spruce$Height_m,na.rm=T)
   median(Spruce$NEW_height-Spruce$Height_m,na.rm=T)
   sd(Spruce$NEW_height-Spruce$Height_m,na.rm=T)
   var(Spruce$NEW_height-Spruce$Height_m,na.rm=T)
   range(Spruce$NEW_height-Spruce$Height_m,na.rm=T)
   
   
   mean(Spruce$Height_m-Spruce$Estim_Ht,na.rm=T)
   median(Spruce$Height_m-Spruce$Estim_Ht,na.rm=T)
   sd(Spruce$Height_m-Spruce$Estim_Ht,na.rm=T)
   var(Spruce$Height_m-Spruce$Estim_Ht,na.rm=T)
   range(Spruce$Height_m-Spruce$Estim_Ht,na.rm=T)
 
        
   
   
   
   unique(AFI_ISN_Data$cohort_name)
   
   
   
   #Spruce 
   # Height_m~( 1.330e+02+-4.853e-01*plotba_m2_ha+1.449e-02*Density_ha+2.357*BAL_ties_adjusted_3)*(1-exp(-9.206e-03*dbhcm^(8.395e-01-2.698e-03*BAL_ties_adjusted_3)))
   #Larch & Pine
   # (31.482382+0.255232*plotba_m2_ha-0.021031*Density_ha)*(1-exp(-0.029592*dbhcm^(1.035300+0.003920*BAL_ties_adjusted_3)))
   # Douglas fir
   # Height_m~(62.380874-0.181227*plotba_m2_ha+0.005063*Density_ha-0.416852*BAL_ties_adjusted_3)*(1-exp(-0.012156*dbhcm^(1.053443+0.006430*BAL_ties_adjusted_3)))
   # Other conifers
   # Height_m~(131.068583-3.068923*plotba_m2_ha+0.060772*Density_ha)*(1-exp(-0.003678*dbhcm^(1.291154+0.005438*BAL_ties_adjusted_3)))
   # Slow-growing broadleaves
   #Height_m~(26.640039-0.004558*Density_ha)*(1-exp(-0.017944*dbhcm^(1.302086)))
   # Fast-growing broadleaves
   #(24.425932+0.029322*plotba_m2_ha)*(1-exp(-0.011050*dbhcm^(1.669975)))
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   