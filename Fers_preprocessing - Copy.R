#### FERS single tree modelling 
#11/04/22 updated increment for spruce
# 22/03/22 fixed bugs with ccf bal, density, plot ba m2

Preproc<-Rushmore_cycle_1


Preprocessing_FERS<- function(Preproc){
  library(magrittr)
#Replace all 999 with NA
Preproc[Preproc==999]<- NA

## Density_ha

# Density =D2*ROUND(SUM(J$2:J$14),0)
# D2 = Plot adj
# J = expansionfactor
# use idplots to subset


Preproc<- Preproc[order(Preproc$dbhcm),]
Preproc$Xi<-1:NROW(Preproc)



id<- unique(Preproc$idplots)
Preproc$Density_ha <- NA

for (i in id) {
  y <- round(sum(Preproc[Preproc$idplots == i,  ]$expansionfactor))/
    Preproc[Preproc$idplots == i,  ][1,"Plot.size..ha."]
  Preproc[Preproc$idplots == i,  ]$Density_ha <-
    rep(y, NROW(Preproc[Preproc$idplots == i, ]))
}
rm(i,y)

## BASEL _M2 _ HA
# Ha is 1000m
# Basel area per tree is pi*DBH^2 
# therefore example is 5.2^2*pi/1000 =  
#Preproc[Preproc$BASEL_M_HA>1,]$dbhcm
Preproc$BASEL_M_HA <- (Preproc$dbhcm/200)^2*pi/Preproc$Plot.size..ha.

##  RepreBAHA
# =K2*J2
# k2 is the basel area m2 per hectre
# j = expansion factor

Preproc$RepreBAHA <- (Preproc$dbhcm/200)^2*pi/Preproc$Plot.size..ha.*Preproc$expansionfactor

## BAL
# =SUM(M3:M$14)-M3
# m = RepreBAHA
#54   95  154 1088 6739
for (i in id) {
  for(n in 1:NROW(Preproc[Preproc$idplots==i,])){
    x1<- as.double(Preproc[Preproc$idplots==i,][n,"Xi"])
    Preproc[Preproc$Xi == x1,"BAL"] <- sum(Preproc[Preproc$idplots==i,][n:NROW(Preproc[Preproc$idplots==i,]),"RepreBAHA"])-
      as.double(Preproc[Preproc$idplots==i,][n,"RepreBAHA"])
  }
 min_dbh<- min(Preproc[Preproc$idplots==i,"dbhcm"])
 Tree_id<-Preproc[Preproc$idplots==i & Preproc$dbhcm==min_dbh,]$Tree_ID
 Preproc[Preproc$idplots==i & Preproc$Tree_ID==Tree_id[1],"BAL"][1] <- sum(Preproc[Preproc$idplots==i,]$RepreBAHA)
}
rm(x1,n,i,Tree_id)

### dbhcm ties
#=IF(AND(A2=A3,B2=B3,I2=I3),0,1)
#a = cohort_name
#b = idplots 
# I = DBHcm

Preproc$dbhcm_ties<- 1
if(NROW(Preproc)>1){
for (i in 1:(NROW(Preproc)-1)) {
  if(Preproc[i,"cohort_name"]==Preproc[i+1,"cohort_name"] &&
     Preproc[i,"idplots"]==Preproc[i+1,"idplots"] &&
     Preproc[i,"dbhcm"]==Preproc[i+1,"dbhcm"]){
    Preproc[i,"dbhcm_ties"] <-0
  }
}
} else {
  Preproc[i,"dbhcm_ties"] <-0
}




### bal ties adjusted (1)
# =O2*SUM(M2:M$14)-O2*M2
# O = dbhcm ties
#M = RepreBAHA
Preproc$BAL_ties_adjusted_1<-NA
for (i in id) {
  
  for (n in 1:NROW(Preproc[Preproc$idplots==i,])) {
    x1<- as.double(Preproc[Preproc$idplots==i,][n,"Xi"])
    x2<-Preproc[Preproc$idplots==i,][n,"dbhcm_ties"]*as.double(sum(Preproc[Preproc$idplots==i,][n:NROW(Preproc[Preproc$idplots==i,]),"RepreBAHA"]))-
        Preproc[Preproc$idplots==i,][n,"dbhcm_ties"]*Preproc[Preproc$idplots==i,][n,"RepreBAHA"]
    
    Preproc[Preproc$Xi == x1,"BAL_ties_adjusted_1"] <- x2
  }
  min_dbh<- min(Preproc[Preproc$idplots==i,"dbhcm"])
  Tree_id<-Preproc[Preproc$idplots==i & Preproc$dbhcm==min_dbh,]$Tree_ID
  Preproc[Preproc$idplots==i & Preproc$Tree_ID==Tree_id[1],"BAL_ties_adjusted_1"][1] <- sum(Preproc[Preproc$idplots==i,]$RepreBAHA)
}
rm(x1,x2,i)



### bal ties adjusted (2)
# =IF(AND(P2=0,O2=0),P3,P2)
#p=bal ties adjusted (1)
#O=dbhcm ties
Preproc$BAL_ties_adjusted_2<- Preproc$BAL_ties_adjusted_1
for(i in 1:(NROW(Preproc)-1)) {
  if(Preproc[i,"BAL_ties_adjusted_1"]==0 && Preproc[i,"dbhcm_ties"]==0){
    Preproc[i,"BAL_ties_adjusted_2"] <- Preproc[i+1,"BAL_ties_adjusted_1"]
  }
}
rm(i)
##bal ties adjusted (3)
#=IF(AND(Q2=0,O2=0),Q3,Q2)
#Q = bal ties adjusted (2)
# O = DBH_ties
Preproc$BAL_ties_adjusted_3<- Preproc$BAL_ties_adjusted_2
for(i in 1:(NROW(Preproc)-1)) {
  if(Preproc[i,"BAL_ties_adjusted_2"]==0 && Preproc[i,"dbhcm_ties"]==0){
    Preproc[i,"BAL_ties_adjusted_3"] <- Preproc[i+1,"BAL_ties_adjusted_2"]
  }
}
rm(i)

## plotba_m2_ha
#=SUM(M$2:M$14)
#sum(RepreBAHA) by plot
Preproc$plotba_m2_ha <- NA

for (i in id) {
  y <- sum(Preproc[Preproc$idplots == i,  ]$RepreBAHA)
  Preproc[Preproc$idplots == i,  ]$plotba_m2_ha <-
    rep(y, NROW(Preproc[Preproc$idplots == i, ]))
}
rm(i,y)

## Estim_Ht
#=(33.6931-0.274*R2+0.1603*AA2)*(1-EXP(-0.02401*I2^(0.8846+0.006412*R2)))
# Cohort determines function
cohort<- unique(Preproc$cohort_name)
#Spruce                   Pine                    
#[3] Slow-growing broadleaves Fast-growing broadleaves
#[5] Larch                    Other conifers 
# Spruce
#=(33.6931-0.274*R2+0.1603*AA2)*(1-EXP(-0.02401*I2^(0.8846+0.006412*R2)))
#R = bal ties adjusted (3)   I = dbhcm
#AA = plotba_m2_ha
Preproc$Estim_Ht <- NA

Preproc[Preproc$cohort_name=="Spruce",]$Estim_Ht <- (33.6931-0.274*Preproc[Preproc$cohort_name=="Spruce",]$BAL_ties_adjusted_3 +
                                                       0.1603*Preproc[Preproc$cohort_name=="Spruce",]$plotba_m2_ha)*
                                                     (1-exp(-0.02401*Preproc[Preproc$cohort_name=="Spruce",]$dbhcm^(0.8846+0.006412*
                                                      Preproc[Preproc$cohort_name=="Spruce",]$BAL_ties_adjusted_3)))

# Pine 
#=(16.9015+0.08348*R19+0.08036*AA19)*(1-EXP(-0.04292*I19))
Preproc[Preproc$cohort_name=="Pine",]$Estim_Ht <- (16.9015+0.08348*Preproc[Preproc$cohort_name=="Pine",]$BAL_ties_adjusted_3+
                                                     0.08036*Preproc[Preproc$cohort_name=="Pine",]$plotba_m2_ha)*
                                                     (1-exp(-0.04292*Preproc[Preproc$cohort_name=="Pine",]$dbhcm))

# Slow- growing broadleaves 
#=(29.6779+0.1034*R34)*(1-EXP(-0.04494*I34^0.7813))
Preproc[Preproc$cohort_name=="Slow-growing broadleaves",]$Estim_Ht <- (29.6779+0.1034*
                                                  Preproc[Preproc$cohort_name=="Slow-growing broadleaves",]$BAL_ties_adjusted_3 )*
                                                  (1-exp(-0.04494*
                                                           Preproc[Preproc$cohort_name=="Slow-growing broadleaves",]$dbhcm^0.7813))
# Fast-growing broadleaves
Preproc[Preproc$cohort_name=="Fast-growing broadleaves",]$Estim_Ht <- (14.6661+0.1167*
                                                  Preproc[Preproc$cohort_name=="Fast-growing broadleaves",]$BAL_ties_adjusted_3+
                                                    0.01872*Preproc[Preproc$cohort_name=="Fast-growing broadleaves",]$plotba_m2_ha)*
                                                    (1-exp(-0.0761624*Preproc[Preproc$cohort_name=="Fast-growing broadleaves",]$dbhcm))
#Larch 
Preproc[Preproc$cohort_name=="Larch",]$Estim_Ht <- (32.5922+0.1052*Preproc[Preproc$cohort_name=="Larch",]$BAL_ties_adjusted_3 + 
                                                      0.1229*Preproc[Preproc$cohort_name=="Larch",]$plotba_m2_ha)*
                                                      (1-exp(-0.0235*Preproc[Preproc$cohort_name=="Larch",]$dbhcm))
#Other conifers 
Preproc[Preproc$cohort_name=="Other conifers",]$Estim_Ht <-(23.2265+0.1381*Preproc[Preproc$cohort_name=="Other conifers",]$BAL_ties_adjusted_3+
                                                              0.07035*Preproc[Preproc$cohort_name=="Other conifers",]$plotba_m2_ha)*
                                                           (1-exp(-0.02724*Preproc[Preproc$cohort_name=="Other conifers",]$dbhcm^(1.1021)))

#Douglas fir
Preproc[Preproc$cohort_name=="Douglas fir",]$Estim_Ht <-(23.2265+0.1381*Preproc[Preproc$cohort_name=="Douglas fir",]$BAL_ties_adjusted_3+
                                                              0.07035*Preproc[Preproc$cohort_name=="Douglas fir",]$plotba_m2_ha)*
  (1-exp(-0.02724*Preproc[Preproc$cohort_name=="Douglas fir",]$dbhcm^(1.1021)))
# this has to go after plotba_m2_ha 

#height_imp
# if height ==NA , use height est
Preproc$Height_imp <- Preproc$Height_m
Preproc[is.na(Preproc$Height_imp) ,]$Height_imp <- Preproc[is.na(Preproc$Height_imp),]$Estim_Ht



## OGCD
Preproc$OGCD <- rep(0,NROW(Preproc))

# by cohort
#Spruce                   Pine                    
#[3] Slow-growing broadleaves Fast-growing broadleaves
#[5] Larch                    Other conifers 
# I = dbhcm
# Spruce
#=EXP(-0.3232)*((I2)^0.6441)
 
Preproc[Preproc$cohort_name=="Spruce",]$OGCD <-exp(-0.3232)*((Preproc[Preproc$cohort_name=="Spruce",]$dbhcm)^0.6441)
#Pine
# =EXP(-0.1797)*((I17)^0.6267)
Preproc[Preproc$cohort_name=="Pine",]$OGCD <- exp(-0.1797)*((Preproc[Preproc$cohort_name=="Pine",]$dbhcm)^0.6267)
#Slow-growing broadleaves 
#=EXP(-0.3973)*((I28)^0.7328)
Preproc[Preproc$cohort_name=="Slow-growing broadleaves",]$OGCD <- exp(-0.3973)*((Preproc[Preproc$cohort_name=="Slow-growing broadleaves",]$dbhcm)^0.7328)
#Fast-growing broadleaves
#=EXP(0.1366)*((I33)^0.6183)
Preproc[Preproc$cohort_name=="Fast-growing broadleaves",]$OGCD <- exp(0.1366)*((Preproc[Preproc$cohort_name=="Fast-growing broadleaves",]$dbhcm)^0.6183)
#Larch
# =EXP(-0.3396)*((I90)^0.6823)
Preproc[Preproc$cohort_name=="Larch",]$OGCD <- exp(-0.3396)*((Preproc[Preproc$cohort_name=="Larch",]$dbhcm)^0.6823)
#Other conifer
# =EXP(0.092)*((I142)^0.538)
Preproc[Preproc$cohort_name=="Other conifers",]$OGCD <- exp(0.092)*((Preproc[Preproc$cohort_name=="Other conifers",]$dbhcm)^0.538)
# Douglas fir
Preproc[Preproc$cohort_name=="Douglas fir",]$OGCD <- exp(0.092)*((Preproc[Preproc$cohort_name=="Douglas fir",]$dbhcm)^0.538)

## OGCA 
# pi/4*OGCD^2
Preproc$OGCA <- (pi/4)*(Preproc$OGCD^2)


## EP_OGCA
Preproc$EP_OGCA <- Preproc$OGCA*Preproc$expansionfactor

## CCF
## perplot sum(EP_OGCA)
Preproc$CCF <- NA

for (i in unique(Preproc$idplots)) {
  y <- sum(Preproc[Preproc$idplots == i,  ]$EP_OGCA)/(Preproc[1,]$Plot.size..ha.*100)
  Preproc[Preproc$idplots == i,  ]$CCF <-
    rep(y, NROW(Preproc[Preproc$idplots == i, ]))
}
rm(i,y)

## Logit(CR)
Preproc$Logit_CR <- NA
# by cohort
#Spruce                   Pine                    
#[3] Slow-growing broadleaves Fast-growing broadleaves
#[5] Larch                    Other conifers 
#z = CCF
# AC = height imp
# I = dbhcm
#R = bal ties adjusted (3) 
# Spruce
#=4.8705-0.01757*R2-0.3979*LN(Z2)-0.1194*AC2-0.2962*(AC2/I2)+0.000322*I2^2
for (i in Preproc$Xi) {
  
if(!is.na(Preproc[Preproc$Xi==i,]$Height_m)){
  Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$Logit_CR <- 4.8705-0.01757*Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$BAL_ties_adjusted_3-
    0.3979*log(Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$CCF)-
    0.1194*Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$Height_m-
    0.2962*(Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$Height_m/
              Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$dbhcm)+
    0.000322*Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$dbhcm^2
  #Pine 
  #=3.8478-0.02428*R15-0.2136*LN(Z15)-0.1375*AC15+0.000234*I15^2
  Preproc[Preproc$cohort_name=="Pine" & Preproc$Xi==i,]$Logit_CR <- 3.8478-0.02428*Preproc[Preproc$cohort_name=="Pine" & Preproc$Xi==i,]$BAL_ties_adjusted_3-
    0.2136*log(Preproc[Preproc$cohort_name=="Pine" & Preproc$Xi==i,]$CCF)-
    0.1375*Preproc[Preproc$cohort_name=="Pine" & Preproc$Xi==i,]$Height_m+
    0.000234*Preproc[Preproc$cohort_name=="Pine" & Preproc$Xi==i,]$dbhcm^2
  # Slow growing Broadleaves
  #=1.4773-0.00547*R27-0.01788*AC27-0.5786*(AC27/I27)
  Preproc[Preproc$cohort_name=="Slow-growing broadleaves" & Preproc$Xi==i,]$Logit_CR <- 1.4773-0.00547*Preproc[Preproc$cohort_name=="Slow-growing broadleaves" & Preproc$Xi==i,]$BAL_ties_adjusted_3-
    0.01788*Preproc[Preproc$cohort_name=="Slow-growing broadleaves" & Preproc$Xi==i,]$Height_m-
    0.5786*(Preproc[Preproc$cohort_name=="Slow-growing broadleaves" & Preproc$Xi==i,]$Height_m/
              Preproc[Preproc$cohort_name=="Slow-growing broadleaves" & Preproc$Xi==i,]$dbhcm)
  #fast growing Broadleave
  #=2.4539-0.00941*R33-0.1457*LN(Z33)-0.04574*AC33-0.5915*(AC33/I33)+0.000162*I33^2
  Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & Preproc$Xi==i,]$Logit_CR <- 2.4539-
    0.00941*Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & Preproc$Xi==i,]$BAL_ties_adjusted_3-
    0.1457*log(Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & Preproc$Xi==i,]$CCF)-
    0.04574*Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & Preproc$Xi==i,]$Height_m-
    0.5915*(Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & Preproc$Xi==i,]$Height_m/
              Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & Preproc$Xi==i,]$dbhcm)+
    0.000162*Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & Preproc$Xi==i,]$dbhcm^2
  # Larch
  # =5.8306-0.01822*R136-0.7948*LN(Z136)-0.03937*AC136
  Preproc[Preproc$cohort_name=="Larch" & Preproc$Xi==i,]$Logit_CR <- 5.8306-
    0.01822*Preproc[Preproc$cohort_name=="Larch" & Preproc$Xi==i,]$BAL_ties_adjusted_3-
    0.7948*log(Preproc[Preproc$cohort_name=="Larch" & Preproc$Xi==i,]$CCF)-
    0.03937*Preproc[Preproc$cohort_name=="Larch" & Preproc$Xi==i,]$Height_m
  #Other Conifers
  # =4.1759-0.01941*R137-0.3945*LN(Z137)-0.0965*AC137+0.000463*I137^2
  Preproc[Preproc$cohort_name=="Other conifers" & Preproc$Xi==i,]$Logit_CR <- 4.1759-
    0.01941*Preproc[Preproc$cohort_name=="Other conifers" & Preproc$Xi==i,]$BAL_ties_adjusted_3-
    0.3945*log(Preproc[Preproc$cohort_name=="Other conifers" & Preproc$Xi==i,]$CCF)-
    0.0965*Preproc[Preproc$cohort_name=="Other conifers" & Preproc$Xi==i,]$Height_m+
    0.000463*Preproc[Preproc$cohort_name=="Other conifers" & Preproc$Xi==i,]$dbhcm^2
  # Douglas Fir
  Preproc[Preproc$cohort_name=="Douglas fir" & Preproc$Xi==i,]$Logit_CR <- 4.1759-
    0.01941*Preproc[Preproc$cohort_name=="Douglas fir" & Preproc$Xi==i,]$BAL_ties_adjusted_3-
    0.3945*log(Preproc[Preproc$cohort_name=="Douglas fir" & Preproc$Xi==i,]$CCF)-
    0.0965*Preproc[Preproc$cohort_name=="Douglas fir" & Preproc$Xi==i,]$Height_m+
    0.000463*Preproc[Preproc$cohort_name=="Douglas fir" & Preproc$Xi==i,]$dbhcm^2
}else if(is.na(Preproc[Preproc$Xi==i,]$Height_m)){
Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$Logit_CR <- 4.8705-0.01757*Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$BAL_ties_adjusted_3-
  0.3979*log(Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$CCF)-
  0.1194*Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$Height_imp-
  0.2962*(Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$Height_imp/
            Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$dbhcm)+
  0.000322*Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$dbhcm^2
#Pine 
#=3.8478-0.02428*R15-0.2136*LN(Z15)-0.1375*AC15+0.000234*I15^2
Preproc[Preproc$cohort_name=="Pine" & Preproc$Xi==i,]$Logit_CR <- 3.8478-0.02428*Preproc[Preproc$cohort_name=="Pine" & Preproc$Xi==i,]$BAL_ties_adjusted_3-
  0.2136*log(Preproc[Preproc$cohort_name=="Pine" & Preproc$Xi==i,]$CCF)-
  0.1375*Preproc[Preproc$cohort_name=="Pine" & Preproc$Xi==i,]$Height_imp+
  0.000234*Preproc[Preproc$cohort_name=="Pine" & Preproc$Xi==i,]$dbhcm^2
# Slow growing Broadleaves
#=1.4773-0.00547*R27-0.01788*AC27-0.5786*(AC27/I27)
Preproc[Preproc$cohort_name=="Slow-growing broadleaves" & Preproc$Xi==i,]$Logit_CR <- 1.4773-0.00547*Preproc[Preproc$cohort_name=="Slow-growing broadleaves" & Preproc$Xi==i,]$BAL_ties_adjusted_3-
  0.01788*Preproc[Preproc$cohort_name=="Slow-growing broadleaves" & Preproc$Xi==i,]$Height_imp-
  0.5786*(Preproc[Preproc$cohort_name=="Slow-growing broadleaves" & Preproc$Xi==i,]$Height_imp/
            Preproc[Preproc$cohort_name=="Slow-growing broadleaves" & Preproc$Xi==i,]$dbhcm)
#fast growing Broadleave
#=2.4539-0.00941*R33-0.1457*LN(Z33)-0.04574*AC33-0.5915*(AC33/I33)+0.000162*I33^2
Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & Preproc$Xi==i,]$Logit_CR <- 2.4539-
  0.00941*Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & Preproc$Xi==i,]$BAL_ties_adjusted_3-
  0.1457*log(Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & Preproc$Xi==i,]$CCF)-
  0.04574*Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & Preproc$Xi==i,]$Height_imp-
  0.5915*(Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & Preproc$Xi==i,]$Height_imp/
            Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & Preproc$Xi==i,]$dbhcm)+
  0.000162*Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & Preproc$Xi==i,]$dbhcm^2
# Larch
# =5.8306-0.01822*R136-0.7948*LN(Z136)-0.03937*AC136
Preproc[Preproc$cohort_name=="Larch" & Preproc$Xi==i,]$Logit_CR <- 5.8306-
  0.01822*Preproc[Preproc$cohort_name=="Larch" & Preproc$Xi==i,]$BAL_ties_adjusted_3-
  0.7948*log(Preproc[Preproc$cohort_name=="Larch" & Preproc$Xi==i,]$CCF)-
  0.03937*Preproc[Preproc$cohort_name=="Larch" & Preproc$Xi==i,]$Height_imp
#Other Conifers
# =4.1759-0.01941*R137-0.3945*LN(Z137)-0.0965*AC137+0.000463*I137^2
Preproc[Preproc$cohort_name=="Other conifers" & Preproc$Xi==i,]$Logit_CR <- 4.1759-
  0.01941*Preproc[Preproc$cohort_name=="Other conifers" & Preproc$Xi==i,]$BAL_ties_adjusted_3-
  0.3945*log(Preproc[Preproc$cohort_name=="Other conifers" & Preproc$Xi==i,]$CCF)-
  0.0965*Preproc[Preproc$cohort_name=="Other conifers" & Preproc$Xi==i,]$Height_imp+
  0.000463*Preproc[Preproc$cohort_name=="Other conifers" & Preproc$Xi==i,]$dbhcm^2
# Douglas Fir
Preproc[Preproc$cohort_name=="Douglas fir" & Preproc$Xi==i,]$Logit_CR <- 4.1759-
  0.01941*Preproc[Preproc$cohort_name=="Douglas fir" & Preproc$Xi==i,]$BAL_ties_adjusted_3-
  0.3945*log(Preproc[Preproc$cohort_name=="Douglas fir" & Preproc$Xi==i,]$CCF)-
  0.0965*Preproc[Preproc$cohort_name=="Douglas fir" & Preproc$Xi==i,]$Height_imp+
  0.000463*Preproc[Preproc$cohort_name=="Douglas fir" & Preproc$Xi==i,]$dbhcm^2
}
}
## CR_calculated
Preproc$C_R_est <- exp(Preproc$Logit_CR)/(1+exp(Preproc$Logit_CR))

  Preproc$C_R<-NA
  if("cr" %in% colnames(Preproc)){
    Preproc$C_R<-Preproc$cr
  }
  Preproc[is.na(Preproc$C_R),]$C_R <- Preproc[is.na(Preproc$C_R),]$C_R_est
# DBH_Increment
Preproc$DBH_Increment <- rep(0,NROW(Preproc))
# DBH by cohort
#Spruce                   Pine                    
#[3] Slow-growing broadleaves Fast-growing broadleaves
#[5] Larch                    Other conifers 
# I = dbhcm
#H = cr
#V = CR
#z = CCF
# R = bal ties adjusted (3)
# Spruce

#EXP(-1.8628+0.9456*LN(I2)-0.0005*I2^2+IF(H2=999,LN(V2)*1.1639,LN(H2)*1.1639)-0.000638*Z2-0.00273*R2)
#0.232021549674621+(EXP(-1.11705075590022+0.340165099931831*LN(DBH)+0.0000120955631786065*DBH^2+LN(CR)*0.612
#                            99248598331)))*(EXP(-0.0541004377395342-0.0206101556189141*BAL-
#                                                  0.166818867928906*LN(CFCF)))*((-0.690698317750551+3.06372049516438*(SI/18.3570027593354)))
#SI= (11.39900498*(1 -(1-Preproc$Height_m^0.6241574/11.39900498)^((30 --0.75)/(AGE --0.75)))) ^ (1
#                                                                                  /0.6241574)

#16




# 
# Preproc[Preproc$cohort_name=="Spruce" & is.na(Preproc$cr),]$DBH_Increment <- 0.232021549674621+(exp(-1.11705075590022+0.340165099931831*
#                                                                                                      log(Preproc[Preproc$cohort_name=="Spruce" & is.na(Preproc$cr),]$dbhcm)+
#                                                                                                       0.0000120955631786065*Preproc[Preproc$cohort_name=="Spruce" & is.na(Preproc$cr),]$dbhcm^2+
#                                                                                                        log(Preproc[Preproc$cohort_name=="Spruce" & is.na(Preproc$cr),]$C_R)*0.61299248598331))*
#                                                                                                         (exp(-0.0541004377395342-0.0206101556189141*
#                                                                                                          Preproc[Preproc$cohort_name=="Spruce"& is.na(Preproc$cr),]$BAL_ties_adjusted_3-
#                                                                                                            0.166818867928906*log(Preproc[Preproc$cohort_name=="Spruce"& is.na(Preproc$cr),]$CCF)))*
#                                                                                                            (-0.690698317750551+3.06372049516438*
#                                                                                                             (Preproc[Preproc$cohort_name=="Spruce" & is.na(Preproc$cr),]$YC/16))
# 
# 
# Preproc[Preproc$cohort_name=="Spruce" & !is.na(Preproc$cr),]$DBH_Increment <- 0.232021549674621+(exp(-1.11705075590022+0.340165099931831*
#                                                                                           log(Preproc[Preproc$cohort_name=="Spruce" & !is.na(Preproc$cr),]$dbhcm)+
#                                                                                             0.0000120955631786065*Preproc[Preproc$cohort_name=="Spruce" & !is.na(Preproc$cr),]$dbhcm^2+
#                                                                                             log(Preproc[Preproc$cohort_name=="Spruce" & !is.na(Preproc$cr),]$cr)*0.61299248598331))*
#                                                                                              (exp(-0.0541004377395342-0.0206101556189141*
#                                                                                               Preproc[Preproc$cohort_name=="Spruce"& !is.na(Preproc$cr),]$BAL_ties_adjusted_3-
#                                                                                                 0.166818867928906*log(Preproc[Preproc$cohort_name=="Spruce"& !is.na(Preproc$cr),]$CCF)))*
#                                                                                                 (-0.690698317750551+3.06372049516438*
#                                                                                                  (Preproc[Preproc$cohort_name=="Spruce" & !is.na(Preproc$cr),]$YC/16))

# yc/scaling factor


Preproc[Preproc$cohort_name=="Spruce" & is.na(Preproc$cr),]$DBH_Increment <- 0.232021549674621+(exp(-1.11705075590022+0.340165099931831*
                                                                                                      log(Preproc[Preproc$cohort_name=="Spruce" & is.na(Preproc$cr),]$dbhcm)+
                                                                                                      0.0000120955631786065*Preproc[Preproc$cohort_name=="Spruce" & is.na(Preproc$cr),]$dbhcm^2+
                                                                                                      log(Preproc[Preproc$cohort_name=="Spruce" & is.na(Preproc$cr),]$C_R)*0.61299248598331))*
  (exp(-0.0541004377395342-0.0206101556189141*
         Preproc[Preproc$cohort_name=="Spruce"& is.na(Preproc$cr),]$BAL_ties_adjusted_3-
         0.166818867928906*log(Preproc[Preproc$cohort_name=="Spruce"& is.na(Preproc$cr),]$CCF)))*
  (-0.690698317750551+3.06372049516438*
     (Preproc[Preproc$cohort_name=="Spruce" & is.na(Preproc$cr),]$YC/18.3570027593354))


Preproc[Preproc$cohort_name=="Spruce" & !is.na(Preproc$cr),]$DBH_Increment <- 0.232021549674621+(exp(-1.11705075590022+0.340165099931831*
                                                                                                       log(Preproc[Preproc$cohort_name=="Spruce" & !is.na(Preproc$cr),]$dbhcm)+
                                                                                                       0.0000120955631786065*Preproc[Preproc$cohort_name=="Spruce" & !is.na(Preproc$cr),]$dbhcm^2+
                                                                                                       log(Preproc[Preproc$cohort_name=="Spruce" & !is.na(Preproc$cr),]$cr)*0.61299248598331))*
  (exp(-0.0541004377395342-0.0206101556189141*
         Preproc[Preproc$cohort_name=="Spruce"& !is.na(Preproc$cr),]$BAL_ties_adjusted_3-
         0.166818867928906*log(Preproc[Preproc$cohort_name=="Spruce"& !is.na(Preproc$cr),]$CCF)))*
  (-0.690698317750551+3.06372049516438*
     (Preproc[Preproc$cohort_name=="Spruce" & !is.na(Preproc$cr),]$YC/18.3570027593354))



# Preproc[Preproc$cohort_name=="Spruce" & is.na(Preproc$cr),]$DBH_Increment <-exp(-1.8628+
#                                                             0.9456*log(Preproc[Preproc$cohort_name=="Spruce" & is.na(Preproc$cr),]$dbhcm)-
#                                                             0.0005*Preproc[Preproc$cohort_name=="Spruce" & is.na(Preproc$cr),]$dbhcm^2+
#                                                               log(Preproc[Preproc$cohort_name=="Spruce" & is.na(Preproc$cr),]$C_R)*1.1639-
#                                                                 0.000638*Preproc[Preproc$cohort_name=="Spruce"& is.na(Preproc$cr),]$CCF-
#                                                                 0.00273*Preproc[Preproc$cohort_name=="Spruce"& is.na(Preproc$cr),]$BAL_ties_adjusted_3)
# Preproc[Preproc$cohort_name=="Spruce" & !is.na(Preproc$cr),]$DBH_Increment <- exp(-1.8628+
#                                                                                 0.9456*log(Preproc[Preproc$cohort_name=="Spruce" & !is.na(Preproc$cr),]$dbhcm)-
#                                                                                    0.0005*Preproc[Preproc$cohort_name=="Spruce" & !is.na(Preproc$cr),]$dbhcm^2+
#                                                                                   log(Preproc[Preproc$cohort_name=="Spruce" & !is.na(Preproc$cr),]$cr)*1.1639-
#                                                                                     0.000638*Preproc[Preproc$cohort_name=="Spruce"& !is.na(Preproc$cr),]$CCF-
#                                                                                    0.00273*Preproc[Preproc$cohort_name=="Spruce"& !is.na(Preproc$cr),]$BAL_ties_adjusted_3)                                                            
#Pine
# EXP(-1.3466+0.741*LN(I18)-0.001*I18^2+0.998*IF(H18=999,LN(V18),LN(H18))-0.00066*Z18-0.00417*R18)
Preproc[Preproc$cohort_name=="Pine" & is.na(Preproc$cr),]$DBH_Increment <- exp(-1.3466+
                                                             0.741*log(Preproc[Preproc$cohort_name=="Pine" & is.na(Preproc$cr),]$dbhcm)-
                                                             0.001*Preproc[Preproc$cohort_name=="Pine" & is.na(Preproc$cr),]$dbhcm^2+
                                                             0.998*log(Preproc[Preproc$cohort_name=="Pine" & is.na(Preproc$cr),]$C_R)-
                                                             0.00066*Preproc[Preproc$cohort_name=="Pine" & is.na(Preproc$cr),]$CCF-
                                                             0.00417*Preproc[Preproc$cohort_name=="Pine" & is.na(Preproc$cr),]$BAL_ties_adjusted_3 )
Preproc[Preproc$cohort_name=="Pine" & !is.na(Preproc$cr),]$DBH_Increment <- exp(-1.3466+
                                                              0.741*log(Preproc[Preproc$cohort_name=="Pine" & !is.na(Preproc$cr),]$dbhcm)-
                                                              0.001*Preproc[Preproc$cohort_name=="Pine" & !is.na(Preproc$cr),]$dbhcm^2+
                                                              0.998*log(Preproc[Preproc$cohort_name=="Pine" & !is.na(Preproc$cr),]$cr)-
                                                              0.00066*Preproc[Preproc$cohort_name=="Pine" & !is.na(Preproc$cr),]$CCF-
                                                              0.00417*Preproc[Preproc$cohort_name=="Pine" & !is.na(Preproc$cr),]$BAL_ties_adjusted_3 )
#Slow-growing broadleaves 
#=EXP(-2.5897+LN(I28)*0.7534-0.00068*I28^2-0.0006*Z28-0.00979*R28)
Preproc[Preproc$cohort_name=="Slow-growing broadleaves",]$DBH_Increment <- exp(-2.5897+
                                                         log(Preproc[Preproc$cohort_name=="Slow-growing broadleaves",]$dbhcm)*0.7534-
                                                         0.00068*Preproc[Preproc$cohort_name=="Slow-growing broadleaves",]$dbhcm^2-
                                                         0.0006*Preproc[Preproc$cohort_name=="Slow-growing broadleaves",]$CCF-
                                                         0.00979*Preproc[Preproc$cohort_name=="Slow-growing broadleaves",]$BAL_ties_adjusted_3)
  
#Fast-growing broadleaves
#EXP(-2.8528+LN(I33)*1.1729-0.00012*I33^2+IF(H33=999,LN(V33)*0.8241,LN(H33)*0.8241)-0.000015*Z33)
Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & is.na(Preproc$cr) ,]$DBH_Increment <- exp(-2.8528+
                                                      log(Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & is.na(Preproc$cr) ,]$dbhcm)*1.1729-
                                                      0.00012*Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & is.na(Preproc$cr) ,]$dbhcm^2+
                                                      log(Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & is.na(Preproc$cr) ,]$C_R)*0.8241-
                                                      0.000015*Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & is.na(Preproc$cr) ,]$CCF)
Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & !is.na(Preproc$cr) ,]$DBH_Increment <- exp(-2.8528+
                                                log(Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & !is.na(Preproc$cr) ,]$dbhcm)*1.1729-
                                                0.00012*Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & !is.na(Preproc$cr) ,]$dbhcm^2+
                                                log(Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & !is.na(Preproc$cr) ,]$cr)*0.8241-
                                                0.000015*Preproc[Preproc$cohort_name=="Fast-growing broadleaves" & !is.na(Preproc$cr) ,]$CCF)
#Larch
# =EXP(-2.2969+LN(I103)*0.6338-0.00096*Z103)
Preproc[Preproc$cohort_name=="Larch",]$DBH_Increment <- exp(-2.2969+log(Preproc[Preproc$cohort_name=="Larch",]$dbhcm)*0.6338-
                                                              0.00096*Preproc[Preproc$cohort_name=="Larch",]$CCF)
#Other conifer
#EXP(-1.4191+0.554*LN(I133)-0.00025*I133^2+IF(H133=999,LN(V133)*0.5549,0.5549*LN(H133))-0.00052*Z133-0.00646*R133)
Preproc[Preproc$cohort_name=="Other conifers" & is.na(Preproc$cr),]$DBH_Increment <- exp(-1.4191+
                                                                       0.554*log(Preproc[Preproc$cohort_name=="Other conifers" & is.na(Preproc$cr),]$dbhcm)-
                                                                       0.00025*Preproc[Preproc$cohort_name=="Other conifers" & is.na(Preproc$cr),]$dbhcm^2+
                                                                       log(Preproc[Preproc$cohort_name=="Other conifers" & is.na(Preproc$cr),]$C_R)*0.5549-
                                                                       0.00052*Preproc[Preproc$cohort_name=="Other conifers" & is.na(Preproc$cr),]$CCF-
                                                                       0.00646*Preproc[Preproc$cohort_name=="Other conifers" & is.na(Preproc$cr),]$BAL_ties_adjusted_3)
Preproc[Preproc$cohort_name=="Other conifers" & !is.na(Preproc$cr),]$DBH_Increment <- exp(-1.4191+
                                                    0.554*log(Preproc[Preproc$cohort_name=="Other conifers" & !is.na(Preproc$cr),]$dbhcm)-
                                                    0.00025*Preproc[Preproc$cohort_name=="Other conifers" & !is.na(Preproc$cr),]$dbhcm^2+
                                                    log(Preproc[Preproc$cohort_name=="Other conifers" & !is.na(Preproc$cr),]$cr)*0.5549-
                                                    0.00052*Preproc[Preproc$cohort_name=="Other conifers" & !is.na(Preproc$cr),]$CCF-
                                                    0.00646*Preproc[Preproc$cohort_name=="Other conifers" & !is.na(Preproc$cr),]$BAL_ties_adjusted_3)




Preproc[Preproc$cohort_name=="Douglas fir" & is.na(Preproc$cr),]$DBH_Increment <- 0.11545819232061+(exp(-3.35500746346963+
                                           0.76115629775393*log(Preproc[Preproc$cohort_name=="Douglas fir" & is.na(Preproc$cr),]$dbhcm)-
                                            0.000163629827225797*Preproc[Preproc$cohort_name=="Douglas fir" & is.na(Preproc$cr),]$dbhcm^2+
                                             log(Preproc[Preproc$cohort_name=="Douglas fir" & is.na(Preproc$cr),]$C_R)*0.832657425914154))*
                                              (exp(0.684553692330231+-0.0320013664713506*
                                               Preproc[Preproc$cohort_name=="Douglas fir" & is.na(Preproc$cr),]$BAL_ties_adjusted_3+
                                                -0.0949251336968429*log(Preproc[Preproc$cohort_name=="Douglas fir" & is.na(Preproc$cr),]$CCF)))*
                                                 (exp(0.700527016137579+0.220511238026885*
                                                  (Preproc[Preproc$cohort_name=="Douglas fir" & is.na(Preproc$cr),]$YC/17.2)))

Preproc[Preproc$cohort_name=="Douglas fir" & !is.na(Preproc$cr),]$DBH_Increment <-0.11545819232061+(exp(-3.35500746346963+
                        0.76115629775393*log(Preproc[Preproc$cohort_name=="Douglas fir" & !is.na(Preproc$cr),]$dbhcm)-
                         0.000163629827225797*Preproc[Preproc$cohort_name=="Douglas fir" & !is.na(Preproc$cr),]$dbhcm^2+
                          (Preproc[Preproc$cohort_name=="Douglas fir" & !is.na(Preproc$cr),]$cr)*0.832657425914154))*
                           (exp(0.684553692330231+-0.0320013664713506*
                            Preproc[Preproc$cohort_name=="Douglas fir" & !is.na(Preproc$cr),]$BAL_ties_adjusted_3-
                             0.0949251336968429*log(Preproc[Preproc$cohort_name=="Douglas fir" & !is.na(Preproc$cr),]$CCF)))*
                              (exp(0.700527016137579+0.220511238026885*
                               (Preproc[Preproc$cohort_name=="Douglas fir" & !is.na(Preproc$cr),]$YC/17.2)))
return(Preproc)

}







# 
# dbh_class_function<- function(DBH_class,dbh_tarif){
#   for(i in c(1:NROW(DBH_class))){
#     if(DBH_class[i,"dbhcm"]>=10 & DBH_class[i,"dbhcm"]<90){
#       DBH_class[i,c("dbhclass","tarif")] <- c(plyr::round_any(DBH_class[i,]$dbhcm-10,
#                                                               4, f = ceiling)+10,
#                                               dbh_tarif[dbh_tarif$dbhclass==
#                                                           (plyr::round_any(DBH_class[i,]$dbhcm-10,
#                                                                            4, f = ceiling)+10),"tarif"])
#     }
#   }
#   
#   DBH_class[DBH_class$dbhcm <10,c("dbhclass","tarif")] <- dbh_tarif[dbh_tarif$dbhclass==10,c("dbhclass" , "tarif")]
#   DBH_class[DBH_class$dbhcm >=90,c("dbhclass","tarif")] <- dbh_tarif[dbh_tarif$dbhclass==90,c("dbhclass" , "tarif")]
#   return(DBH_class)
# }


dbh_class_function<- function(DBH_class,dbh_tarif){
  for(i in c(1:NROW(DBH_class))){
    
      DBH_class[i,c("dbhclass","tarif")] <- c(plyr::round_any(DBH_class[i,]$dbhcm,
                                                              5, f = ceiling),
                                              dbh_tarif[dbh_tarif$dbhclass==
                                                          (plyr::round_any(DBH_class[i,]$dbhcm,
                                                                           5, f = ceiling)),"tarif"]*DBH_class[i,]$BASEL_M_HA)
    
  }
  return(DBH_class)
}





N_One_cal <-  function(BASAL,Q,Max_DBH,H){
  K_two<- pi/40000
  c<- ceiling((Max_DBH)/H)
  Fract_one <- c^2/(1-Q)
  Fract_two <- 2*c*Q/((1-Q)^2)
  Fract_three <- Q*(1+Q)*(1-Q^c)/((1-Q)^3)
  K_three<- K_two*(H^2)*(Fract_one-Fract_two+Fract_three)
  N_values<- data.frame(DBH_class=seq(from=((c)*H),to=H,by=-H),N_value=0)
  for(i in 1:c){
    N_values[i,"N_value"] <- Q^(i-1)*(BASAL/K_three)
  }
  plot_N<-ggplot(N_values,aes(x= DBH_class   ,y = N_value)) + geom_smooth(se=F)+scale_y_continuous(breaks=c(0,1,10,100,1000),trans='log')# +
  #  scale_x_continuous(breaks = N_values$dbhclass) + 
   # theme_bw()
  
  return(list(K_three,N_values,plot_N))
}






