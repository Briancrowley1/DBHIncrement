library(readxl)
library("lmtest")
library("sandwich")
library(minpack.lm)
library(e1071)
library(pdp)
library(corrr)
library(glmnet)

DF_cr<-read_excel(file.choose(),sheet = 2)
OC_cr<-read_excel(file.choose())
OC_cr$`Measured CR`


OC_cr[OC_cr$`Measured height`>800,]$`Measured height`<-NA
  OC_cr[OC_cr$`Measured CR`>800,]$`Measured CR`<-NA

DF_cr[DF_cr$`Measured height`>800,]$`Measured height`<-NA
DF_cr[DF_cr$`Measured CR`>800,]$`Measured CR`<-NA




plot(DF_cr$`Measured CR`,DF_cr$Carbware_Age,xlim=c(0,1))
plot(DF_cr$`Measured CR`,DF_cr$SPP,xlim=c(0,1))
plot(DF_cr$`Measured CR`,DF_cr$DBH,xlim=c(0,1))
plot(DF_cr$`Measured CR`,DF_cr$`Tree-ha`,xlim=c(0,1))
plot(DF_cr$`Measured CR`,DF_cr$BAL,xlim=c(0,1))
plot(DF_cr$`Measured CR`,DF_cr$`Measured height`,xlim=c(0,1))
plot(DF_cr$`Measured CR`,DF_cr$CCF,xlim=c(0,1))
plot(DF_cr$`Measured CR`,DF_cr$BA_ha,xlim=c(0,1))
 

plot(OC_cr$`Measured CR`,OC_cr$Carbware_Age,xlim=c(0,1))
plot(OC_cr$`Measured CR`,OC_cr$SPP,xlim=c(0,1))
plot(OC_cr$`Measured CR`,OC_cr$DBH,xlim=c(0,1))
plot(OC_cr$`Measured CR`,OC_cr$`Tree-ha`,xlim=c(0,1))
plot(OC_cr$`Measured CR`,OC_cr$BAL,xlim=c(0,1))
plot(OC_cr$`Measured CR`,OC_cr$`Measured height`,xlim=c(0,1))
plot(OC_cr$`Measured CR`,OC_cr$CCF,xlim=c(0,1))
plot(OC_cr$`Measured CR`,OC_cr$BA_ha,xlim=c(0,1))


??quantile

Q1 <- quantile(OC_cr$`Measured height`, .25,na.rm=T)
Q3 <- quantile(OC_cr$`Measured height`, .75,na.rm=T)
IQR <- IQR(OC_cr$`Measured height`,na.rm=T)

no_outliers <- subset(OC_cr, OC_cr$`Measured height` > (Q1 - 1.5*IQR) & OC_cr$`Measured height` < (Q3 + 1.5*IQR))
dim(no_outliers)
dim(OC_cr)

OC_cr<-no_outliers
NROW(OC_cr)
NROW(OC_cr_org[!is.na(OC_cr_org$`Measured height`),])


Q1 <- quantile(OC_cr$DBH, .25,na.rm=T)
Q3 <- quantile(OC_cr$DBH, .75,na.rm=T)
IQR <- IQR(OC_cr$DBH,na.rm=T)

no_outliers <- subset(OC_cr, OC_cr$DBH > (Q1 - 1.5*IQR) & OC_cr$DBH < (Q3 + 1.5*IQR))
dim(no_outliers)
dim(OC_cr)
OC_cr<-no_outliers

################################################
mod <- lm(OC_cr$`Measured CR`~OC_cr$`Measured height`)
cooksd <- cooks.distance(mod)
max(cooksd)
sample_size<-NROW(OC_cr)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
NROW(influential)

OC_cr<-OC_cr[-influential,]
#######################################################

mod <- lm(OC_cr$`Measured CR`~OC_cr$BAL)
cooksd <- cooks.distance(mod)
max(cooksd)
sample_size<-NROW(OC_cr)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
NROW(influential)

OC_cr<-OC_cr[-influential,]
#######################################################

mod <- lm(OC_cr$`Measured CR`~OC_cr$DBH)
cooksd <- cooks.distance(mod)
max(cooksd)
sample_size<-NROW(OC_cr)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
NROW(influential)

OC_cr<-OC_cr[-influential,]

#######################################################

mod <- lm(OC_cr$`Measured CR`~OC_cr$Carbware_Age)
cooksd <- cooks.distance(mod)
max(cooksd)
sample_size<-NROW(OC_cr)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
NROW(influential)

OC_cr<-OC_cr[-influential,]



##############################################################################################################
##############################################################################################################
##############################################################################################################
DF_cr_org<-DF_cr
DF_cr<-DF_cr_org
unique(Douglas$Site_code)
#plot(DF_cr$cr,DF_cr$,xlim=c(0,1))
plot(DF_cr$`Measured CR`,DF_cr$SPP,xlim=c(0,1))
plot(DF_cr$`Measured CR`,DF_cr$DBH,xlim=c(0,1))
plot(DF_cr$`Measured CR`,DF_cr$Density_ha,xlim=c(0,1))
plot(DF_cr$`Measured CR`,DF_cr$BAL,xlim=c(0,1))
plot(DF_cr$`Measured CR`,DF_cr$`Measured height`,xlim=c(0,1))
plot(DF_cr$`Measured CR`,DF_cr$CCF,xlim=c(0,1))
plot(DF_cr$`Measured CR`,DF_cr$BA_ha,xlim=c(0,1))
##############################################################################################################
##############################################################################################################
mod <- lm(DF_cr$`Measured CR`~DF_cr$`Measured height`)
cooksd <- cooks.distance(mod)
max(cooksd)
sample_size<-NROW(DF_cr)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
NROW(influential)

DF_cr<-DF_cr[-influential,]

##############################################################################################################
mod <- lm(DF_cr$`Measured CR`~DF_cr$DBH)
cooksd <- cooks.distance(mod)
max(cooksd)
sample_size<-NROW(DF_cr)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
NROW(influential)

DF_cr<-DF_cr[-influential,]
##############################################################################################################
mod <- lm(DF_cr$`Measured CR`~DF_cr$CCF)
cooksd <- cooks.distance(mod)
max(cooksd)
sample_size<-NROW(DF_cr)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
NROW(influential)

DF_cr<-DF_cr[-influential,]

##############################################################################################################
mod <- lm(DF_cr$`Measured CR`~DF_cr$BA_ha)
cooksd <- cooks.distance(mod)
max(cooksd)
sample_size<-NROW(DF_cr)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
NROW(influential)

DF_cr<-DF_cr[-influential,]





1.914e-01-1.425e-02*Preproc[Preproc$cohort_name=="Douglas fir" & Preproc$Xi==i,]$BAL+ 
  2.961e-01*log(Preproc[Preproc$cohort_name=="Douglas fir" & Preproc$Xi==i,]$CCF)+ 
  -1.524e+00*(Preproc[Preproc$cohort_name=="Douglas fir" & Preproc$Xi==i,]$Height_imp/Preproc[Preproc$cohort_name=="Douglas fir" & Preproc$Xi==i,]$ dbhcm)+ 
  -1.073e-04*Preproc[Preproc$cohort_name=="Douglas fir" & Preproc$Xi==i,]$dbhcm^2 



DF_cr$sl<-log(DF_cr$`Measured CR`/(1-DF_cr$`Measured CR`))


test1<-nls(sl~a+b*BA_ha+
      c*log(CCF)+
      #d*`Measured height`+
      e*(`Measured height`/DBH)+
      f*DBH^2,data=DF_cr,start = c(a=1.914e-01,
                                   b=-1.425e-02,
                                   c=2.961e-01,
                                   #d=0,
                                   e=-1.524e+00,
                                   f=-1.073e-04))
summary(test1)
AIC(test1)
DF_cr_org$test1<-3.3990741  -0.0253937*DF_cr_org$BA_ha+
  -0.2871765*log(DF_cr_org$CCF)+
  #d*DF_cr_org$`Measured height`+
  -0.4617208*(DF_cr_org$`Measured height`/DF_cr_org$DBH)+
  -0.0002994*DF_cr_org$DBH^2
plot(DF_cr_org$sl,DF_cr_org$test1,xlim=c(-1.7,3),ylim=c(-1.7,3))
abline(coef = c(0,1))
plot(DF_cr$sl,DF_cr$test1,xlim=c(-1,2),ylim=c(-1,2))


DF_cr_org$test2<-exp(DF_cr_org$test1)/(1+exp(DF_cr_org$test1))
plot(DF_cr_org$`Measured CR`,DF_cr_org$test2,xlim=c(0,1),ylim=c(0,1))


plot(DF_cr_org$`Measured CR`,0.9996508*DF_cr_org$test2,xlim=c(0,1),ylim=c(0,1))
abline(coef = c(0,1))
exp(sum(DF_cr$sl,na.rm=T))/exp(sum(DF_cr[!is.na(DF_cr$`Measured CR`),]$test1))
min(DF_cr_org$sl,na.rm=T)
abline(coef = c(0,1))



DF_cr_org$org_cr_model<-1.914e-01-1.425e-02*DF_cr_org$BAL+ 
  2.961e-01*log(DF_cr_org$CCF)+ 
  -1.524e+00*(DF_cr_org$`Measured height`/DF_cr_org$DBH)+ 
  -1.073e-04*DF_cr_org$DBH^2 


plot(DF_cr_org$`Measured CR`,DF_cr_org$org_cr_model,xlim=c(0,1),ylim=c(0,1))

abline(coef = c(0,1))
min(DF_cr_org$org_cr_model,na.rm=T)

summary(lm(DF_cr_org$`Measured CR`~DF_cr_org$org_cr_model))
summary(lm(DF_cr_org$`Measured CR`~DF_cr_org$test2))

##########################################################
##########################################################
##########################################################
Douglas<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Douglas fir",]
Douglas<-Douglas[Douglas$Site_code!="67",]
DF_cr<-Douglas
plot(DF_cr$cr,DF_cr$SPP,xlim=c(0,1))
plot(DF_cr$cr,DF_cr$dbhcm,xlim=c(0,1))
plot(DF_cr$cr,DF_cr$Density_ha,xlim=c(0,1))
plot(DF_cr$cr,DF_cr$BAL,xlim=c(0,1))
plot(DF_cr$cr,DF_cr$Height_imp,xlim=c(0,1))
plot(DF_cr$cr,DF_cr$CCF,xlim=c(0,1))
plot(DF_cr$cr,DF_cr$plotba_m2_ha,xlim=c(0,1))
########################################################
##########################################################
############################################################

Douglas$sl_est<-1.914e-01-1.425e-02*Douglas$BAL_ties_adjusted_3+
  2.961e-01*log(Douglas$CCF)+
  -1.524e+00*(Douglas$Height_m/Douglas$dbhcm)+
  -1.073e-04*Douglas$dbhcm^2

Douglas$test1<-3.3990741  -0.0253937*Douglas$plotba_m2_ha+
  -0.2871765*log(Douglas$CCF)+
  #d*DF_cr_org$`Measured height`+
  -0.4617208*(Douglas$Height_imp/Douglas$dbhcm)+
  -0.0002994*Douglas$dbhcm^2
Douglas$test2<-exp(Douglas$test1)/(1+exp(Douglas$test1))
#Douglas$cr_im<-exp(Douglas$sl_est)/(1+exp(Douglas$sl_est))
plot(Douglas$cr,Douglas$test2,xlim=c(0,1),ylim=c(0,1))

plot(Douglas[Douglas$Site_code!="67",]$cr,Douglas[Douglas$Site_code!="67",]$test2,xlim=c(0,1),ylim=c(0,1))
abline(coef = c(0,1))
plot(Douglas$cr,Douglas$C_R_est,xlim=c(0,1),ylim=c(0,1))
abline(coef = c(0,1))
summary(lm(Douglas$cr~Douglas$test2))
summary(lm(Douglas$cr~Douglas$C_R_est))
###########################################################################
###########################################################################
###########################################################################
###########################################################################
SS_cr$Measured.CR
SS_cr<-SS_cr_org
plot(SS_cr$Measured.CR,SS_cr$SPP,xlim=c(0,1))
plot(SS_cr$Measured.CR,SS_cr$DBH,xlim=c(0,1))
plot(SS_cr$Measured.CR,SS_cr$Tree.ha,xlim=c(0,1))
plot(SS_cr$Measured.CR,SS_cr$BAL,xlim=c(0,1))
plot(SS_cr$Measured.CR,SS_cr$Measured.height,xlim=c(0,1))
plot(SS_cr$Measured.CR,SS_cr$Measured.height/SS_cr$DBH,xlim=c(0,1))

plot(SS_cr$Measured.CR,SS_cr$CCF,xlim=c(0,1))
plot(SS_cr$test2,SS_cr$Annual.DBH.increment_observsed,xlim=c(0,1))
##############################################################################################################
##############################################################################################################
mod <- lm(SS_cr$Measured.CR~SS_cr$Measured.height)
cooksd <- cooks.distance(mod)
max(cooksd)
sample_size<-NROW(SS_cr)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
NROW(influential)

SS_cr<-SS_cr[-influential,]
ggplot(SS_cr,aes(as.character(PlotID),Measured.CR))+geom_boxplot()+ylim(0,1)

View(SS_cr)
##############################################################################################################
mod <- lm(log(SS_cr$Measured.CR)~SS_cr$DBH)
cooksd <- cooks.distance(mod)
max(cooksd)
sample_size<-NROW(SS_cr)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
NROW(influential)

SS_cr<-SS_cr[-influential,]
##############################################################################################################
# mod <- lm(SS_cr$Measured.CR~SS_cr$CCF)
# cooksd <- cooks.distance(mod)
# max(cooksd)
# sample_size<-NROW(SS_cr)
# influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
# NROW(influential)
# 
# SS_cr<-SS_cr[-influential,]

##############################################################################################################
mod <- lm(SS_cr$Measured.CR~SS_cr$BA_ha)
cooksd <- cooks.distance(mod)
max(cooksd)
sample_size<-NROW(SS_cr)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
NROW(influential)

SS_cr<-SS_cr[-influential,]
##############################################################################################################
##############################################################################################################

SS_cr$old_cr<--5.871e-01-
  4.598e-03*SS_cr$BAL+
  2.426e-01*log(SS_cr$CCF) -
  2.721e-02*SS_cr$Measured.height -
  7.982e-01*(SS_cr$Measured.height/SS_cr$DBH) + 
  1.543e-04*SS_cr$DBH^2 
plot(SS_cr$Measured.CR,SS_cr$old_cr,xlim=c(0,1),ylim=c(0,1))

SS_cr$old_cr1<-4.1759-
  0.01941*SS_cr$BAL-
  0.3945*log(SS_cr$CCF)-
  0.0965*SS_cr$Measured.height+
  0.000463*SS_cr$DBH^2
##############################################################################################################
##############################################################################################################
SS_cr$sl<-log(SS_cr$Measured.CR/(1-SS_cr$Measured.CR))
after<-nls(sl~ a+ b*BAL+
             c*log(CCF)+
             d*Measured.height+
             e*(Measured.height/DBH)+
             f*DBH^2,
           data=SS_cr,
           start = c(a=5.211  ,b=-1.476e-02,c=-3.681e-01,d=-1.554e-01  ,
                     e= 2.346e-01   ,f=9.326e-05))
summary(after)
AIC(after)
SS_cr<-SS_cr_org
SS_cr$test1<-a+b*SS_cr$BAL+
  c*log(SS_cr$CCF)+
  d*SS_cr$Measured.height+
  e*(SS_cr$Measured.height/SS_cr$DBH)+
  f*SS_cr$DBH^2

plot(after)

abline(coef = c(0,1))
SS_cr$test2<-exp(SS_cr$test1)/(1+exp(SS_cr$test1))
plot(SS_cr$Measured.CR,SS_cr$test2*0.9801289,xlim=c(0,1),ylim=c(0,1))
abline(coef = c(0,1))


plot(SS_cr$Measured.CR,SS_cr$old_cr,xlim=c(0,1),ylim=c(0,1))
abline(coef = c(0,1))
plot(SS_cr$Measured.CR,SS_cr$old_cr1,xlim=c(0,1),ylim=c(0,1))
abline(coef = c(0,1))

sum(exp(SS_cr$Measured.CR),na.rm=T)/sum(exp(SS_cr[!is.na(SS_cr$Measured.CR),]$test2),na.rm=T)



# y=exp(x)/(1+exp(x))
# y+y*exp(x)=exp(x)
# y=exp(x)(1-y)
# y/(1-y)=exp(x)
# x=log(y/(1-y)

Spruce$test1<-a+b*Spruce$BAL+
  c*log(Spruce$CCF)+
  d*Spruce$Height_imp+
  e*(Spruce$Height_imp/Spruce$dbhcm)+
  f*Spruce$dbhcm^2
Spruce$test2<-exp(Spruce$test1)/(1+exp(Spruce$test1))

plot(Spruce$cr,Spruce$test2,xlim=c(0,1),ylim = c(0,1))
abline(coef = c(0,1))


#################################################################
unique(AFI_ISN_Data$cohort_name)
Spruce<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]
Douglas<-Douglas[Douglas$Site_code!="67",]
DF_cr<-Douglas
plot(DF_cr$cr,DF_cr$SPP,xlim=c(0,1))
plot(DF_cr$cr,DF_cr$dbhcm,xlim=c(0,1))
plot(DF_cr$cr,DF_cr$Density_ha,xlim=c(0,1))
plot(DF_cr$cr,DF_cr$BAL,xlim=c(0,1))
plot(DF_cr$cr,DF_cr$Height_m,xlim=c(0,1))
plot(DF_cr$cr,DF_cr$CCF,xlim=c(0,1))
plot(DF_cr$cr,DF_cr$plotba_m2_ha,xlim=c(0,1))

# 
# best cr for each cohort
# calibrate  dbh inc model for each cohort
# use better model afi/isn/ nfi
# 
# summary of 
# kevin model, used model and NFI model.
# only use afi for spruce and douglas fir
# summary document of models, rsquared AIC and model info
# best model used for next step.

#model selected , need parameters in table.
# like for cr model and height. 
##########################################################################
Spruce$dbh_act_increment#
Spruce<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]

Spruce<-Spruce[Spruce$dbh_act_increment>0 & !is.na(Spruce$dbh_act_increment),]

summary(lm(Spruce$dbh_act_increment~Spruce$Density_ha*Spruce$dbhcm))


plot(Spruce$dbhcm,Spruce$dbh_act_increment)
plot(Spruce$dbh_act_increment,Spruce$Estim_Ht)
plot(Spruce$dbh_act_increment,Spruce$BASEL_M_HA)
plot(Spruce$dbh_act_increment,Spruce$Density_ha)
plot(Spruce$dbh_act_increment,Spruce$RepreBAHA)
plot(Spruce$dbh_act_increment,log(1/Spruce$BAL))
plot(Spruce$dbh_act_increment,log(Spruce$plotba_m2_ha))
plot(Spruce$dbh_act_increment,Spruce$CCF)
plot(Spruce$dbh_act_increment,Spruce$cr)
plot(Spruce$dbh_act_increment,Spruce$average_est_inc)
abline(coef = c(0,1))
SS_cr<-SS_cr_org[sample(1:NROW(SS_cr_org),1000),]
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$YC)
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$SPP)
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$Age)
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$DBH)
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$Tree.ha)
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$BAL)
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$Measured.height)
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$Measured.CR)
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$CCF)
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$BA_ha)
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$Pre.DBH.inc.current.model,
     xlim=c(0,2.5),ylim=c(0,2.5))
abline(coef = c(0,1))

cor(SS_cr$Annual.DBH.increment_observsed,SS_cr$Pre.DBH.inc.current.model)^2


plot(DF_cr_org$`Annual DBH increment_observsed`,DF_cr_org$YC)
plot(DF_cr_org$`Annual DBH increment_observsed`,DF_cr_org$SPP)
plot(DF_cr_org$`Annual DBH increment_observsed`,DF_cr_org$DBH)
plot(DF_cr_org$`Annual DBH increment_observsed`,DF_cr_org$`Tree-ha`)
plot(DF_cr_org$`Annual DBH increment_observsed`,DF_cr_org$BAL)
plot(DF_cr_org$`Annual DBH increment_observsed`,DF_cr_org$`Measured height`)
plot(DF_cr_org$`Annual DBH increment_observsed`,DF_cr_org$`Measured CR`)
plot(DF_cr_org$`Annual DBH increment_observsed`,DF_cr_org$CCF)
plot(DF_cr_org$`Annual DBH increment_observsed`,DF_cr_org$BA_ha)


ll<-0.5 * (sum(log(w)) - n * (log(2 * pi) + 1 - log(n) + log(sum(w * res^2))))

y<-rnorm(100)
x<-rnorm(100)
m<-lm(y ~ x) 
Spruce$res<-Spruce$dbh_act_increment-Spruce$newModel4
res<-Spruce$res
res<-res[!is.na(res)]
n<-length(res)    
w<-rep(1,n) #not

ll<-0.5 * (sum(log(w)) - n * (log(2 * pi) + 1 - log(n) + log(sum(w * res^2))))
plot(Spruce[!is.na(Spruce$res),]$dbh_act_increment,res/sd(res))
abline(coef = c(0,0))

k.original<-length(m$coefficients)
df.ll<-9+1 
bic<- -2 * ll + log(n) * df.ll
aic<- -2 * ll + 2 * df.ll
aic

      a+(exp(b+
               c*log(DBH)+
               d*DBH^2+
               log(Measured.CR)*e))*(exp(f+
                                  g*BAL+
                                  h*log(CCF)))*(i+
                                                  j*(YC/k))

   ( nls(Annual.DBH.increment_observsed~a+exp(d*DBH^2+
                                               log(Measured.CR)*e)*(exp(f+
                                                                  g*BAL+
                                                                  h*log(CCF)))*(YC),
        data=SS_cr_org,
        start=
          c(a=8.955e-02   ,
            d=-6.821e-06 ,
            e=4.988e-01 ,
            f=-3.164e+00,
            g=-1.587e-02,
            h=-1.103e-01
            )))

 SS_cr_org$new_model_inc<-  9.302e-02+exp(1.787e-04*SS_cr_org$DBH^2+
           log(SS_cr_org$Measured.CR)*3.979e-01)*(exp(-2.637e+00+
                                              -1.876e-02*SS_cr_org$BAL+
                                              -7.085e-02*log(SS_cr_org$CCF)))*(SS_cr_org$YC)
plot(SS_cr_org$Annual.DBH.increment_observsed,SS_cr_org$new_model_inc,xlim=c(0,2.5),
     ylim = c(0,2.5))   
abline(coef = c(0,1))   
   cor(SS_cr_org[!is.na(SS_cr_org$new_model_inc),]$Annual.DBH.increment_observsed,SS_cr_org[!is.na(SS_cr_org$new_model_inc),]$new_model_inc)^2
plot(SS_cr_org$Annual.DBH.increment_observsed,SS_cr_org$Pre.DBH.inc.current.model,xlim=c(0,2.5),
     ylim = c(0,2.5))   
abline(coef = c(0,1))  
   
   
   #############################################################################################
#############################################################################################
#############################################################################################
#  AIC calculator
   Spruce1<-Spruce[!is.na(Spruce$dbh_act_increment)&!is.na(Spruce$average_est_inc),]
res<-Spruce1$dbh_act_increment-Spruce1$average_est_inc
n<-nrow(Spruce1)    
w<-rep(1,n) #not

ll<-0.5 * (sum(log(w)) - n * (log(2 * pi) + 1 - log(n) + log(sum(w * res^2))))


k.original<-length(m$coefficients)
df.ll<-6+1 
bic<- -2 * ll + log(n) * df.ll
aic<- -2 * ll + 2 * df.ll
aic
sd(res)
#############################################################################################
#############################################################################################
#############################################################################################
# group values in BAL ranges

SS_cr_ba<-SS_cr_org[SS_cr_org$BAL>10 & SS_cr_org$BAL<15,]
summary( nls(Annual.DBH.increment_observsed~exp(d*DBH^2+
                                             log(Measured.CR)*e)*(exp(f+
                                                                        h*log(CCF)))*(YC),
      data=SS_cr_ba,
      start=
        c(
          d=0.0001678  ,
          e=4.988e-01 ,
          f=-2.686e+00,
          h=-5.377e-02
        )))


SS_cr_ba$ba_model<-   exp(2.574e-04 *SS_cr_ba$DBH^2+
                                      log(SS_cr_ba$Measured.CR)*4.313e-01 )*
                      (exp(-2.759-7.078e-02*log(SS_cr_ba$CCF)))*
                        (SS_cr_ba$YC)
plot(SS_cr_ba$Annual.DBH.increment_observsed,SS_cr_ba$ba_model,xlim=c(0,2.5),
     ylim = c(0,2.5))
abline(coef=c(0,1))
plot(SS_cr_ba$Annual.DBH.increment_observsed,SS_cr_ba$Pre.DBH.inc.current.model,xlim=c(0,2.5),
     ylim = c(0,2.5))
abline(coef=c(0,1))
SS_cr_ba<-SS_cr_ba[!is.na(SS_cr_ba$ba_model),]
cor(SS_cr_ba$Annual.DBH.increment_observsed,SS_cr_ba$ba_model)^2


plot(Spruce$dbh_act_increment,Spruce$Estim_Ht,xlim=c(0,2))

nls(dbh_act_increment~a+ c*exp(Estim_Ht^b),data=Spruce,start=c(a=-3.00845,b=0.08594 ,c=1))


plot(Spruce$dbh_act_increment,-1.2582+Spruce$Estim_Ht^0.2135,xlim=c(0,2),ylim = c(0,2))
abline(coef = c(0,1))
Douglas<- AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Douglas fir",]
Douglas<-Douglas[Douglas$dbh_act_increment>0,]

Spruce<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]

Spruce<-Spruce[Spruce$dbh_act_increment>0 & !is.na(Spruce$dbh_act_increment),]
Spruce<-Spruce[Spruce$Site_code!="Baronscourt",]
Spruce<-Spruce[Spruce$Site_code!="Nant yr Eira INV",]
Spruce<-Spruce[Spruce$Site_code!="Bryn Arau Duon INV",]

#Spruce<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Douglas fir",]
max(Spruce$dbh_act_increment)
Spruce<-Spruce[Spruce$dbh_act_increment<5,]
NROW(Spruce)
plot(Spruce$dbh_act_increment,Spruce$dbhcm)
plot(Spruce$dbh_act_increment,Spruce$Estim_Ht)
plot(Spruce$dbh_act_increment,Spruce$BASEL_M_HA)
plot(Spruce$dbh_act_increment,Spruce$Density_ha)
plot(Spruce$dbh_act_increment,Spruce$RepreBAHA)
plot(Spruce$dbh_act_increment,Spruce$BAL_ties_adjusted_3)
plot(Spruce$dbh_act_increment,(Spruce$plotba_m2_ha))
plot(Spruce$dbh_act_increment,Spruce$CCF)

plot(Spruce$dbh_act_increment,Spruce$cr)
plot(Spruce$dbh_act_increment,Spruce$average_est_inc,xlim=c(0,3),ylim=c(0,3))
abline(coef = c(0,1))
plot(Spruce$dbh_act_increment,Spruce$BAL*Spruce$CCF)

plot(Spruce$dbhcm,Spruce$Estim_Ht)
plot(Spruce$dbhcm,Spruce$Density_ha)
plot(Spruce$dbhcm,Spruce$BAL)
plot(Spruce$dbhcm,(Spruce$plotba_m2_ha))
plot(Spruce$dbhcm,Spruce$CCF)

plot(Spruce$Estim_Ht,Spruce$Density_ha)
plot(Spruce$Estim_Ht,Spruce$BAL)
plot(Spruce$Estim_Ht,(Spruce$plotba_m2_ha))
plot(Spruce$Estim_Ht,Spruce$CCF)

plot(Spruce$Density_ha,Spruce$BAL)
plot(Spruce$Density_ha,(Spruce$plotba_m2_ha))
plot(Spruce$Density_ha,Spruce$CCF)

plot(Spruce$BAL,(Spruce$plotba_m2_ha))

plot(Spruce$plotba_m2_ha,Spruce$CCF)
################################################
Spruce1<-Spruce[,c("dbh_act_increment","dbhcm","Height_imp","Density_ha",
                   "BAL","plotba_m2_ha","CCF","cr")]

mod <- lm(Spruce$dbh_act_increment~Spruce$BAL)
cooksd <- cooks.distance(mod)
max(cooksd)
sample_size<-NROW(OC_cr)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
NROW(influential)

Spruce<-Spruce[-influential,]
################################################
mod <- lm(Spruce$dbh_act_increment~Spruce$Estim_Ht)
cooksd <- cooks.distance(mod)
max(cooksd)
sample_size<-NROW(OC_cr)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
NROW(influential)

Spruce<-Spruce[-influential,]
################################################

################################################
mod <- lm(Spruce$dbh_act_increment~Spruce$BAL)
cooksd <- cooks.distance(mod)
max(cooksd)
sample_size<-NROW(OC_cr)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
NROW(influential)

Spruce<-Spruce[-influential,]
################################################


#Spruce<-Spruce[-influential,]
################################################
mod <- lm(Spruce$dbh_act_increment~Spruce$CCF)
cooksd <- cooks.distance(mod)
max(cooksd)
sample_size<-NROW(OC_cr)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
NROW(influential)

Spruce<-Spruce[-influential,]
################################################


Spruce<-Spruce[-influential,]
Spruce<-Spruce[Spruce$cr>0 & !is.na(Spruce$cr),]
  
  
  summary(lm(dbh_act_increment~Estim_Ht/dbhcm+ exp(Estim_Ht)+BAL*
       plotba_m2_ha +cr+CCF*Density_ha,Spruce1))
  
  
  library(corrr)
library(ggplot2)
  
  library(ggcorrplot)
  library("factoextra")
  library(FactoMineR)
  
  
  plot(as.factor(Spruce$Development.stage),Spruce$dbh_act_increment)
  Spruce1<-Spruce1[!is.na(Spruce1$cr),]
  Spruce1<-Spruce1[!is.na(Spruce1$Height_imp),]
  
  Spruce_n <- scale(Spruce1)
  head(Spruce_n)
  corr_matrix <- cor(Spruce_n)
  ggcorrplot(corr_matrix)
  data.pca <- princomp(corr_matrix)
  summary(data.pca)  
  
  data.pca$loadings[, 1:2]
  fviz_eig(data.pca, addlabels = TRUE)
  fviz_pca_var(data.pca, col.var = "black")
  
  
  
  
  
  
  
  plot(1:100,1:100*10+(1:100)^1.722)
  
  plot(1:100,(1:100)^2)
  
  
  
  View(Spruce[Spruce$Estim_Ht>14 & Spruce$dbhcm<12,])
 
     a+(exp(b+c*log(DBH)+d*DBH^2+
              log(Measured.CR)*e))+(exp(f+
                                          g*BAL+
                                          h*log(CCF)))+(i+
                                                          j*(YC/k))
  
  nls(dbh_act_increment~a+h*log(CCF)+(exp(b+c*(dbhcm)+g*BAL)),data=Spruce,
      start=c(a=-0.6701  ,b=0.01737 ,c=0.007669,h=0.002390,g=-0.001185))
  
  summary(nls(dbh_act_increment~a+0.002390*log(CCF)+
                                        exp(Height_imp)+(exp(0.01737+0.007669*dbhcm+
                                                -0.001185*BAL)),
      data=Spruce,start=c(a=0.6),
      control = list(maxiter = 500)))
  
  
  Spruce$new_inc<-  0.002390*log(Spruce$CCF)-0.6701+
    (exp(0.01737+0.007669*Spruce$dbhcm+
             -0.001185*Spruce$BAL))
       
       
  plot(Spruce$dbh_act_increment,(Spruce$new_inc),xlim = c(0,2.5),ylim=c(0,2.5))
  abline(coef = c(0,1))
  legend("bottomright", legend = unique(as.factor(Spruce$Size)), col = 1:4, pch = 19, bty = "n")
  
  library(e1071)
  library(pdp)
  ############################################################################
  Spruce$BAL
  model.svm <- svm(dbh_act_increment~BAL,Spruce, probability = TRUE)
  pred <- predict(model.svm, Spruce, probability = TRUE)
  
  
  par.Petal_W <- partial(model.svm, pred.var = c("BAL"), chull = TRUE)
  plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
  plot.Petal_W
  
  ############################################################################
  Spruce$Estim_Ht
  model.svm <- svm(dbh_act_increment~Estim_Ht,Spruce, probability = TRUE)
  pred <- predict(model.svm, Spruce, probability = TRUE)
  
  
  par.Petal_W <- partial(model.svm, pred.var = c("Estim_Ht"), chull = TRUE)
  plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
  plot.Petal_W
  
  ############################################################################
  Spruce$dbhcm
  model.svm <- svm(dbh_act_increment~dbhcm,Spruce, probability = TRUE)
  pred <- predict(model.svm, Spruce, probability = TRUE)
  
  
  par.Petal_W <- partial(model.svm, pred.var = c("dbhcm"), chull = TRUE)
  plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
  plot.Petal_W
  
  ############################################################################
  Spruce$cr
  model.svm <- svm(dbh_act_increment~cr,Spruce, probability = TRUE)
  pred <- predict(model.svm, Spruce, probability = TRUE)
  
  
  par.Petal_W <- partial(model.svm, pred.var = c("cr"), chull = TRUE)
  plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
  plot.Petal_W
  
  ############################################################################
  Spruce$YC
  model.svm <- svm(dbh_act_increment~YC,Spruce, probability = TRUE)
  pred <- predict(model.svm, Spruce, probability = TRUE)
  
  
  par.Petal_W <- partial(model.svm, pred.var = c("YC"), chull = TRUE)
  plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
  plot.Petal_W
  
  ############################################################################
  Spruce$Density_ha
  model.svm <- svm(dbh_act_increment~Density_ha,Spruce, probability = TRUE)
  pred <- predict(model.svm, Spruce, probability = TRUE)
  
  
  par.Petal_W <- partial(model.svm, pred.var = c("Density_ha"), chull = TRUE)
  plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
  plot.Petal_W
  
  ############################################################################
  Spruce$plotba_m2_ha
  model.svm <- svm(dbh_act_increment~plotba_m2_ha,Spruce, probability = TRUE)
  pred <- predict(model.svm, Spruce, probability = TRUE)
  
  
  par.Petal_W <- partial(model.svm, pred.var = c("plotba_m2_ha"), chull = TRUE)
  plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
  plot.Petal_W
  
  ############################################################################
  Spruce$CCF
  model.svm <- svm(dbh_act_increment~CCF+BAL+plotba_m2_ha+Density_ha,Spruce, probability = TRUE)
  pred <- predict(model.svm, Spruce, probability = TRUE)
  
  
  par.Petal_W <- partial(model.svm, pred.var = c("CCF"), chull = TRUE)
  plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
  plot.Petal_W
  
  Spruce$Estim_Ht
  model.svm <- svm(dbh_act_increment~Estim_Ht/dbhcm + CCF+plotba_m2_ha+Density_ha+YC+BAL+cr,
                   Spruce, probability = TRUE)
  pred <- predict(model.svm, Spruce, probability = TRUE)
  
  
  par.Petal_W <- partial(model1, pred.var = c("Estim_Ht"), chull = TRUE)
  plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
  plot.Petal_W
  
  # Predicting
  pred <- predict(model.svm, Spruce, probability = TRUE)
  # Confusion Matrix
  table(Spruce$dbh_act_increment, pred)
  
  
 stvi_af<- read.csv(file.choose())
 Spruce_stvi<-stvi_af[stvi_af$cohort_name=="Spruce",]
 Spruce_stvi<-Spruce_stvi[Spruce_stvi$dbh_act_increment>0,]
  plot(Spruce_stvi$dbh_act_increment,Spruce_stvi$STVI_dbh)
  
  
  summary(model1)
 model1<- nls(dbh_act_increment~a+f*dbhcm^2+exp((Estim_Ht/dbhcm)^b+c*BAL/plotba_m2_ha+CCF^e),
              Spruce,
              start=c(a=-3.7137,b=-0.1721,c=-0.1283,e=  -0.1650 ,f=1 ),
              control = nls.control(maxiter = 1000))
 AIC(model1)
 
plot(Spruce$dbh_act_increment,
     -2.293+1.548e-04*Spruce$dbhcm^2+exp((Spruce$Estim_Ht/Spruce$dbhcm)^6.735e-02+
                              -7.364e-02*Spruce$BAL/Spruce$plotba_m2_ha+Spruce$CCF^-5.335e-01) ,
     xlim=c(0,3),ylim = c(0,3))  
  plot(Spruce$dbh_act_increment,Spruce$average_est_inc,xlim = c(0,3),ylim = c(0,3))
  
  
  
  

  model1<- nls(dbh_act_increment~a+YC/16.75991*(f*dbhcm^2+g*BAL+exp((Estim_Ht)^b+CCF^e)),
               Spruce,
               start=c(a=5.138e-01 ,b=2.045e-01,e= 4.032e-01 ,
                       f= 1.481e-04 ,g=-3.515e-03 ),
               control = nls.control(maxiter = 1000))
  
  model1<- nls(dbh_act_increment~a+YC/16.75991*(f*dbhcm^2+g*BAL+CCF^e)+exp((Estim_Ht)^b),
               Spruce,
               start=c(a= -2.5  ,b=3.575e-02,e=0,
                       f= 1.245e-04,g=-3.528e-03  ),
               control = nls.control(maxiter = 1000))
  plot(model1)
  summary(model1)
  
  mean(Spruce$YC,na.rm=T)
  
  plot(Spruce$dbh_act_increment,
       5.198e-01 +1.528e-04*Spruce$dbhcm^2+-4.253e-03 *Spruce$BAL+exp((Spruce$Estim_Ht)^2.466e-01+-5.869e-01*Spruce$plotba_m2_ha+Spruce$CCF^3.894e-01),
       xlim=c(0,3),ylim=c(0,3))
  abline(coef = c(0,1))
  
  range(5.198e-01 +1.528e-04*Spruce$dbhcm^2+-4.253e-03 *Spruce$BAL+exp((Spruce$Estim_Ht)^2.466e-01+-5.869e-01*Spruce$plotba_m2_ha+Spruce$CCF^3.894e-01),
        na.rm=T)
  
  range(Spruce$newModel,na.rm=T)
  
  #########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Spruce$newModel<-5.198e-01 +1.528e-04*Spruce$dbhcm^2+-4.253e-03 *Spruce$BAL+exp((Spruce$Estim_Ht)^2.466e-01+-5.869e-01*Spruce$plotba_m2_ha+Spruce$CCF^3.894e-01)
  Spruce$newModel_YC<--2.621+Spruce$YC/16.75991*(1.087e-04*Spruce$dbhcm^2-
                                                   3.656e-03*Spruce$BAL+
                                                   Spruce$CCF^-4.398e-01)+
                                                  exp((Spruce$Estim_Ht)^3.871e-02)
  
  plot(Spruce$dbh_act_increment,Spruce$newModel_YC,xlim=c(0,3),ylim=c(0,3))
  abline(coef = c(0,1))
  range(Spruce$newModel_YC,na.rm=T)
   #########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  Spruce$residuals<- Spruce$dbh_act_increment-Spruce$newModel
  variance = lm(abs(Spruce[!is.na(Spruce$dbh_act_increment) & !is.na(Spruce$average_est_inc),]$residuals) ~ 
                  Spruce[!is.na(Spruce$dbh_act_increment) & !is.na(Spruce$average_est_inc),]$newModel)$fitted.values^2
  Spruce$weights<-NA
  Spruce[!is.na(Spruce$dbh_act_increment) & !is.na(Spruce$average_est_inc),]$weights <- 1 / variance
  Spruce$weights=Spruce$weights/mean(Spruce$weights,na.rm=T)
  model2<- nls(dbh_act_increment~a+f*dbhcm^2+g*BAL+exp((Estim_Ht)^b+c*plotba_m2_ha+CCF^e),
               Spruce[!is.na(Spruce$dbh_act_increment) & !is.na(Spruce$average_est_inc),],
               start=c(a=5.144e-01,b=3.919e-02,c=-4.690e-01,e= 3.556e-01 ,
                       f= 1.544e-04 ,g=-4.159e-03 ),
               control = nls.control(maxiter = 1000),
               weights = Spruce[!is.na(Spruce$dbh_act_increment) & !is.na(Spruce$average_est_inc),]$weights)
  AIC(model2)
  summary(model2)
  plot(model2)
  plot(model1)
  y=c(6,9,12,14,15)
  x=c(2,3,5,7,9)
  q
 q<- lm(y~x)
plot(Spruce$dbhcm,Spruce$weights)
  q$fitted.values
  weight_model<-lm(log(weights)~dbhcm,Spruce )
  AIC(weight_model)
  summary(weight_model)
  lines(Spruce$dbhcm, exp(0.9702509-0.0312535*Spruce$dbhcm), col="red", lwd=2)
  plot(Spruce$dbhcm,Spruce$residuals)
  
  
  model1<- nls(dbh_act_increment~(a+f*dbhcm^2+g*BAL+exp((Estim_Ht)^b+c*plotba_m2_ha+CCF^e)),
               Spruce,
               start=c(a=5.144e-01,b=3.919e-02,c=-4.690e-01,e= 3.556e-01 ,
                       f= 1.544e-04 ,g=-4.159e-03 ),
               control = nls.control(maxiter = 1000))
  
  Spruce$newModel3<-5.434e-01 +
    1.465e-04*Spruce$dbhcm^2-5.050e-03*Spruce$BAL+
    exp((Spruce$Estim_Ht)^3.352e-01+
          -6.838e-01*Spruce$plotba_m2_ha+
          Spruce$CCF^4.203e-01)
  Spruce$newModel4<-(5.434e-01 +
    1.465e-04*Spruce$dbhcm^2-5.050e-03*Spruce$BAL+
    exp((Spruce$Estim_Ht)^3.352e-01+
          -6.838e-01*Spruce$plotba_m2_ha+
          Spruce$CCF^4.203e-01))*exp(0.9702509-0.0312535*Spruce$dbhcm)
  
  plot(Spruce$dbh_act_increment,Spruce$newModel3,
       xlim=c(0,3),ylim = c(0,3))
  abline(coef = c(0,1))
  plot(Spruce$dbh_act_increment,Spruce$newModel4,
       xlim=c(0,3),ylim = c(0,3))
  abline(coef = c(0,1))
  

  
  correction_factor<-data.frame(Site=character(),Correction_factor=as.numeric())
 Spruce$newModel1<-Spruce$newModel
 
 for(st in unique(Spruce$Site_code)){
    factor_cor<-sum(exp(Spruce[Spruce$Site_code==st,]$dbh_act_increment),na.rm=T)/sum(exp(Spruce[Spruce$Site_code==st,]$newModel),na.rm=T)
     Spruce[Spruce$Site_code==st,]$newModel1<-Spruce[Spruce$Site_code==st,]$newModel*factor_cor
     correction_factor[NROW(correction_factor)+1,]<-c(st,factor_cor)
     }
 plot(Spruce$dbh_act_increment,Spruce$newModel1,xlim=c(0,3),ylim=c(0,3))
 abline(coef = c(0,1))
 
  
   plot(Spruce$newModel,(Spruce$dbh_act_increment-Spruce$average_est_inc)/sd(Spruce$dbh_act_increment-Spruce$average_est_inc,na.rm=T))
abline(coef = c(0,0))  

  
  sum(exp(Spruce$dbh_act_increment),na.rm=T)/sum(exp(Spruce$newModel),na.rm=T)
  
  for(st in unique(Spruce$Site_code)){
    dev.new()
    plot(Spruce[Spruce$Site_code==st,]$dbh_act_increment,Spruce[Spruce$Site_code==st,]$average_est_inc,
         xlim=c(0,3),ylim=c(0,3))
    title(st)
    abline(coef = c(0,1))
  }

  
  
  # Load libraries

  
  # Robust t test
  coeftest(model1, vcov = vcovHC(model1, type = "HC0"))
  


  # uses output of nlsLM() of the minpack.lm package to get an asymptotic 
  # covariance matrix without assuming homoscedasticity
  
  # arguments:
  # 
  #nlslmout: return value from nlsLM()
  plot(Spruce$Estim_Ht,Spruce$dbh_act_increment)
  model5<- nlsLM(dbh_act_increment~(a+YC*K+exp(b*Estim_Ht)+f*dbhcm^2+g*BAL+exp(c*plotba_m2_ha+CCF^e)),
               Spruce,
               start=c(a=5.144e-01,b=0,c=-4.690e-01,e= 3.556e-01 ,
                       f= 1.544e-04 ,g=-4.159e-03 ,K=0),
               control = nls.control(maxiter = 1000))
  AIC(model5)
  plot(model5)
  # value: approximate covariance matrix for the estimated
  #        parameter vector
  
  nlsvcovhc <- function(nlslmout) {
    # notation: g(t,b) is the regression model, where x is the vector of
    # variables for a given observation; b is the estimated parameter
    # vector; x is the matrix of predictor values
    b <- coef(nlslmout)
    m <- nlslmout$m
    # y - g:
    resid <- m$resid()
    # row i of hmat will be deriv of g(x[i,],b) with respect to b
    hmat <- m$gradient()
    # calculate the artificial "x" and "y" of the algorithm
    fakex <- hmat
    fakey <- resid + hmat %*% b
    # -1 means no constant term in the model
    lmout <- lm(fakey ~ fakex - 1)
    vcovHC(lmout)
  }
  
  nlsvcovhc(model1)
  vcov(model1)

  

  model5<- nlsLM(new_x_exact~(a+f*dbhcm^2+g*BAL+exp((Estim_Ht)^b+c*plotba_m2_ha+CCF^e)),
                 Spruce,
                 start=c(a=5.144e-01,b=3.919e-02,c=-4.690e-01,e= 3.556e-01 ,
                         f= 1.544e-04 ,g=-4.159e-03 ),
                 control = nls.control(maxiter = 1000))
  AIC(model5)
  plot(model5)
  
  library(MASS)
  b<-boxcox(lm(dbh_act_increment~dbhcm*BAL*Estim_Ht*CCF*plotba_m2_ha,Spruce))
  

  
  lambda <- b$x[which.max(b$y)]
  lambda
  Spruce$new_x_exact <- (Spruce$dbh_act_increment^ lambda - 1) / lambda  
hist(new_x_exact,breaks = 15)

  
model6<- nlsLM(new_x_exact~(a+f*dbhcm^2+g*BAL+exp((Estim_Ht)^b+c*plotba_m2_ha+CCF^e)),
               Spruce,
               start=c(a=5.144e-01,b=3.919e-02,c=-4.690e-01,e= 3.556e-01 ,
                       f= 1.544e-04 ,g=-4.159e-03 ),
               control = nls.control(maxiter = 1000))
model7<- nlsLM(new_x_exact~a+f*dbhcm^2+g*BAL+exp((Estim_Ht)^b+CCF^e+c*plotba_m2_ha),
               Spruce,
               start=c(a=-11.957795 ,b=-9.670302 ,c=0,
                       e=-1,f=0.001494,g=0.065365 ),
               control = nls.control(maxiter = 1000))

library(lmtest)
AIC(model7)
plot(model7)
Spruce$newModel
plot(Spruce$residuals,Spruce$dbh_act_increment)
plot(Spruce$residuals,Spruce$Estim_Ht)
plot(Spruce$residuals,Spruce$BASEL_M_HA)
plot(Spruce$residuals,Spruce$Density_ha)
plot(Spruce$residuals,Spruce$RepreBAHA)
plot(Spruce$residuals,Spruce$BAL)
plot(Spruce$residuals,(Spruce$plotba_m2_ha))
plot(Spruce$residuals,Spruce$CCF)


x<-nlsLM(dbh_act_increment~a+exp(b*newModel3^c),Spruce,start=c(a=-1,b=1,c=1))
AIC(x)
plot(Spruce$dbh_act_increment,-0.8764 +exp(0.6334*Spruce$newModel^0.9693),
     xlim = c(0,3),ylim=c(0,3))
abline(coef = c(0,1))

plot(Spruce$dbh_act_increment,Spruce$newModel3,
     xlim = c(0,3),ylim=c(0,3))
abline(coef = c(0,1))
############################################################################
Spruce$BAL
model.svm <- svm(new_x_exact~BAL,Spruce, probability = TRUE)
pred <- predict(model.svm, Spruce, probability = TRUE)


par.Petal_W <- partial(model.svm, pred.var = c("BAL"), chull = TRUE)
plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
plot.Petal_W

############################################################################
Spruce$Estim_Ht
model.svm <- svm(new_x_exact~Estim_Ht,Spruce, probability = TRUE)
pred <- predict(model.svm, Spruce, probability = TRUE)


par.Petal_W <- partial(model.svm, pred.var = c("Estim_Ht"), chull = TRUE)
plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
plot.Petal_W

############################################################################
Spruce$dbhcm
model.svm <- svm(new_x_exact~dbhcm,Spruce, probability = TRUE)
pred <- predict(model.svm, Spruce, probability = TRUE)


par.Petal_W <- partial(model.svm, pred.var = c("dbhcm"), chull = TRUE)
plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
plot.Petal_W

############################################################################
Spruce$cr
model.svm <- svm(new_x_exact~cr,Spruce, probability = TRUE)
pred <- predict(model.svm, Spruce, probability = TRUE)


par.Petal_W <- partial(model.svm, pred.var = c("cr"), chull = TRUE)
plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
plot.Petal_W

############################################################################
Spruce$YC
model.svm <- svm(new_x_exact~YC,Spruce, probability = TRUE)
pred <- predict(model.svm, Spruce, probability = TRUE)


par.Petal_W <- partial(model.svm, pred.var = c("YC"), chull = TRUE)
plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
plot.Petal_W

############################################################################
Spruce$Density_ha
model.svm <- svm(new_x_exact~Density_ha,Spruce, probability = TRUE)
pred <- predict(model.svm, Spruce, probability = TRUE)


par.Petal_W <- partial(model.svm, pred.var = c("Density_ha"), chull = TRUE)
plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
plot.Petal_W

############################################################################
Spruce$plotba_m2_ha
model.svm <- svm(new_x_exact~plotba_m2_ha,Spruce, probability = TRUE)
pred <- predict(model.svm, Spruce, probability = TRUE)


par.Petal_W <- partial(model.svm, pred.var = c("plotba_m2_ha"), chull = TRUE)
plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
plot.Petal_W

############################################################################
Spruce$CCF
model.svm <- svm(new_x_exact~CCF+BAL+plotba_m2_ha+Density_ha,Spruce, probability = TRUE)
pred <- predict(model.svm, Spruce, probability = TRUE)


par.Petal_W <- partial(model.svm, pred.var = c("CCF"), chull = TRUE)
plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
plot.Petal_W
  






#Define predictor and response variables
y <- Spruce[!is.na(Spruce$BAL) ,]$dbh_act_increment
x <- Spruce[!is.na(Spruce$BAL) , c('dbhcm', 'BAL', 'Estim_Ht', 'plotba_m2_ha','CCF')]

x<-data.matrix(x)
#fit lasso regression model using k-fold cross-validation
cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model$lambda.min

#display optimal lambda value
best_lambda

#view plot of test MSE's vs. lambda values
plot(cv_model)

#view coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

tLL <- best_model$nulldev - deviance(best_model)
k <- best_model$df
n <- best_model$nobs
AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
AICc

BIC<-log(n)*k - tLL
BIC

#find R-squared of model on training data
y_predicted <- predict(best_model, s = best_lambda, newx = x)

sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

rsq <- 1 - sse/sst
rsq
plot(y,y_predicted,xlim=c(0,3),ylim=c(0,3))
abline(coef = c(0,1))
plot(y_predicted,(y-y_predicted)/sd(y-y_predicted,na.rm=T))
mean(y-y_predicted,na.rm=T)/sd(y-y_predicted,na.rm=T)

plot(Spruce$dbh_act_increment,Spruce$newModel,xlim=c(0,3),ylim=c(0,3))


plot(Spruce[!is.na(Spruce$BAL),]$dbh_act_increment,predict(model2),xlim=c(0,3),ylim=c(0,3))
abline(coef = c(0,1))



mean(y_predicted - y)/sd((y_predicted - y))
model1
resid<-(y_predicted - y)
plot(y_predicted,resid/sd(resid))
plot(model1)
hist(Spruce$newModel,breaks=20)
lmtest::bptest(model5)

library(regtools)
nlshc(model5)

qqplot(Spruce$newModel,Spruce$dbh_act_increment)
qqline(Spruce$dbh_act_increment)

library(nlstools)
nlsResiduals(model1_D)
O2K.res1 <- nlsResiduals(model1_D)
plot(O2K.res1, which = 0)

# Histogram and qq-plot
plot(O2K.res1, which = 5)
plot(O2K.res1, which = 6)

# Tests
test.nlsResiduals(O2K.res1)

####################################################################################
nlsResiduals(model5)
O2K.res1 <- nlsResiduals(model5)
plot(O2K.res1, which = 0)

# Histogram and qq-plot
plot(O2K.res1, which = 5)
plot(O2K.res1, which = 6)

# Tests
test.nlsResiduals(O2K.res1)
####################################################################################
####################################################################################
########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################
####################################################################################

Douglas<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Douglas fir",]
Douglas<-Douglas[Douglas$dbh_act_increment>0,]
Douglas<-Douglas[Douglas$dbh_act_increment<2,]

plot(Douglas$dbh_act_increment,Douglas$Estim_Ht)
plot(Douglas$dbh_act_increment,Douglas$BASEL_M_HA)
plot(Douglas$dbh_act_increment,Douglas$Density_ha)
plot(Douglas$dbh_act_increment,Douglas$RepreBAHA)
plot(Douglas$dbh_act_increment,Douglas$BAL)
plot(Douglas$dbh_act_increment,(Douglas$plotba_m2_ha))
plot(Douglas$dbh_act_increment,Douglas$CCF)
plot(Douglas$dbh_act_increment,Douglas$YC)
table(Douglas[!is.na(Douglas$dbh_act_increment),]$YC)


########################################################################################################################################################################
########################################################################################################################################################################
####################################################################################
############################################################################
#Douglas$Estim_Ht
  model.svm <- svm(dbh_act_increment~Estim_Ht,Douglas, probability = TRUE)
  pred <- predict(model.svm, Spruce, probability = TRUE)


      par.Petal_W <- partial(model.svm, pred.var = c("Estim_Ht"), chull = TRUE)
      plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
      plot.Petal_W

############################################################################
      #Douglas$dbhcm
  model.svm <- svm(dbh_act_increment~dbhcm,Douglas, probability = TRUE)
  pred <- predict(model.svm, Spruce, probability = TRUE)


    par.Petal_W <- partial(model.svm, pred.var = c("dbhcm"), chull = TRUE)
    plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
    plot.Petal_W

    
    ################################################
    mod <- lm(Douglas$dbh_act_increment~Douglas$dbhcm)
    cooksd <- cooks.distance(mod)
    max(cooksd)
    sample_size<-NROW(Douglas)
    influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
    NROW(influential)
    plot(Douglas$dbh_act_increment,Douglas$dbhcm)
    
    Douglas<-Douglas[-influential,]
    plot(Douglas$dbh_act_increment,Douglas$dbhcm)
    
    #######################################################
    mod <- lm(Douglas$dbh_act_increment~Douglas$Estim_Ht)
    cooksd <- cooks.distance(mod)
    max(cooksd)
    sample_size<-NROW(Douglas)
    influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
    NROW(influential)
    plot(Douglas$dbh_act_increment,Douglas$Estim_Ht)
    
    Douglas<-Douglas[-influential,]
    plot(Douglas$dbh_act_increment,Douglas$Estim_Ht*Douglas$dbhcm)
    





median(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Douglas fir",]$YC)



model1_D<- nls(dbh_act_increment~(a+f*dbhcm^2+g*BAL+exp((Estim_Ht)^b+c*plotba_m2_ha+CCF^e)),
             Douglas,
             start=c(a=5.144e-01,b=3.919e-02,c=-4.690e-01,e= 3.556e-01 ,
                     f= 1.544e-04 ,g=-4.159e-03 ),
             control = nls.control(maxiter = 1000))

model1_D<- nls(dbh_act_increment~a+YC/26*(b*dbhcm^2+c*BAL)+exp((Estim_Ht/dbhcm)^d),
               Douglas,
               start=c(a=-2.063,b=1.569e-04  ,c=-2.007e-02 ,d=7.895e-01),
               control = nls.control(maxiter = 1000))
AIC(model1_D)
plot(model1_D)
summary(model1_D)

      # Douglas$model1_D<- -2.180+3.509e-05*Douglas$dbhcm^2-4.963e-03*Douglas$BAL+
      #                       exp((Douglas$Estim_Ht/Douglas$dbhcm)^-1.182e-01)
Douglas$model1_D<--2.191+Douglas$YC/26*(4.421e-05*Douglas$dbhcm^2-4.831e-03*Douglas$BAL)+
  exp((Douglas$Estim_Ht/Douglas$dbhcm)^-1.096e-01)
dev.new()
  plot(Douglas$dbh_act_increment,Douglas$model1_D,xlim=c(0,3),ylim=c(0,3))
      abline(coef = c(0,1))
      
      Douglas$resid<-Douglas$dbh_act_increment-Douglas$model1_D
      
        plot(Douglas$model1_D,Douglas$resid/sd(Douglas$resid,na.rm=T),ylim=c(-5,5))
        abline(coef=c(0,0))
        hist(Douglas$resid,breaks=80)
        
        Douglas$resid_org<-Douglas$dbh_act_increment-Douglas$average_est_inc
        plot(Douglas$average_est_inc,Douglas$resid_org/sd(Douglas$resid_org,na.rm=T),ylim=c(-5,5))
        abline(coef=c(0,0))
        
        

      plot(Douglas$resid,Douglas$Estim_Ht,xlim=c(-2,2))
      plot(Douglas$resid,Douglas$BASEL_M_HA,xlim=c(-2,2))
      plot(Douglas$resid,Douglas$Density_ha,xlim=c(-2,2))
      plot(Douglas$resid,Douglas$RepreBAHA,xlim=c(-2,2))
      plot(Douglas$resid,Douglas$BAL,xlim=c(-2,2))
      plot(Douglas$resid,(Douglas$plotba_m2_ha),xlim=c(-2,2))
      plot(Douglas$resid,Douglas$CCF,xlim=c(-2,2))
      plot(Douglas$resid,Douglas$YC,xlim=c(-2,2))
      plot(Douglas$resid,Douglas$dbhcm,xlim=c(-2,2))
      plot(Douglas$dbh_act_increment,as.factor(Douglas$Development.stage),xlim=c(-2,2))
      
      
      
      
      #Define predictor and response variables
      y <- Douglas[!is.na(Douglas$BAL) ,]$dbh_act_increment
      x <- Douglas[!is.na(Douglas$BAL) , c('dbhcm', 'BAL', 'Estim_Ht')]
      
      x<-data.matrix(x)
      #fit lasso regression model using k-fold cross-validation
      cv_model <- cv.glmnet(x, y, alpha = 1)
      best_lambda <- cv_model$lambda.min
      
      #display optimal lambda value
      best_lambda
      
      #view plot of test MSE's vs. lambda values
      plot(cv_model)
      
      #view coefficients of best model
      best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
      coef(best_model)
      tLL <- best_model$nulldev - deviance(best_model)
      k <- best_model$df
      n <- best_model$nobs
      AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
      AICc
      
      BIC<-log(n)*k - tLL
      BIC
      
      #find R-squared of model on training data
      y_predicted <- predict(best_model, s = best_lambda, newx = x)
      
      sst <- sum((y - mean(y))^2)
      sse <- sum((y_predicted - y)^2)
      
      rsq <- 1 - sse/sst
      rsq
      plot(y,y_predicted,xlim=c(0,3),ylim=c(0,3))
      abline(coef = c(0,1))
      plot(y_predicted,(y-y_predicted)/sd(y-y_predicted,na.rm=T),ylim=c(-3,3),xlim=c(0,1.5))
      abline(coef = c(0,0))

####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################


Conifers<- AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Other conifers" ,]
Conifers<-Conifers[Conifers$dbh_act_increment>0,]
Conifers<-Conifers[Conifers$dbh_act_increment<5,]

plot(Conifers$dbh_act_increment,Conifers$average_est_inc,xlim=c(0,3),ylim=c(0,3))
abline(coef=c(0,1))
plot(Conifers$dbh_act_increment,Conifers$dbhcm)
plot(Conifers$dbh_act_increment,Conifers$Estim_Ht)
plot(Conifers$dbh_act_increment,Conifers$BASEL_M_HA)
plot(Conifers$dbh_act_increment,Conifers$Density_ha)
plot(Conifers$dbh_act_increment,Conifers$RepreBAHA)
plot(Conifers$dbh_act_increment,Conifers$BAL_ties_adjusted_3)
plot(Conifers$dbh_act_increment,(Conifers$plotba_m2_ha))
plot(Conifers$dbh_act_increment,Conifers$CCF)


################################################
mod <- lm(Conifers$dbh_act_increment~Conifers$dbhcm)
cooksd <- cooks.distance(mod)
max(cooksd)
sample_size<-NROW(Conifers)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
NROW(influential)
plot(Conifers$dbh_act_increment,Conifers$dbhcm)

Conifers<-Conifers[-influential,]
plot(Conifers$dbh_act_increment,Conifers$dbhcm)

#######################################################
mod <- lm(Conifers$dbh_act_increment~Conifers$Estim_Ht)
cooksd <- cooks.distance(mod)
max(cooksd)
sample_size<-NROW(Conifers)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
NROW(influential)
plot(Conifers$dbh_act_increment,Conifers$Estim_Ht)

Conifers<-Conifers[-influential,]
plot(Conifers$dbh_act_increment,Conifers$Estim_Ht)



####################################################################################

model7<- nlsLM(dbh_act_increment~a+f*dbhcm^2+g*BAL+exp((Estim_Ht)^b+CCF^e+c*plotba_m2_ha),
               Conifers,
               start=c(a=-11.957795 ,b=-9.670302 ,c=0,
                       e=-1,f=0.001494,g=0.065365 ),
               control = nls.control(maxiter = 1000))

model7<- nlsLM(dbh_act_increment~a+d*BAL+exp((Estim_Ht)^c),
               Conifers,
               start=c(a=1 ,c=1 ,d=0),
               control = nls.control(maxiter = 1000))
AIC(model7)
summary(model7)
Conifers$newmodel<--2.549+6.292e-05*Conifers$dbhcm^2+exp((Conifers$Estim_Ht)^8.192e-02 )

plot(Conifers$dbh_act_increment,Conifers$newmodel,xlim=c(0,2),ylim=c(0,2))
abline(coef = c(0,1))
Conifers$res
sqrt(sum(res)/(length(res)-4-1))

##################################################################################
oc_dbh<-read_excel(file.choose())
plot(oc_dbh$`Annual DBH increment_observsed`,oc_dbh$DBH)
plot(oc_dbh$`Annual DBH increment_observsed`,oc_dbh$`Measured height`)
plot(oc_dbh$`Annual DBH increment_observsed`,oc_dbh$`Tree-ha`)
plot(oc_dbh$`Annual DBH increment_observsed`,oc_dbh$BA_ha)
plot(oc_dbh$`Annual DBH increment_observsed`,oc_dbh$`Measured CR`)
plot(oc_dbh$`Annual DBH increment_observsed`,oc_dbh$BAL)
plot(oc_dbh$`Annual DBH increment_observsed`,(oc_dbh$`Pre DBH inc current model`))
plot(oc_dbh$`Annual DBH increment_observsed`,oc_dbh$CCF)



##douglas and spruce are complete, create report for these two and send on
# finish it for other cohorts, same methodology
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################

plot(Douglas$model1_D,Douglas$dbh_act_increment-Douglas$model1_D)
abline(coef=c(0,0))
b<-boxcox(lm(dbh_act_increment~dbhcm*BAL*Estim_Ht*CCF*plotba_m2_ha,Spruce))



lambda <- b$x[which.max(b$y)]
lambda
Spruce$new_x_exact <- (Spruce$dbh_act_increment^ lambda - 1) / lambda  
hist(new_x_exact,breaks = 15)


model5<- nlsLM(new_x_exact~a+f*dbhcm^2+g*BAL+exp((Estim_Ht)^b+CCF^e+c*plotba_m2_ha),
               Spruce,
               start=c(a=-11.957795 ,b=-9.670302 ,c=0,
                       e=-1,f=0.001494,g=0.065365 ),
               control = nls.control(maxiter = 1000))

Spruce$x_est<--9.633+6.096e-05*Spruce$dbhcm^2+
  -7.643e-03*Spruce$BAL+exp((Spruce$Estim_Ht)^5.173e-02+
                              Spruce$CCF^1.250e-02 -6.328e-04*Spruce$plotba_m2_ha) 
Spruce$newModel5<-(1+Spruce$x_est*lambda)^(1/lambda)

    Spruce$resid5<-Spruce$dbh_act_increment-Spruce$newModel5
    
  plot(Spruce$newModel,Spruce$dbh_act_increment,xlim=c(0,3),ylim=c(0,3))#
      abline(coef=c(0,1))
      dev.new()
  plot(Spruce$dbh_act_increment,Spruce$newModel5,xlim=c(0,3),ylim=c(0,3))
      abline(coef=c(0,1))
      dev.new()
  plot(Spruce$dbh_act_increment,Spruce$average_est_inc,xlim=c(0,3),ylim=c(0,3))
    abline(coef=c(0,1))
    
    ##################################################################################
    FGB_dbh<-read_excel(file.choose())
    FGB_dbh<-FGB_dbh[FGB_dbh$`Measured height`<998,]
    FGB_dbh<-FGB_dbh[FGB_dbh$`Measured CR`<998,]
    plot(FGB_dbh$`Annual DBH increment_observsed`,FGB_dbh$DBH)
    plot(FGB_dbh$`Annual DBH increment_observsed`,FGB_dbh$`Measured height`)
    plot(FGB_dbh$`Annual DBH increment_observsed`,FGB_dbh$`Tree-ha`)
    plot(FGB_dbh$`Annual DBH increment_observsed`,FGB_dbh$BA_ha)
    plot(FGB_dbh$`Annual DBH increment_observsed`,FGB_dbh$`Measured CR`)
    plot(FGB_dbh$`Annual DBH increment_observsed`,FGB_dbh$BAL)
    plot(FGB_dbh$`Annual DBH increment_observsed`,(FGB_dbh$`Pre DBH inc current model`),
         xlim=c(0,3),ylim=c(0,3))
    abline(coef=c(0,1))
    plot(FGB_dbh$`Annual DBH increment_observsed`,FGB_dbh$CCF/FGB_dbh$BAL)
    ################################################
    mod <- lm(FGB_dbh$dbh_inc~FGB_dbh$BAL)
    cooksd <- cooks.distance(mod)
    max(cooksd)
    sample_size<-NROW(OC_cr)
    influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
    NROW(influential)
    
    FGB_dbh<-FGB_dbh[-influential,]
    ################################################
    FGB_dbh$dbh_inc<-FGB_dbh$`Annual DBH increment_observsed`
    FGB_dbh$Estim_Ht<-FGB_dbh$`Measured height`
    model6<- nlsLM(new_x_exact~a+f*dbhcm^2+g*BAL+exp((Estim_Ht)^b+CCF^e+c*plotba_m2_ha),
                   FGB_dbh,
                   start=c(a=-11.957795 ,b=-9.670302 ,c=0,
                           e=-1,f=0.001494,g=0.065365 ),
                   control = nls.control(maxiter = 1000))
    
    model6<- nlsLM(dbh_inc~a+exp((Estim_Ht)^c+b*DBH+e*BAL+CCF^d),
                   FGB_dbh,
                   start=c(a=-7.030 ,b=1.321e-06 ,c=1.835e-02,
                           d=-7.319e-03,e=-7.256e-04 ),
                   control = nls.control(maxiter = 1000))
    AIC(model6)
    summary(model6)
    FGB_dbh$model6<- -6.9983079+
      exp((FGB_dbh$Estim_Ht)^0.0220661+
            -0.0008160*FGB_dbh$BAL+
            -0.0002173*FGB_dbh$DBH+
            FGB_dbh$CCF^-0.0080254)
    
    plot(FGB_dbh$`Annual DBH increment_observsed`,(FGB_dbh$`Pre DBH inc current model`),
         xlim=c(0,3),ylim=c(0,3))
    abline(coef=c(0,1))
    plot(FGB_dbh$`Annual DBH increment_observsed`,(FGB_dbh$model6),
         xlim=c(0,3),ylim=c(0,3))
    abline(coef=c(0,1))
    ##################################################################################
    SGB_dbh<-read_excel(file.choose())
    SGB_dbh<-SGB_dbh[SGB_dbh$`Measured height`<998,]
    SGB_dbh<-SGB_dbh[SGB_dbh$`Measured CR`<998,]
    
    SGB_dbh$dbh_inc<-SGB_dbh$`Annual DBH increment_observsed`
    SGB_dbh$Estim_Ht<-SGB_dbh$`Measured height`
    
    plot(SGB_dbh$`Annual DBH increment_observsed`,SGB_dbh$DBH)
    plot(SGB_dbh$`Annual DBH increment_observsed`,SGB_dbh$`Measured height`)
    plot(SGB_dbh$`Annual DBH increment_observsed`,SGB_dbh$`Tree-ha`)
    plot(SGB_dbh$`Annual DBH increment_observsed`,SGB_dbh$BA_ha)
    plot(SGB_dbh$`Annual DBH increment_observsed`,SGB_dbh$`Measured CR`)
    plot(SGB_dbh$`Annual DBH increment_observsed`,SGB_dbh$BAL)
    plot(SGB_dbh$`Annual DBH increment_observsed`,(SGB_dbh$`Pre DBH inc current model`),
         xlim=c(0,2),ylim=c(0,2))
    abline(coef=c(0,1))
    plot(SGB_dbh$`Annual DBH increment_observsed`,SGB_dbh$CCF)
    
    model7<- nlsLM(dbh_inc~a+exp((Estim_Ht)^c+b*DBH+e*BAL+CCF^d),
                   SGB_dbh,
                   start=c(a=-7.030 ,b=1.321e-06 ,c=1.835e-02,
                           d=-7.319e-03,e=-7.256e-04 ),
                   control = nls.control(maxiter = 1000))

    AIC(model7)
    summary(model7)
    
    SGB_dbh$model7<- -6.8532705+
      exp((SGB_dbh$Estim_Ht)^0.0159007+
            -0.0006533*SGB_dbh$BAL+
            -0.0001409*SGB_dbh$DBH+
            SGB_dbh$CCF^-0.0095174)
    
    plot(SGB_dbh$`Annual DBH increment_observsed`,(SGB_dbh$`Pre DBH inc current model`),
         xlim=c(0,2),ylim=c(0,2))
    abline(coef=c(0,1))
    plot(SGB_dbh$`Annual DBH increment_observsed`,(SGB_dbh$model7),
         xlim=c(0,2),ylim=c(0,2))
    abline(coef=c(0,1))
    
    
    