DF<-structure(list(Nest = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 16L, 10L, 4L, 5L, 7L, 12L, 4L, 6L, 20L, 8L, 14L, 16L, 3L, 9L, 15L, 19L, 6L, 7L, 17L, 18L, 12L, 13L, 10L, 20L, 5L, 8L, 11L, 16L, 6L, 12L, 1L, 2L, 4L, 6L, 9L, 18L, 21L, 16L, 3L, 20L),
                                .Label = c("WTSN01", "WTSN02", "WTSN04", "WTSN05", "WTSN06", "WTSN07", "WTSN08", "WTSN09", "WTSN10", "WTSN12", "WTSN13", "WTSN14", "WTSN16", "WTSN18", "WTSN20", "WTSN21", "WTSN23", "WTSN24", "WTSN25", "WTSN26", "WTSN28", "WTSN29"), class = "factor"),
               Hatch = structure(c(16177, 16177, 16177, 16165, 16185, 16189, 16188, 16193, 16181, 16181, 16177, 16181, 16180, 16195, 16200, 16177, 16182, 16176, 16173, 16189, 16181, 16178, 16177, 16181, 16165, 16185, 16188, 16181, 16165, 16189, 16189, 16193, 16195, 16177, 16177, 16181, 16200, 16173, 16189, 16188, 16182, 16176, 16181, 16180, 16181, 16189, 16185, 16193, 16177, 16177, 16189, 16181, 16177, 16177, 16165, 16189, 16181, 16176, 16181, 16177, 16177, 16189),
                                 class = "Date"), 
               Age = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                       0, 0, 0, 0, 0, 6, 7.5, 8, 8, 8, 8, 8.5, 8.5, 8.5, 9, 9, 9, 
                       9.5, 9.5, 9.5, 9.5, 10, 10, 10, 10, 10.5, 10.5, 11, 11, 11.5, 
                       11.5, 11.5, 11.5, 12, 12, 12.5, 12.5, 12.5, 12.5, 12.5, 12.5, 
                       12.5, 13, 13.5, 13.5), Weight = c(1.022, 1.022, 1.022, 1.022, 
                                                         1.022, 1.022, 1.022, 1.022, 1.022, 1.022, 1.022, 1.022, 1.022, 
                                                         1.022, 1.022, 1.022, 1.022, 1.022, 1.022, 1.022, 1.022, 1.022, 
                                                         8.1, 8.5, 8.8, 8.8, 9.6, 8.6, 9.7, 11, 9.9, 11.1, 9.9, 12, 
                                                         10.5, 10.5, 7, 11.2, 11.9, 11.4, 11, 11.9, 11.2, 11.7, 9.1, 
                                                         12.3, 12.3, 13, 11.6, 13.4, 12.2, 11.1, 12.7, 11.3, 12.2, 
                                                         12.4, 11.8, 12.9, 11.2, 13.2, 11, 14.1)),
          .Names = c("Nest", "Hatch", "Age", "Weight"),
          row.names = c(NA, 62L), class = "data.frame")


plot(Weight ~ Age, data=DF)
fit <- nls(Weight ~ SSlogis(Age, Asym, xmid, scal), data=DF)
summary(fit)
curve(predict(fit, newdata = data.frame(Age=x)), add=TRUE)
library(nlme)
Logistic_gnls <- gnls(Weight ~ Asym/(1+exp((xmid-Age)/scal)), data = DF,
                      start = coef(fit))
coef(Logistic_gnls)










Height_m<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce"& AFI_ISN_Data$Height_m<200,]$Height_m
BAL<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce"& AFI_ISN_Data$Height_m<200,]$BAL_ties_adjusted_3
plotba_m2_ha<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce"& AFI_ISN_Data$Height_m<200,]$plotba_m2_ha
dbhcm<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce" & AFI_ISN_Data$Height_m<200,]$dbhcm


Height_m~ (a1 + a2BAL + a3BA + fSITE)(1 − exp(b.DBH(c1−c2BAL)
))



nls(Height_m~ (a1 + a2*BAL + a3*plotba_m2_ha)*(1- exp(b*DBH(c1-c2*BAL)
)),start=c())


plot( dbhcm,Height_m)
fit <- nls(Height_m ~ 1.3+exp(a+b/dbhcm),start=c(a=3.751325 ,b=-12))
summary(fit)
curve(predict(fit, newdata = data.frame(dbhcm=x)), add=TRUE)
library(nlme)
Logistic_gnls <- gnls(Height_m ~ 1.3+exp(a+b/dbhcm),
                      start = coef(fit))
coef(Logistic_gnls)


fit <- nls(Height_m ~ 1.3+exp(a+b/dbhcm+c*BAL),start=c(a=3.751325 ,b=-12,c=-1))

fun <- function(x, y) 1.3+exp(3.651-1.976e+01/x+3.504e-03*y)

res<-mapply(fun,dbhcm,BAL)
cols <- c("black", "cornflowerblue", "orange")
matplot(dbhcm, res, col=cols, type="l", lty=1, lwd=2, xlab="x", ylab="result")
legend("bottomright",  title="value of y", lwd=2, col=cols)


a=3.651603 
b= -18.071062 

plot(1.3+exp(a+b/dbhcm),Height_m)
abline(coef = c(0,1))
plot(plotba_m2_ha,Height_m)

nls(Height_m ~ SSbiexp(dbhcm,1,1,.1,-1),AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce" & !is.na(AFI_ISN_Data$Height_m),c("dbhcm","Height_m","BAL_ties_adjusted_3","plotba_m2_ha")])



?SSbiexp

fm1 <- nlsList(SSasympOff, CO2)
summary(fm1)

plot(fm1)

CO2.Qn1 <- CO2[CO2$Plant == "Qn1", ]
SSasympOff(CO2.Qn1$conc, 32, -4, 43)


nlsList(model, data, start, control, level, subset,
        na.action = na.fail, pool = TRUE, warn.nls = NA)





# site_index = (11.39900498*(1 -(1-TH^0.6241574/11.39900498)^((30 --0.75)/(Age --0.75)))) ^ (1 /0.6241574)
rep()

x<-c()
cyc<-c()

for (i in unique(AFI_Final_Data$Site_code)){
  x<-c(x,rep(i,max(AFI_Final_Data[AFI_Final_Data$Site_code==i,]$Cycle)*max(AFI_Final_Data[AFI_Final_Data$Site_code==i,]$idplots)))
  cyc<-c(cyc,1:max(AFI_Final_Data[AFI_Final_Data$Site_code==i,]$Cycle))
}


Site_Info<-data.frame(Site= x,Basal_Area=NA,TH=NA,Age=NA,Cycle=cyc,Site_Index=NA)


for( i  in Site_Info$Site ){
  for ( n in 1:4){
    if(!is_empty(Site_Info[Site_Info$Site==i & Site_Info$Cycle==n,"TH"])){
  Site_Info[Site_Info$Site==i & Site_Info$Cycle==n,"TH"]<- mean(AFI_Final_Data[AFI_Final_Data$dbhcm==max(AFI_Final_Data[AFI_Final_Data$Site_code==i & 
                                                                                                                          AFI_Final_Data$cohort_name=="Spruce" &  AFI_Final_Data$Cycle==n,"dbhcm"]) & 
                                                                                 AFI_Final_Data$Site_code==i  & AFI_Final_Data$Cycle==n & AFI_Final_Data$cohort_name=="Spruce" ,"Height_m"],na.rm=T)
     
    if(is.nan(Site_Info[Site_Info$Site==i & Site_Info$Cycle==n,"TH"])){
      Site_Info[Site_Info$Site==i & Site_Info$Cycle==n,"TH"]<- mean(AFI_Final_Data[AFI_Final_Data$dbhcm==max(AFI_Final_Data[AFI_Final_Data$Site_code==i 
                                                                                                                            & AFI_Final_Data$cohort_name=="Spruce"  & AFI_Final_Data$Cycle==n,"dbhcm"]) &
                                                                                     AFI_Final_Data$Site_code==i  & AFI_Final_Data$Cycle==n & AFI_Final_Data$cohort_name=="Spruce" ,"Estim_Ht"],na.rm=T)
    }
  Site_Info[Site_Info$Site==i & Site_Info$Cycle==n,"Basal_Area"]<-sum(AFI_Final_Data[AFI_Final_Data$Site_code==i  & AFI_Final_Data$Cycle==n ,]$RepreBAHA,na.rm=T)
    }
  }
}
AFI_Final_Data[AFI_Final_Data$dbhcm==max(AFI_Final_Data[AFI_Final_Data$Site_code=="67" & AFI_Final_Data$Cycle==2,"dbhcm"]) & AFI_Final_Data$Site_code=="67"  & AFI_Final_Data$Cycle==2,]
Site_Info[Site_Info$Site=="106" & Site_Info$Cycle==1,"TH"]<-32.6


# monivea plant yr=1952
# age a cycle 1 is 59
Site_Info[Site_Info$Site=="102" & Site_Info$Cycle==1,"Age"]<-59
# cycle 2 = 64
Site_Info[Site_Info$Site=="102" & Site_Info$Cycle==2,"Age"]<-64
#cycle 3 = 69
Site_Info[Site_Info$Site=="102" & Site_Info$Cycle==3,"Age"]<-69
#mellory plt yr = 1967
# age @ cycle 1 = 44
Site_Info[Site_Info$Site=="103" & Site_Info$Cycle==1,"Age"]<-44
# cycle 2 = 49
Site_Info[Site_Info$Site=="103" & Site_Info$Cycle==2,"Age"]<-49
# cycle 3 = 54
Site_Info[Site_Info$Site=="103" & Site_Info$Cycle==3,"Age"]<-54
#Berth_Ddu plt yr = 1991
# age @ cycle 1 = 24
Site_Info[Site_Info$Site=="123" & Site_Info$Cycle==1,"Age"]<-24

# cycle 2 = 30
Site_Info[Site_Info$Site=="123" & Site_Info$Cycle==2,"Age"]<-30
# rushmroe pl yr = 1962-68
#cycl1 age = 50
Site_Info[Site_Info$Site=="120" & Site_Info$Cycle==1,"Age"]<-50
#cycl2 age = 55
Site_Info[Site_Info$Site=="120" & Site_Info$Cycle==2,"Age"]<-55

Site_Info$Site_Index<- (11.39900498*(1 -(1-Site_Info$TH^0.6241574/11.39900498)^((30 --0.75)/(Site_Info$Age --0.75)))) ^ (1 /0.6241574)


i="L3 Chase Wood"
# need to find 
# age 
# TH
#cycle
#cal site index

View(AFI_ISN_Data[AFI_ISN_Data$Site_code=="L3 Chase Wood",])


Finalized_stourhead_67
imptfit1<-ImputeHeights(Finalized_stourhead_67$dbhcm,Finalized_stourhead_67$Height_m,Finalized_stourhead_67$idplots)

summary(imptfit1$model)



plot(Finalized_stourhead_67$Height_m,imptfit$hpred)
abline(coef = c(0,1))






plot(Finalized_stourhead_67$Height_m,Finalized_stourhead_67$Estim_Ht)





slow_broadleaves<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Slow-growing broadleaves",]

slow_broadleaves_model<-nls(Height_m ~ 1.3+ exp(a+b/(dbhcm)), data =slow_broadleaves[ !is.na(slow_broadleaves$Height_m),],start = c(a=1,b=1)) 






################################
# Gernealised DH model.
# hawkins 
# E(H/u)= (u+a+b*BA+c*Density+d*BAL)(1-exp(e*dbh^(f+g*BAL)))






Height_m<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce" & AFI_ISN_Data$Height_m<200 &
                        !is.na(AFI_ISN_Data$Density_ha) & !is.na(AFI_ISN_Data$Height_m),]$Height_m
BAL<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce" & AFI_ISN_Data$Height_m<200 &
                   !is.na(AFI_ISN_Data$Density_ha) & !is.na(AFI_ISN_Data$Height_m),]$BAL_ties_adjusted_3
plotba_m2_ha<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce" & AFI_ISN_Data$Height_m<200 &
                            !is.na(AFI_ISN_Data$Density_ha) & !is.na(AFI_ISN_Data$Height_m),]$plotba_m2_ha
dbhcm<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce" & AFI_ISN_Data$Height_m<200 &
                     !is.na(AFI_ISN_Data$Density_ha) & !is.na(AFI_ISN_Data$Height_m),]$dbhcm
Density.plot<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce" & AFI_ISN_Data$Height_m<200 &
                            !is.na(AFI_ISN_Data$Density_ha) & !is.na(AFI_ISN_Data$Height_m),]$Density_ha


fit <-nls(Height_m~(a+b*plotba_m2_ha+c*Density.plot+d*BAL)*(1-exp(e*dbhcm^(f+g*BAL))),
    start = c(a=12.7, b= 0.26,c=-0.002,d=-0.003,e=-0.07,f=0.8,g=0))
AIC(fit)

nls(Height_m~(a+b*plotba_m2_ha+c*Density.plot+d*BAL),
    start = c(a=12.7, b= 0.26,c=-0.002,d=-0.003))



plot( dbhcm,Height_m)
summary(fit)
curve(predict(fit, newdata = data.frame(dbhcm=x)), add=TRUE)


height_est_hawk<-(133.497949-0.474430*plotba_m2_ha+0.013860*Density.plot+2.348380*BAL)*(1-exp(-0.009277*dbhcm^(0.836398-0.002686*BAL)))

curve(dbhcm,height_est_hawk)

nls(Height_m~(1-exp(e*dbhcm^(f+g*BAL))),
    start = c(e=-0.07,f=1,g=1))



(a+b*plotba_m2_ha+c*Density.plot+d*BAL)*(1-exp(e*dbh^(f+g*BAL)))


plot(Height_m,height_est_hawk)
abline(coef = c(0,1))

12.7 0.26 -0.002 -0.003 -0.07 12.100 2.20

library(minpack.lm)





nlsLM(Height_m~(a+b*plotba_m2_ha+c*Density.plot+d*BAL)*(1-exp(e*dbh^(f+g*BAL))),
      start = c(a=12.7, b= 0.26,c=-0.002,d=-0.003,e=-0.07,f=0,g=0))





g<-function(x)(133.497949-0.474430*plotba_m2_ha+0.013860*Density.plot+2.348380*BAL)*(1-exp(-0.009277*x^(0.836398-0.002686*BAL)))

curve(g, newdata = data.frame(dbhcm=x), add=TRUE)


h <- Vectorize(g)

length(g(dbhcm))

curve(h,0,100)


library(ggplot2)
ggplot()+geom_point(aes(x=dbhcm,y=Height_m))+geom_smooth(aes(x=dbhcm,y=height_est_hawk))





summary(fit)



AIC(fit)



mean(Height_m-height_est_hawk)
median(Height_m-height_est_hawk)
sd(Height_m-height_est_hawk)
range(Height_m-height_est_hawk)



fit4<-nlsLM(Height_m~1.3+(a+b*plotba_m2_ha+c*Density.plot+d*BAL)*(1-exp(e*dbhcm^(f+g*BAL))),
      start = c(a=109.050231, b=-0.414839,c=0.012120,d=1.927113,e=-0.008671,f=0.899896,g=-0.002651),control=nls.control(maxiter = 50, tol = 1e-7),model = T)


fit1 <-nls(Height_m~1.3+(a+b*plotba_m2_ha+c*Density.plot+d*BAL)*(1-exp(e*dbhcm^(f+g*BAL))),
          start = c(a=12.7, b= 0.26,c=-0.002,d=-0.003,e=-0.07,f=0.8,g=0),control=nls.control(maxiter = 500, tol = 1e-7),model = T)


fit3<-nls(Height_m ~ 1.3+ exp(a+b/(dbhcm)), 
          data =AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce" & AFI_ISN_Data$Height_m<200 &
                                      !is.na(AFI_ISN_Data$Density_ha) & !is.na(AFI_ISN_Data$Height_m),],start = c(a=1,b=1)) 
AIC(fit4)
AIC(fit)

plot( dbhcm,Height_m)
summary(fit)
curve(predict(fit3, newdata = data.frame(dbhcm=x)), add=TRUE)


fit1 <-nls(Height_m~(a+b*plotba_m2_ha+c*Density.plot+d*BAL+h*dbhcm)*(i-exp(e*dbhcm^(f+g*BAL))),
           start = c(a=12.7, b= 0.26,c=-0.002,d=-0.003,e=-0.07,f=0.8,g=0,h=0,i=2))
AIC(fit1)
summary(fit1)


H = (a1 + a2BAL + a3BA )(1 − exp(b.DBH(c1−c2BAL)
))





ggplot()+geom_point(aes(x=dbhcm,y=Height_m))+geom_smooth(aes(x=dbhcm,y=height_est_new))+geom_smooth(aes(x=dbhcm,y=height_est_hawk))






height_est_new<-(27.3667711-0.1001058*plotba_m2_ha+0.0027899  *Density.plot-0.1519468*BAL+0.2445477*dbhcm)*(1-exp(-0.0440555*dbhcm^(0.8566675 +0.0069102*BAL)))



# Pine
























test<- AFI_Final_Data[!is.na(AFI_Final_Data$cr),]
test<-AFI_Final_Data[AFI_Final_Data$cr>0,]

plot(test$cr,test$C_R_est)
unique(test$cohort_name)
i="Slow-growing broadleaves"  

plot(test[test$cohort_name==i,]$cr,test[test$cohort_name==i,]$C_R_est)

#Spruce


#cr=exp(Preproc$Logit_CR)/(1+exp(Preproc$Logit_CR))

before<-(nls(cr~ exp(a+b*BAL_ties_adjusted_3+
  c*log(CCF)+
  d*Height_m+
  e*(Height_m/dbhcm)+
  f*dbhcm^2)/(1+exp(a-b*BAL_ties_adjusted_3+
                      c*log(CCF)+
                      d*Height_m+
                      e*(Height_m/dbhcm)+
                      f*dbhcm^2)),data = Spruce,start = c(a=-4,b=-0.01,c=0.4,d=-.1,e=-.3,f=0.00003)))


plot(Spruce$C_R_est,Spruce$cr)


Spruce$CR_NEW_EST<-exp(-1.628-3.591e-03*Spruce$BAL_ties_adjusted_3+
      2.407e-01*log(Spruce$CCF)+
      1.982e-03*Spruce$Height_m+
      5.262e-02*(Spruce$Height_m/Spruce$dbhcm)+
      8.474e-05*Spruce$dbhcm^2)/(1+exp(-1.628-3.591e-03*Spruce$BAL_ties_adjusted_3+
                                         2.407e-01*log(Spruce$CCF)+
                                         1.982e-03*Spruce$Height_m+
                                         5.262e-02*(Spruce$Height_m/Spruce$dbhcm)+
                                         8.474e-05*Spruce$dbhcm^2))


  4.8705-0.01757*Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$BAL_ties_adjusted_3-
    0.3979*log(Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$CCF)-
    0.1194*Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$Height_m-
    0.2962*(Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$Height_m/
              Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$dbhcm)+
    0.000322*Preproc[Preproc$cohort_name=="Spruce" & Preproc$Xi==i,]$dbhcm^2




mean(Spruce$CR_NEW_EST-Spruce$cr,na.rm=T)
median(Spruce$CR_NEW_EST-Spruce$cr,na.rm=T)
sd(Spruce$CR_NEW_EST-Spruce$cr,na.rm=T)
var(Spruce$CR_NEW_EST-Spruce$cr,na.rm=T)
cor(Spruce$CR_NEW_EST-Spruce$cr,na.rm=T)
abline(coef = c(0,1))

AIC(nls(cr~ exp(a+b*BAL_ties_adjusted_3+
                  c*log(CCF)+
                  d*Height_m+
                  e*(Height_m/dbhcm)+
                  f*dbhcm^2)/(1+exp(a-b*BAL_ties_adjusted_3+
                                      c*log(CCF)+
                                      d*Height_m+
                                      e*(Height_m/dbhcm)+
                                      f*dbhcm^2)),data = Other_conifers,start = c(a=4,b=0.01,c=0.4,d=-.1,e=.3,f=0.00003)))



Other_conifers$CR_NEW_EST<-exp(2.5981228+0.0141208*Other_conifers$BAL_ties_adjusted_3-
                                 0.4067218*log(Other_conifers$CCF)-
                                 0.0359391*Other_conifers$Height_m-
                                 0.3317210*(Other_conifers$Height_m/Other_conifers$dbhcm)+
                                 0.0001344*Other_conifers$dbhcm^2)/(1+exp(2.5981228+0.0141208*Other_conifers$BAL_ties_adjusted_3-
                                                                            0.4067218*log(Other_conifers$CCF)-
                                                                            0.0359391*Other_conifers$Height_m-
                                                                            0.3317210*(Other_conifers$Height_m/Other_conifers$dbhcm)+
                                                                            0.0001344*Other_conifers$dbhcm^2))



plot(Other_conifers$CR_NEW_EST,Other_conifers$cr)
abline(coef=c(0,1))
mean(Other_conifers$CR_NEW_EST-Other_conifers$cr,na.rm=T)
sd(Other_conifers$CR_NEW_EST-Other_conifers$cr,na.rm=T)
var(Other_conifers$CR_NEW_EST-Other_conifers$cr,na.rm=T)
median(Other_conifers$CR_NEW_EST-Other_conifers$cr,na.rm=T)

summary(nls(cr~ exp(a+b*BAL_ties_adjusted_3+
                      c*log(CCF)+
                      d*Height_m+
                      e*(Height_m/dbhcm)+
                      f*dbhcm^2)/(1+exp(a-b*BAL_ties_adjusted_3+
                                          c*log(CCF)+
                                          d*Height_m+
                                          e*(Height_m/dbhcm)+
                                          f*dbhcm^2)),data = Douglas,start = c(a=4,b=0.01,c=0.4,d=-.1,e=.3,f=0.00003)))


Larch
Douglasfir

summary(nls(cr~ exp(a+c*log(CCF))/(1+exp(a+c*log(CCF))),
                                  data = Larch,start = c(a=-4,c=0.4)))



summary(nls(cr~ exp(a+b*BAL_ties_adjusted_3+
                      c*log(CCF)+
                      d*Height_m+
                      e*(Height_m/dbhcm)+
                      f*dbhcm^2)/(1+exp(a-b*BAL_ties_adjusted_3+
                                          c*log(CCF)+
                                          d*Height_m+
                                          e*(Height_m/dbhcm)+
                                          f*dbhcm^2)),data = Fast_growing,start = c(a=4,b=0.01,c=-0.4,d=-.1,e=.3,f=-0.00003)))


summary(nls(cr~ exp(a+b*BAL_ties_adjusted_3+
                      c*log(CCF)+
                      d*Height_m+
                      e*(Height_m/dbhcm)+
                      f*dbhcm^2)/(1+exp(a-b*BAL_ties_adjusted_3+
                                          c*log(CCF)+
                                          d*Height_m+
                                          e*(Height_m/dbhcm)+
                                          f*dbhcm^2)),data = slow_broadleaves,start = c(a=4,b=-0.01,c=0.4,d=.1,e=.3,f=-0.00003)))

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



##########################



Spruce<-AFI_Final_Data[AFI_Final_Data$cohort_name=="Spruce",]
Douglas<-AFI_Final_Data[AFI_Final_Data$cohort_name=="Douglas fir",]
Larch<-AFI_Final_Data[AFI_Final_Data$cohort_name=="Larch",]

plot(Larch$cr,Larch$BAL_ties_adjusted_3)
cor(Larch[!is.na(Larch$cr) & !is.na(Larch$BAL_ties_adjusted_3),]$cr,Larch[!is.na(Larch$cr) & !is.na(Larch$BAL_ties_adjusted_3),]$BAL_ties_adjusted_3)
lm(cr~.,data = Spruce)

colnames(Douglas)

   
         
[13] "Height_m"      "Density_ha"          "BASEL_M_HA"        "cr"      "CCF"               "dbhcm"    "BAL_ties_adjusted_3" "plotba_m2_ha"  
[25]         "BAL"          "OGCD"                "OGCA"                "EP_OGCA"            
[37]               "Logit_CR"            "C_R_est"             "C_R"               



Spruce[,c("cr")]

nls(cr~exp(a)/(1+exp(a)),data = Spruce[Spruce$cr==0.4739336 &!is.na(Spruce$cr),],start=c(a=2))


nls(0.4739336~ exp(a)/(1+exp(a)) ,data = Spruce[Spruce$cr==0.4739336,] ,start=c(a=2)      )



0.4739336+0.4739336*exp(a)=exp(a)

0.4739336=exp(a)-0.4739336*exp(a)

0.5260664*exp(a)=0.4739336

exp(a)=0.4739336/0.5260664

exp(a)=0.9009007
a=log(0.9009007)






Spruce_67$sl<-log(Spruce_67$cr/(1-Spruce_67$cr))
Spruce_67$sl[is.infinite(Spruce_67$sl)]<-NA
after<-nls(sl~a-b*BAL_ties_adjusted_3+
  c*log(CCF)+
  d*Height_imp+
  e*(Height_imp/dbhcm)+
  f*dbhcm^2,data=Spruce_67[!is.na(Spruce_67$sl),],start = c(a=-5.301e-01,b=-1.738e-03,c=2.041e-01,d=-2.300e-02,e=-6.676e-01,f=1.404e-04))

after<-nls(sl~a+
             c*log(CCF)+
             d*Height_m+
             f*log(dbhcm),data=Spruce_67[!is.na(Spruce_67$sl),],start = c(a=-5.301e-01,c=2.041e-01,d=-2.300e-02,f=1.404e-04))


summary(before)
AIC(before)
summary(after)
AIC(after)

plot(Spruce_67$sl,(-1.591+ 1.336e-02*Spruce_67$BAL_ties_adjusted_3+5.997e-01*log(Spruce$CCF)-
                     4.314e-02*Spruce_67$Height_imp+7.612e-05*Spruce_67$dbhcm^2))


is.infinite()



Spruce_67$Logit_NEW<- -3.71665 +  0.48735 *log(Spruce_67$CCF)+  -0.05625*Spruce_67$Height_m+  0.88023*log(Spruce_67$dbhcm)
plot(Spruce_67$sl,Spruce_67$Logit_NEW)
abline(coef = c(0,1))

Spruce_67$NEW_CR<- exp(Spruce_67$Logit_NEW)/(1+exp(Spruce_67$Logit_NEW))

plot(Spruce_67$NEW_CR,Spruce_67$cr,xlim=c(0,1),ylim= c(0,1))
plot(Spruce_67$C_R_est,Spruce_67$cr,xlim=c(0,1),ylim= c(0,1))
abline(coef = c(0,1))
lm(cr~NEW_CR,Spruce_67)

mean(Spruce$NEW_CR-Spruce$cr,na.rm = T)


plot(Spruce$C_R_est,Spruce$cr,xlim=c(0,1),ylim= c(0,1))
abline(coef = c(0,1))





nls(cr~a/(1+b*BAL_ties_adjusted_3)+c*(1-exp(d*dbhcm)),data=Spruce_67,start = c(a=7.84,b=0.0057,c=1.27,d=-0.142),control = nls.control(maxiter = 1000))



nls(cr~a/(1+b*BAL),data=Spruce,start = c(a=1,b=-1))



plot(Spruce_67$cr,(1.056908/(1+0.002986 *Spruce_67$BAL_ties_adjusted_3)-0.595041*(1-exp(-0.174307*Spruce_67$dbhcm))),xlim=c(0,1),ylim= c(0,1))

unique(Spruce$Site_code)
#67

Spruce_67<-Spruce[Spruce$Site_code=="67",]

"cr"                  "dbhcm"           
[13] "Height_m"      "Density_ha"          "BASEL_M_HA"         
[25] "RepreBAHA"           "BAL"     "BAL_ties_adjusted_3" "plotba_m2_ha"        "OGCD"                "OGCA"                "EP_OGCA"            
[37] "CCF"                 "Logit_CR"            "C_R_est"             "C_R"               


Larch<-AFI_Final_Data[AFI_Final_Data$cohort_name=="Larch",]
plot(AFI_Final_Data$cr,AFI_Final_Data$Height_m,xlim=c(0,1))
plot(AFI_Final_Data$cr,AFI_Final_Data$dbhcm,xlim=c(0,1))
plot(AFI_Final_Data$cr,AFI_Final_Data$Density_ha,xlim=c(0,1))#
plot(AFI_Final_Data$cr,AFI_Final_Data$BASEL_M_HA,xlim=c(0,1))
plot(AFI_Final_Data$cr,AFI_Final_Data$BAL,xlim=c(0,1))#
plot(AFI_Final_Data$cr,AFI_Final_Data$plotba_m2_ha,xlim=c(0,1))#
plot(AFI_Final_Data$cr,AFI_Final_Data$CCF,xlim=c(0,1))#




plot(Larch$cr,Larch$dbhcm*Larch$Height_m)

plot(Larch$cr,Larch$Height_m/Larch$plotba_m2_ha)

cor(Spruce_67[!is.na(Spruce_67$cr) & !is.na(Spruce_67$Height_m) & !is.na(Spruce_67$plotba_m2_ha),]$cr,
    Spruce_67[!is.na(Spruce_67$cr) & !is.na(Spruce_67$Height_m) & !is.na(Spruce_67$plotba_m2_ha),]$Height_m/Spruce_67[!is.na(Spruce_67$cr) & !is.na(Spruce_67$Height_m) & !is.na(Spruce_67$plotba_m2_ha),]$plotba_m2_ha)



cor(Spruce_67[!is.na(Spruce_67$cr) & !is.na(Spruce_67$Height_m) & !is.na(Spruce_67$plotba_m2_ha),]$cr,
    Spruce_67[!is.na(Spruce_67$cr) & !is.na(Spruce_67$Height_m) & !is.na(Spruce_67$plotba_m2_ha),]$Height_m)



plot(Spruce_67[!is.na(Spruce_67$cr) & !is.na(Spruce_67$Height_m) & !is.na(Spruce_67$CCF),]$cr,
    Spruce_67[!is.na(Spruce_67$cr) & !is.na(Spruce_67$Height_m) & !is.na(Spruce_67$CCF),]$Height_m/
      Spruce_67[!is.na(Spruce_67$cr) & !is.na(Spruce_67$Height_m) & !is.na(Spruce_67$CCF),]$CCF)



CCF
Height_m
plotba_m2_ha
Density_ha
dbhcm
BAL


AIC(lm(cr~Height_m/CCF,Spruce_67))


plot(Spruce_67$cr,(6.540e-01-8.124e-03*Spruce_67$Height_m+4.763e-05*Spruce_67$Height_m/Spruce_67$CCF),xlim = c(0,1),ylim = c(0,1))
abline(coef = c(0,1))









plot(Spruce_103$cr,Spruce_103$BAL)




i=9


Plot103_STVI


Spruce_103<-AFI_Final_Data[AFI_Final_Data$Site_code=="103",]

Spruce_103$dbh_STVI<-NA
#unique(as.numeric(Spruce_103$idplots))
for ( i in 1:9 ){
  Spruce_103[Spruce_103$idplots==i,]$dbh_STVI<-Plot103_STVI[[i]]$STVI_dbh
}
Spruce_106<-AFI_Final_Data[AFI_Final_Data$Site_code=="106" & AFI_Final_Data$cohort_name=="Spruce" ,]
Spruce_106$dbh_STVI<-NA

for ( i in unique(as.numeric(Spruce_106$idplots))){
  Spruce_106[Spruce_106$idplots==i,]$dbh_STVI<-Plot106_STVI[[i]]$STVI_dbh
}

plot(Spruce_103$cr,Spruce_103$dbh_STVI,ylim=c(0,1))

plot(Spruce_106$cr,Spruce_106$dbh_STVI,ylim=c(0,1))


Plot103_STVI_ss<-STVI_per_plot(Spruce_103)







unique(testing$cohort_name)

i="Other conifers"

ggplot(testing[testing$cohort_name %in% i,])+geom_point(aes(x=BAL_ties_adjusted_3,y=Density_ha,col=type))







(plotba_m2_ha+0.060772*Density_ha)*(1-exp(-0.003678*dbhcm^(1.291154+0.005438*BAL_ties_adjusted_3



                                                       #   plotba_m2_ha







library(ggplot2)

  mean(AFI_Final_Data$Height_m-AFI_Final_Data$Estim_Ht,na.rm=T)
  median(AFI_Final_Data$Height_m-AFI_Final_Data$Estim_Ht,na.rm=T)
  sd(AFI_Final_Data$Height_m-AFI_Final_Data$Estim_Ht,na.rm=T)
  
  ggplot(AFI_Final_Data)+geom_point(aes(x=Height_m,y=Estim_Ht,col = cohort_name)) + geom_abline(slope = 1)+xlim(0,60)+ylim(0,60)
  plot(Preproc$Height_m,Preproc$Estim_Ht)
  abline(coef=c(0,1))
  
  
  
  
  for ( i in unique(AFI_Final_Data$species)){
    dev.new()
    plot(AFI_Final_Data[AFI_Final_Data$species==i,]$cr,AFI_Final_Data[AFI_Final_Data$species==i,]$plotba_m2_ha,xlim=c(0,1),main = i)
  }
  
  
  
  ##
  
  Plot102<-Plot102[!is.na(Plot102$Diam1),]

  Finalized_Monivea_102$dbh_STVI<-NA
  Finalized_Monivea_102$stviX<-NA
  Finalized_Monivea_102$Diam1<-  Finalized_Monivea_102$dbhcm
  Finalized_Monivea_102$PlotNo<-Finalized_Monivea_102$idplots
  Finalized_Monivea_102$xcoord<-Finalized_Monivea_102$X
  Finalized_Monivea_102$ycoord<-Finalized_Monivea_102$Y
  for (i in unique(Finalized_Monivea_102$cohort_name)){
  
  start.time <- Sys.time()
  Plot102_STVI<-STVI_per_plot(Finalized_Monivea_102[Finalized_Monivea_102$cohort_name==i,])
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken

  if(!is_empty(Plot102_STVI)){
  for ( n in 1:length(Plot102_STVI)){
    Finalized_Monivea_102[Finalized_Monivea_102$idplots==n & Finalized_Monivea_102$cohort_name==i,]$dbh_STVI<-Plot102_STVI[[n]]$STVI_dbh
    Finalized_Monivea_102[Finalized_Monivea_102$idplots==n & Finalized_Monivea_102$cohort_name==i,]$stviX<- Plot102_STVI[[n]]$stvi_x
  }
    
  }
  }
  
  
  
  
  plot(Finalized_Monivea_102$cr,Finalized_Monivea_102$stviX)
  ##
  Rushmore_AFI_final
  Rushmore_AFI_final$dbh_STVI<-NA
  Rushmore_AFI_final$stviX<-NA
  Rushmore_AFI_final$Diam1<-  Rushmore_AFI_final$dbhcm
  Rushmore_AFI_final$PlotNo<-Rushmore_AFI_final$idplots
  Rushmore_AFI_final$xcoord<-Rushmore_AFI_final$X
  Rushmore_AFI_final$ycoord<-Rushmore_AFI_final$Y
  for (i in unique(Rushmore_AFI_final$cohort_name)){
    
    start.time <- Sys.time()
    Plot102_STVI<-STVI_per_plot(Rushmore_AFI_final[Rushmore_AFI_final$cohort_name==i,])
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    
    if(!is_empty(Plot102_STVI)){
      for ( n in 1:length(Plot102_STVI)){
        Rushmore_AFI_final[Rushmore_AFI_final$idplots==n & Rushmore_AFI_final$cohort_name==i,]$dbh_STVI<-Plot102_STVI[[n]]$STVI_dbh
        Rushmore_AFI_final[Rushmore_AFI_final$idplots==n & Rushmore_AFI_final$cohort_name==i,]$stviX<- Plot102_STVI[[n]]$stvi_x
      }
      
    }
  }
  ##

  Berth_Ddu_AFI_final$dbh_STVI<-NA
  Berth_Ddu_AFI_final$stviX<-NA
  Berth_Ddu_AFI_final$Diam1<-  Berth_Ddu_AFI_final$dbhcm
  Berth_Ddu_AFI_final$PlotNo<-Berth_Ddu_AFI_final$idplots
  Berth_Ddu_AFI_final$xcoord<-Berth_Ddu_AFI_final$X
  Berth_Ddu_AFI_final$ycoord<-Berth_Ddu_AFI_final$Y
  for (i in unique(Berth_Ddu_AFI_final$cohort_name)){
    
    start.time <- Sys.time()
    Plot102_STVI<-STVI_per_plot(Berth_Ddu_AFI_final[Berth_Ddu_AFI_final$cohort_name==i,])
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    
    if(!is_empty(Plot102_STVI)){
      for ( n in 1:length(Plot102_STVI)){
        Berth_Ddu_AFI_final[Berth_Ddu_AFI_final$idplots==n & Berth_Ddu_AFI_final$cohort_name==i,]$dbh_STVI<-Plot102_STVI[[n]]$STVI_dbh
        Berth_Ddu_AFI_final[Berth_Ddu_AFI_final$idplots==n & Berth_Ddu_AFI_final$cohort_name==i,]$stviX<- Plot102_STVI[[n]]$stvi_x
      }
      
    }
  }
  
  ###

  Finalized_Mellory_103$dbh_STVI<-NA
  Finalized_Mellory_103$stviX<-NA
  Finalized_Mellory_103$Diam1<-  Finalized_Mellory_103$dbhcm
  Finalized_Mellory_103$PlotNo<-Finalized_Mellory_103$idplots
  Finalized_Mellory_103$xcoord<-Finalized_Mellory_103$X
  Finalized_Mellory_103$ycoord<-Finalized_Mellory_103$Y
  for (i in unique(Finalized_Mellory_103$cohort_name)){
    
    start.time <- Sys.time()
    Plot102_STVI<-STVI_per_plot(Finalized_Mellory_103[Finalized_Mellory_103$cohort_name==i,])
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    
    if(!is_empty(Plot102_STVI)){
      for ( n in 1:length(Plot102_STVI)){
        Finalized_Mellory_103[Finalized_Mellory_103$idplots==n & Finalized_Mellory_103$cohort_name==i,]$dbh_STVI<-Plot102_STVI[[n]]$STVI_dbh
        Finalized_Mellory_103[Finalized_Mellory_103$idplots==n & Finalized_Mellory_103$cohort_name==i,]$stviX<- Plot102_STVI[[n]]$stvi_x
      }
      
    }
  }
  
  
  
  Finalized_Mellory_103$sl_est<-NA
  plot(Finalized_Mellory_103$cr,Finalized_Mellory_103$stviX)
  Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Larch",]$sl_est<-  1.8314266 - 0.0594158*Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Larch",]$BAL_ties_adjusted_3+
    -0.4166372*(Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Larch",]$Height_imp/Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Larch",]$dbhcm)+
    -0.0003718*Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Larch",]$dbhcm^2
  
  Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Fast-growing broadleaves",]$sl_est<-  4.9197814 - 0.0151770*Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Fast-growing broadleaves",]$BAL_ties_adjusted_3+
    -0.8604138*log(Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Fast-growing broadleaves",]$CCF)+
    0.0233728*Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Fast-growing broadleaves",]$Height_imp+
    -1.8017215*(Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Fast-growing broadleaves",]$Height_imp/Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Fast-growing broadleaves",]$dbhcm)+
    -0.0004166*Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Fast-growing broadleaves",]$dbhcm^2
  Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Slow-growing broadleaves",]$sl_est<-  -3.985e-01 - 1.752e-02*Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Slow-growing broadleaves",]$BAL_ties_adjusted_3+
    -4.585e-02*log(Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Slow-growing broadleaves",]$CCF)+
    -1.830e-02*Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Slow-growing broadleaves",]$Height_imp+
    8.923e-01*(Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Slow-growing broadleaves",]$Height_imp/Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Slow-growing broadleaves",]$dbhcm)+
    3.145e-05*Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Slow-growing broadleaves",]$dbhcm^2
  Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Spruce",]$sl_est<-  -4.919e-01 - 5.104e-03*Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Spruce",]$BAL_ties_adjusted_3+
    2.160e-01*log(Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Spruce",]$CCF)+
    -2.292e-02*Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Spruce",]$Height_imp+
    -8.134e-01 *(Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Spruce",]$Height_imp/Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Spruce",]$dbhcm)+
    1.367e-04*Finalized_Mellory_103[Finalized_Mellory_103$cohort_name=="Spruce",]$dbhcm^2

  Finalized_Mellory_103$cr_new_est<-exp(Finalized_Mellory_103$sl_est)/(1+exp(Finalized_Mellory_103$sl_est))
  
  plot(Finalized_Mellory_103$cr,Finalized_Mellory_103$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  abline(coef=c(0,1))
  plot(Finalized_Mellory_103$cr-Finalized_Mellory_103$cr_new_est,Finalized_Mellory_103$stviX,xlim=c(-1,1),ylim=c(0,1))
  plot(Finalized_Mellory_103[!is.na(Finalized_Mellory_103$cr) & !is.na(Finalized_Mellory_103$stviX),]$cr,Finalized_Mellory_103[!is.na(Finalized_Mellory_103$cr) & !is.na(Finalized_Mellory_103$stviX),]$stviX)
  
  cor(Finalized_Mellory_103[!is.na(Finalized_Mellory_103$cr) & !is.na(Finalized_Mellory_103$stviX),]$cr,Finalized_Mellory_103[!is.na(Finalized_Mellory_103$cr) & !is.na(Finalized_Mellory_103$stviX),]$stviX)
   ######
  test_cr<-rbind(Finalized_Mellory_103[,c("cr","stviX","dbh_STVI","Site_code")],
  Rushmore_AFI_final[,c("cr","stviX","dbh_STVI","Site_code")],
  Berth_Ddu_AFI_final[,c("cr","stviX","dbh_STVI","Site_code")],
  Finalized_Monivea_102[,c("cr","stviX","dbh_STVI","Site_code")])
  plot(test_cr$cr,test_cr$stviX,xlim=c(0,1),ylim=c(0,1))
  plot(test_cr$cr,test_cr$dbh_STVI,xlim=c(0,1),ylim=c(0,1))
  ggplot(test_cr)+geom_point(aes(x=cr,y=stviX,col=as.factor(Site_code)))+xlim(0,1)+ylim(0,1)
  ####
  
  
  
  for(i in unique(AFI_Final_Data$cohort_name)){
    dev.new()
    plot(AFI_Final_Data[AFI_Final_Data$cohort_name==i,]$cr,AFI_Final_Data[AFI_Final_Data$cohort_name==i,]$dbhcm,xlim=c(0,1),main=i)
    dev.new()
    plot(AFI_Final_Data[AFI_Final_Data$cohort_name==i,]$cr,AFI_Final_Data[AFI_Final_Data$cohort_name==i,]$Density_ha,xlim=c(0,1),ylim=c(0,300),main=i)
    dev.new()
    plot(AFI_Final_Data[AFI_Final_Data$cohort_name==i,]$cr,AFI_Final_Data[AFI_Final_Data$cohort_name==i,]$BAL_ties_adjusted_3,xlim=c(0,1),main=i)
    dev.new()
    plot(AFI_Final_Data[AFI_Final_Data$cohort_name==i,]$cr,AFI_Final_Data[AFI_Final_Data$cohort_name==i,]$plotba_m2_ha,xlim=c(0,1),main=i)
    dev.new()
    plot(AFI_Final_Data[AFI_Final_Data$cohort_name==i,]$cr,AFI_Final_Data[AFI_Final_Data$cohort_name==i,]$CCF,xlim=c(0,1),main=i)
  }
 
  
  
  
  i="Other conifers"
  
  AFI_Final_Data[AFI_Final_Data$cohort_name==i & AFI_Final_Data$Density_ha>500,]
  
  
  
  
  
 Fast_growing <-test[test$cohort_name=="Fast-growing broadleaves",]
  
 
 Fast_growing$sl<-log(Fast_growing$cr/(1-Fast_growing$cr))
 Fast_growing$sl[is.infinite(Fast_growing$sl)]<-NA
 after<-nls(sl~a-b*BAL_ties_adjusted_3+
              c*log(CCF)+
              d*Height_m+
              e*(Height_m/dbhcm)+
              f*dbhcm^2,data=Fast_growing[!is.na(Fast_growing$cr) & Fast_growing$cr>0,],start = c(a=-5.301e-01,b=-1.738e-03,c=2.041e-01,d=-2.300e-02,e=-6.676e-01,f=1.404e-04))
 Fast_growing$sl_est<- 7.2101096-0.0550380*Fast_growing$BAL_ties_adjusted_3+
   -1.4634666*log(Fast_growing$CCF)+0.0580135*Fast_growing$plotba_m2_ha+
   -1.1701386*(Fast_growing$Height_m/Fast_growing$dbhcm)+
   -0.0003753*Fast_growing$dbhcm^2
 
 Fast_growing$cr_new_est<-exp(Fast_growing$sl_est)/(1+exp(Fast_growing$sl_est))
 AIC(after)
 summary(after)
 
 after<-nls(sl~a-b*BAL_ties_adjusted_3+
              c*log(CCF)+d*plotba_m2_ha+
              e*(Height_m/dbhcm)+
              f*dbhcm^2,data=Fast_growing[!is.na(Fast_growing$cr) & Fast_growing$cr>0,],start = c(a=-5.301e-01,b=-1.738e-03,c=2.041e-01,d=1,e=-6.676e-01,f=1.404e-04))
 
 
 
  plot(Fast_growing$cr,Fast_growing$C_R_est,xlim=c(0,1),ylim=c(0,1))
  abline(coef = c(0,1))  
  plot(Fast_growing$cr,Fast_growing$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  abline(coef = c(0,1))  
  
  plot(after)
  
  Fast_growing$plotba_m2_ha
  NROW(Fast_growing[!is.na(Fast_growing$cr) & Fast_growing$cr>0,])*log((sum(Fast_growing$cr-Fast_growing$C_R_est,na.rm = T))^2/45)+2*7
  
  
  mean(Fast_growing$cr-Fast_growing$C_R_est,na.rm=T)
  sd(Fast_growing$cr-Fast_growing$C_R_est,na.rm=T)  
  
  mean(Fast_growing$cr-Fast_growing$cr_new_est,na.rm=T)
  sd(Fast_growing$cr-Fast_growing$cr_new_est,na.rm=T) 
  
  plot(Fast_growing$C_R_est,(Fast_growing$cr-Fast_growing$C_R_est)/ sd(Fast_growing$cr-Fast_growing$C_R_est,na.rm=T) )
  abline(coef=c(0,0))
  
  plot(Fast_growing$cr_new_est,(Fast_growing$cr-Fast_growing$cr_new_est)/ sd(Fast_growing$cr-Fast_growing$cr_new_est,na.rm=T) )
  abline(coef=c(0,0))
  
  sqrt(sum((Fast_growing$cr-Fast_growing$cr_new_est)^2,na.rm=T)/45)
  
  
  
  ######
  
  Larch_Pine<- AFI_Final_Data[AFI_Final_Data$cohort_name %in% c("Larch","Pine"),] 
  
  
  Larch_Pine$sl<-log(Larch_Pine$cr/(1-Larch_Pine$cr))
  Larch_Pine$sl[is.infinite(Larch_Pine$sl)]<-NA
  after<-nls(sl~a-b*BAL_ties_adjusted_3+
               e*(Height_imp/dbhcm)+
               f*dbhcm^2,data=Larch_Pine[!is.na(Larch_Pine$sl),],start = c(a=-5.301e-01,b=-1.738e-03,e=-6.676e-01,f=1.404e-04))
  
  after<-nls(sl~a-b*BAL_ties_adjusted_3+
               f*dbhcm^2,data=Larch_Pine[!is.na(Larch_Pine$sl),],start = c(a=-5.301e-01,b=-1.738e-03,f=1.404e-04))
  
  Larch_Pine$sl_est<-  1.6527801 - 0.0627860*Larch_Pine$BAL_ties_adjusted_3+
    -0.0003818*Larch_Pine$dbhcm^2
  Larch_Pine$cr_new_est<-exp(Larch_Pine$sl_est)/(1+exp(Larch_Pine$sl_est))
  AIC(after)
  summary(after)
  plot(Larch_Pine$cr,Larch_Pine$cr_new_est)
  abline(coef = c(0,1))  
  plot(Larch_Pine$cr,Larch_Pine$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  abline(coef = c(0,1))  
  
  
  unique(AFI_Final_Data$cohort_name)
    
  
  ######
  
slow_broadleaves  <- test[test$cohort_name=="Slow-growing broadleaves",]
  
  slow_broadleaves$sl<-log(slow_broadleaves$cr/(1-slow_broadleaves$cr))
  slow_broadleaves$sl[is.infinite(slow_broadleaves$sl)]<-NA
  after<-nls(sl~a+b*BAL_ties_adjusted_3+
               c*plotba_m2_ha+
               d*Height_imp+
               e*(Height_imp/dbhcm)+
               f*dbhcm^2,data=slow_broadleaves[!is.na(slow_broadleaves$sl) & slow_broadleaves$cr>0,],
             start = c(a=-5.301e-01,b=-1.738e-03,c=2.041e-01,d=-2.300e-02,e=-6.676e-01,f=1.404e-04))
  
  after<-nls(sl~a+b*BAL_ties_adjusted_3+
               c*plotba_m2_ha+
               e*(Height_imp/dbhcm),data=slow_broadleaves[!is.na(slow_broadleaves$sl) & slow_broadleaves$cr>0,],
             start = c(a=-1.38486,b=-0.02662,c=0.01441,e=1.20780    ))
  
  
  slow_broadleaves$sl_est<-  -1.38486-0.02662*slow_broadleaves$BAL_ties_adjusted_3+
    0.01441*slow_broadleaves$plotba_m2_ha+
    1.20780*(slow_broadleaves$Height_imp/slow_broadleaves$dbhcm)
  slow_broadleaves$cr_new_est<-exp(slow_broadleaves$sl_est)/(1+exp(slow_broadleaves$sl_est))
  AIC(after)
  summary(after)
  plot(slow_broadleaves$cr,slow_broadleaves$C_R_est,xlim=c(0,1),ylim=c(0,1))
  abline(coef = c(0,1))  
  plot(slow_broadleaves$cr,slow_broadleaves$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  abline(coef = c(0,1))  
  plot(slow_broadleaves$cr,slow_broadleaves$cr_new,xlim=c(0,1),ylim=c(0,1))
  abline(coef = c(0,1)) 
  
  NROW(slow_broadleaves[!is.na(slow_broadleaves$cr) & slow_broadleaves$cr>0,])*log((sum(slow_broadleaves$cr-slow_broadleaves$C_R_est,na.rm = T))^2/56)+2*7
  NROW(slow_broadleaves[!is.na(slow_broadleaves$cr) & slow_broadleaves$cr>0,])*log((sum(slow_broadleaves$cr-slow_broadleaves$cr_new,na.rm = T))^2/56)+2*7
  
  sqrt(sum((slow_broadleaves$cr-slow_broadleaves$C_R_est)^2,na.rm=T)/56)
  sqrt(sum((slow_broadleaves$cr-slow_broadleaves$cr_new)^2,na.rm=T)/56)
  
  ####################
  Spruce<-test[test$cohort_name=="Spruce",]
  Spruce$sl<-log(Spruce$cr/(1-Spruce$cr))
  Spruce$sl[is.infinite(Spruce$sl)]<-NA
  after<-nls(sl~a+b*BAL_ties_adjusted_3+
               c*log(CCF)+
               d*Height_imp+
               e*(Height_imp/dbhcm)+
               f*dbhcm^2,data=Spruce[!is.na(Spruce$sl) & Spruce$cr>0,],
             start = c(a=-5.301e-01,b=-1.738e-03,c=2.041e-01,d=-2.300e-02,e=-6.676e-01,f=1.404e-04))
  
  Spruce$sl_est<- -5.871e-01 -4.598e-03*Spruce$BAL_ties_adjusted_3+
    2.426e-01*log(Spruce$CCF)+
    -2.721e-02 *Spruce$Height_m+
    -7.982e-01*(Spruce$Height_m/Spruce$dbhcm)+
    1.543e-04 *Spruce$dbhcm^2
  Spruce$cr_new_est<-exp(Spruce$sl_est)/(1+exp(Spruce$sl_est))
  AIC(after)
  summary(after)
  plot(Spruce$sl,Spruce$sl_est)
  abline(coef = c(0,1))  
  plot(Spruce$cr,Spruce$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  abline(coef = c(0,1))  
  
  plot(Spruce$cr,Spruce$C_R_est,xlim=c(0,1),ylim=c(0,1))
  
  plot(Spruce$cr,Spruce$C_R_est,xlim=c(0,1),ylim=c(0,1))
abline
  NROW(Spruce[!is.na(Spruce$cr) & Spruce$cr>0,])*log((sum(Spruce$cr-Spruce$C_R_est,na.rm = T))^2/1205)+2*7
  sqrt(sum((Spruce$cr-Spruce$cr_new_est)^2,na.rm=T)/1205)
  
  
  
  plot(Spruce$C_R_est,(Spruce$cr-Spruce$C_R_est)/ sd(Spruce$cr-Spruce$C_R_est,na.rm=T) )
  abline(coef=c(0,0))
  qqnorm((Spruce$cr-Spruce$C_R_est))
  qqline((Spruce$cr-Spruce$C_R_est))
  
  plot(Spruce$cr_new_est,(Spruce$cr-Spruce$cr_new_est)/ sd(Spruce$cr-Spruce$cr_new_est,na.rm=T) )
  abline(coef=c(0,0))
  qqnorm((Spruce$cr-Spruce$cr_new_est))
  qqline((Spruce$cr-Spruce$cr_new_est))
  
  
  
  Spruce$cr-Spruce$C_R_est
  AFI_Final_Data$CCF<-1/AFI_Final_Data$CCF
  plot(Fast_growing$cr,Fast_growing$CCF)
  abline(coef = c(0,1))
  summary(lm(AFI_Final_Data$cr~AFI_Final_Data$CCF))
  plot(AFI_Final_Data$cr,exp(-.005*AFI_Final_Data$CCF),xlim=c(0,1))
  abline(coef = c(0,1))
  nls(cr~a*exp(b*CCF),AFI_Final_Data,start = c(a=))
  ################################################################################
  ####################
  Larch<-test[test$cohort_name=="Larch",]
  Larch$sl<-log(Larch$cr/(1-Larch$cr))
  Larch$sl[is.infinite(Larch$sl)]<-NA
  after<-nls(cr~a+b*BAL_ties_adjusted_3+
               f*dbhcm^2,data=Larch[!is.na(Larch$sl) & Larch$cr>0,],
             start = c(a=-5.301e-01,b=-1.738e-03,f=1.404e-04))
  
  Larch$sl_est<- -5.871e-01 -4.598e-03*Larch$BAL_ties_adjusted_3+
    2.426e-01*log(Larch$CCF)+
    -2.721e-02 *Larch$Height_m+
    -7.982e-01*(Larch$Height_m/Larch$dbhcm)+
    1.543e-04 *Larch$dbhcm^2
  Larch$cr_new_est<-exp(Larch$sl_est)/(1+exp(Larch$sl_est))
  AIC(after)
  summary(after)
  plot(Larch$sl,Larch$sl_est)
  abline(coef = c(0,1))  
  plot(Larch$cr,Larch$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  abline(coef = c(0,1))  
  
  plot(Larch$cr,Larch$C_R_est,xlim=c(0,1),ylim=c(0,1))
  
  plot(Larch$cr,Larch$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  
  NROW(Larch[!is.na(Larch$cr) & Larch$cr>0,])*log((sum(Larch$cr-Larch$C_R_est,na.rm = T))^2/62)+2*7
  sqrt(sum((Larch$cr-Larch$C_R_est)^2,na.rm=T)/62)
  
  
  
  plot(Larch$C_R_est,(Larch$cr-Larch$C_R_est)/ sd(Larch$cr-Larch$C_R_est,na.rm=T) )
  abline(coef=c(0,0))
  qqnorm((Larch$cr-Larch$C_R_est))
  qqline((Larch$cr-Larch$C_R_est))
  
  plot(Larch$cr_new_est,(Larch$cr-Larch$cr_new_est)/ sd(Larch$cr-Larch$cr_new_est,na.rm=T) )
  abline(coef=c(0,0))
  qqnorm((Larch$cr-Larch$cr_new_est))
  qqline((Larch$cr-Larch$cr_new_est))
  
  plot(Larch$cr,Larch$C_R_est,xlim=c(0,1),ylim=c(0,1))
  abline(coef = c(0,1)) 
  plot(Larch$cr,Larch$C_R_est,xlim=c(0,1),ylim=c(0,1))
  abline(coef = c(0,1)) 
  
  
  
  mean((Larch$cr-Larch$cr_new_est),na.rm=T)
  mean((Larch$cr-Larch$C_R_est),na.rm=T)
  range((Larch$cr-Larch$cr_new_est),na.rm=T)
  range((Larch$cr-Larch$C_R_est),na.rm=T)
  sd((Larch$cr-Larch$cr_new_est),na.rm=T)
  sd((Larch$cr-Larch$C_R_est),na.rm=T)
  
  
  
  Larch$cr-Larch$C_R_est
  AFI_Final_Data$CCF<-1/AFI_Final_Data$CCF
  plot(Fast_growing$cr,Fast_growing$CCF)
  abline(coef = c(0,1))
  summary(lm(AFI_Final_Data$cr~AFI_Final_Data$CCF))
  plot(AFI_Final_Data$cr,exp(-.005*AFI_Final_Data$CCF),xlim=c(0,1))
  abline(coef = c(0,1))
  nls(cr~a*exp(b*CCF),AFI_Final_Data,start = c(a=))
  ################################################################################
  Other_conifers<-test[test$cohort_name=="Other conifers",]
  Other_conifers$sl<-log(Other_conifers$cr/(1-Other_conifers$cr))
  Other_conifers$sl[is.infinite(Other_conifers$sl)]<-NA
  after<-nls(sl~a+b*BAL_ties_adjusted_3+
               e*(Height_m/dbhcm),data=Other_conifers[!is.na(Other_conifers$cr) & Other_conifers$cr>0,],
             start = c(a=-5.301e-01,b=-1.738e-03,e=-6.676e-01))
  
  Other_conifers$sl_est<- 1.33314 + 0.07584 * Other_conifers$BAL_ties_adjusted_3 +  -3.09514  * (Other_conifers$Height_imp/Other_conifers$dbhcm)
  
  Other_conifers$cr_new_est<-exp(Other_conifers$sl_est)/(1+exp(Other_conifers$sl_est))
  AIC(after)
  summary(after)
  plot(Other_conifers$sl,Other_conifers$sl_est)
  abline(coef = c(0,1))  
  plot(Other_conifers$cr,Other_conifers$cr_new_est,xlim=c(0,1),ylim=c(0,1))
 
  
  plot(Other_conifers$cr,Other_conifers$C_R_est,xlim=c(0,1),ylim=c(0,1))
  
  plot(Other_conifers$cr,Other_conifers$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  
  NROW(Other_conifers[!is.na(Other_conifers$cr) & Other_conifers$cr>0,])*log((sum(Other_conifers$cr-Other_conifers$C_R_est,na.rm = T))^2/41)+2*4
  sqrt(sum((Other_conifers$cr-Other_conifers$cr_new_est)^2,na.rm=T)/41)
  sqrt(sum((Other_conifers$cr-Other_conifers$C_R_est)^2,na.rm=T)/41)
  
  
  plot(Other_conifers$C_R_est,(Other_conifers$cr-Other_conifers$C_R_est)/ sd(Other_conifers$cr-Other_conifers$C_R_est,na.rm=T) )
  abline(coef=c(0,0))
  qqnorm((Other_conifers$cr-Other_conifers$C_R_est))
  qqline((Other_conifers$cr-Other_conifers$C_R_est))
  
  plot(Other_conifers$cr_new_est,(Other_conifers$cr-Other_conifers$cr_new_est)/ sd(Other_conifers$cr-Other_conifers$cr_new_est,na.rm=T),
       Main = " Fitted values vs Standardised residuals" , xlab = "Estiamted CR",ylab = "Standardised Residuals")
  abline(coef=c(0,0))
  qqnorm((Other_conifers$cr-Other_conifers$cr_new_est))
  qqline((Other_conifers$cr-Other_conifers$cr_new_est))
  
  mean((Other_conifers$cr-Other_conifers$cr_new_est),na.rm=T)
  mean((Other_conifers$cr-Other_conifers$C_R_est),na.rm=T)
  range((Other_conifers$cr-Other_conifers$cr_new_est),na.rm=T)
  range((Other_conifers$cr-Other_conifers$C_R_est),na.rm=T)
  sd((Other_conifers$cr-Other_conifers$cr_new_est),na.rm=T)
  sd((Other_conifers$cr-Other_conifers$C_R_est),na.rm=T)
  
  
  ################################################################################
  DouglasFir<-test[test$cohort_name=="Douglas fir",]
  DouglasFir$sl<-log(DouglasFir$cr/(1-DouglasFir$cr))
  DouglasFir$sl[is.infinite(DouglasFir$sl)]<-NA
  after<-nls(sl~a+b*BAL_ties_adjusted_3+
               c*log(CCF)+
               e*(Height_m/dbhcm)+
               f*dbhcm^2,data=DouglasFir[!is.na(DouglasFir$sl) & DouglasFir$cr>0,],
             start = c(a=0,b=-1.738e-03,c=2.041e-01,e=-6.676e-01,f=1.404e-04))
  
  DouglasFir$sl_est<--1.441e-02*DouglasFir$BAL_ties_adjusted_3+
    3.247e-01*log(DouglasFir$CCF)+
    -1.457*(DouglasFir$Height_m/DouglasFir$dbhcm)+
    -1.033e-04*DouglasFir$dbhcm^2
  
  DouglasFir$sl_est<-1.914e-01-1.425e-02*DouglasFir$BAL_ties_adjusted_3+
    2.961e-01*log(DouglasFir$CCF)+
    -1.524e+00*(DouglasFir$Height_m/DouglasFir$dbhcm)+
    -1.073e-04*DouglasFir$dbhcm^2
  
    
    DouglasFir$cr_new_est<-exp(DouglasFir$sl_est)/(1+exp(DouglasFir$sl_est))
  AIC(after)
  summary(after)
  plot(Other_conifers$sl,Other_conifers$sl_est)
  abline(coef = c(0,1))  
  plot(Other_conifers$cr,Other_conifers$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  
  
  plot(Other_conifers$cr,Other_conifers$C_R_est,xlim=c(0,1),ylim=c(0,1))
  
  plot(Other_conifers$cr,Other_conifers$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  
  NROW(DouglasFir[!is.na(DouglasFir$cr) & DouglasFir$cr>0,])*log((sum(DouglasFir$cr-DouglasFir$C_R_est,na.rm = T))^2/41)+2*4
  sqrt(sum((DouglasFir$cr-DouglasFir$cr_new_est)^2,na.rm=T)/201)
  sqrt(sum((DouglasFir$cr-DouglasFir$C_R_est)^2,na.rm=T)/201)
  
  
  plot(Other_conifers$C_R_est,(Other_conifers$cr-Other_conifers$C_R_est)/ sd(Other_conifers$cr-Other_conifers$C_R_est,na.rm=T) )
  abline(coef=c(0,0))
  qqnorm((Other_conifers$cr-Other_conifers$C_R_est))
  qqline((Other_conifers$cr-Other_conifers$C_R_est))
  
  plot(Other_conifers$cr_new_est,(Other_conifers$cr-Other_conifers$cr_new_est)/ sd(Other_conifers$cr-Other_conifers$cr_new_est,na.rm=T),
       Main = " Fitted values vs Standardised residuals" , xlab = "Estiamted CR",ylab = "Standardised Residuals")
  abline(coef=c(0,0))
  qqnorm((Other_conifers$cr-Other_conifers$cr_new_est))
  qqline((Other_conifers$cr-Other_conifers$cr_new_est))
   
  mean((Other_conifers$cr-Other_conifers$cr_new_est),na.rm=T)
  mean((Other_conifers$cr-Other_conifers$C_R_est),na.rm=T)
  range((Other_conifers$cr-Other_conifers$cr_new_est),na.rm=T)
  range((Other_conifers$cr-Other_conifers$C_R_est),na.rm=T)
  sd((Other_conifers$cr-Other_conifers$cr_new_est),na.rm=T)
  sd((Other_conifers$cr-Other_conifers$C_R_est),na.rm=T)
  
  
  
  
  
  
  
  
  
  
  
  ###############################################################################
  "Height_m"      "Density_ha"          "BASEL_M_HA"        "cr"      "CCF"               "dbhcm"    "BAL_ties_adjusted_3" "plotba_m2_ha"  
  
  summary(lm(cr~Height_m*Density_ha*BASEL_M_HA*CCF*dbhcm*BAL_ties_adjusted_3*plotba_m2_ha,AFI_Final_Data))
  
  plot(AFI_Final_Data$Height_m,AFI_Final_Data$Estim_Ht)
  abline(coef=c(0,1))
  CCF                                                                        
  Density_ha:CCF 
  BASEL_M_HA:CCF
  Height_m:BAL_ties_adjusted_3 
  Height_m:Density_ha:CCF
  Density_ha:BASEL_M_HA:CCF
  Density_ha:CCF:dbhcm 
  Density_ha:CCF:plotba_m2_ha
  plot(AFI_Final_Data$cr,AFI_Final_Data$Density_ha*AFI_Final_Data$BASEL_M_HA*AFI_Final_Data$CCF,xlim=c(0,1))
  
  range((1/((1+exp(-(-1.287+0.097*AFI_ISN_Data$dbhcm-0.313*AFI_ISN_Data$Height_m)))^(1/6))),na.rm=T)
  plot(AFI_ISN_Data$cr,(1/((1+exp(-(-5.612491-0.025735*AFI_ISN_Data$BAL+0.034080*AFI_ISN_Data$Height_m)))^(1/6))),xlim=c(0,1),ylim=c(0,1))
  abline(coef = c(0,1))
  summary(nls(cr~1/((1+exp(-(a+b*dbhcm+c*Height_m)))^(1/6)),AFI_ISN_Data,start = c(a=-1,b=.01,c=-.4)))
  summary((nls(cr~1/((1+exp(-(a+b*BAL+c*Height_m)))^(1/6)),AFI_ISN_Data,start = c(a=-6.871887,c=0.059697,b=1))))
  
  
  
  ggplot(AFI_ISN_Data)+geom_point(aes(x=cr,y=(1/((1+exp(-(-5.612491-0.025735*BAL+0.034080*Height_m)))^(1/6))),col=Density_ha))+
    xlim(0,1)+ylim(0,1)+geom_abline(slope=1,intercept = 0)
  
  ggplot(AFI_ISN_Data)+geom_point(aes(x=cr,y=(1/((1+exp(-(-5.612491-0.025735*BAL+0.034080*Height_m)))^(1/6))),col=CCF))+
    xlim(0,1)+ylim(0,1)+geom_abline(slope=1,intercept = 0) 
  
  ggplot(AFI_ISN_Data)+geom_point(aes(x=cr,y=(1/((1+exp(-(-5.612491-0.025735*BAL+0.034080*Height_m)))^(1/6))),col=plotba_m2_ha))+
    xlim(0,1)+ylim(0,1)+geom_abline(slope=1,intercept = 0) 
  
  
  plot(AFI_ISN_Data_before$Height_m,AFI_ISN_Data_before$Estim_Ht)

  plot(AFI_Final_Data$Height_m,AFI_Final_Data$Estim_Ht)
  abline(coef = c(0,1))
  mean(AFI_ISN_Data$cr-AFI_ISN_Data$C_R_est,na.rm=T)
  sd(AFI_ISN_Data$cr-AFI_ISN_Data$C_R_est,na.rm=T)
  var(AFI_ISN_Data$cr-AFI_ISN_Data$C_R_est,na.rm=T)
  median(AFI_ISN_Data$cr-AFI_ISN_Data$C_R_est,na.rm=T)
  plot(AFI_ISN_Data$cr,AFI_ISN_Data$C_R_est,xlim=c(0,1))
  AFI_ISN_Data_before<- read.csv(file.choose())
  
  

  plot(log(seq(0,1,0.01)/(1-seq(0,1,0.01))),seq(0,1,0.01))
  
  
  
  
  mean(DouglasFir$cr-DouglasFir$C_R_est,na.rm=T)
  sd(DouglasFir$cr-DouglasFir$C_R_est,na.rm=T)
  var(DouglasFir$cr-DouglasFir$C_R_est,na.rm=T)
  range(DouglasFir$cr-DouglasFir$C_R_est,na.rm=T)
  
  
  mean(DouglasFir$cr-DouglasFir$cr_new_est,na.rm=T)
  sd(DouglasFir$cr-DouglasFir$cr_new_est,na.rm=T)
  var(DouglasFir$cr-DouglasFir$cr_new_est,na.rm=T)
  median(DouglasFir$cr-DouglasFir$cr_new_est,na.rm=T)
  range(DouglasFir$cr-DouglasFir$cr_new_est,na.rm=T)
  
  
  
  plot(AFI_Final_Data$C_R_est,AFI_Final_Data$cr,ylim=c(0,1))
  plot(Spruce$C_R_est,(Spruce$cr-Spruce$C_R_est)/sd(Spruce$cr-Spruce$C_R_est,na.rm=T),ylim=c(-5,5))
  abline(coef = c(0,0))
  plot(Spruce$cr_new_est,(Spruce$cr-Spruce$cr_new_est)/sd(Spruce$cr-Spruce$cr_new_est,na.rm=T),ylim=c(-5,5))
  abline(coef = c(0,0))
  
  range(Spruce$C_R_est,na.rm=T)
  range(Spruce$cr,na.rm=T)
  plot(Spruce$cr,Spruce$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  abline(coef = c(0,1))  
  
  plot(Spruce[Spruce$cr>0,]$cr,Spruce[Spruce$cr>0,]$C_R_est,xlim=c(0,1),ylim=c(0,1))
  
  
  
  after<-nls(sl~a+b*BAL_ties_adjusted_3+
               c*log(CCF)+
               d*Height_imp+
               e*(Height_imp/dbhcm)+
               f*dbhcm^2,data=Spruce[!is.na(Spruce$sl) & Spruce$cr>0,],start = c(a=-5.301e-01,b=-1.738e-03,c=2.041e-01,d=-2.300e-02,e=-6.676e-01,f=1.404e-04))
  
  
  after1<-nls(cr~a+b*BAL_ties_adjusted_3+
               c*log(CCF)+
               d*Height_imp+
               e*(Height_imp/dbhcm)+
               f*dbhcm^2,data=Spruce[!is.na(Spruce$sl) & Spruce$cr>0,],start = c(a=-5.301e-01,b=-1.738e-03,c=2.041e-01,d=-2.300e-02,e=-6.676e-01,f=1.404e-04))
  
  summary(after1)
  AIC(after1)
  summary(after)
  AIC(after)
  plot(Spruce$cr,3.706e-01-1.161e-03*Spruce$BAL_ties_adjusted_3+
         5.117e-02*log(Spruce$CCF)+
         -5.606e-03*Spruce$Height_imp+
         -1.681e-01*(Spruce$Height_imp/Spruce$dbhcm)+
         3.337e-05*Spruce$dbhcm^2,xlim=c(0,1),ylim=c(0,1))
  abline(coef=c(0,1))
  
  
  qqline(Spruce$cr-3.706e-01-1.161e-03*Spruce$BAL_ties_adjusted_3+
         5.117e-02*log(Spruce$CCF)+
         -5.606e-03*Spruce$Height_imp+
         -1.681e-01*(Spruce$Height_imp/Spruce$dbhcm)+
         3.337e-05*Spruce$dbhcm^2)
  
  
  
  
  
  qqnorm(test$cr-test$C_R_est)
  qqline(test$cr-test$C_R_est)
  
  
  
  
  test<-AFI_Final_Data
  test<-test[test$cr>0,]
  test$dbh_STVI<-NA
  test$stviX<-NA
  test$Diam1<-  test$dbhcm
  test$PlotNo<-test$idplots
  test$xcoord<-test$X
  test$ycoord<-test$Y
 # [test$Site_code==n,]
  for(u in unique(AFI_Final_Data$Site_code)){

  for (i in unique(test$cohort_name)){
    
    start.time <- Sys.time()
    Plot102_STVI<-STVI_per_plot(test[test$cohort_name==i & test$Site_code==u,])
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    
    if(!is_empty(Plot102_STVI)){
      for ( n in 1:length(Plot102_STVI)){
        if(!is_empty( test[test$idplots==n & test$cohort_name==i & test$Site_code==u,]$dbh_STVI)){
        test[test$idplots==n & test$cohort_name==i & test$Site_code==u,]$dbh_STVI<-Plot102_STVI[[n]]$STVI_dbh
        test[test$idplots==n & test$cohort_name==i & test$Site_code==u,]$stviX<- Plot102_STVI[[n]]$stvi_x
        }
      }
      
    }
  }
  }
  plot(test$cr,test$dbh_STVI,xlim=c(0,1))
  
  ggplot
  
  
 summary( nls(cr~1/(1+exp(a+b*BAL_ties_adjusted_3+
                            c*log(CCF)+
                            d*Height_imp+
                            e*(Height_m/dbhcm)+
                            f*dbhcm^2)),data=Spruce[!is.na(Spruce$sl) & Spruce$cr>0,],start = c(a=-5.301e-01,b=-1.738e-03,c=2.041e-01,d=-2.300e-02,e=-6.676e-01,f=1.404e-04))
  
 )
  plot(Spruce$cr,1/(1+exp(5.072e-01+ 4.815e-03*Spruce$BAL_ties_adjusted_3+
                            -2.106e-01*log(Spruce$CCF)+
                            2.431e-02*Spruce$Height_m+
                            6.873e-01*(Spruce$Height_m/Spruce$dbhcm)+
                            -1.406e-04*Spruce$dbhcm^2)),xlim=c(0,1),ylim=c(0,1))
abline(coef = c(0,1))  
plot(Spruce$cr,Spruce$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  View(Spruce)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # word document for cr
  #other conifers
  #og model
  plot(Other_conifers$cr,Other_conifers$C_R_est,xlim=c(0,1),ylim=c(0,1),main = "Original CR model.",xlab = "Actual CR",ylab = "Estimated CR")
  abline(coef=c(0,1))  
  plot(Other_conifers$cr,Other_conifers$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  abline(coef=c(0,1))    
  plot(Other_conifers$C_R_est,(Other_conifers$cr-Other_conifers$C_R_est)/ sd(Other_conifers$cr-Other_conifers$C_R_est,na.rm=T),
       main = " Original Fitted values vs Standardised residuals" , xlab = "Estiamted CR",ylab = "Standardised Residuals",xlim=c(0,1),ylim=c(-3,3))
  abline(coef=c(0,0))
  qqnorm((Other_conifers$cr-Other_conifers$C_R_est),ylim = c(-1,1))
  qqline((Other_conifers$cr-Other_conifers$C_R_est))

  
  
  
  
  
  plot(Other_conifers$cr,Other_conifers$cr_new_est,xlim=c(0,1),ylim=c(0,1),main = "New CR model.",xlab = "Actual CR",ylab = "Estimated CR")
  abline(coef=c(0,1))  
  
  
  plot(Other_conifers$cr_new_est,(Other_conifers$cr-Other_conifers$cr_new_est)/ sd(Other_conifers$cr-Other_conifers$cr_new_est,na.rm=T),
       main = " New Fitted values vs Standardised residuals" , xlab = "Estiamted CR",ylab = "Standardised Residuals",xlim=c(0,1),ylim=c(-3,3))
  abline(coef=c(0,0))
  
  qqnorm((Other_conifers$cr-Other_conifers$cr_new_est),ylim = c(-1,1))
  qqline((Other_conifers$cr-Other_conifers$cr_new_est))
  
  
  
  
  #Spruce
  plot(Spruce$cr,Spruce$C_R_est,xlim=c(0,1),ylim=c(0,1),main = "Original CR model.",xlab = "Actual CR",ylab = "Estimated CR")
  abline(coef=c(0,1))  
  plot(Spruce$cr,Spruce$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  abline(coef=c(0,1))    
  plot(Spruce$C_R_est,(Spruce$cr-Spruce$C_R_est)/ sd(Spruce$cr-Spruce$C_R_est,na.rm=T),
       main = " Original Fitted values vs Standardised residuals" , xlab = "Estiamted CR",ylab = "Standardised Residuals",xlim=c(0,1),ylim=c(-3,3))
  abline(coef=c(0,0))
  qqnorm((Spruce$cr-Spruce$C_R_est),ylim = c(-1,1))
  qqline((Spruce$cr-Spruce$C_R_est))
  
  NROW(Spruce[!is.na(Spruce$cr) & Spruce$cr>0,])
  sqrt(sum((Spruce$cr-Spruce$cr_new_est)^2,na.rm=T)/1205)
  sqrt(sum((Spruce$cr-Spruce$C_R_est)^2,na.rm=T)/1205)
  
  
  
  
  plot(Spruce$cr,Spruce$cr_new_est,xlim=c(0,1),ylim=c(0,1),main = "New CR model.",xlab = "Actual CR",ylab = "Estimated CR")
  abline(coef=c(0,1))  
  
  
  plot(Spruce$cr_new_est,(Spruce$cr-Spruce$cr_new_est)/ sd(Spruce$cr-Spruce$cr_new_est,na.rm=T),
       main = " New Fitted values vs Standardised residuals" , xlab = "Estiamted CR",ylab = "Standardised Residuals",xlim=c(0,1),ylim=c(-3,3))
  abline(coef=c(0,0))
  
  qqnorm((Spruce$cr-Spruce$cr_new_est),ylim = c(-1,1))
  qqline((Spruce$cr-Spruce$cr_new_est))
  
  # Larch
  Larch<- Larch_Pine
  plot(Larch$cr,Larch$C_R_est,xlim=c(0,1),ylim=c(0,1),main = "Original CR model.",xlab = "Actual CR",ylab = "Estimated CR")
  abline(coef=c(0,1))  
  plot(Larch$cr,Larch$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  abline(coef=c(0,1))    
  plot(Larch$C_R_est,(Larch$cr-Larch$C_R_est)/ sd(Larch$cr-Larch$C_R_est,na.rm=T),
       main = " Original Fitted values vs Standardised residuals" , xlab = "Estiamted CR",ylab = "Standardised Residuals",xlim=c(0,1),ylim=c(-4,4))
  abline(coef=c(0,0))
  qqnorm((Larch$cr-Larch$C_R_est),ylim = c(-1,1))
  qqline((Larch$cr-Larch$C_R_est))
  
  NROW(Larch[!is.na(Larch$cr) & Larch$cr>0,])
  sqrt(sum((Larch$cr-Larch$cr_new_est)^2,na.rm=T)/62)
  sqrt(sum((Larch$cr-Larch$C_R_est)^2,na.rm=T)/62)
  
  
  
  
  plot(Larch$cr,Larch$cr_new_est,xlim=c(0,1),ylim=c(0,1),main = "New CR model.",xlab = "Actual CR",ylab = "Estimated CR")
  abline(coef=c(0,1))  
  
  
  plot(Larch$cr_new_est,(Larch$cr-Larch$cr_new_est)/ sd(Larch$cr-Larch$cr_new_est,na.rm=T),
       main = " New Fitted values vs Standardised residuals" , xlab = "Estiamted CR",ylab = "Standardised Residuals",xlim=c(0,1),ylim=c(-4,4))
  abline(coef=c(0,0))
  
  qqnorm((Larch$cr-Larch$cr_new_est),ylim = c(-1,1))
  qqline((Larch$cr-Larch$cr_new_est))
  
  # Fast growimh Broadleaves
  plot(Fast_growing$cr,Fast_growing$C_R_est,xlim=c(0,1),ylim=c(0,1),main = "Original CR model.",xlab = "Actual CR",ylab = "Estimated CR")
  abline(coef=c(0,1))  
  plot(Fast_growing$cr,Fast_growing$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  abline(coef=c(0,1))    
  plot(Fast_growing$C_R_est,(Fast_growing$cr-Fast_growing$C_R_est)/ sd(Fast_growing$cr-Fast_growing$C_R_est,na.rm=T),
       main = " Original Fitted values vs Standardised residuals" , xlab = "Estiamted CR",ylab = "Standardised Residuals",xlim=c(0,1),ylim=c(-4,4))
  abline(coef=c(0,0))
  qqnorm((Fast_growing$cr-Fast_growing$C_R_est),ylim = c(-1,1))
  qqline((Fast_growing$cr-Fast_growing$C_R_est))
  
  NROW(Fast_growing[!is.na(Fast_growing$cr) & Fast_growing$cr>0,])
  sqrt(sum((Fast_growing$cr-Fast_growing$cr_new_est)^2,na.rm=T)/45)
  sqrt(sum((Fast_growing$cr-Fast_growing$C_R_est)^2,na.rm=T)/45)
  
  
  
  
  plot(Fast_growing$cr,Fast_growing$cr_new_est,xlim=c(0,1),ylim=c(0,1),main = "New CR model.",xlab = "Actual CR",ylab = "Estimated CR")
  abline(coef=c(0,1))  
  
  
  plot(Fast_growing$cr_new_est,(Fast_growing$cr-Fast_growing$cr_new_est)/ sd(Fast_growing$cr-Fast_growing$cr_new_est,na.rm=T),
       main = " New Fitted values vs Standardised residuals" , xlab = "Estiamted CR",ylab = "Standardised Residuals",xlim=c(0,1),ylim=c(-4,4))
  abline(coef=c(0,0))
  
  qqnorm((Fast_growing$cr-Fast_growing$cr_new_est),ylim = c(-1,1))
  qqline((Fast_growing$cr-Fast_growing$cr_new_est))
  
  
  # Fast growimh Broadleaves
  plot(slow_broadleaves$cr,slow_broadleaves$C_R_est,xlim=c(0,1),ylim=c(0,1),main = "Original CR model.",xlab = "Actual CR",ylab = "Estimated CR")
  abline(coef=c(0,1))  
  plot(slow_broadleaves$cr,slow_broadleaves$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  abline(coef=c(0,1))    
  plot(slow_broadleaves$C_R_est,(slow_broadleaves$cr-slow_broadleaves$C_R_est)/ sd(slow_broadleaves$cr-slow_broadleaves$C_R_est,na.rm=T),
       main = " Original Fitted values vs Standardised residuals" , xlab = "Estiamted CR",ylab = "Standardised Residuals",xlim=c(0,1),ylim=c(-4,4))
  abline(coef=c(0,0))
  qqnorm((slow_broadleaves$cr-slow_broadleaves$C_R_est),ylim = c(-1,1))
  qqline((slow_broadleaves$cr-slow_broadleaves$C_R_est))
  
  NROW(slow_broadleaves[!is.na(slow_broadleaves$cr) & slow_broadleaves$cr>0,])
  sqrt(sum((slow_broadleaves$cr-slow_broadleaves$cr_new_est)^2,na.rm=T)/56)
  sqrt(sum((slow_broadleaves$cr-slow_broadleaves$C_R_est)^2,na.rm=T)/56)
  
  
  
  
  plot(slow_broadleaves$cr,slow_broadleaves$cr_new_est,xlim=c(0,1),ylim=c(0,1),main = "New CR model.",xlab = "Actual CR",ylab = "Estimated CR")
  abline(coef=c(0,1))  
  
  
  plot(slow_broadleaves$cr_new_est,(slow_broadleaves$cr-slow_broadleaves$cr_new_est)/ sd(slow_broadleaves$cr-slow_broadleaves$cr_new_est,na.rm=T),
       main = " New Fitted values vs Standardised residuals" , xlab = "Estiamted CR",ylab = "Standardised Residuals",xlim=c(0,1),ylim=c(-4,4))
  abline(coef=c(0,0))
  
  qqnorm((slow_broadleaves$cr-slow_broadleaves$cr_new_est),ylim = c(-1,1))
  qqline((slow_broadleaves$cr-slow_broadleaves$cr_new_est))
  
  
  # douglas fir 
  
  plot(DouglasFir$cr,DouglasFir$C_R_est,xlim=c(0,1),ylim=c(0,1),main = "Original CR model.",xlab = "Actual CR",ylab = "Estimated CR")
  abline(coef=c(0,1))  
  plot(DouglasFir$cr,DouglasFir$cr_new_est,xlim=c(0,1),ylim=c(0,1))
  abline(coef=c(0,1))    
  plot(DouglasFir$C_R_est,(DouglasFir$cr-DouglasFir$C_R_est)/ sd(DouglasFir$cr-DouglasFir$C_R_est,na.rm=T),
       main = " Original Fitted values vs Standardised residuals" , xlab = "Estiamted CR",ylab = "Standardised Residuals",xlim=c(0,1),ylim=c(-4,4))
  abline(coef=c(0,0))
  qqnorm((DouglasFir$cr-DouglasFir$C_R_est),ylim = c(-1,1))
  qqline((DouglasFir$cr-DouglasFir$C_R_est))
  
  NROW(DouglasFir[!is.na(DouglasFir$cr) & DouglasFir$cr>0,])
  sqrt(sum((DouglasFir$cr-DouglasFir$cr_new_est)^2,na.rm=T)/201)
  sqrt(sum((DouglasFir$cr-DouglasFir$C_R_est)^2,na.rm=T)/201)
  
  
  
  
  plot(DouglasFir$cr,DouglasFir$cr_new_est,xlim=c(0,1),ylim=c(0,1),main = "New CR model.",xlab = "Actual CR",ylab = "Estimated CR")
  abline(coef=c(0,1))  
  
  
  plot(DouglasFir$cr_new_est,(DouglasFir$cr-DouglasFir$cr_new_est)/ sd(DouglasFir$cr-DouglasFir$cr_new_est,na.rm=T),
       main = " New Fitted values vs Standardised residuals" , xlab = "Estiamted CR",ylab = "Standardised Residuals",xlim=c(0,1),ylim=c(-4,4))
  abline(coef=c(0,0))
  
  qqnorm((DouglasFir$cr-DouglasFir$cr_new_est),ylim = c(-1,1))
  qqline((DouglasFir$cr-DouglasFir$cr_new_est))
  
  
  ## Height model
  
  
  
  
  mean(Douglasfir$Height_m-Douglasfir$Estim_Ht,na.rm=T)
  sd(Douglasfir$Height_m-Douglasfir$Estim_Ht,na.rm=T)
  var(Douglasfir$Height_m-Douglasfir$Estim_Ht,na.rm=T)
  range(Douglasfir$Height_m-Douglasfir$Estim_Ht,na.rm=T)
  
  
  mean(Douglasfir$Height_m-Douglasfir$NEW_height,na.rm=T)
  sd(Douglasfir$Height_m-Douglasfir$NEW_height,na.rm=T)
  var(Douglasfir$Height_m-Douglasfir$NEW_height,na.rm=T)
  median(Douglasfir$Height_m-Douglasfir$NEW_height,na.rm=T)
  range(Douglasfir$Height_m-Douglasfir$NEW_height,na.rm=T)
  
  
  
  
  # Other conifers
  # douglas fir 
  
  plot(Other_conifers$Height_m,Other_conifers$Estim_Ht,xlim=c(0,50),ylim=c(0,50),main = "Original Height model.",xlab = "Actual Height",ylab = "Estimated Height")
  abline(coef=c(0,1))  
  plot(Other_conifers$Height_m,Other_conifers$NEW_height,xlim=c(0,1),ylim=c(0,1))
  abline(coef=c(0,1))    
  plot(Other_conifers$Estim_Ht,(Other_conifers$Height_m-Other_conifers$Estim_Ht)/ sd(Other_conifers$Height_m-Other_conifers$Estim_Ht,na.rm=T),
       main = " Original Fitted values vs Standardised residuals" , xlab = "Estiamted Height",ylab = "Standardised Residuals",xlim=c(0,50),ylim=c(-3,3))
  abline(coef=c(0,0))
  qqnorm((Other_conifers$Height_m-Other_conifers$Estim_Ht),ylim=c(-20,20))
  qqline((Other_conifers$Height_m-Other_conifers$Estim_Ht))
  
  NROW(Other_conifers[!is.na(Other_conifers$Height_m) & Other_conifers$Height_m>0,])
  sqrt(sum((Other_conifers$Height_m-Other_conifers$NEW_height)^2,na.rm=T)/68)
  sqrt(sum((Other_conifers$Height_m-Other_conifers$Estim_Ht)^2,na.rm=T)/68)
  
  
  
  
  plot(Other_conifers$Height_m,Other_conifers$NEW_height,xlim=c(0,50),ylim=c(0,50),main = "New Height model.",xlab = "Actual Height",ylab = "Estimated Height")
  abline(coef=c(0,1))  
  
  
  plot(Other_conifers$NEW_height,(Other_conifers$Height_m-Other_conifers$NEW_height)/ sd(Other_conifers$Height_m-Other_conifers$NEW_height,na.rm=T),
       main = " New Fitted values vs Standardised residuals" , xlab = "Estiamted Height",ylab = "Standardised Residuals",xlim=c(0,50),ylim=c(-3,3))
  abline(coef=c(0,0))
  
  qqnorm((Other_conifers$Height_m-Other_conifers$NEW_height),ylim=c(-20,20))
  qqline((Other_conifers$Height_m-Other_conifers$NEW_height))
  
  
  # Spruce
  
  plot(Spruce$Height_m,Spruce$Estim_Ht,xlim=c(0,50),ylim=c(0,50),main = "Original Height model.",xlab = "Actual Height",ylab = "Estimated Height")
  abline(coef=c(0,1))  
  plot(Spruce$Height_m,Spruce$NEW_height,xlim=c(0,1),ylim=c(0,1))
  abline(coef=c(0,1))    
  plot(Spruce$Estim_Ht,(Spruce$Height_m-Spruce$Estim_Ht)/ sd(Spruce$Height_m-Spruce$Estim_Ht,na.rm=T),
       main = " Original Fitted values vs Standardised residuals" , xlab = "Estiamted Height",ylab = "Standardised Residuals"),xlim=c(0,50),ylim=c(-3,3))
  abline(coef=c(0,0))
  qqnorm((Spruce$Height_m-Spruce$Estim_Ht),ylim=c(-20,20))
  qqline((Spruce$Height_m-Spruce$Estim_Ht))
  
  NROW(Spruce[!is.na(Spruce$Height_m) & Spruce$Height_m>0,])
  sqrt(sum((Spruce$Height_m-Spruce$NEW_height)^2,na.rm=T)/1615)
  sqrt(sum((Spruce$Height_m-Spruce$Estim_Ht)^2,na.rm=T)/1615)
  
  
  
  
  plot(Spruce$Height_m,Spruce$NEW_height,xlim=c(0,50),ylim=c(0,50),main = "New Height model.",xlab = "Actual Height",ylab = "Estimated Height")
  abline(coef=c(0,1))  
  
  
  plot(Spruce$NEW_height,(Spruce$Height_m-Spruce$NEW_height)/ sd(Spruce$Height_m-Spruce$NEW_height,na.rm=T),
       main = " New Fitted values vs Standardised residuals" , xlab = "Estiamted Height",ylab = "Standardised Residuals",xlim=c(0,50),ylim=c(-3,3))
  abline(coef=c(0,0))
  
  qqnorm((Spruce$Height_m-Spruce$NEW_height),ylim=c(-20,20))
  qqline((Spruce$Height_m-Spruce$NEW_height))
  
  # Larch
  
  plot(Larch$Height_m,Larch$Estim_Ht,xlim=c(0,50),ylim=c(0,50),main = "Original Height model.",xlab = "Actual Height",ylab = "Estimated Height")
  abline(coef=c(0,1))  
  plot(Larch$Height_m,Larch$NEW_height,xlim=c(0,1),ylim=c(0,1))
  abline(coef=c(0,1))    
  plot(Larch$NEW_height,(Larch$Height_m-Larch$Estim_Ht)/ sd(Larch$Height_m-Larch$Estim_Ht,na.rm=T),
       main = " New Fitted values vs Standardised residuals" , xlab = "Estiamted Height",ylab = "Standardised Residuals",xlim=c(0,50),ylim=c(-3,3))
abline(coef=c(0,0))
qqnorm((Larch$Height_m-Larch$Estim_Ht),ylim=c(-20,20))
qqline((Larch$Height_m-Larch$Estim_Ht))

NROW(Larch[!is.na(Larch$Height_m) & Larch$Height_m>0,])
sqrt(sum((Larch$Height_m-Larch$NEW_height)^2,na.rm=T)/65)
sqrt(sum((Larch$Height_m-Larch$Estim_Ht)^2,na.rm=T)/65)




plot(Larch$Height_m,Larch$NEW_height,xlim=c(0,50),ylim=c(0,50),main = "New Height model.",xlab = "Actual Height",ylab = "Estimated Height")
abline(coef=c(0,1))  


plot(Larch$NEW_height,(Larch$Height_m-Larch$NEW_height)/ sd(Larch$Height_m-Larch$NEW_height,na.rm=T),
     main = " New Fitted values vs Standardised residuals" , xlab = "Estiamted Height",ylab = "Standardised Residuals",xlim=c(0,50),ylim=c(-3,3))
abline(coef=c(0,0))

qqnorm((Larch$Height_m-Larch$NEW_height),ylim=c(-20,20))
qqline((Larch$Height_m-Larch$NEW_height))

  
  
  
  
  
# Fast growing broadleaves

plot(FGbroadleaves$Height_m,FGbroadleaves$Estim_Ht,xlim=c(0,50),ylim=c(0,50),main = "Original Height model.",xlab = "Actual Height",ylab = "Estimated Height")
abline(coef=c(0,1))  
plot(FGbroadleaves$Height_m,FGbroadleaves$NEW_height,xlim=c(0,1),ylim=c(0,1))
abline(coef=c(0,1))    
plot(FGbroadleaves$NEW_height,(FGbroadleaves$Height_m-FGbroadleaves$Estim_Ht)/ sd(FGbroadleaves$Height_m-FGbroadleaves$Estim_Ht,na.rm=T),
     main = " New Fitted values vs Standardised residuals" , xlab = "Estiamted Height",ylab = "Standardised Residuals",xlim=c(0,50),ylim=c(-3,3))
abline(coef=c(0,0))
qqnorm((FGbroadleaves$Height_m-FGbroadleaves$Estim_Ht),ylim=c(-20,20))
qqline((FGbroadleaves$Height_m-FGbroadleaves$Estim_Ht))

NROW(FGbroadleaves[!is.na(FGbroadleaves$Height_m) & FGbroadleaves$Height_m>0,])
sqrt(sum((FGbroadleaves$Height_m-FGbroadleaves$NEW_height)^2,na.rm=T)/70)
sqrt(sum((FGbroadleaves$Height_m-FGbroadleaves$Estim_Ht)^2,na.rm=T)/70)




plot(FGbroadleaves$Height_m,FGbroadleaves$NEW_height,xlim=c(0,50),ylim=c(0,50),main = "New Height model.",xlab = "Actual Height",ylab = "Estimated Height")
abline(coef=c(0,1))  


plot(FGbroadleaves$NEW_height,(FGbroadleaves$Height_m-FGbroadleaves$NEW_height)/ sd(FGbroadleaves$Height_m-FGbroadleaves$NEW_height,na.rm=T),
     main = " New Fitted values vs Standardised residuals" , xlab = "Estiamted Height",ylab = "Standardised Residuals",xlim=c(0,50),ylim=c(-3,3))
abline(coef=c(0,0))

qqnorm((FGbroadleaves$Height_m-FGbroadleaves$NEW_height),ylim=c(-20,20))
qqline((FGbroadleaves$Height_m-FGbroadleaves$NEW_height))

  
  
  
  
  
  
  

# slow growing broadleaves

plot(slow_broadleaves$Height_m,slow_broadleaves$Estim_Ht,xlim=c(0,50),ylim=c(0,50),main = "Original Height model.",xlab = "Actual Height",ylab = "Estimated Height")
abline(coef=c(0,1))  
plot(slow_broadleaves$Height_m,slow_broadleaves$NEW_height,xlim=c(0,1),ylim=c(0,1))
abline(coef=c(0,1))    
plot(slow_broadleaves$NEW_height,(slow_broadleaves$Height_m-slow_broadleaves$Estim_Ht)/ sd(slow_broadleaves$Height_m-slow_broadleaves$Estim_Ht,na.rm=T),
     main = " New Fitted values vs Standardised residuals" , xlab = "Estiamted Height",ylab = "Standardised Residuals",xlim=c(0,50),ylim=c(-3,3))
abline(coef=c(0,0))
qqnorm((slow_broadleaves$Height_m-slow_broadleaves$Estim_Ht),ylim=c(-20,20))
qqline((slow_broadleaves$Height_m-slow_broadleaves$Estim_Ht))

NROW(slow_broadleaves[!is.na(slow_broadleaves$Height_m) & slow_broadleaves$Height_m>0,])
sqrt(sum((slow_broadleaves$Height_m-slow_broadleaves$NEW_height)^2,na.rm=T)/62)
sqrt(sum((slow_broadleaves$Height_m-slow_broadleaves$Estim_Ht)^2,na.rm=T)/62)




plot(slow_broadleaves$Height_m,slow_broadleaves$NEW_height,xlim=c(0,50),ylim=c(0,50),main = "New Height model.",xlab = "Actual Height",ylab = "Estimated Height")
abline(coef=c(0,1))  


plot(slow_broadleaves$NEW_height,(slow_broadleaves$Height_m-slow_broadleaves$NEW_height)/ sd(slow_broadleaves$Height_m-slow_broadleaves$NEW_height,na.rm=T),
     main = " New Fitted values vs Standardised residuals" , xlab = "Estiamted Height",ylab = "Standardised Residuals",xlim=c(0,50),ylim=c(-3,3))
abline(coef=c(0,0))

qqnorm((slow_broadleaves$Height_m-slow_broadleaves$NEW_height),ylim=c(-20,20))
qqline((slow_broadleaves$Height_m-slow_broadleaves$NEW_height))


plot(slow_broadleaves$dbhcm,slow_broadleaves$Height_m)

slow_broadleaves<-slow_broadleaves[!(slow_broadleaves$dbhcm>60 & slow_broadleaves$Height_m<10),]
  
  

# Douglasfir

plot(Douglasfir$Height_m,Douglasfir$Estim_Ht,xlim=c(0,50),ylim=c(0,50),main = "Original Height model.",xlab = "Actual Height",ylab = "Estimated Height")
abline(coef=c(0,1))  
plot(Douglasfir$Height_m,Douglasfir$NEW_height,xlim=c(0,1),ylim=c(0,1))
abline(coef=c(0,1))    
plot(Douglasfir$NEW_height,(Douglasfir$Height_m-Douglasfir$Estim_Ht)/ sd(Douglasfir$Height_m-Douglasfir$Estim_Ht,na.rm=T),
     main = " New Fitted values vs Standardised residuals" , xlab = "Estiamted Height",ylab = "Standardised Residuals",xlim=c(0,50),ylim=c(-4,4))
abline(coef=c(0,0))
qqnorm((Douglasfir$Height_m-Douglasfir$Estim_Ht),ylim=c(-20,20))
qqline((Douglasfir$Height_m-Douglasfir$Estim_Ht))

NROW(Douglasfir[!is.na(Douglasfir$Height_m) & Douglasfir$Height_m>0,])
sqrt(sum((Douglasfir$Height_m-Douglasfir$NEW_height)^2,na.rm=T)/239)
sqrt(sum((Douglasfir$Height_m-Douglasfir$Estim_Ht)^2,na.rm=T)/239)




plot(Douglasfir$Height_m,Douglasfir$NEW_height,xlim=c(0,50),ylim=c(0,50),main = "New Height model.",xlab = "Actual Height",ylab = "Estimated Height")
abline(coef=c(0,1))  


plot(Douglasfir$NEW_height,(Douglasfir$Height_m-Douglasfir$NEW_height)/ sd(Douglasfir$Height_m-Douglasfir$NEW_height,na.rm=T),
     main = " New Fitted values vs Standardised residuals" , xlab = "Estiamted Height",ylab = "Standardised Residuals",xlim=c(0,50),ylim=c(-4,4))
abline(coef=c(0,0))

qqnorm((Douglasfir$Height_m-Douglasfir$NEW_height),ylim=c(-20,20))
qqline((Douglasfir$Height_m-Douglasfir$NEW_height))


plot(Douglasfir$dbhcm,Douglasfir$Height_m)

Douglasfir<-Douglasfir[!(Douglasfir$dbhcm<20 & Douglasfir$Height_m>40),]
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  