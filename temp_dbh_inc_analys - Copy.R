SS_cr<-read.csv(file = file.choose())
View(SS_cr)
SS_cr[SS_cr$Measured.height>800,]$Measured.height<-NA
SS_cr[SS_cr$Measured.CR>800,]$Measured.CR<-NA

ln(i)=a+b*BAL+c*ln(plot_basal_area)+
  d*sqrt(dbhcm)+e*dbhcm^2

x1<-nls(Measured.CR~ exp(a+b*BAL+
               #c*log(CCF)+
               d*Measured.height+
               e*(Measured.height/DBH)+
               f*DBH^2)/(1+exp(a-b*BAL+
                                   #c*log(CCF)+
                                   d*Measured.height+
                                   e*(Measured.height/DBH)+
                                   f*DBH^2)),data = SS_cr,start = c(a=-4,b=-0.01,d=-.1,e=-.3,f=0.00003))


x2<-nls(Measured.CR~ exp(a+b*BAL+
                           #c*log(CCF)+
                           d*Measured.height+
                           e*(Measured.height/DBH)+
                           f*DBH^2),data = SS_cr,start = c(a=2.9,b=-0.01,d=-.1,e=.3,f=0.00003))



SS_cr$CCF

     nls(Annual.DBH.increment_observsed~ a+
           (exp(b+
                  c*log(DBH)+
                  d*DBH^2+
                  log(Measured.CR)*
                  e))*
           (exp(f+
                  g*BAL+
                  h*log(CCF)))*
           (i+
              j*(YC)),
         data=SS_cr)

     SS_cr$BA_ha
     SS_cr$BAL
     
        
    nls( Annual.DBH.increment_observsed~ exp(a1 + 
                                           a2*BAL +
                                           a4*log(BA_ha) +
                                           a5*sqrt(DBH)+
                                           a6*DBH^2),
         data=SS_cr,
         start=c(a1=-0.01499,
                 a2=-0.01254,
                 a4=-0.266,
                 a5=0.2,
                 a6=-0.01))


    test_dbh_inc<-nls( Annual.DBH.increment_observsed~ exp(a1 + 
                                               a2*BAL+
                                               a4*log(BA_ha) +
                                               a5*sqrt(DBH)+
                                               a6*DBH^2),
         data=SS_cr,
         start=c(a1=-1.3867964   ,
                 a2=-0.0118955    ,
                 a4=0.0281847    ,
                 a5=0.2667043 ,
                 a6=-0.0002254 ))

    test_dbh_inc<-nls( Annual.DBH.increment_observsed~ exp(a1 +
                                                             a5*sqrt(DBH) ),
                       data=SS_cr,
                       start=c(a1=-0.9837  ,
                               a5=0.1482))#,
                               a6=-1))
    
    
    
    test_dbh_inc<-lm( log(SS_cr$Annual.DBH.increment_observsed)~ (SS_cr$BAL+
                                               sqrt(SS_cr$DBH)))

SS_cr$dbh_inc_new<- exp(-0.9837   +
                        0.1482*sqrt(SS_cr$DBH))


plot(log(SS_cr$Annual.DBH.increment_observsed),SS_cr$dbh_inc_new,xlim=c(0,2.5),ylim=c(0,2.5))
abline(coef = c(0,1))
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$Pre.DBH.inc.current.model,xlim=c(0,2.5),ylim=c(0,2.5))
abline(coef = c(0,1))


SS_cr$sl<-log(SS_cr$Measured.CR/(1-SS_cr$Measured.CR))


nls(sl~a+b*BAL+
      #c*log(CCF)+
      d*Measured.height+
      e*(Measured.height/DBH)+
      f*DBH^2,data=SS_cr)


nls(sl~a+b*BAL+
      d*Measured.height,data = SS_cr,start = c(a=1.00521 ,b=-0.02555,d=0))
test1<-nls(sl~3.6110-
      0.1552*Measured.height+
        e*(Measured.height/DBH),data = SS_cr,start = c(e=0 ))


test2<-nls(sl~a+
      d*Measured.height+
      f*DBH^2,data = SS_cr,start = c(a=3.6110  ,d=-0.1552,f=0 ))





summary(test1)
AIC(test1)
summary(test2)
AIC(test2)


SS_cr$test_cr<- 3.6110 -0.1552*SS_cr$Measured.height
exp(Preproc$Logit_CR)/(1+exp(Preproc$Logit_CR))

plot(SS_cr$Measured.CR,exp(SS_cr$test_cr)/(1+exp(SS_cr$test_cr)),xlim=c(0,1),ylim=c(0,1))
abline(coef = c(0,1))

mean(SS_cr$Measured.CR-SS_cr$test_cr,na.rm=T)
sd(SS_cr$Measured.CR-SS_cr$test_cr,na.rm=T)
ISN_test<-AFI_ISN_Data[AFI_ISN_Data$dbhcm>27.5 & AFI_ISN_Data$dbhcm<47.5,]

mean(ISN_test$cr-ISN_test$C_R_est,na.rm=T)
sd(ISN_test$cr-ISN_test$C_R_est,na.rm=T)



max(SS_cr_org$Measured.height,na.rm=T)
SS_cr<-SS_cr_org
SS_cr<-SS_cr_org[SS_cr_org$DBH>26 & SS_cr_org$DBH<50,]


plot(SS_cr$Measured.CR,SS_cr$Age,xlim=c(0,1))
plot(SS_cr$Measured.CR,SS_cr$SPP,xlim=c(0,1))
plot(SS_cr$Measured.CR,SS_cr$DBH,xlim=c(0,1))
plot(SS_cr$Measured.CR,SS_cr$Tree.ha,xlim=c(0,1))
plot(SS_cr$Measured.CR,SS_cr$BAL,xlim=c(0,1))
plot(SS_cr$Measured.CR,SS_cr$Measured.height,xlim=c(0,1))
plot(SS_cr$Measured.CR,SS_cr$CCF,xlim=c(0,1))
plot(SS_cr$Measured.CR,SS_cr$BA_ha,xlim=c(0,1))


plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$Age,xlim=c(0,2.6))
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$SPP,xlim=c(0,2.6))
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$DBH,xlim=c(0,2.6))
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$Tree.ha,xlim=c(0,2.6))
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$BAL,xlim=c(0,2.6))
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$Measured.height,xlim=c(0,2.6))
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$CCF,xlim=c(0,2.6))
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$BA_ha,xlim=c(0,2.6))
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$Residual,xlim=c(0,2.6))

max(SS_cr$Annual.DBH.increment_observsed)


plot(SS_cr$Pre.DBH.inc.current.model,SS_cr$Residual/sd(SS_cr$Residual))
summary(lm(SS_cr$Annual.DBH.increment_observsed~SS_cr$Residual))


plot(SS_cr$Annual.DBH.increment_observsed,1.064465*SS_cr$Pre.DBH.inc.current.model,xlim=c(0,2.5),ylim=c(0,2.5))
abline(coef = c(0,1))
sum(exp(SS_cr$Annual.DBH.increment_observsed))/sum(exp(SS_cr$Pre.DBH.inc.current.model))

plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$DBH,ylim=c(0,70),xlim=c(0,3))

SS_cr$dbh_inc_est<- 250*exp(-5.317-0.0430*SS_cr$BAL -0.486*log(SS_cr$BA_ha)+
  0.455*sqrt(SS_cr$DBH)-0.000927*SS_cr$DBH^2)
plot(SS_cr$dbh_inc_est,SS_cr$Annual.DBH.increment_observsed,xlim=c(0,2.5),ylim=c(0,2.5))
abline(coef=c(0,1))
       y=a+b(x-y)
y+by=a+bx
y(1+b)=a+bx
y=(a+bx)/(1+b)

# y=0.827914+0.928290x/(1.92829)  

plot(SS_cr$Annual.DBH.increment_observsed,(0.827914+0.928290*SS_cr$Pre.DBH.inc.current.model)/(1.92829),
     xlim=c(0,2.5),ylim=c(0,2.5)) 
plot(SS_cr$Annual.DBH.increment_observsed,SS_cr$Pre.DBH.inc.current.model,
     xlim=c(0,2.5),ylim=c(0,2.5))
SS_cr_org$Measured.height
summary(lm((Measured.CR)~Measured.height+ CCF,data=SS_cr_org))


SS_cr<-SS_cr_org[SS_cr_org$DBH>20 & SS_cr_org$DBH<30,]

plot(SS_cr$Measured.CR,(1.074-1.917e-02 *SS_cr$Measured.height-4.030e-04*SS_cr$CCF),xlim = c(0,1),ylim = c(0,1))
abline(coef = c(0,1))


hist((1.074-1.917e-02 *SS_cr$Measured.height-4.030e-04*SS_cr$CCF),breaks = 20)
hist(SS_cr$Measured.CR,breaks = 20)





plot(SS_cr$Measured.CR,SS_cr$Measured.height)
plot(SS_cr$Measured.CR,SS_cr$Measured.height*SS_cr$BA_ha^.5)

plot(SS_cr$Measured.CR,SS_cr$DBH,ylim=c(0,.21))
library(ggplot2)
ggplot(Dougl,aes(Development.stage,cr))+geom_boxplot()+ylim(0,1)+facet_wrap(~Size)
table(AFI_ISN_Data$Development.stage,AFI_ISN_Data$Size)

