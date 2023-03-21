library(lmfor)

plot(AFI_ISN_Data$cr,AFI_ISN_Data$average_est_inc_error,xlim=c(0,1),ylim = c(-2,2.1))
abline(h = 0)
?line

x<-plot(AFI_ISN_Data$Height_m~AFI_ISN_Data$average_est_inc_error,ylim=c(0,100))
summary(x)
plot(x)
max(AFI_ISN_Data$Height_m,na.rm=T)


plot(AFI_ISN_Data$Height_m~AFI_ISN_Data$average_est_inc,ylim=c(0,50))
plot(AFI_ISN_Data$Estim_Ht~AFI_ISN_Data$average_est_inc,ylim=c(0,50))



Spruce<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]
DouglasFir<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Douglas fir",]


plot(Spruce$Height_m~Spruce$average_est_inc,ylim=c(0,50))
plot(Spruce$Estim_Ht~Spruce$average_est_inc,ylim=c(0,50))
plot(Spruce$Height_m~Spruce$average_est_inc_error,ylim=c(0,60))
plot(Spruce$cr,Spruce$average_est_inc_error,xlim=c(0,1),ylim = c(-2,2.1))
hist(Spruce$average_est_inc_error,breaks=20)

mean(Spruce$average_est_inc_error,na.rm=T)
median(Spruce$average_est_inc_error,na.rm=T)
sd(Spruce$average_est_inc_error,na.rm=T)
plot(density(Spruce[!is.na(Spruce$average_est_inc_error),]$average_est_inc_error))
?density


plot(DouglasFir$Height_m~DouglasFir$average_est_inc,ylim=c(0,50))
plot(DouglasFir$Estim_Ht~DouglasFir$average_est_inc,ylim=c(0,50))
plot(DouglasFir$Height_m~DouglasFir$average_est_inc_error,ylim=c(0,100))
plot(DouglasFir$cr,DouglasFir$average_est_inc_error,xlim=c(0,1),ylim = c(-2,2.1))

bal is not 0 for highest. 
9 

View()

C_R
# view bal for different plots

# check BAL
plotss<-unique(AFI_ISN_Data$Site_code)
View(AFI_ISN_Data[AFI_ISN_Data$Site_code==plotss[1],])
i=13
View(table(AFI_ISN_Data[AFI_ISN_Data$Site_code==plotss[i],]$idplots,AFI_ISN_Data[AFI_ISN_Data$Site_code==plotss[i],]$BAL))

View(AFI_ISN_Data[AFI_ISN_Data$Site_code==plotss[i],])

View(AFI_ISN_Data)

plot_102<- AFI_ISN_Data[AFI_ISN_Data$Site_code=="102",]








# analysing cr 
Spruce <- AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]
Douglas <- AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Douglas fir",]
exp(Spruce$Logit_CR)/(1+exp(Spruce$Logit_CR))

plot(Spruce$C_R_est,Spruce$cr,xlim=c(-1,2.5))
plot(Spruce$Height_m,Spruce$cr,xlim=c(0,50))
plot(Spruce$BAL_ties_adjusted_3,Spruce$cr)
plot(Spruce$CCF,CCFSpruce$cr)
plot(Spruce$dbhcm,Spruce$cr)

plot(Spruce$Estim_Ht,Spruce$C_R_est,xlim=c(0,50))
plot(Spruce$BAL_ties_adjusted_3,Spruce$C_R_est)
plot(Spruce$CCF,Spruce$C_R_est)
plot(Spruce$dbhcm,Spruce$C_R_est)

BAL_ties_adjusted_3
CCF
Height_imp
dbhcm


plot(Spruce$Estim_Ht,Spruce$Height_m,ylim = c(0,50))

abline(coef = c(0,1))
summary(lm(Spruce$Estim_Ht~Spruce$Height_m))


colnames(Spruce)

lm1 <- lm(Height_m ~ ., data = Spruce[,c("Height_m","cr","dbhcm","Density_ha","BAL_ties_adjusted_3")])
drop1(lm1, test = "F") 


m <- nls(Height_m ~ ., data = Spruce[,c("Height_m","cr","dbhcm","Density_ha","BAL_ties_adjusted_3")])


# log(h-1.3) = a+b/dbh


summary(lm(log(Height_m-1.3) ~ ., data = Spruce[,c("Height_m","cr","dbhcm","Density_ha","BAL_ties_adjusted_3")]))


summary(lm(log(Height_m-1.3) ~ (cr+Density_ha+BAL_ties_adjusted_3)/dbhcm, data = Spruce[,c("Height_m","cr","dbhcm","Density_ha","BAL_ties_adjusted_3")]))


y=Spruce$Height_m
remove<-which(y>100)
y<-y[-remove]

x=Spruce$dbhcm
x<-x[-remove]

nls(log(Height_m-1.3) ~ a1+(a2)/dbhcm, data = Spruce[,c("Height_m","cr","dbhcm","Density_ha","BAL_ties_adjusted_3")])
?nls

est_model<-nls(y~1.3+exp(a+b/x),start = c(a=3,b=-7))
summary(est_model)


plot(x,y)
v<-seq(0,100,1)
w<- 1.3+exp(3.64-18/v)
lines(v,w)


summary(lm(log(Height_m-1.3) ~ ., data = Spruce[,c("Height_m","cr","dbhcm","Density_ha","BAL_ties_adjusted_3")]))
BIC(lm(log(Height_m-1.3) ~ ., data = Spruce[,c("Height_m","cr","dbhcm","Density_ha","BAL_ties_adjusted_3")]))

Spruce$test_height<-1.3+ exp( 2.558+-1.144e-01*Spruce$cr+1.504e-02*Spruce$dbhcm -5.931e-05*Spruce$Density_ha+
                                2.796e-03*Spruce$BAL_ties_adjusted_3)


plot(Spruce$Height_m,Spruce$test_height,xlim=c(0,50),ylim=c(0,50))
lines(0:50,0:50)


mean(Spruce$Height_m-Spruce$test_height,na.rm=T)
median(Spruce$Height_m-Spruce$test_height,na.rm=T)
sd(Spruce$Height_m-Spruce$test_height,na.rm=T)



y=Spruce$Height_m
remove<-which(y>100)
y<-y[-remove]

x=Spruce$dbhcm
x<-x[-remove]



est_model<-nls(y~1.3+exp(a+b/x),start = c(a=3,b=-7))
summary(est_model)


plot(x,y)
v<-seq(0,100,1)
w<- 1.3+exp(3.64-18/v)
lines(v,w)


y~1.3+exp(a+b/x)


Spruce$test_height_nls<-1.3+exp(3.642517-18/Spruce$dbhcm)


mean(Spruce$Height_m-Spruce$test_height_nls,na.rm=T)
median(Spruce$Height_m-Spruce$test_height_nls,na.rm=T)
sd(Spruce$Height_m-Spruce$test_height_nls,na.rm=T)

plot(Spruce$Height_m,Spruce$test_height_nls,xlim=c(0,50))


Spruce_103<-Spruce[Spruce$Site_code=="103",]
summary(lm(log(Height_m-1.3) ~ ., data = Spruce[,c("Height_m","cr","dbhcm","Density_ha","BAL_ties_adjusted_3")]))


y<-Spruce_103[!is.na(Spruce_103$Height_m),]$Height_m
x<-Spruce_103[!is.na(Spruce_103$Height_m),]$dbhcm





y<-Spruce_103$Height_m
x<-Spruce_103$dbhcm








est_model<-nls(y~1.3+exp(a+b/x),
               start = c(a=3,b=-16))
summary(est_model)
Spruce_103$test_height_nls<-1.3+exp(3.610 -16.013 /Spruce_103$dbhcm)


plot(Spruce_103$dbhcm,Spruce_103$test_height_nls,xlab="DBH (cm)",ylab= "Height (m)",main = "DBH vs new estimated height model")



plot(x,y,ylim=c(0,40),xlim=c(0,70),xlab="DBH (cm)",ylab= "Height (m)",main = "DBH vs Height with new estimated height model")
lines(v,w)
v<-seq(0,100,0.5)
w<- 1.3+exp(3.610-16.013/v)

mean(Spruce_103$Height_m-Spruce_103$test_height_nls,na.rm=T)
median(Spruce_103$Height_m-Spruce_103$test_height_nls,na.rm=T)
sd(Spruce_103$Height_m-Spruce_103$test_height_nls,na.rm=T)
cor(Spruce_103[,c("Height_m","test_height_nls")], use = "complete.obs")




BAL<-Spruce_103$BAL_ties_adjusted_3
plotba_m2_ha<-Spruce_103$plotba_m2_ha
dbhcm<-Spruce_103$dbhcm

(33.6931-0.274*BAL + plotba_m2_ha)*(1-exp(-0.02401*dbhcm^(0.8846+0.006412*BAL_ties_adjusted_3)))



test_model<-nls(y~(a-b*BAL + plotba_m2_ha)*(c-exp(d*dbhcm^(e+f*BAL))),
               start = c(a=33,b=-0.2,c=1,d=-0.02,e=0.88,f=0.006))

summary(test_model)





plot(Spruce_103$dbhcm,Spruce_103$Height_m,xlim=c(0,70),ylim=c(0,40),xlab="DBH (cm)", ylab="Height (m)",
     main="DBH vs Height model for plot 103")


plot(Spruce_103$Height_m,Spruce_103$Estim_Ht,xlim=c(0,40),ylim=c(0,40),xlab="Height (m)", ylab="Estimated Height (m)",
     main="Height vs current estimation of Height for plot 103")
abline(coef = c(0,1))

max(Spruce_103$Height_m,na.rm=T)





cor(Spruce_103[!is.na(Spruce_103$Height_m),]$Height_m,Spruce_103[!is.na(Spruce_103$Height_m),]$Estim_Ht)

mean(Spruce_103$Height_m-Spruce_103$Estim_Ht,na.rm=T)
median(Spruce_103$Height_m-Spruce_103$Estim_Ht,na.rm=T)
sd(Spruce_103$Height_m-Spruce_103$Estim_Ht,na.rm=T)
cor(Spruce_103[,c("Height_m","Estim_Ht")], use = "complete.obs")




plot(Spruce_103$Height_m,Spruce_103$test_height_nls,xlim=c(0,40),ylim=c(0,40),xlab="Height (m)", ylab="Estimated Height (m)",
     main="Height vs new estimation of Height for plot 103")






test_model<-nls(y~(a+b*BAL + g*plotba_m2_ha)*(c-exp(d*dbhcm^(e+f*BAL))),
                start = c(a=33,b=-0.2,c=1,d=-0.02,e=0.88,f=0.006,g=0.16))

summary(test_model)

AIC(test_model)

Spruce_103$new_est_height_test3<-(21.633449-0.127*Spruce_103$BAL_ties_adjusted_3+ 0.123632*Spruce_103$plotba_m2_ha)*
                                    (1.137490 - exp(-0.006151*Spruce_103$dbhcm^(1.580691+0.005517*Spruce_103$BAL_ties_adjusted_3)))

plot(Spruce_103$Height_m,Spruce_103$new_est_height_test3,xlim=c(0,40),ylim=c(0,40),xlab="Height (m)", ylab="Estimated Height (m)",
     main="Height vs new estimation of Height for Site 103")


mean(Spruce_103$Height_m-Spruce_103$new_est_height_test3,na.rm=T)
median(Spruce_103$Height_m-Spruce_103$new_est_height_test3,na.rm=T)
sd(Spruce_103$Height_m-Spruce_103$new_est_height_test3,na.rm=T)
cor(Spruce_103[,c("Height_m","new_est_height_test3")], use = "complete.obs")



plot(Spruce_103$dbhcm,Spruce_103$new_est_height_test3)




lm(log(height-1.3)~)



#AIC
AIC(test_model)
AIC(est_model)


table(AFI_ISN_Data[AFI_ISN_Data$Type=="AFI",]$Site_code)
#start with 103
#plot 6
i=6
table(Finalized_Mellory_103$idplots)
summary(lm(cr~C_R_est,Finalized_Mellory_103[!is.na(Finalized_Mellory_103$cr),]))

Spruce_103$Estim_Ht
Spruce_103$Height_m
Spruce_103<-Spruce_103[Spruce_103$Height_m<200,]
Spruce_103<-Spruce_103[!is.na(Spruce_103$Height_m),]
Spruce_103<-Spruce_103[!is.na(Spruce_103$Estim_Ht),]


cor(Spruce_103$Estim_Ht,
    Spruce_103$Height_m)

plot(Spruce_103$Estim_Ht,
    Spruce_103$Height_m)
abline(coef = c(0,1))




summary(lm(Spruce_103[!is.na(Spruce_103$Height_m),]$Estim_Ht~
    Spruce_103[!is.na(Spruce_103$Height_m),]$Height_m))


mean(Spruce_103$Estim_Ht-Spruce_103$Height_m)
median(Spruce_103$Estim_Ht-Spruce_103$Height_m)
sd(Spruce_103$Estim_Ht-Spruce_103$Height_m)


# initial method to interpret est heeight
# Preproc$Estim_Ht <- NA
# 
# Preproc[Preproc$cohort_name=="Spruce",]$Estim_Ht <- (33.6931-0.274*Preproc[Preproc$cohort_name=="Spruce",]$BAL_ties_adjusted_3 +
#                                                        0.1603*Preproc[Preproc$cohort_name=="Spruce",]$plotba_m2_ha)*
#   (1-exp(-0.02401*Preproc[Preproc$cohort_name=="Spruce",]$dbhcm^(0.8846+0.006412*
#                                                                    Preproc[Preproc$cohort_name=="Spruce",]$BAL_ties_adjusted_3)))



nls(log(Height_m-1.3) ~ a1+(a2)/dbhcm, data = Spruce_103,
    start = c(a1=1,a2=1))



Spruce_103$dbhcmfract<-1/Spruce_103$dbhcm

summary(lm(log(Height_m-1.3) ~ dbhcmfract, data = Spruce_103))

NEW_MODEL<-function(Spruce_103){
  height_estimated<-1.3+exp(3.51-14.84585/Spruce_103$dbhcm)
  return(height_estimated)
}

Spruce_103$NEW_height_est<-NEW_MODEL(Spruce_103)


table(Spruce_103$idplots)



mean(Spruce_103$NEW_height_est-Spruce_103$Height_m)
median(Spruce_103$NEW_height_est-Spruce_103$Height_m)
sd(Spruce_103$NEW_height_est-Spruce_103$Height_m)
plot(Spruce_103$NEW_height_est,
     Spruce_103$Height_m)
abline(coef=c(0,1))



summary(lm(log(Height_m-1.3) ~ ., data = Spruce_103[Spruce_103$idplots==3,c("Height_m","dbhcm","BAL_ties_adjusted_3")]))


summary(lm(log(Height_m-1.3) ~ ., data = Spruce_103[Spruce_103$idplots==3,c("Height_m","dbhcmfract","BAL_ties_adjusted_3")]))

test_set<-list()
model_data<-list()
for(i in 1:10){
  idplot_data<-Spruce_103[Spruce_103$idplots==i,c("Height_m","dbhcmfract")]
  
  train_no<-sample(1:NROW(idplot_data),(0.7*NROW(idplot_data)))
  test_no<-c(1:NROW(idplot_data))[!(1:NROW(idplot_data)  %in% train_no)]
  
  idplot_data_test<- idplot_data[test_no,]
  idplot_data_train<-idplot_data[train_no,]
  test_set[[i]]<-idplot_data_test
  model_data[[i]]<-lm(log(Height_m-1.3) ~ ., data =idplot_data_train)
print(summary(lm(log(Height_m-1.3) ~ ., data =idplot_data_train)))
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
}






NEW_MODEL<-function(dbhcm){
  height_estimated<-1.3+exp(3.6171-17.3389/dbhcm)
  return(height_estimated)
}



NEW_MODEL(1/test_set[[3]]$dbhcmfract)


test_set[[3]]$Height_m-NEW_MODEL(1/test_set[[3]]$dbhcmfract)





height_est<-1.3+exp(model_data[[i]]$coefficients[1]+model_data[[i]]$coefficients[2]*test_set[[i]]$dbhcmfract)

mean(height_est-test_set[[i]]$Height_m)
range(height_est-test_set[[i]]$Height_m)
sd(height_est-test_set[[i]]$Height_m)

plot(1/idplot_data$dbhcmfract,idplot_data$Height_m)

plot(1/test_set[[i]]$dbhcmfract,test_set[[i]]$Height_m)
plot(1/idplot_data_train$dbhcmfract,idplot_data_train$Height_m)



# 3.525304  -12.583780 
AIC(lm(log(Height_m-1.3) ~ ., data =idplot_data_train))


(nls(Height_m ~ 1.3+ exp(a+b/dbhcm), data =Spruce_103,start = c(a=3,b=-12)))





height_est<-1.3+exp(model_data[[i]]$coefficients[1]+model_data[[i]]$coefficients[2]*test_set[[i]]$dbhcmfract)
height_est1<-1.3+exp(3.51035-12.13274*test_set[[i]]$dbhcmfract)



mean(height_est1-test_set[[i]]$Height_m)
range(height_est1-test_set[[i]]$Height_m)
sd(height_est1-test_set[[i]]$Height_m)


plot(Spruce_103$dbhcm,log(Spruce_103$Height_m))
plot(Spruce_103$dbhcm,Spruce_103$Height_m)





ImputeHeights(1/idplot_data_train$dbhcmfract,idplot_data_train$Height_m,plot=1)



height_est_n<-1.3+(1/test_set[[i]]$dbhcmfract)^2/((1.4966598 +0.1610986 *(1/test_set[[i]]$dbhcmfract))^2)

mean(height_est_n-test_set[[i]]$Height_m)
range(height_est_n-test_set[[i]]$Height_m)
sd(height_est_n-test_set[[i]]$Height_m)



nas<-ImputeHeights(1/idplot_data_train$dbhcmfract,idplot_data_train$Height_m,plot=1)
wyk<-ImputeHeights(1/idplot_data_train$dbhcmfract,idplot_data_train$Height_m,plot=1,modelName = "wykoff")

ImputeHeights(idplot_data_train$dbhcmfract,idplot_data_train$Height_m,plot=1,modelName = log(h-1.3)~d)




AIC(wyk$model)



height_est_w<-1.3+exp(3.572476 -12.642426/((1/test_set[[i]]$dbhcmfract)+1))


median(height_est_w-test_set[[i]]$Height_m)
range(height_est_w-test_set[[i]]$Height_m)
sd(height_est_w-test_set[[i]]$Height_m)












AFI_Final_Data$



site<-unique(AFI_Final_Data$Site_code)
for(i in site){
  print(i)
  print( table(AFI_Final_Data[AFI_Final_Data$Site_code==i,]$idplots,AFI_Final_Data[AFI_Final_Data$Site_code==i,]$Cycle))
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
}





site_data<-AFI_Final_Data[AFI_Final_Data$Site_code==!123,]



 sds<- (nls(Height_m ~ 1.3+ exp(a+b/dbhcm), data =Spruce_103,start = c(a=3,b=-12)))


  new_spruce_103<-Spruce_103[Spruce_103$Cycle==3,]
 ax<- nls(Height_m ~ 1.3+ exp(a+b/dbhcm), data =new_spruce_103,start = c(a=3,b=-12))
mean(resid(ax  ))
mean(resid(sds  ))

sd(resid(ax  ))
sd(resid(sds  ))


range(resid(ax  ))
range(resid(sds  ))




plot(sds)


table(new_spruce_103$idplots)
######################################################################################################################################################





unique(AFI_ISN_Data$Development.stage)


dev.cr<-AFI_ISN_Data[!is.na(AFI_ISN_Data$Development.stage)& AFI_ISN_Data$Development.stage %in%c( "2b, 3" ,"2b,3"),]

dev.new()
  plot(dev.cr$cr,dev.cr$dbh_act_increment,xlim=c(0,1),ylim = c(0,5))

























