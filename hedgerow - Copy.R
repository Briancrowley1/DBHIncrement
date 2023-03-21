library(rstatix)
library(ggplot2)
library(tidyr)
library(ggthemes)
install.packages("lmerTest")
library(car)
library(multcompView)
library(lsmeans)
library(tidyverse)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(lme4)
library(nlme)
library(lmerTest)




#create lmm for type and age class with site as random vairable

lme.7 <- lmerTest::lmer(BIOMA~A_class+ (1|Site),data = kev.work,REML =F)
lme.8 <- lmerTest::lmer(BIOMA~Type*A_class+(1|Site),data = kev.work,REML =F)
lme.9 <- lmerTest::lmer(BIOMA~Type+(1|Site),data = kev.work,REML =F)

anova(lme.7)
anova(lme.8)
anova(lme.9)
Anova(lme.7)
Anova(lme.8)
Anova(lme.9)
plot(lme.9, type = c("p", "smooth"))
# 
ranova(lme.7)
ranova(lme.8)
ranova(lme.9)
####
lme.9 <- lmerTest::lmer(R.S~Type+(1|Site),data = kev.work,REML =F)

lme.10 <- lmerTest::lmer(BIOMA~(1|Site),data = kev.work,REML =F)

# library(emmeans)
# View(emmeans(test1, list(pairwise ~Site), adjust = "tukey"))
# 
# test1<-lm(BIOMA~Site,data = kev.work)

# library(lmerTest)
# difflsmeans(test1, test.effs = "Site", ddf="Kenward-Roger")


#######################################################################
# run LMM with Site as fixed factor to compare groups with a class and site as random variable
kev.work$Site<-as.factor(kev.work$Site)
model <- lmer (BIOMA~Type + (1|Site), data = kev.work)
summary(model)
emmeans(model, list(pairwise ~ Type), adjust = "tukey")
anova(model)


model <- lmer (BIOMB~Site + (1|Type) + (1|A_class), data = kev.work)
summary(model)
emmeans(model, list(pairwise ~ Site), adjust = "tukey")

model <- lmer (LnB~Site + (1|Type) + (1|A_class), data = kev.work)
summary(model)
emmeans(model, list(pairwise ~ Site), adjust = "tukey")


model <- lmer (R.S~Site + (1|Type) + (1|A_class), data = kev.work)
summary(model)
emmeans(model, list(pairwise ~ Site), adjust = "tukey")


model <- lmer (LnSR~Site + (1|Type) + (1|A_class), data = kev.work)
summary(model)
emmeans(model, list(pairwise ~ Site), adjust = "tukey")

model <- lmer (Tr~Site + (1|Type) + (1|A_class), data = kev.work)
summary(model)
emmeans(model, list(pairwise ~ Site), adjust = "tukey")

model <- lmer (LVOS~Site + (1|Type) + (1|A_class), data = kev.work)
summary(model)
emmeans(model, list(pairwise ~ Site), adjust = "tukey")

model <- lmer (Vol~Site + (1|Type) + (1|A_class), data = kev.work)
summary(model)
emmeans(model, list(pairwise ~ Site), adjust = "tukey")




######
kev.work$Site<-as.factor(kev.work$Site)
model <- lmer (BIOMA~Type + (1|Site), data = kev.work)
summary(model)
emmeans(model, list(pairwise ~ Type), adjust = "tukey")
anova(model)



model <- lmer (BIOMB~Type+ (1|Site), data = kev.work)
summary(model)
emmeans(model, list(pairwise ~ Type), adjust = "tukey")
anova(model)

model <- lmer (LnB~Type + (1|Site), data = kev.work)
summary(model)
emmeans(model, list(pairwise ~ Type), adjust = "tukey")
anova(model)

model <- lmer (R.S~Type + (1|Site), data = kev.work)
summary(model)
emmeans(model, list(pairwise ~ Type), adjust = "tukey")
anova(model)

model <- lmer (LnSR~Type + (1|Site), data = kev.work)
summary(model)
emmeans(model, list(pairwise ~ Type), adjust = "tukey")
anova(model)

model <- lmer (Tr~Type + (1|Site), data = kev.work)
summary(model)
emmeans(model, list(pairwise ~ Type), adjust = "tukey")
anova(model)

model <- lmer (LVOS~Type + (1|Site), data = kev.work)
summary(model)
emmeans(model, list(pairwise ~ Type), adjust = "tukey")
anova(model)

model <- lmer (Vol~Type + (1|Site), data = kev.work)
summary(model)
emmeans(model, list(pairwise ~ Type), adjust = "tukey")
anova(model)
##
#BIOMA
ggplot(kev.work)+ geom_boxplot(aes(x=Site,y= BIOMA,fill = Type))
#BIOMB
ggplot(kev.work)+ geom_boxplot(aes(x=Site,y= BIOMB,fill = Type))
#LnB
ggplot(kev.work)+ geom_boxplot(aes(x=Site,y= LnB,fill = Type))
#R.S
ggplot(kev.work)+ geom_boxplot(aes(x=Site,y= R.S,fill = Type))
#LnSR
ggplot(kev.work)+ geom_boxplot(aes(x=Site,y= LnSR,fill = Type))
#Tr
ggplot(kev.work)+ geom_boxplot(aes(x=Site,y= Tr,fill = Type))
#LVOS
ggplot(kev.work)+ geom_boxplot(aes(x=Site,y= LVOS,fill = Type))
#VOL
ggplot(kev.work)+ geom_boxplot(aes(x=Site,y= Vol,fill = Type))








