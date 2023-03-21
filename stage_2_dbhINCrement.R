library(dplyr)
library(readxl)
library(robustbase)
library(rlang)
library(ggplot2)
library(tidyr)
library(reshape2)
library(data.table)
#####################################################################
#AFI
dataset_AFI_raw<-read_excel("D:/FERS Project/STVI/DATA/EIB Research Data Databasa final.xlsx",sheet = "AFI",skip = 3)
EIB_Site_Specie_YC_DevStage_to_preporc(dataset_AFI_raw)
# calculating dbh increment
# Stourhead = 67 , "204 Dropping Gutter"
Stourhead<-read_excel("D:/FERS Project/STVI/DATA/67_remesure_MASTER (1).xlsx",sheet = "Arbres")
Stourhead_AFI<-AFI_Version2(Stourhead,2006,dataset_AFI_raw,"204 Dropping Gutter")


# monivea = 102 , "Monivea"
Monivea<-read_excel("D:/FERS Project/STVI/DATA/102_remesure.xlsx",sheet = "Trees")
Monivea_AFI<-AFI_Version3(Monivea,2011,dataset_AFI_raw,"Monivea")
#Monivea_AFI<-AFI_Version3(Monivea,Date_measured,dataset_AFI,"Monivea")

Monivea_AFI_pre<-Preprocessing_FERS(Monivea_AFI)
View(Monivea_AFI_pre)
# Mellory = 103 , "Mellory"
Mellory<-read_excel("D:/FERS Project/STVI/DATA/103_remesure.xlsx",sheet = "Trees")

Mellory_AFI<-AFI_Version3(Mellory,2011,dataset_AFI_raw,"Mellory")
Mellory_AFI_pre<-Preprocessing_FERS(Mellory_AFI)

# Knockrath = 106 , "19/20 Knockrath"
Knockrath<-read_excel("D:/FERS Project/STVI/DATA/106_remesure.xlsx",sheet = "Trees")
Knockrath_AFI<-AFI_Version3(Knockrath,2011,dataset_AFI_raw,"19/20 Knockrath")
Knockrath_AFI_pre<-Preprocessing_FERS(Knockrath_AFI)

# Rushmore = 120, "J10/15 Chase Woods"
Rushmore<-read_excel("D:/FERS Project/STVI/DATA/120-Chasse Woods - Rushmore Estate.xlsx",sheet = "Arbres")
Rushmore_AFI<-AFI_Version2(Rushmore,2012,dataset_AFI_raw,"J10/15 Chase Woods")
Rushmore_AFI_pre<-Preprocessing_FERS(Rushmore_AFI)

# Berth_Ddu = 123 ,      "2/3 Berth Ddu"
Berth_Ddu<-read_excel("D:/FERS Project/STVI/DATA/123_remesure_MASTER.xlsx",sheet = "Trees")
Berth_Ddu_AFI<-AFI_Version3(Berth_Ddu,2015,dataset_AFI_raw,"2/3 Berth Ddu")
Berth_Ddu_AFI_pre<-Preprocessing_FERS_new(Berth_Ddu_AFI)

# Baronscourt 
Baronscourt <-read_excel("D:/FERS Project/STVI/DATA/Baronscourtv2PRM.xlsx",sheet = "Trees")
Baronscourt_YC<-read_excel("D:/FERS Project/STVI/DATA/Baronscourt Sample Plot Yield Class estimate.xlsx",skip = 4)

colnames(Baronscourt)[match("ForestNum",names(Baronscourt))]<- "StandNum"
Baronscourt$TopHeight<-as.numeric(Baronscourt$Height)
Baronscourt$CrownHeight <-NA
Baronscourt$Height<-as.numeric(Baronscourt$Height)
colnames(Baronscourt)[match("Dist",names(Baronscourt))]<- "Distance"
#EIB_data[16,]<-c("Barenscourt (NI)","Barenscourt","SS",NA,NA,NA,NA)
Baronscourt_AFI<-AFI_Version3(Baronscourt,2021,dataset_AFI,"Barenscourt")
Baronscourt_AFI$Site_code<-"Baronscourt"
Baronscourt_AFI<-Baronscourt_AFI[!is.na(Baronscourt_AFI$cohort_name),]
Baronscourt_AFI<-Baronscourt_AFI[!is.na(Baronscourt_AFI$idplots),]
# yield class
colnames(Baronscourt)

  for (i in unique(Baronscourt_AFI$idplots)){
    if(!is_empty( Baronscourt_AFI[Baronscourt_AFI$idplots==i & Baronscourt_AFI$cohort_name=="Spruce",]$YC)){
         Baronscourt_AFI[Baronscourt_AFI$idplots==i & Baronscourt_AFI$cohort_name=="Spruce",]$YC<-Baronscourt_YC[Baronscourt_YC$PLOT==i,]$YC...8
         
    }
    if(!is_empty( Baronscourt_AFI[Baronscourt_AFI$idplots==i & Baronscourt_AFI$species=="Norway spruce",]$YC)){
    Baronscourt_AFI[Baronscourt_AFI$idplots==i & Baronscourt_AFI$species=="Norway spruce",]$YC<-Baronscourt_YC[Baronscourt_YC$PLOT==i,]$YC...11
    }
  }

View(Baronscourt_AFI)
Baronscourt_AFI[is.na(Baronscourt_AFI$X),]
Baronscourt_AFI[Baronscourt_AFI$Tree_ID==5 & Baronscourt_AFI$idplots==3,]$X<-Baronscourt_AFI[Baronscourt_AFI$Tree_ID==5 & Baronscourt_AFI$idplots==3 & !is.na(Baronscourt_AFI$X) ,]$X
Baronscourt_AFI[Baronscourt_AFI$Tree_ID==5 & Baronscourt_AFI$idplots==3,]$Y<-Baronscourt_AFI[Baronscourt_AFI$Tree_ID==5 & Baronscourt_AFI$idplots==3 & !is.na(Baronscourt_AFI$Y) ,]$X



 Baronscourt_AFI_pre<-Preprocessing_FERS_new(Baronscourt_AFI)

NCOL(Stourhead_AFI)
AFI_Data<-rbind(Berth_Ddu_AFI,Rushmore_AFI,Knockrath_AFI,Mellory_AFI,Monivea_AFI,Stourhead_AFI)
table(AFI_Data$Site_code)
AFI_Data$Data_Type<- "AFI"
#ISN
#####################################################################
#ISN
ISN_Limit_Fix<- function(AFI_dataset){
  
AFI_dataset$Limit<-0
AFI_dataset$Distance<-as.numeric(sub("_.*", "", AFI_dataset$Tree_ID))
if(!is_empty( AFI_dataset[(AFI_dataset$dbhcm)>30 &
                          AFI_dataset$Distance*0.03*100>(AFI_dataset$dbhcm) & 
                          !is.na(AFI_dataset$dbhcm),]$Limit)){
  AFI_dataset[(AFI_dataset$dbhcm)>=30 & AFI_dataset$Distance*0.03*100>AFI_dataset$dbhcm & !is.na(AFI_dataset$dbhcm),]$Limit<-1
}
if(!is_empty(AFI_dataset[(AFI_dataset$dbhcm)<30 & 
                         AFI_dataset$Distance>10 & !is.na(AFI_dataset$dbhcm),]$Limit)){
  AFI_dataset[(AFI_dataset$dbhcm)<30 & AFI_dataset$Distance>10 &
                !is.na(AFI_dataset$dbhcm),]$Limit<-1
}
return(AFI_dataset)
}


# Cranborne 208
Cranborne_208_1<-read_excel("D:/FERS Project/STVI/DATA/ISN Cranborne 208 213a 1 1.xlsm",sheet = "Trees")
Cranborne_208_2<-read_excel("D:/FERS Project/STVI/DATA/ISN Cranborne 208 213a 2 PSC19.xlsm",sheet = "Trees")
Cranborne_208_3<-read_excel("D:/FERS Project/STVI/DATA/ISN Cranborne 208 213a 3 PSC19.xlsm",sheet = "Trees")
#View(Cranborne_208_1)
Cranborne_208_1_isn<-ISN_dataset_Pre(Cranborne_208_1,"2012/13",dataset_from_xlsx,"208, 213a Boulsbury")
Cranborne_208_1_isn$Cycle<-1
Cranborne_208_2_isn<-ISN_dataset_Pre(Cranborne_208_2,"2017/18",dataset_from_xlsx,"208, 213a Boulsbury")
Cranborne_208_2_isn$Cycle<-2
Cranborne_208_3_isn<-ISN_dataset_Pre(Cranborne_208_3,"2021/22",dataset_from_xlsx,"208, 213a Boulsbury")
Cranborne_208_3_isn$Cycle<-3
Cranborne_208_isn<-rbind(Cranborne_208_1_isn,Cranborne_208_2_isn,Cranborne_208_3_isn)
Cranborne_208_isn<-ISN_Limit_Fix(Cranborne_208_isn)
Cranborne_208_isn$Limit
# Cranborne  37c
Cranborne_37c_1<-read_excel("D:/FERS Project/STVI/DATA/ISN Cranborne 37c 1 PSC19.xls",sheet = "Trees")
Cranborne_37c_2<-read_excel("D:/FERS Project/STVI/DATA/ISN Cranborne 37c 2 PSC 19.xlsm",sheet = "Trees")
#View(read_excel("D:/FERS Project/STVI/DATA/ISN Cranborne 37c 1 PSC19.xls",sheet = "Trees"))
Cranborne_37c_1_isn<-ISN_dataset_Pre(Cranborne_37c_1,"2012/13",dataset_from_xlsx,"37c Daggons")
Cranborne_37c_1_isn$Cycle<-1
Cranborne_37c_2_isn<-ISN_dataset_Pre(Cranborne_37c_2,"2017/18",dataset_from_xlsx,"37c Daggons")
Cranborne_37c_2_isn$Cycle<-2
Cranborne_37c_isn<-rbind(Cranborne_37c_1_isn,Cranborne_37c_2_isn)
Cranborne_37c_isn<-ISN_Limit_Fix(Cranborne_37c_isn)

#Rushmore L3
Rushmore_l3_1<-read_excel("D:/FERS Project/STVI/DATA/ISN Rushmore L3 1 PSC 19.xlsm",sheet = "Trees")
Rushmore_l3_2<-read_excel("D:/FERS Project/STVI/DATA/ISN Rushmore L3 2 PSC 19 (1).xlsm",sheet = "Trees")

Rushmore_l3_1_isn<-ISN_dataset_Pre(Rushmore_l3_1,"2017/18",dataset_from_xlsx,"L3 Chase Wood")
Rushmore_l3_1_isn$Cycle<-1
Rushmore_l3_2_isn<-ISN_dataset_Pre(Rushmore_l3_2,"2021/22",dataset_from_xlsx,"L3 Chase Wood")
Rushmore_l3_2_isn$Cycle<-2
Rushmore_l3_isn<-rbind(Rushmore_l3_1_isn,Rushmore_l3_2_isn)
Rushmore_l3_isn<-ISN_Limit_Fix(Rushmore_l3_isn)
#Rushmore M9
Rushmore_m9_1<-read_excel("D:/FERS Project/STVI/DATA/ISN Rushmore M9,10 M11 M13,15  1 PSC19 (1).xlsm",sheet = "Trees")
Rushmore_m9_2<-read_excel("D:/FERS Project/STVI/DATA/ISN Rushmore M9,10, M11, M13,15  2 PSC 19 (2).xlsm",sheet = "Trees")

Rushmore_m9_1_isn<-ISN_dataset_Pre(Rushmore_m9_1,"2017/18",dataset_from_xlsx,"M9/10, M11, M13/15 Chase Wood")
Rushmore_m9_1_isn$Cycle<-1
Rushmore_m9_2_isn<-ISN_dataset_Pre(Rushmore_m9_2,"2021/22",dataset_from_xlsx,"M9/10, M11, M13/15 Chase Wood")
Rushmore_m9_2_isn$Cycle<-2
Rushmore_m9_isn<-rbind(Rushmore_m9_1_isn,Rushmore_m9_2_isn)
Rushmore_m9_isn<-ISN_Limit_Fix(Rushmore_m9_isn)

# Rusmore b2
Rushmore_b2_1<-read_excel("D:/FERS Project/STVI/DATA/ISN Rushmore B 2 3  1 PSC 19.xlsm",sheet = "Trees")
Rushmore_b2_2<-read_excel("D:/FERS Project/STVI/DATA/ISN Rushmore B 2 3  2 PSC 19.xlsm",sheet = "Trees")

Rushmore_b2_1_isn<-ISN_dataset_Pre(Rushmore_b2_1,"2017/18",dataset_from_xlsx,"B2/3 Farnham Wood")
Rushmore_b2_1_isn$Cycle<-1
Rushmore_b2_2_isn<-ISN_dataset_Pre(Rushmore_b2_2,"2021/22",dataset_from_xlsx,"B2/3 Farnham Wood")
Rushmore_b2_2_isn$Cycle<-2
Rushmore_b2_isn<-rbind(Rushmore_b2_1_isn,Rushmore_b2_2_isn)
Rushmore_b2_isn<-ISN_Limit_Fix(Rushmore_b2_isn)

# Crichel down
Crichel_down_1<-read_excel("D:/FERS Project/STVI/DATA/ISN Crichel 14b 1 PSC 19.xlsm",sheet = "Trees")
Crichel_down_2<-read_excel("D:/FERS Project/STVI/DATA/ISN Crichel 14b 2 PSC19.xlsm",sheet = "Trees")
#View(Crichel_down_isn)
Crichel_down_1_isn<-ISN_dataset_Pre(Crichel_down_1,"2018/19",dataset_from_xlsx,"14b Chetterwood")
Crichel_down_1_isn$Cycle<-1
Crichel_down_2_isn<-ISN_dataset_Pre(Crichel_down_2,"2021/22",dataset_from_xlsx,"14b Chetterwood")
Crichel_down_2_isn$Cycle<-2
Crichel_down_isn<-rbind(Crichel_down_1_isn,Crichel_down_2_isn)
Crichel_down_isn<-ISN_Limit_Fix(Crichel_down_isn)

# llchester 47
llchester_47<-read_excel("D:/FERS Project/STVI/DATA/ISN 2nd measure Llethir Gwinau input data.xlsx",sheet = "Trees")
llchester_47<-as.data.frame(llchester_47)
colnames(llchester_47)[5]<-"Bering"
llchester_47<-llchester_47[!is.na(llchester_47$Diam1),]
llchester_47$Diam<-llchester_47$Diam1
llchester_47[!is.na(llchester_47$Diam2),]$Diam<-(llchester_47[!is.na(llchester_47$Diam2),]$Diam1+llchester_47[!is.na(llchester_47$Diam2),]$Diam2)/2

llchester_47$`N/ha`<-NA
llchester_47[llchester_47$Diam<=30,]$`N/ha`<-10000/pi/100
llchester_47[llchester_47$Diam>30,]$`N/ha` <-100000000*0.03^2/pi/llchester_47[llchester_47$Diam>30,]$Diam^2

llchester_47$Limit<-0
llchester_47[llchester_47$Diam>30 & llchester_47$Distance*0.03*100>llchester_47$Diam,]$Limit<-1
llchester_47$SpeciesGroup<-"Spruce"

llchester_47_1<-llchester_47[llchester_47$Cycle==1,]

llchester_47_2<-llchester_47[llchester_47$Cycle==2,]
llchester_47_1_isn<-ISN_dataset_Pre(llchester_47_1,"2017/18",dataset_from_xlsx,"47/48/55 Llethr Gwinau")
llchester_47_1_isn$Cycle<-1
llchester_47_2_isn<-ISN_dataset_Pre(llchester_47_2,"2021/22",dataset_from_xlsx,"47/48/55 Llethr Gwinau")
llchester_47_2_isn$Cycle<-2
llchester_47_isn<-rbind(llchester_47_1_isn,llchester_47_2_isn)
llchester_47_isn<-ISN_Limit_Fix(llchester_47_isn)

#####################################################################
####################
# ## ISN LRS 
####################
####################

table(Bryn_Arau_Duon$Species)

ISN_LRS_YC_bry<- read_excel("D:/FERS Project/STVI/DATA/ISN Wales YC & Dev Stage Data.xlsx",sheet = "Bryn Arau Duon INV")
#View(ISN_LRS_YC_bry)

Bryn_Arau_Duon<-read_excel("D:/FERS Project/STVI/DATA/Bryn_Arau_Duon_inventaires input data.xlsx",sheet = "Trees")
Bryn_Arau_Duon$PlotNum<-as.numeric(Bryn_Arau_Duon$PlotNum)
ISN_LRS<-Bryn_Arau_Duon
#View(Bryn_Arau_Duon)
ISN_LRS_initial<-data.frame(Date_measured=NA,species=ISN_LRS$Species,
                                Site_code=ISN_LRS$ForestNum,idplots=ISN_LRS$PlotNum,
                            Tree_ID=ISN_LRS$TreeNum,X=ISN_LRS$Dist*cos(ISN_LRS$Bearing),
                            Y=ISN_LRS$Dist*sin(ISN_LRS$Bearing),Plot.size..ha.=NA,
                                cohort_name=NA,
                            cr=NA,dbh1=ISN_LRS$Diam1,dbh2=ISN_LRS$Diam2,
                            dbhcm=NA,
                                expansionfactor=NA,Height_m=ISN_LRS$Height,YC=NA,Development.stage= NA,Limit=NA,
                            Cycle=ISN_LRS$Cycle,Distance = ISN_LRS$Dist,Harvest=ISN_LRS$Harvest)

  ISN_LRS_initial$Plot.size..ha.<-1
  ISN_LRS_initial$dbhcm<-ISN_LRS_initial$dbh1
  ISN_LRS_initial[!is.na(ISN_LRS_initial$dbh2),"dbhcm"]<-(ISN_LRS_initial[!is.na(ISN_LRS_initial$dbh2),"dbh1"]+
                                                       ISN_LRS_initial[!is.na(ISN_LRS_initial$dbh2),"dbh2"])/2
  ISN_LRS_initial<-subset(ISN_LRS_initial,select=-c(dbh1,dbh2))
  ISN_LRS_initial$Limit<-0
  if(!is_empty( ISN_LRS_initial[ISN_LRS_initial$dbhcm>30 &
                                ISN_LRS_initial$Distance*0.03*100>ISN_LRS_initial$dbhcm & 
                            !is.na(ISN_LRS_initial$dbhcm),]$Limit)){
    ISN_LRS_initial[ISN_LRS_initial$dbhcm>=30 & ISN_LRS_initial$Distance*0.03*100>ISN_LRS_initial$dbhcm & 
                      !is.na(ISN_LRS_initial$dbhcm),]$Limit<-1
  }
  if(!is_empty(ISN_LRS_initial[ISN_LRS_initial$dbhcm<30 & 
                               ISN_LRS_initial$Distance>10 & !is.na(ISN_LRS_initial$dbhcm),]$Limit)){
    ISN_LRS_initial[ISN_LRS_initial$dbhcm<30 & ISN_LRS_initial$Distance>10 &
                  !is.na(ISN_LRS_initial$dbhcm),]$Limit<-1
  }
  
cohort_fun<-  function(Arbres){
  if(!is_empty(Arbres[Arbres$species=="Douglas",]$cohort_name)){
    Arbres[Arbres$species=="Douglas",]$cohort_name<-"Douglas fir"}
  if(!is_empty(Arbres[Arbres$species=="Pacific silver fir",]$cohort_name)){
    Arbres[Arbres$species=="Pacific silver fir",]$cohort_name <- "Other conifers"}
  if(!is_empty(Arbres[Arbres$species=="Japanese larch",]$cohort_name)){
    Arbres[Arbres$species=="Japanese larch",]$cohort_name <- "Larch"}
  if(!is_empty(Arbres[Arbres$species=="Douglas fir",]$cohort_name)){
    Arbres[Arbres$species=="Douglas fir",]$cohort_name<-"Douglas fir"}
  if(!is_empty(Arbres[Arbres$species=="Elder",]$cohort_name)){
    Arbres[Arbres$species=="Elder",]$cohort_name<-"Slow-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$species=="Ash",]$cohort_name)){
    Arbres[Arbres$species=="Ash",]$cohort_name<-"Fast-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$species=="Birch",]$cohort_name)){
    Arbres[Arbres$species=="Birch",]$cohort_name<-"Fast-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$species=="Rowan",]$cohort_name)){
    Arbres[Arbres$species=="Rowan",]$cohort_name<-"Fast-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$species=="Sycamore",]$cohort_name)){
    Arbres[Arbres$species=="Sycamore",]$cohort_name<-"Fast-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$species=="Willow",]$cohort_name)){
    Arbres[Arbres$species=="Willow",]$cohort_name<-"Fast-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$species=="Scots pine",]$cohort_name)){
    Arbres[Arbres$species=="Scots pine",]$cohort_name<-"Pine"}
  if(!is_empty(Arbres[Arbres$species=="Lodgepole pine",]$cohort_name)){
    Arbres[Arbres$species=="Lodgepole pine",]$cohort_name<-"Pine"}
  if(!is_empty(Arbres[Arbres$species=="Birch",]$cohort_name)){
    Arbres[Arbres$species=="Birch",]$cohort_name<-"Fast-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$species=="Lawson cypress",]$cohort_name)){
    Arbres[Arbres$species=="Lawson cypress",]$cohort_name<-"Other conifers"}
  if(!is_empty(Arbres[Arbres$species=="Norway spruce",]$cohort_name)){
    Arbres[Arbres$species=="Norway spruce",]$cohort_name<-"Spruce"}
  if(!is_empty(Arbres[Arbres$species=="Sitka spruce",]$cohort_name)){
    Arbres[Arbres$species=="Sitka spruce",]$cohort_name<-"Spruce"}
  if(!is_empty(Arbres[Arbres$species=="Sitka",]$cohort_name)){
    Arbres[Arbres$species=="Sitka",]$cohort_name<-"Spruce"}
  if(!is_empty(Arbres[Arbres$species=="Oak",]$cohort_name)){
    Arbres[Arbres$species=="Oak",]$cohort_name<-"Slow-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$species=="Western red cedar",]$cohort_name)){
    Arbres[Arbres$species=="Western red cedar",]$cohort_name<-"Other conifers"}
  if(!is_empty(Arbres[Arbres$species=="Grey willow",]$cohort_name)){
      Arbres[Arbres$species=="Grey willow",]$cohort_name<-"Fast-growing broadleaves"} 
  if(!is_empty(Arbres[Arbres$species=="Common alder",]$cohort_name)){
        Arbres[Arbres$species=="Common alder",]$cohort_name<-"Fast-growing broadleaves"}  
  if(!is_empty(Arbres[Arbres$species=="Hybrid larch",]$cohort_name)){
          Arbres[Arbres$species=="Hybrid larch",]$cohort_name<-"Larch"}  
  if(!is_empty(Arbres[Arbres$species=="Hawthorn",]$cohort_name)){
            Arbres[Arbres$species=="Hawthorn",]$cohort_name<-"Slow-growing broadleaves"} 
  if(!is_empty(Arbres[Arbres$species=="Goat willow",]$cohort_name)){
              Arbres[Arbres$species=="Goat willow",]$cohort_name<-"Fast-growing broadleaves"}
    return(Arbres)
  }
ISN_LRS_initial<-  cohort_fun(ISN_LRS_initial)
# expansion factor
ISN_LRS_initial[ISN_LRS_initial$dbhcm<=30 & !is.na(ISN_LRS_initial$dbhcm),]$expansionfactor<-10000/pi/100

ISN_LRS_initial[ISN_LRS_initial$dbhcm>30 & !is.na(ISN_LRS_initial$dbhcm),]$expansionfactor<-100000000*0.03^2/pi/
                  (ISN_LRS_initial[ISN_LRS_initial$dbhcm>30 & !is.na(ISN_LRS_initial$dbhcm),]$dbhcm^2)
# trees <27.5cm will have exp_fact of area of plot/ pi*100
# dev stage
#ISN_LRS_initial$Development.stage<-ISN_LRS_YC_bry$`Dev Stage`
for(i in unique(ISN_LRS_initial$idplots)){
  ISN_LRS_initial[ISN_LRS_initial$idplots==i,]$Development.stage<- ISN_LRS_YC_bry[ISN_LRS_YC_bry$`SP Nb.`==i,]$`Dev Stage`
  
    if(!is_empty( ISN_LRS_YC_bry[ISN_LRS_YC_bry$`SP Nb.`==i & ISN_LRS_YC_bry$Species=="SS" ,]$YC) & 
       !is_empty(ISN_LRS_initial[ISN_LRS_initial$idplots==i & ISN_LRS_initial$species=="Sitka spruce",]$YC)){
    ISN_LRS_initial[ISN_LRS_initial$idplots==i & ISN_LRS_initial$species=="Sitka spruce",]$YC<- 
                                  ISN_LRS_YC_bry[ISN_LRS_YC_bry$`SP Nb.`==i & ISN_LRS_YC_bry$Species=="SS" ,]$YC
    }
  
}


for(i in unique(ISN_LRS_initial$idplots)){
  ISN_LRS_initial[ISN_LRS_initial$idplots==i,]$Development.stage<- ISN_LRS_YC_bry[ISN_LRS_YC_bry$`SP Nb.`==i,]$`Dev Stage`
  
    if(!is_empty( ISN_LRS_YC_bry[ISN_LRS_YC_bry$`SP Nb.`==i & ISN_LRS_YC_bry$Species=="SS" ,]$YC) & 
       !is_empty(ISN_LRS_initial[ISN_LRS_initial$idplots==i & ISN_LRS_initial$species %like% "spruce",]$YC)){
      ISN_LRS_initial[ISN_LRS_initial$idplots==i & ISN_LRS_initial$species %like% "spruce",]$YC<- 
        ISN_LRS_YC_bry[ISN_LRS_YC_bry$`SP Nb.`==i & ISN_LRS_YC_bry$Species=="SS" ,]$YC
    
  }
}


ISN_LRS_initial$TreeName<- paste(ISN_LRS_initial$idplots,ISN_LRS_initial$Tree_ID)
ISN_LRS_initial<-ISN_LRS_initial[ISN_LRS_initial$TreeName !="NA NA",]
for (i in unique(ISN_LRS_initial$TreeName)){
  ISN_LRS_initial[ISN_LRS_initial$TreeName==i & !is.na(ISN_LRS_initial$TreeName),]$X<-ISN_LRS_initial[ISN_LRS_initial$TreeName==i  &
                                                                                                        !is.na(ISN_LRS_initial$TreeName)& !is.na(ISN_LRS_initial$X),]$X
  ISN_LRS_initial[ISN_LRS_initial$TreeName==i & !is.na(ISN_LRS_initial$TreeName),]$Y<-ISN_LRS_initial[ISN_LRS_initial$TreeName==i  &
                                                                                                        !is.na(ISN_LRS_initial$TreeName)& !is.na(ISN_LRS_initial$Y),]$Y
}

#View(ISN_LRS_initial)
#View(Bryn_Arau_Duon_isn)
ISN_LRS_initial[ISN_LRS_initial$Cycle==1,]$Date_measured<-2009
ISN_LRS_initial[ISN_LRS_initial$Cycle==2,]$Date_measured<-2017

ISN_LRS_YC_bry[ISN_LRS_YC_bry$Species=="SS",]$Species<-"Sitka spruce"
ISN_LRS_YC_bry[ISN_LRS_YC_bry$Species=="LP",]$Species<-"Lodgepole pine"

#
ISN_LRS_initial$Site_code<-"Bryn Arau Duon INV"
Bryn_Arau_Duon_isn<-ISN_LRS_initial
table(Bryn_Arau_Duon_isn$species)
unique(Bryn_Arau_Duon_isn[is.na(Bryn_Arau_Duon_isn$YC) & Bryn_Arau_Duon_isn$cohort_name=="Spruce",]$idplots)

Preprocessing_FERS(Bryn_Arau_Duon_isn)
######## ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## 
######## Llethr Gwinau
#

ISN_LRS_YC_Llethr<- read_excel("D:/FERS Project/STVI/DATA/ISN Wales YC & Dev Stage Data.xlsx",sheet = "ISN Llethr Gwinau",skip = 2)
#View(ISN_LRS_YC_Llethr)

Llethir<-read_excel("D:/FERS Project/STVI/DATA/ISN 2nd measure Llethir Gwinau input data.xlsx",sheet = "Trees")
Llethir1<-read_excel("D:/FERS Project/STVI/DATA/ISN 2nd measure Llethir Gwinau input data (1).xlsx",sheet = "Trees")
#View(Llethir)
harvest_Llethir<- sum(Llethir$Harvest,na.rm=T)
ISN_LRS<-Llethir

ISN_LRS_initial<-data.frame(Date_measured=NA,species=ISN_LRS$Species,
                            Site_code=ISN_LRS$StandNo,idplots=ISN_LRS$PlotNo,
                            Tree_ID=ISN_LRS$TreeNo,X=ISN_LRS$Distance*cos(ISN_LRS$Bearing),
                            Y=ISN_LRS$Distance*sin(ISN_LRS$Bearing),Plot.size..ha.=NA,
                            cohort_name=NA,
                            cr=NA,dbh1=ISN_LRS$Diam1,dbh2=ISN_LRS$Diam2,
                            dbhcm=NA,
                            expansionfactor=NA,Height_m=NA,YC=NA,Development.stage= NA,Limit=NA,
                            Cycle=ISN_LRS$Cycle,Distance = ISN_LRS$Distance,Harvest=ISN_LRS$Harvest)

ISN_LRS_initial$Plot.size..ha.<-1
ISN_LRS_initial$dbhcm<-ISN_LRS_initial$dbh1
ISN_LRS_initial[!is.na(ISN_LRS_initial$dbh2),"dbhcm"]<-(ISN_LRS_initial[!is.na(ISN_LRS_initial$dbh2),"dbh1"]+
                                                          ISN_LRS_initial[!is.na(ISN_LRS_initial$dbh2),"dbh2"])/2
ISN_LRS_initial<-subset(ISN_LRS_initial,select=-c(dbh1,dbh2))
ISN_LRS_initial$Limit<-0
if(!is_empty( ISN_LRS_initial[ISN_LRS_initial$dbhcm>30 &
                              ISN_LRS_initial$Distance*0.03*100>ISN_LRS_initial$dbhcm & 
                              !is.na(ISN_LRS_initial$dbhcm),]$Limit)){
  ISN_LRS_initial[ISN_LRS_initial$dbhcm>=30 & ISN_LRS_initial$Distance*0.03*100>ISN_LRS_initial$dbhcm & 
                    !is.na(ISN_LRS_initial$dbhcm),]$Limit<-1
}
if(!is_empty(ISN_LRS_initial[ISN_LRS_initial$dbhcm<30 & 
                             ISN_LRS_initial$Distance>10 & !is.na(ISN_LRS_initial$dbhcm),]$Limit)){
  ISN_LRS_initial[ISN_LRS_initial$dbhcm<30 & ISN_LRS_initial$Distance>10 &
                    !is.na(ISN_LRS_initial$dbhcm),]$Limit<-1
}

ISN_LRS_initial[ISN_LRS_initial$species=="SS" & !is.na(ISN_LRS_initial$species),]$cohort_name<-"Spruce"
# expansion factor
ISN_LRS_initial[ISN_LRS_initial$dbhcm<=30 & !is.na(ISN_LRS_initial$dbhcm),]$expansionfactor<-10000/pi/100

ISN_LRS_initial[ISN_LRS_initial$dbhcm>30 & !is.na(ISN_LRS_initial$dbhcm),]$expansionfactor<-100000000*0.03^2/pi/
  (ISN_LRS_initial[ISN_LRS_initial$dbhcm>30 & !is.na(ISN_LRS_initial$dbhcm),]$dbhcm^2)
# trees <27.5cm will have exp_fact of area of plot/ pi*100
# dev stage
#ISN_LRS_initial$Development.stage<-ISN_LRS_YC_bry$`Dev Stage`
for(i in unique(ISN_LRS_initial$idplots)){
 
  
    if(!is_empty( ISN_LRS_YC_Llethr[i ,]$YC) & 
       !is_empty(ISN_LRS_initial[ISN_LRS_initial$idplots==i & ISN_LRS_initial$species=="SS",]$YC)){
      ISN_LRS_initial[ISN_LRS_initial$idplots==i & ISN_LRS_initial$species=="SS" & !is.na(ISN_LRS_initial$species),]$YC<- 
        ISN_LRS_YC_Llethr[i,]$YC
    
  }
}

table(ISN_LRS_YC_bry$Species)

ISN_LRS_initial$TreeName<- paste(ISN_LRS_initial$idplots,ISN_LRS_initial$Tree_ID)
ISN_LRS_initial<-ISN_LRS_initial[ISN_LRS_initial$TreeName !="NA NA",]
for (i in unique(ISN_LRS_initial$TreeName)){
  ISN_LRS_initial[ISN_LRS_initial$TreeName==i & !is.na(ISN_LRS_initial$TreeName),]$X<-ISN_LRS_initial[ISN_LRS_initial$TreeName==i  &
                                                                                                        !is.na(ISN_LRS_initial$TreeName)& !is.na(ISN_LRS_initial$X),]$X
  ISN_LRS_initial[ISN_LRS_initial$TreeName==i & !is.na(ISN_LRS_initial$TreeName),]$Y<-ISN_LRS_initial[ISN_LRS_initial$TreeName==i  &
                                                                                                        !is.na(ISN_LRS_initial$TreeName)& !is.na(ISN_LRS_initial$Y),]$Y
  ISN_LRS_initial[ISN_LRS_initial$TreeName==i & !is.na(ISN_LRS_initial$TreeName),"species"]<-ISN_LRS_initial[ISN_LRS_initial$TreeName==i & !is.na(ISN_LRS_initial$TreeName) & 
                                                                                                               !is.na(ISN_LRS_initial$species),"species"]
  ISN_LRS_initial[ISN_LRS_initial$TreeName==i & !is.na(ISN_LRS_initial$TreeName),]$cohort_name<-ISN_LRS_initial[ISN_LRS_initial$TreeName==i & !is.na(ISN_LRS_initial$TreeName) & 
                                                                                                               !is.na(ISN_LRS_initial$species),]$cohort_name
}

View(ISN_LRS_initial)
ISN_LRS_initial[ISN_LRS_initial$Cycle==1,]$Date_measured<-2017
ISN_LRS_initial[ISN_LRS_initial$Cycle==2,]$Date_measured<-2021


ISN_LRS_initial$Development.stage<-"2a"
ISN_LRS_initial<-  cohort_fun(ISN_LRS_initial)
Llethir_Gwinau<-ISN_LRS_initial
View(Llethir_Gwinau)

##########################
#####
#### nant yr Eira ISN



ISN_LRS_YC_NYE<- read_excel("D:/FERS Project/STVI/DATA/ISN Wales YC & Dev Stage Data.xlsx",sheet = "ISN Nant yr Eira",skip = 2)
#View(ISN_LRS_YC_NYE)
ISN_LRS_YC_NYE$YC<-as.numeric(ISN_LRS_YC_NYE$YC)
Nant_yr_Eira<-read_excel("D:/FERS Project/STVI/DATA/ISN 2nd measure Nant yr Eira input data.xlsx",sheet = "Trees")
#View(Nant_yr_Eira)

ISN_LRS<-Nant_yr_Eira

ISN_LRS_initial<-data.frame(Date_measured=NA,species=ISN_LRS$Species,
                            Site_code=ISN_LRS$StandNo,idplots=ISN_LRS$PlotNo,
                            Tree_ID=ISN_LRS$TreeNo,X=ISN_LRS$Distance*cos(ISN_LRS$Bering),
                            Y=ISN_LRS$Distance*sin(ISN_LRS$Bering),Plot.size..ha.=NA,
                            cohort_name=NA,
                            cr=NA,dbh1=ISN_LRS$Diam1,dbh2=ISN_LRS$Diam2,
                            dbhcm=NA,
                            expansionfactor=NA,Height_m=NA,YC=NA,Development.stage= NA,Limit=NA,
                            Cycle=ISN_LRS$Cycle,Distance = ISN_LRS$Distance,Harvest=ISN_LRS$Harvest )

ISN_LRS_initial$Plot.size..ha.<-1
ISN_LRS_initial$dbhcm<-ISN_LRS_initial$dbh1
ISN_LRS_initial[!is.na(ISN_LRS_initial$dbh2),"dbhcm"]<-(ISN_LRS_initial[!is.na(ISN_LRS_initial$dbh2),"dbh1"]+
                                                          ISN_LRS_initial[!is.na(ISN_LRS_initial$dbh2),"dbh2"])/2
ISN_LRS_initial<-subset(ISN_LRS_initial,select=-c(dbh1,dbh2))
ISN_LRS_initial$Limit<-0
if(!is_empty( ISN_LRS_initial[ISN_LRS_initial$dbhcm>30 &
                              ISN_LRS_initial$Distance*0.03*100>ISN_LRS_initial$dbhcm & 
                              !is.na(ISN_LRS_initial$dbhcm),]$Limit)){
  ISN_LRS_initial[ISN_LRS_initial$dbhcm>=30 & ISN_LRS_initial$Distance*0.03*100>ISN_LRS_initial$dbhcm & 
                    !is.na(ISN_LRS_initial$dbhcm),]$Limit<-1
}
if(!is_empty(ISN_LRS_initial[ISN_LRS_initial$dbhcm<30 & 
                             ISN_LRS_initial$Distance>10 & !is.na(ISN_LRS_initial$dbhcm),]$Limit)){
  ISN_LRS_initial[ISN_LRS_initial$dbhcm<30 & ISN_LRS_initial$Distance>10 &
                    !is.na(ISN_LRS_initial$dbhcm),]$Limit<-1
}
#
ISN_LRS_initial[ISN_LRS_initial$species=="SS" & !is.na(ISN_LRS_initial$species),]$cohort_name<-"Spruce"
# expansion factor
ISN_LRS_initial[ISN_LRS_initial$dbhcm<=30 & !is.na(ISN_LRS_initial$dbhcm),]$expansionfactor<-10000/pi/100

ISN_LRS_initial[ISN_LRS_initial$dbhcm>30 & !is.na(ISN_LRS_initial$dbhcm),]$expansionfactor<-100000000*0.03^2/pi/
  (ISN_LRS_initial[ISN_LRS_initial$dbhcm>30 & !is.na(ISN_LRS_initial$dbhcm),]$dbhcm^2)
# trees <27.5cm will have exp_fact of area of plot/ pi*100
# dev stage
#ISN_LRS_initial$Development.stage<-ISN_LRS_YC_bry$`Dev Stage`
for(i in unique(ISN_LRS_initial[!is.na(ISN_LRS_initial$idplots),]$idplots)){

  
    if(!is_empty( ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$Plot==paste("SP",i,sep="")  & !is.na(ISN_LRS_YC_NYE$Plot),]$YC) & 
       !is_empty(ISN_LRS_initial[ISN_LRS_initial$idplots==i & ISN_LRS_initial$species=="SS",]$YC)){
      ISN_LRS_initial[ISN_LRS_initial$idplots==i & ISN_LRS_initial$species=="SS" & !is.na(ISN_LRS_initial$species),]$YC<- 
        ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$Plot==paste("SP",i,sep="")  & !is.na(ISN_LRS_YC_NYE$Plot),]$YC[1]
    }
  
}
table(ISN_LRS_YC_NYE$ID)
ISN_LRS_initial$Development.stage<-"2a"
#View(ISN_LRS_initial)
    ISN_LRS_initial$TreeName<- paste(ISN_LRS_initial$idplots,ISN_LRS_initial$Tree_ID)
    ISN_LRS_initial<-ISN_LRS_initial[ISN_LRS_initial$TreeName !="NA NA",]
     for (i in unique(ISN_LRS_initial$TreeName)){
    ISN_LRS_initial[ISN_LRS_initial$TreeName==i & !is.na(ISN_LRS_initial$TreeName),]$X<-ISN_LRS_initial[ISN_LRS_initial$TreeName==i  &
                                                                                                          !is.na(ISN_LRS_initial$TreeName)& !is.na(ISN_LRS_initial$X),]$X
    ISN_LRS_initial[ISN_LRS_initial$TreeName==i & !is.na(ISN_LRS_initial$TreeName),]$Y<-ISN_LRS_initial[ISN_LRS_initial$TreeName==i  &
                                                                                                          !is.na(ISN_LRS_initial$TreeName)& !is.na(ISN_LRS_initial$Y),]$Y
     }

#View(ISN_LRS_initial)
for(i in unique(ISN_LRS_initial$TreeName)){
  ISN_LRS_initial[ISN_LRS_initial$TreeName==i,]$cohort_name<-ISN_LRS_initial[ISN_LRS_initial$TreeName==i & !is.na(ISN_LRS_initial$cohort_name),]$cohort_name
  ISN_LRS_initial[ISN_LRS_initial$TreeName==i,]$species<-ISN_LRS_initial[ISN_LRS_initial$TreeName==i & !is.na(ISN_LRS_initial$species),]$species
}

ISN_LRS_initial[ISN_LRS_initial$Cycle==1,]$Date_measured<-2017
ISN_LRS_initial[ISN_LRS_initial$Cycle==2,]$Date_measured<-2021

ISN_LRS_initial<-  cohort_fun(ISN_LRS_initial)
View(ISN_LRS_initial)

Nant_yr_Eira_isn<-ISN_LRS_initial
View(Nant_yr_Eira_isn)
Nant_yr_Eira_isn[Nant_yr_Eira_isn$TreeName=="4 1",]$YC<-Nant_yr_Eira_isn[Nant_yr_Eira_isn$TreeName=="4 1",]$YC[1]
Nant_yr_Eira_isn[Nant_yr_Eira_isn$TreeName=="4 1",]$cohort_name<-Nant_yr_Eira_isn[Nant_yr_Eira_isn$TreeName=="4 1",]$cohort_name[1]
Nant_yr_Eira_isn[Nant_yr_Eira_isn$TreeName=="4 1",]$species<-Nant_yr_Eira_isn[Nant_yr_Eira_isn$TreeName=="4 1",]$species[1]

#####################################################################
Nant_yr_Eira_INV
#
Nant_yr_Eira_INV<-read_excel("D:/FERS Project/STVI/DATA/Nant_yr_Eira_inventaires input data.xlsx",sheet = "Trees")
ISN_LRS_YC_NYE<- read_excel("D:/FERS Project/STVI/DATA/ISN Wales YC & Dev Stage Data.xlsx",sheet = "Nant yr Eira INV")
#View(Nant_yr_Eira_INV)
ISN_LRS<-Nant_yr_Eira_INV

ISN_LRS_initial<-data.frame(Date_measured=NA,species=ISN_LRS$Species,
                            Site_code=ISN_LRS$ForestNum,idplots=ISN_LRS$PlotNum,
                            Tree_ID=ISN_LRS$TreeNum,X=ISN_LRS$Dist*cos(ISN_LRS$Bearing),
                            Y=ISN_LRS$Dist*sin(ISN_LRS$Bearing),Plot.size..ha.=NA,
                            cohort_name=NA,
                            cr=NA,dbh1=ISN_LRS$Diam1,dbh2=ISN_LRS$Diam2,
                            dbhcm=NA,
                            expansionfactor=NA,Height_m=NA,YC=NA,Development.stage= NA,Limit=NA,
                            Cycle=ISN_LRS$Cycle,Distance = ISN_LRS$Dist,Harvest=ISN_LRS$Harvest)

ISN_LRS_initial$Plot.size..ha.<-1
ISN_LRS_initial$dbhcm<-ISN_LRS_initial$dbh1
ISN_LRS_initial[!is.na(ISN_LRS_initial$dbh2),"dbhcm"]<-(ISN_LRS_initial[!is.na(ISN_LRS_initial$dbh2),"dbh1"]+
                                                          ISN_LRS_initial[!is.na(ISN_LRS_initial$dbh2),"dbh2"])/2
ISN_LRS_initial<-subset(ISN_LRS_initial,select=-c(dbh1,dbh2))
ISN_LRS_initial$Limit<-0
harvest<-ISN_LRS_initial[is.na(ISN_LRS_initial$dbhcm),]
ISN_LRS_initial<-ISN_LRS_initial[!is.na(ISN_LRS_initial$dbhcm),]
ISN_LRS_initial<-ISN_LRS_initial[!is.na(ISN_LRS_initial$Distance),]
if(!is_empty( ISN_LRS_initial[ISN_LRS_initial$dbhcm>30 &
                              ISN_LRS_initial$Distance*0.03*100>ISN_LRS_initial$dbhcm & 
                              !is.na(ISN_LRS_initial$dbhcm),]$Limit)){
  ISN_LRS_initial[ISN_LRS_initial$dbhcm>=30 & ISN_LRS_initial$Distance*0.03*100>ISN_LRS_initial$dbhcm & 
                    !is.na(ISN_LRS_initial$dbhcm),]$Limit<-1
}
if(!is_empty(ISN_LRS_initial[ISN_LRS_initial$dbhcm<30 & 
                             ISN_LRS_initial$Distance>10 & !is.na(ISN_LRS_initial$dbhcm),]$Limit)){
  ISN_LRS_initial[ISN_LRS_initial$dbhcm<30 & ISN_LRS_initial$Distance>10 &
                    !is.na(ISN_LRS_initial$dbhcm),]$Limit<-1 
}
#

# expansion factor
ISN_LRS_initial[ISN_LRS_initial$dbhcm<=30 & !is.na(ISN_LRS_initial$dbhcm),]$expansionfactor<-10000/pi/100

ISN_LRS_initial[ISN_LRS_initial$dbhcm>30 & !is.na(ISN_LRS_initial$dbhcm),]$expansionfactor<-100000000*0.03^2/pi/
  (ISN_LRS_initial[ISN_LRS_initial$dbhcm>30 & !is.na(ISN_LRS_initial$dbhcm),]$dbhcm^2)
# trees <27.5cm will have exp_fact of area of plot/ pi*100
# dev stage
#ISN_LRS_initial$Development.stage<-ISN_LRS_YC_bry$`Dev Stage`
unique(ISN_LRS_YC_NYE$Species)
ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$Species=="NS",]$Species<-"Norway spruce"
ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$Species=="SS",]$Species<-"Sitka spruce"
ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$Species=="SYC",]$Species<-"Sycamore"
ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$Species=="WRC",]$Species<-"Western red cedar"
#ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$Species=="CAR",]$Species<-"Sitka spruce"
ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$Species=="BI",]$Species<-"Birch"
#ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$Species=="SCI",]$Species<-"Sitka spruce"
ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$Species=="HL",]$Species<-"Hybrid larch"
ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$Species=="JL",]$Species<-"Japanese larch"
ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$Species=="DF",]$Species<-"Douglas fir"
ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$Species=="ROW",]$Species<-"Rowan"
ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$Species=="OAK",]$Species<-"Oak"
ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$Species=="GW",]$Species<-"Goat willow"

ISN_LRS_initial<-  cohort_fun(ISN_LRS_initial)
for(i in unique(ISN_LRS_initial$idplots)){
  for(n in unique(ISN_LRS_initial$species)){
  if(!is_empty(ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$`SP Nb.`==i & ISN_LRS_YC_NYE$Species==n,]$`Dev Stage`)){
  ISN_LRS_initial[ISN_LRS_initial$idplots==i,]$Development.stage<- ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$`SP Nb.`==i & ISN_LRS_YC_NYE$Species==n,]$`Dev Stage`[1]}
  
  
  if(!is_empty( ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$`SP Nb.`== i  & ISN_LRS_YC_NYE$Species==n,]$YC) & 
     !is_empty(ISN_LRS_initial[ISN_LRS_initial$idplots==i & ISN_LRS_initial$species==n,]$YC)){
    ISN_LRS_initial[ISN_LRS_initial$idplots==i & ISN_LRS_initial$species==n & !is.na(ISN_LRS_initial$species),]$YC<- 
     max(ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$`SP Nb.`== i & ISN_LRS_YC_NYE$Species==n,]$YC)
  }
    
  }
}
table(ISN_LRS_YC_NYE$Species)
#View(ISN_LRS_initial)
ISN_LRS_initial[ISN_LRS_initial$Cycle==1,]$Date_measured<-2012
ISN_LRS_initial[ISN_LRS_initial$Cycle==2,]$Date_measured<-2019


NROW(Nant_yr_Eira_INV_isn)
Nant_yr_Eira_INV_isn<-ISN_LRS_initial
Nant_yr_Eira_INV_isn[Nant_yr_Eira_INV_isn$idplots==19 & Nant_yr_Eira_INV_isn$cohort_name=="Spruce",]$YC<-Nant_yr_Eira_INV_isn[Nant_yr_Eira_INV_isn$idplots==19 & Nant_yr_Eira_INV_isn$cohort_name=="Spruce",]$YC[1]

#Nant_yr_Eira_INV_isn<-Nant_yr_Eira_INV_isn[!(Nant_yr_Eira_INV_isn$idplots %in% c(9, 16, 24, 26, 29, 35, 48, 49, 50, 51, 54, 70)),]
#plots to manually fix 
# 9 16 24 26 29 35 48 49 50 51 54 70
#####################################################################

# MArtilscope
#Stourhead_dropping_gutter
Stourhead_dg_1<- read_excel("D:/FERS Project/STVI/DATA/Marteloscope_Dropping Gutter South_Cpt 204_April 2022 measure_complete.xlsx",sheet="Aug 2007")
Stourhead_dg_2<- read_excel("D:/FERS Project/STVI/DATA/Marteloscope_Dropping Gutter South_Cpt 204_April 2022 measure_complete.xlsx",sheet="Feb 2012")
Stourhead_dg_3<- read_excel("D:/FERS Project/STVI/DATA/Marteloscope_Dropping Gutter South_Cpt 204_April 2022 measure_complete.xlsx",sheet="Aug 2017 full")
Stourhead_dg_4<- read_excel("D:/FERS Project/STVI/DATA/Marteloscope_Dropping Gutter South_Cpt 204_April 2022 measure_complete.xlsx",skip = 1,sheet="Field sheet April 2022")
Stourhead_dg_1<-Stourhead_dg_1[!is.na(Stourhead_dg_1$specie),]
Stourhead_dg_2<-Stourhead_dg_2[!is.na(Stourhead_dg_2$Specie),]
Stourhead_dg_3<-Stourhead_dg_3[!is.na(Stourhead_dg_3$Specie),]

colnames(Stourhead_dg_1)
colnames(Stourhead_dg_2)
colnames(Stourhead_dg_3)
colnames(Stourhead_dg_4)
colnames(Stourhead_dg_4) %in% colnames(Stourhead_dg_2)


colnames(Stourhead_dg_2)

colnames(Stourhead_dg_1)[1]<-"N? Tree"
colnames(Stourhead_dg_1)[2]<-"N? Ranging pool" 
colnames(Stourhead_dg_1)[3]<-"Bearing (deg)" 
colnames(Stourhead_dg_1)[4]<-"Bearing (gr)" 
colnames(Stourhead_dg_1)[5]<-"Distance" 
colnames(Stourhead_dg_1)[6]<-"Specie"
colnames(Stourhead_dg_1)[7]
colnames(Stourhead_dg_1)[8]




Stourhead_dg_1<-as.data.frame(Stourhead_dg_1)
marti<- function(Stourhead_dg_1){
  if(is_empty(Stourhead_dg_1$Specie)){
  Mart_dataset_initial<-data.frame(Date_measured=NA,species=Stourhead_dg_1$specie,
                                   Site_code=NA,
                                   idplots=Stourhead_dg_1$`nb ranging pool`,
                                   Tree_ID=Stourhead_dg_1$`nb tree` ,
                                   Distance = Stourhead_dg_1$distance,
                                   X=Stourhead_dg_1$distance*cos(Stourhead_dg_1$`bearing (gr)`),
                                   Y=Stourhead_dg_1$distance*sin(Stourhead_dg_1$`bearing (gr)`),
                                   Plot.size..ha.=1,
                                   cohort_name=NA,
                                   cr=NA,
                                   dbhcm=(Stourhead_dg_1[,7]+Stourhead_dg_1[,8])/2,
                                   expansionfactor=NA,
                                   Height_m=NA,
                                   YC=NA,
                                   Development.stage= "2b",
                                   Limit=NA,
                                   Cycle=1)
  }else{
  
Mart_dataset_initial<-data.frame(Date_measured=NA,species=Stourhead_dg_1$Specie,
                                Site_code=Stourhead_dg_1$`N? Ranging pool`,
                                idplots=Stourhead_dg_1$`N? Ranging pool`,
                                Tree_ID=Stourhead_dg_1$`N? Tree` ,
                                Distance = Stourhead_dg_1$Distance,
                                X=Stourhead_dg_1$Distance*cos(Stourhead_dg_1$`Bearing (gr)`),
                                Y=Stourhead_dg_1$Distance*sin(Stourhead_dg_1$`Bearing (gr)`),
                                Plot.size..ha.=1,
                                cohort_name=NA,
                                cr=NA,
                                dbhcm=(Stourhead_dg_1[,7]+Stourhead_dg_1[,8])/2,
                                expansionfactor=NA,
                                Height_m=NA,
                                YC=NA,
                                Development.stage= "2b",
                                Limit=NA,
                                Cycle=1)
}


Mart_dataset_initial$Limit<-0
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$dbhcm>30 &
                                  Mart_dataset_initial$Distance*0.03*100>Mart_dataset_initial$dbhcm & 
                                  !is.na(Mart_dataset_initial$dbhcm),]$Limit)){
  Mart_dataset_initial[Mart_dataset_initial$dbhcm>30 &
                         Mart_dataset_initial$Distance*0.03*100>Mart_dataset_initial$dbhcm &
                         !is.na(Mart_dataset_initial$dbhcm),]$Limit<-1
}

# expansion factor
Mart_dataset_initial[Mart_dataset_initial$dbhcm<=30 & !is.na(Mart_dataset_initial$dbhcm),]$expansionfactor<-10000/pi/100

Mart_dataset_initial[Mart_dataset_initial$dbhcm>30 & !is.na(Mart_dataset_initial$dbhcm),]$expansionfactor<-
  100000000*0.03^2/pi/(Mart_dataset_initial[Mart_dataset_initial$dbhcm>30 & !is.na(Mart_dataset_initial$dbhcm),]$dbhcm^2)
# trees <27.5cm will have exp_fact of area of plot/ pi*100




if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Douglas",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Douglas",]$cohort_name<-"Douglas fir"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Pacific silver fir",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Pacific silver fir",]$cohort_name <- "Other conifers"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Japanese larch",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Japanese larch",]$cohort_name <- "Larch"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Douglas fir",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Douglas fir",]$cohort_name<-"Douglas fir"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Elder",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Elder",]$cohort_name<-"Slow-growing broadleaves"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Ash",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Ash",]$cohort_name<-"Fast-growing broadleaves"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Birch",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Birch",]$cohort_name<-"Fast-growing broadleaves"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Willow",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Willow",]$cohort_name<-"Fast-growing broadleaves"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Beech",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Beech",]$cohort_name<-"Slow-growing broadleaves"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Sessile oak",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Sessile oak",]$cohort_name<-"Slow-growing broadleaves"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Birch",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Birch",]$cohort_name<-"Fast-growing broadleaves"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Lawson cypress",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Lawson cypress",]$cohort_name<-"Other conifers"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Norway spruce",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Norway spruce",]$cohort_name<-"Spruce"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Sitka spruce",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Sitka spruce",]$cohort_name<-"Spruce"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Sitka",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Sitka",]$cohort_name<-"Spruce"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Oak",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Oak",]$cohort_name<-"Slow-growing broadleaves"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Larch",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Larch",]$cohort_name <- "Larch"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Hemlock",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Hemlock",]$cohort_name <- "Other conifers"}
if(!is_empty(Mart_dataset_initial[Mart_dataset_initial$species=="Alder",]$cohort_name)){
  Mart_dataset_initial[Mart_dataset_initial$species=="Alder",]$cohort_name <- "Fast-growing broadleaves"}
return(Mart_dataset_initial)
}

Stourhead_dg_2_mart<-marti(Stourhead_dg_2)
Stourhead_dg_3_mart<-marti(Stourhead_dg_3)

Stourhead_dg_1_mart<-marti(Stourhead_dg_1)
Stourhead_dg_1_mart$Date_measured<-"2007"
Stourhead_dg_2_mart$Date_measured<-"2012"
Stourhead_dg_2_mart$Cycle<-2
Stourhead_dg_3_mart$Date_measured<-"2017"
Stourhead_dg_3_mart$Cycle<-3
rbind(Stourhead_dg_1_mart,Stourhead_dg_2_mart,Stourhead_dg_3_mart)
#####################################################################
# Stourhead
#great combe
Stourhead_GC_1<- read_excel("D:/FERS Project/STVI/DATA/Marteloscope_Gt Combe_Cpt 102e_April 2022 measure_complete.xlsx",sheet="Aug 2007 measure")
Stourhead_GC_2_3<- read_excel("D:/FERS Project/STVI/DATA/Marteloscope_Gt Combe_Cpt 102e_April 2022 measure_complete.xlsx",sheet="Aug 2013 & Aug 2018 measure")
Stourhead_GC_4<- read_excel("D:/FERS Project/STVI/DATA/Marteloscope_Gt Combe_Cpt 102e_April 2022 measure_complete.xlsx",sheet=" April 2022 measure")

# QC of data
plot(Stourhead_AFI$dbhcm,Stourhead_AFI$Height_m)
plot(Stourhead_AFI$dbhcm,Stourhead_AFI$cr)

adjbox(Stourhead_2cycles$dbhcm~Stourhead_cycle_1_pre[!is.na(Stourhead_cycle_1_pre$error),]$cohort_name)





Stourhead_AFI<-Stourhead_AFI[!is.na(Stourhead_AFI$dbhcm),]
Stourhead_AFI<-Stourhead_AFI[Stourhead_AFI$Limit==0,]
stourheadata<-list()
n<-1
for (i in unique(Stourhead_AFI$Cycle)){

  stourheadata[[n]]<-Preprocessing_FERS(Stourhead_AFI[Stourhead_AFI$Cycle==i,])
  n<-n+1
}

stourheadata[[1]]$Date_measured<-"16/17"
stourheadata[[2]]$Date_measured<-"21/22"

stourhead_data<-rbind(stourheadata[[1]],stourheadata[[2]])



Stourhead_AFI[Stourhead_AFI$Height_m<10 & !is.na(Stourhead_AFI$Height_m),]
Stourhead_AFI[Stourhead_AFI$Height_m==43 & !is.na(Stourhead_AFI$Height_m),]
Stourhead_AFI<-Stourhead_AFI[Stourhead_AFI$TREE_NAME!="9 5",]
Stourhead_AFI<-Stourhead_AFI[Stourhead_AFI$TREE_NAME!="3 24",]

Stourhead_AFI_2<- Stourhead_AFI[Stourhead_AFI$Limit==0,]

Stourhead_AFI_Harvest<-Stourhead_AFI %>%
  group_by(TREE_NAME)%>% 
  filter(n() == 1)

Stourhead_2cycles<-Stourhead_AFI %>%
  group_by(TREE_NAME)%>% 
  filter(n() > 1)
Stourhead_2cycles$DBH_INC<-NA
for( i in unique(Stourhead_2cycles$TREE_NAME)){
  Stourhead_2cycles[Stourhead_2cycles$TREE_NAME== i & Stourhead_2cycles$Cycle==4,]$DBH_INC<-  Stourhead_2cycles[Stourhead_2cycles$TREE_NAME== i & Stourhead_2cycles$Cycle==4,]$dbhcm- 
    Stourhead_2cycles[Stourhead_2cycles$TREE_NAME== i & Stourhead_2cycles$Cycle==3,]$dbhcm
}
# remove negative incrmenets
bad_rows<-Stourhead_2cycles[Stourhead_2cycles$DBH_INC<=0 & !is.na(Stourhead_2cycles$DBH_INC),]
Stourhead_2cycles_1<-Stourhead_2cycles[!is.na(Stourhead_2cycles$cr),]
Stourhead_2cycles<-Stourhead_2cycles[Stourhead_2cycles$DBH_INC>0 & !is.na(Stourhead_2cycles$DBH_INC),]



Stourhead_2cycles_22<-Stourhead_2cycles_1[!is.na(Stourhead_2cycles_1$DBH_INC) ,]
Stourhead_2cycles_22<-Stourhead_2cycles_22[Stourhead_2cycles_22$DBH_INC>=0 ,]


Stourhead_2cycles_21<-Stourhead_2cycles_1[is.na(Stourhead_2cycles_1$DBH_INC) ,]

#####
Stourhead_cycle_1<-Stourhead_AFI[Stourhead_AFI$Cycle==3,]
#remove rows with negative increment
Stourhead_cycle_1<-Stourhead_cycle_1[!(Stourhead_cycle_1$TREE_NAME %in% bad_rows$TREE_NAME), ]
Stourhead_cycle_1<-Stourhead_cycle_1[!is.na(Stourhead_cycle_1$Limit==0),]

####
Stourhead_cycle_1_pre<-Preprocessing_FERS(Stourhead_cycle_1)
Stourhead_cycle_1_pre$Initial_dbhcm<-Stourhead_cycle_1_pre$dbhcm
if(!is_empty(Stourhead_cycle_1_pre[is.na(Stourhead_cycle_1_pre$DBH_Increment),]$DBH_Increment)){
Stourhead_cycle_1_pre[is.na(Stourhead_cycle_1_pre$DBH_Increment),]$DBH_Increment<-0
}
Stourhead_cycle_1_pre$dbhcm<-Stourhead_cycle_1_pre$dbhcm+Stourhead_cycle_1_pre$DBH_Increment

Stourhead_cycle_1_pre2<-Preprocessing_FERS(Stourhead_cycle_1_pre)
Stourhead_cycle_1_pre2$dbhcm<-Stourhead_cycle_1_pre2$dbhcm+Stourhead_cycle_1_pre2$DBH_Increment

Stourhead_cycle_1_pre3<-Preprocessing_FERS(Stourhead_cycle_1_pre2)
Stourhead_cycle_1_pre3$dbhcm<-Stourhead_cycle_1_pre3$dbhcm+Stourhead_cycle_1_pre3$DBH_Increment

Stourhead_cycle_1_pre4<-Preprocessing_FERS(Stourhead_cycle_1_pre3)
Stourhead_cycle_1_pre4$dbhcm<-Stourhead_cycle_1_pre4$dbhcm+Stourhead_cycle_1_pre4$DBH_Increment


Stourhead_cycle_1_pre5<-Preprocessing_FERS(Stourhead_cycle_1_pre4)
Stourhead_cycle_1_pre5$dbhcm<-Stourhead_cycle_1_pre5$dbhcm+Stourhead_cycle_1_pre5$DBH_Increment

#########################
Stourhead_cycle_1_pre5$EST_increment<-Stourhead_cycle_1_pre5$dbhcm-Stourhead_cycle_1_pre5$Initial_dbhcm
Stourhead_cycle_1_pre5$dbh_act_increment<-NA
Stourhead_cycle_1_pre$dbh_act_increment<-NA

for(i in unique(Stourhead_2cycles[(Stourhead_2cycles$TREE_NAME %in% Stourhead_cycle_1_pre$TREE_NAME),]$TREE_NAME)){
  if(!is_empty(Stourhead_cycle_1_pre5[Stourhead_cycle_1_pre5$TREE_NAME==i,]$dbh_act_increment))
    Stourhead_cycle_1_pre5[Stourhead_cycle_1_pre5$TREE_NAME==i,]$dbh_act_increment <-Stourhead_2cycles[Stourhead_2cycles$TREE_NAME== i & Stourhead_2cycles$Cycle==4,]$DBH_INC[1]
  Stourhead_cycle_1_pre[Stourhead_cycle_1_pre$TREE_NAME==i,]$dbh_act_increment     <-Stourhead_2cycles[Stourhead_2cycles$TREE_NAME== i & Stourhead_2cycles$Cycle==4,]$DBH_INC[1]/5
}

Stourhead_cycle_1_pre5$error<-Stourhead_cycle_1_pre5$dbh_act_increment-Stourhead_cycle_1_pre5$EST_increment
Stourhead_cycle_1_pre$error<-Stourhead_cycle_1_pre$dbh_act_increment-Stourhead_cycle_1_pre$DBH_Increment
adjbox(Stourhead_AFI[Stourhead_AFI$cohort_name=="Spruce",]$dbhcm)
boxplot(Stourhead_cycle_1_pre5[!is.na(Stourhead_cycle_1_pre5$error),]$error~Stourhead_cycle_1_pre5[!is.na(Stourhead_cycle_1_pre5$error),]$cohort_name)
adjbox(Stourhead_cycle_1_pre[!is.na(Stourhead_cycle_1_pre$error),]$error~Stourhead_cycle_1_pre[!is.na(Stourhead_cycle_1_pre$error),]$cohort_name)
#remove trees that were axed

plot(Stourhead_cycle_1_pre$dbh_act_increment,Stourhead_cycle_1_pre$DBH_Increment,na.rm=T)
lines(0:12,0:12)

################################
Stourhead_cycle_1_pre<-Stourhead_cycle_1_pre[!(Stourhead_cycle_1_pre$TREE_NAME %in% Stourhead_AFI_Harvest$TREE_NAME),]
################################


Stourhead_cycle_1_pre<-dbh_class1(Stourhead_cycle_1_pre,tarif9)
Stourhead_cycle_1_pre1<- Stourhead_cycle_1_pre[Stourhead_cycle_1_pre$cohort_name %in% c("Spruce","Douglas fir"),]
df1<-Stourhead_cycle_1_pre1[,c("dbhclass","DBH_Increment","dbh_act_increment")]

df2<- melt(df1,id.vars = "dbhclass" )

ggplot(df2, aes(x=as.factor(dbhclass), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+ labs(x = "DBH class (cm)", y = "DBH increment", title= "DBh Actual increment vs Estimated")


#########################

exp(-2.8528+
      log(Stourhead_cycle_1_pre[Stourhead_cycle_1_pre$cohort_name=="Fast-growing broadleaves" & !is.na(Stourhead_cycle_1_pre$cr) ,]$dbhcm)*1.1729-
      0.00012*Stourhead_cycle_1_pre[Stourhead_cycle_1_pre$cohort_name=="Fast-growing broadleaves" & !is.na(Stourhead_cycle_1_pre$cr) ,]$dbhcm^2+
      log(Stourhead_cycle_1_pre[Stourhead_cycle_1_pre$cohort_name=="Fast-growing broadleaves" & !is.na(Stourhead_cycle_1_pre$cr) ,]$cr)*0.8241-
      0.000015*Stourhead_cycle_1_pre[Stourhead_cycle_1_pre$cohort_name=="Fast-growing broadleaves" & !is.na(Stourhead_cycle_1_pre$cr) ,]$CCF)

exp(-2.8528+
      log(Stourhead_cycle_1_pre[Stourhead_cycle_1_pre$cohort_name=="Fast-growing broadleaves",]$dbhcm)*1.1729-
      0.00012*Stourhead_cycle_1_pre[Stourhead_cycle_1_pre$cohort_name=="Fast-growing broadleaves"  ,]$dbhcm^2+
      log(Stourhead_cycle_1_pre[Stourhead_cycle_1_pre$cohort_name=="Fast-growing broadleaves" ,]$C_R)*0.8241-
      0.000015*Stourhead_cycle_1_pre[Stourhead_cycle_1_pre$cohort_name=="Fast-growing broadleaves" ,]$CCF)

Stourhead_cycle_1_pre[Stourhead_cycle_1_pre$cohort_name=="Fast-growing broadleaves",]$cr

################################






#####################################################################
# Monivea

Monivea_AFI$TREE_NAME<- paste(Monivea_AFI$idplots,Monivea_AFI$Tree_ID)
Monivea_AFI_2<- Monivea_AFI[Monivea_AFI$Limit==0,]
Monivea_Harvest<-Monivea_AFI %>%
  group_by(TREE_NAME)%>% 
  filter(n() == 1)

Monivea_2cycles<-Monivea_AFI %>%
  group_by(TREE_NAME)%>% 
  filter(n() > 1)
Monivea_2cycles$DBH_INC<-NA
for( i in unique(Monivea_2cycles$TREE_NAME)){
  Monivea_2cycles[Monivea_2cycles$TREE_NAME== i & Monivea_2cycles$Cycle==3,]$DBH_INC<-  Monivea_2cycles[Monivea_2cycles$TREE_NAME== i & Monivea_2cycles$Cycle==3,]$dbhcm- 
                                                                                                            Monivea_2cycles[Monivea_2cycles$TREE_NAME== i & Monivea_2cycles$Cycle==2,]$dbhcm
}

Monivea_cycle_1<-Monivea_AFI[Monivea_AFI$Cycle==1,]
Monivea_cycle_1<-Monivea_cycle_1[Monivea_cycle_1$Limit==0,]


dbh_inc_yrs(Monivea_cycle_1,5)


#####################################################################
plot(Rushmore_AFI$dbhcm,Rushmore_AFI$Height_m)
#Rushmore
Rushmore[Rushmore$Cycle==2 & Rushmore$NumPlac==2 & Rushmore$NumArbre==3 & Rushmore$Distance==8.9,]$NumPlac<-"1"
Rushmore_AFI<-AFI_Version2(Rushmore,2021,dataset_AFI,"J10/15 Chase Woods")
#Rushmore_AFI<-Rushmore_AFI[Rushmore_AFI$Limit==0,]
Rushmore_AFI_2<- Rushmore_AFI[Rushmore_AFI$Limit==0,]
Rushmore_AFI_Harvest<-Rushmore_AFI %>%
  group_by(TREE_NAME)%>%
  filter(n() == 1)

Rushmore_2cycles<-Rushmore_AFI %>%
  group_by(TREE_NAME)%>% 
  filter(n() > 1)
Rushmore_2cycles$DBH_INC<-NA
for( i in unique(Rushmore_2cycles$TREE_NAME)){
  Rushmore_2cycles[Rushmore_2cycles$TREE_NAME== i & Rushmore_2cycles$Cycle==2,]$DBH_INC<-  Rushmore_2cycles[Rushmore_2cycles$TREE_NAME== i & Rushmore_2cycles$Cycle==2,]$dbhcm- 
    Rushmore_2cycles[Rushmore_2cycles$TREE_NAME== i & Rushmore_2cycles$Cycle==1,]$dbhcm
}


Rushmore_2cycles_1<-Rushmore_2cycles[!is.na(Rushmore_2cycles$cr),]

Rushmore_2cycles_22<-Rushmore_2cycles_1[!is.na(Rushmore_2cycles_1$DBH_INC) ,]
Rushmore_2cycles_22<-Rushmore_2cycles_22[Rushmore_2cycles_22$DBH_INC>=0 ,]

Rushmore_2cycles_21<-Rushmore_2cycles_1[is.na(Rushmore_2cycles_1$DBH_INC) ,]

#####
Rushmore_cycle_1<-Rushmore_AFI[Rushmore_AFI$Cycle==1,]
Rushmore_cycle_1<-Rushmore_cycle_1[Rushmore_cycle_1$Limit==0,]
Rushmore_cycle_1<-Rushmore_cycle_1[Rushmore_cycle_1$TREE_NAME!="1 102",]
#Rushmore_cycle_1<-Rushmore_cycle_1[!is.na(Rushmore_cycle_1$cr),]
# Rushmore_cycle_1[Rushmore_cycle_1_pre5$species== "Douglas",]$cohort_name<-"Douglas fir"
# Rushmore_cycle_1[Rushmore_cycle_1_pre5$cohort_name== "Spruce",]$cohort_name<-"Douglas fir"
####Stourhead_AFI$species
Rushmore_cycle_1_pre<-Preprocessing_FERS(Rushmore_cycle_1)
Rushmore_cycle_1_pre$Initial_dbhcm<-Rushmore_cycle_1_pre$dbhcm
Rushmore_cycle_1_pre$dbhcm<-Rushmore_cycle_1_pre$dbhcm+Rushmore_cycle_1_pre$DBH_Increment

Rushmore_cycle_1_pre2<-Preprocessing_FERS(Rushmore_cycle_1_pre)
Rushmore_cycle_1_pre2$dbhcm<-Rushmore_cycle_1_pre2$dbhcm+Rushmore_cycle_1_pre2$DBH_Increment

Rushmore_cycle_1_pre3<-Preprocessing_FERS(Rushmore_cycle_1_pre2)
Rushmore_cycle_1_pre3$dbhcm<-Rushmore_cycle_1_pre3$dbhcm+Rushmore_cycle_1_pre3$DBH_Increment

Rushmore_cycle_1_pre4<-Preprocessing_FERS(Rushmore_cycle_1_pre3)
Rushmore_cycle_1_pre4$dbhcm<-Rushmore_cycle_1_pre4$dbhcm+Rushmore_cycle_1_pre4$DBH_Increment

Rushmore_cycle_1_pre5<-Preprocessing_FERS(Rushmore_cycle_1_pre4)
Rushmore_cycle_1_pre5$dbhcm<-Rushmore_cycle_1_pre5$dbhcm+Rushmore_cycle_1_pre5$DBH_Increment

Rushmore_cycle_1_pre5$EST_increment<-Rushmore_cycle_1_pre5$dbhcm-Rushmore_cycle_1_pre5$Initial_dbhcm
Rushmore_cycle_1_pre5$dbh_act_increment<-NA
Rushmore_cycle_1_pre$dbh_act_increment<-NA

for(i in unique(Rushmore_cycle_1[(Rushmore_cycle_1$TREE_NAME %in% Rushmore_2cycles$TREE_NAME),]$TREE_NAME)){
  if(!is_empty(Rushmore_cycle_1_pre5[Rushmore_cycle_1_pre5$TREE_NAME==i,]$dbh_act_increment)){
        Rushmore_cycle_1_pre5[Rushmore_cycle_1_pre5$TREE_NAME==i,]$dbh_act_increment<-Rushmore_2cycles[Rushmore_2cycles$TREE_NAME== i & Rushmore_2cycles$Cycle==2,]$DBH_INC
        Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$TREE_NAME==i,]$dbh_act_increment<-Rushmore_2cycles[Rushmore_2cycles$TREE_NAME== i & Rushmore_2cycles$Cycle==2,]$DBH_INC/5
  }
}
Rushmore_cycle_1_pre5$error<-Rushmore_cycle_1_pre5$dbh_act_increment-Rushmore_cycle_1_pre5$EST_increment
Rushmore_cycle_1_pre$error<-Rushmore_cycle_1_pre$dbh_act_increment-Rushmore_cycle_1_pre$DBH_Increment

mean(Rushmore_cycle_1_pre5$error,na.rm = T)
plot(Rushmore_cycle_1_pre5[Rushmore_cycle_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$dbh_act_increment,
     Rushmore_cycle_1_pre5[Rushmore_cycle_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$DBH_Increment,na.rm=T)
lines(0:12,0:12)
###########################
Rushmore_cycle_1_pre<-Rushmore_cycle_1_pre[!(Rushmore_cycle_1_pre$TREE_NAME %in% Rushmore_AFI_Harvest$TREE_NAME),]
#########################

adjbox(Rushmore_cycle_1_pre5[Rushmore_cycle_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$dbh_act_increment)
adjboxStats(Rushmore_cycle_1_pre5[Rushmore_cycle_1_pre5$cohort_name=="Spruce" ,]$dbh_act_increment)



Rushmore_cycle_1_pre5<-dbh_class1(Rushmore_cycle_1_pre5,tarif9)
Rushmore_cycle_1_pre<-dbh_class1(Rushmore_cycle_1_pre,tarif9)
Rushmore_cycle_1_pre<-Rushmore_cycle_1_pre[!is.na(Rushmore_cycle_1_pre$dbh_act_increment),]
df1<-Rushmore_cycle_1_pre[,c("dbhclass","DBH_Increment","dbh_act_increment")]

df2<- melt(df1,id.vars = "dbhclass" )

ggplot(df2, aes(x=as.factor(dbhclass), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+ labs(x = "DBH class (cm)", y = "DBH increment", title= "DBh Actual increment vs Estimated")


tarif9<- data.frame(dbhclass =seq(5,110,5)) 




View(Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$dbhclass==25,])




mean(Rushmore_cycle_1_pre$error,na.rm = T)
plot(Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$dbh_act_increment,
     Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$DBH_Increment,na.rm=T,
     xlim=c(0,2.5), ylim=c(0,2.50),xlab = "DBH Actual increment (cm)",ylab="DBH Estimated increment (cm)")
lines(0:12,0:12)

sqrt(mean((Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$dbh_act_increment - 
             Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$DBH_Increment)^2))
boxplot(Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$error~
          Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$idplots,xlab= "Plot",ylab="Residual")

table(Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$idplots)
summary(aov(Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$dbh_act_increment ~ 
              Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$DBH_Increment))

adjbox(Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$dbh_act_increment)
adjbox(Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$DBH_Increment)
#qqplot
qqplot(Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$dbh_act_increment,
       Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$DBH_Increment)
cor(Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$dbh_act_increment,
    Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$DBH_Increment)

# normality test of residuals
shapiro.test(Rushmore_cycle_1_pre$error)
plot(lm(Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$dbh_act_increment~
          Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$DBH_Increment))

#

plot(Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$dbh_act_increment,
    Rushmore_cycle_1_pre[Rushmore_cycle_1_pre$cohort_name %in% c("Douglas fir","Spruce"),]$error)



#########################################

Rushmore_c1_c2_c3

Rushmore_c1_c2_c3$Tree_NAME<-paste(Rushmore_c1_c2_c3$idplots,Rushmore_c1_c2_c3$Tree_ID)
cycle1<-Rushmore_c1_c2_c3[Rushmore_c1_c2_c3$Cycle==1,]
cycle2<-Rushmore_c1_c2_c3[Rushmore_c1_c2_c3$Cycle==2,]
cycle2$dbh_inc<-0
for(i in unique(cycle2$Tree_NAME)){
  if(!is_empty(cycle1[cycle1$Tree_NAME==i,]$dbhcm)){
  cycle2[cycle2$Tree_NAME==i,]$dbh_inc <- cycle2[cycle2$Tree_NAME==i,]$dbhcm-cycle1[cycle1$Tree_NAME==i,]$dbhcm
  }
}
cycle1<-cycle1[!is.na(cycle1$cr),]
cycle1<-cycle1[cycle1$Tree_ID %in% cycle2$Tree_ID,]
cycle1_test<-cycle1
cycle1_test<-cycle1_test[cycle1_test$Limit==0,]
cycle1_test<-Preprocessing_FERS(cycle1_test)
cycle1_test<-cycle1_test[cycle1_test$Limit==0,]
cycle1_test$initialDBH<-cycle1_test$dbhcm
i=0
while(i<5){
  cycle1_test$dbhcm<-cycle1_test$dbhcm+cycle1_test$DBH_Increment
  cycle1_test<-Preprocessing_FERS(cycle1_test)
i<-i+1
}
cycle1_test$cal_DBH_INC<-cycle1_test$dbhcm-cycle1_test$initialDBH
testing<-left_join(cycle2,cycle1_test[,c("Tree_NAME","cal_DBH_INC")],by = "Tree_NAME")
plot(data.frame(testing[,c("cal_DBH_INC")],testing[,c("dbh_inc")]))
cycle2_test$cal_DBH_INC<-cycle2_test$dbhcm-cycle2_test$initialDBH






cycle1$Tree_NAME
cycle2$Tree_NAME




subset(cycle1, !(Tree_NAME %in% cycle2$Tree_NAME))
subset(cycle1, (Tree_NAME %in% cycle2$Tree_NAME))

NROW(subset(cycle2, !(Tree_NAME %in% cycle1$Tree_NAME)))





#####################################################################

#Berth_Ddu_AFI
Berth_Ddu_AFI<-AFI_Version3(Berth_Ddu,2021,dataset_AFI,"2/3 Berth Ddu")
View(Berth_Ddu_AFI)
#Berth_Ddu_AFI<-Berth_Ddu_AFI[Berth_Ddu_AFI$Limit==0,]
Berth_Ddu_AFI$TREE_NAME<- paste(Berth_Ddu_AFI$idplots,Berth_Ddu_AFI$Tree_ID)
Berth_Ddu_AFI_2<- Berth_Ddu_AFI[Berth_Ddu_AFI$Limit==0,]
Berth_Ddu_AFI_Harvest<-Berth_Ddu_AFI %>%
  group_by(TREE_NAME)%>% 
  filter(n() == 1)

Berth_Ddu_AFI_2cycles<-Berth_Ddu_AFI %>%
  group_by(TREE_NAME)%>% 
  filter(n() > 1)
Berth_Ddu_AFI_2cycles$DBH_INC<-NA
for( i in unique(Berth_Ddu_AFI_2cycles$TREE_NAME)){
  Berth_Ddu_AFI_2cycles[Berth_Ddu_AFI_2cycles$TREE_NAME== i & Berth_Ddu_AFI_2cycles$Cycle==2,]$DBH_INC<-  Berth_Ddu_AFI_2cycles[Berth_Ddu_AFI_2cycles$TREE_NAME== i & Berth_Ddu_AFI_2cycles$Cycle==2,]$dbhcm- 
    Berth_Ddu_AFI_2cycles[Berth_Ddu_AFI_2cycles$TREE_NAME== i & Berth_Ddu_AFI_2cycles$Cycle==1,]$dbhcm
}

errors<-Berth_Ddu_AFI_2cycles[Berth_Ddu_AFI_2cycles$DBH_INC<=0 & !is.na(Berth_Ddu_AFI_2cycles$DBH_INC),]
Berth_Ddu_AFI_2cycles_1<-Berth_Ddu_AFI_2cycles
Berth_Ddu_AFI_2cycles_1<-Berth_Ddu_AFI_2cycles_1[!(Berth_Ddu_AFI_2cycles_1$Tree_ID %in% errors$Tree_ID),]
Berth_Ddu_AFI_2cycles_22<-Berth_Ddu_AFI_2cycles_1[Berth_Ddu_AFI_2cycles_1$Cycle==2 ,]
Berth_Ddu_AFI_2cycles_22<-Berth_Ddu_AFI_2cycles_22[Berth_Ddu_AFI_2cycles_22$DBH_INC>=0 ,]

Berth_Ddu_AFI_2cycles_21<-Berth_Ddu_AFI_2cycles_1[Berth_Ddu_AFI_2cycles_1$Cycle==1 ,]

#####
Berth_Ddu_AFI_2cycles_1<-Berth_Ddu_AFI_2cycles_1[Berth_Ddu_AFI_2cycles_1$Cycle==1,]
Berth_Ddu_AFI_2cycles_1<-Berth_Ddu_AFI_2cycles_1[Berth_Ddu_AFI_2cycles_1$Limit==0,]

####
Berth_Ddu_AFI_2cycles_1_pre<-Preprocessing_FERS(Berth_Ddu_AFI_2cycles_1)
Berth_Ddu_AFI_2cycles_1_pre$Initial_dbhcm<-Berth_Ddu_AFI_2cycles_1_pre$dbhcm
Berth_Ddu_AFI_2cycles_1_pre$dbhcm<-Berth_Ddu_AFI_2cycles_1_pre$dbhcm+Berth_Ddu_AFI_2cycles_1_pre$DBH_Increment

Berth_Ddu_AFI_2cycles_1_pre2<-Preprocessing_FERS(Berth_Ddu_AFI_2cycles_1_pre)
Berth_Ddu_AFI_2cycles_1_pre2$dbhcm<-Berth_Ddu_AFI_2cycles_1_pre2$dbhcm+Berth_Ddu_AFI_2cycles_1_pre2$DBH_Increment

Berth_Ddu_AFI_2cycles_1_pre3<-Preprocessing_FERS(Berth_Ddu_AFI_2cycles_1_pre2)
Berth_Ddu_AFI_2cycles_1_pre3$dbhcm<-Berth_Ddu_AFI_2cycles_1_pre3$dbhcm+Berth_Ddu_AFI_2cycles_1_pre3$DBH_Increment

Berth_Ddu_AFI_2cycles_1_pre4<-Preprocessing_FERS(Berth_Ddu_AFI_2cycles_1_pre3)
Berth_Ddu_AFI_2cycles_1_pre4$dbhcm<-Berth_Ddu_AFI_2cycles_1_pre4$dbhcm+Berth_Ddu_AFI_2cycles_1_pre4$DBH_Increment

Berth_Ddu_AFI_2cycles_1_pre5<-Preprocessing_FERS(Berth_Ddu_AFI_2cycles_1_pre4)
Berth_Ddu_AFI_2cycles_1_pre5$dbhcm<-Berth_Ddu_AFI_2cycles_1_pre5$dbhcm+Berth_Ddu_AFI_2cycles_1_pre5$DBH_Increment

Berth_Ddu_AFI_2cycles_1_pre6<-Preprocessing_FERS(Berth_Ddu_AFI_2cycles_1_pre5)
Berth_Ddu_AFI_2cycles_1_pre6$dbhcm<-Berth_Ddu_AFI_2cycles_1_pre6$dbhcm+Berth_Ddu_AFI_2cycles_1_pre6$DBH_Increment

Berth_Ddu_AFI_2cycles_1_pre6$EST_increment<-Berth_Ddu_AFI_2cycles_1_pre6$dbhcm-Berth_Ddu_AFI_2cycles_1_pre6$Initial_dbhcm
Berth_Ddu_AFI_2cycles_1_pre6$dbh_act_increment<-NA
Berth_Ddu_AFI_2cycles_1_pre$dbh_act_increment<-NA

for(i in unique(Berth_Ddu_AFI_2cycles$TREE_NAME)){
  if(!is_empty(Berth_Ddu_AFI_2cycles_1_pre6[Berth_Ddu_AFI_2cycles_1_pre6$TREE_NAME==i,]$dbh_act_increment)){
    Berth_Ddu_AFI_2cycles_1_pre6[Berth_Ddu_AFI_2cycles_1_pre6$TREE_NAME==i,]$dbh_act_increment<-
                        Berth_Ddu_AFI_2cycles[Berth_Ddu_AFI_2cycles$TREE_NAME== i & Berth_Ddu_AFI_2cycles$Cycle==2,]$DBH_INC
      
      Berth_Ddu_AFI_2cycles_1_pre[Berth_Ddu_AFI_2cycles_1_pre$TREE_NAME==i,]$dbh_act_increment<-
        Berth_Ddu_AFI_2cycles[Berth_Ddu_AFI_2cycles$TREE_NAME== i & Berth_Ddu_AFI_2cycles$Cycle==2,]$DBH_INC/6
  }
  

  }
Berth_Ddu_AFI_2cycles_1_pre6$error<-Berth_Ddu_AFI_2cycles_1_pre6$dbh_act_increment-Berth_Ddu_AFI_2cycles_1_pre6$EST_increment
Berth_Ddu_AFI_2cycles_1_pre$error<-Berth_Ddu_AFI_2cycles_1_pre$dbh_act_increment -Berth_Ddu_AFI_2cycles_1_pre$DBH_Increment


# residual analysis 

mean(Berth_Ddu_AFI_2cycles_1_pre5$error,na.rm = T)
plot(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$dbh_act_increment,
     Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$EST_increment,na.rm=T,
     col = Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$idplots,
     xlim=c(0,20), ylim=c(0,20))
lines(0:12,0:12)

sqrt(mean((Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$dbh_act_increment - 
             Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$EST_increment)^2))
boxplot(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$error~
           Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$idplots)

table(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$idplots)
summary(aov(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$dbh_act_increment ~ 
      Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$EST_increment))
      
adjbox(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$dbh_act_increment)
adjbox(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$EST_increment)
#qqplot
qqplot(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$dbh_act_increment,
       Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$EST_increment)
cor(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$dbh_act_increment,
    Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$EST_increment)

# normality test of residuals
shapiro.test(Berth_Ddu_AFI_2cycles_1_pre5$error)
plot(lm(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$dbh_act_increment~
   Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$EST_increment))

#




dbh_class1<- function(DBH_class,dbh_tarif){
  DBH_class$dbhclass<-NA
  for(i in c(1:NROW(DBH_class))){
    
    DBH_class[i,c("dbhclass")] <- c(plyr::round_any(DBH_class[i,]$dbhcm,
                                                    5, f = ceiling))
    
  }
  return(DBH_class)
}





Berth_Ddu_AFI_2cycles_1_pre5<-dbh_class1(Berth_Ddu_AFI_2cycles_1_pre5,tarif9)


df1<-Berth_Ddu_AFI_2cycles_1_pre5[,c("dbhclass","EST_increment","dbh_act_increment")]

df2<- melt(df1,id.vars = "dbhclass" )

ggplot(df2, aes(x=dbhclass, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')


tarif9<- data.frame(dbhclass =seq(5,110,5)) 






















Berth_Ddu_AFI_2cycles_1_pre5<-Berth_Ddu_AFI_2cycles_1_pre5[abs(Berth_Ddu_AFI_2cycles_1_pre5$error)<33,]







plot(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$error,Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name%in% c("Douglas fir","Spruce"),]$cr)
plot(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$error,Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name%in% c("Douglas fir","Spruce"),]$dbhcm)
plot(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name %in% c("Douglas fir","Spruce"),]$error,Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name%in% c("Douglas fir","Spruce"),]$X)
plot(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name%in% c("Douglas fir","Spruce"),]$error,Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name%in% c("Douglas fir","Spruce"),]$Y)
plot(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name%in% c("Douglas fir","Spruce"),]$error,sqrt(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name%in% c("Douglas fir","Spruce"),]$X^2+
                                                                                                    Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name%in% c("Douglas fir","Spruce"),]$Y^2))
plot(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name%in% c("Douglas fir","Spruce"),]$error,Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name%in% c("Douglas fir","Spruce"),]$Height_m)
plot(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name%in% c("Douglas fir","Spruce"),]$error,Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name%in% c("Douglas fir","Spruce"),]$BAL)
plot(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name%in% c("Douglas fir","Spruce"),]$error,Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name%in% c("Douglas fir","Spruce"),]$DBH_Increment)
plot(Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name%in% c("Douglas fir","Spruce"),]$error,Berth_Ddu_AFI_2cycles_1_pre5[Berth_Ddu_AFI_2cycles_1_pre5$cohort_name%in% c("Douglas fir","Spruce"),]$dbh_act_increment)


0.11545819232061+(exp(-3.35500746346963+
                        0.76115629775393*log(Preproc[Preproc$cohort_name=="Douglas fir" & is.na(Preproc$cr),]$dbhcm)-
                        0.000163629827225797*Preproc[Preproc$cohort_name=="Douglas fir" & is.na(Preproc$cr),]$dbhcm^2+
                        log(Preproc[Preproc$cohort_name=="Douglas fir" & is.na(Preproc$cr),]$C_R)*0.832657425914154))*
  (exp(0.684553692330231+-0.0320013664713506*
         Preproc[Preproc$cohort_name=="Douglas fir" & is.na(Preproc$cr),]$BAL_ties_adjusted_3+
         -0.0949251336968429*log(Preproc[Preproc$cohort_name=="Douglas fir" & is.na(Preproc$cr),]$CCF)))*
  (exp(0.700527016137579+0.220511238026885*
         (Preproc[Preproc$cohort_name=="Douglas fir" & is.na(Preproc$cr),]$YC/17.2)))













###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

x<-dbh_inc_yrs(Rushmore_cycle_1,5)
sds<-Rushmore_AFI
Rushmore_AFI<-Mellory_cycle_1_PRE[Mellory_cycle_1_PRE$Cycle %in% c(1,2),]#Mellory_cycle_1_PRE[Mellory_cycle_1_PRE$Cycle %in% c(2,3),]
Stourhead_AFI<-Stourhead_AFI[!is.na(Stourhead_AFI$dbhcm),]
dbh_inc_yrs<-function(Rushmore_AFI,years,cycle2,cycle1){
  
  Rushmore_AFI<-Rushmore_AFI[Rushmore_AFI$Limit==0,]
  Rushmore_AFI_2<- Rushmore_AFI[Rushmore_AFI$Limit==0,]
  Rushmore_AFI_Harvest<-Rushmore_AFI %>%
    group_by(TREE_NAME)%>%
    filter(n() == 1)

  Rushmore_2cycles<-Rushmore_AFI %>%
    group_by(TREE_NAME)%>%
    filter(n() > 1)
  
  Rushmore_2cycles_cycle1<-Rushmore_2cycles[Rushmore_2cycles$Cycle==cycle1,]
  Rushmore_2cycles_cycle2<-Rushmore_2cycles[Rushmore_2cycles$Cycle==cycle2,]
  
  Rushmore_2cycles_cycle1<-   Rushmore_2cycles_cycle1[!(duplicated(Rushmore_2cycles_cycle1$TREE_NAME) | duplicated(Rushmore_2cycles_cycle1$TREE_NAME, fromLast = TRUE)), ]
  Rushmore_2cycles_cycle2<-   Rushmore_2cycles_cycle2[!(duplicated(Rushmore_2cycles_cycle2$TREE_NAME) | duplicated(Rushmore_2cycles_cycle2$TREE_NAME, fromLast = TRUE)), ]
  
  Rushmore_2cycles<- rbind(Rushmore_2cycles_cycle1,Rushmore_2cycles_cycle2)
  
  Rushmore_2cycles<-Rushmore_2cycles %>%
    group_by(TREE_NAME)%>%
    filter(n() > 1)
  
  Rushmore_2cycles$DBH_INC<-NA
  for( i in unique(Rushmore_2cycles$TREE_NAME)){
    Rushmore_2cycles[Rushmore_2cycles$TREE_NAME== i & Rushmore_2cycles$Cycle==cycle1,]$DBH_INC<-  Rushmore_2cycles[Rushmore_2cycles$TREE_NAME== i & Rushmore_2cycles$Cycle==cycle2,]$dbhcm-
      Rushmore_2cycles[Rushmore_2cycles$TREE_NAME== i & Rushmore_2cycles$Cycle==cycle1,]$dbhcm
  }

  
  Rushmore_2cycles_1<-Rushmore_2cycles[!is.na(Rushmore_2cycles$cr),]
  
  Rushmore_2cycles_22<-Rushmore_2cycles_1[!is.na(Rushmore_2cycles_1$DBH_INC) ,]
  Rushmore_2cycles_22<-Rushmore_2cycles_22[Rushmore_2cycles_22$DBH_INC>=0 ,]
  
  Rushmore_2cycles_21<-Rushmore_2cycles_1[is.na(Rushmore_2cycles_1$DBH_INC) ,]
  
  #####
  Rushmore_cycle_1<-Rushmore_AFI[Rushmore_AFI$Cycle==cycle1,]
  Rushmore_cycle_1<-Rushmore_cycle_1[Rushmore_cycle_1$Limit==0,]
 
  
 
 
  i <-2
  list_of_y<-list()
  Rushmore_cycle_1$Initial_dbhcm<-Rushmore_cycle_1$dbhcm
  list_of_y[[1]]<-Preprocessing_FERS(Rushmore_cycle_1)
  list_of_y[[2]]<-list_of_y[[1]]
  list_of_y[[2]]$dbhcm<-list_of_y[[1]]$dbhcm+list_of_y[[1]]$DBH_Increment
  list_of_y[[2]]$Height_m<-NA
  
  while(i+1 <=years){
    
    list_of_y[[i]]<-Preprocessing_FERS(list_of_y[[i]])
    list_of_y[[i+1]]<-list_of_y[[i]]
    list_of_y[[i+1]]$dbhcm<-list_of_y[[i+1]]$dbhcm+list_of_y[[i+1]]$DBH_Increment
    
    
    i<-i+1
  }
  list_of_y[[i]]<-Preprocessing_FERS(list_of_y[[i]])
  
  
  list_of_y[[length(list_of_y)]]$dbh_act_increment<-NA
  list_of_y[[1]]$dbh_act_increment<-NA
  data_tree<-list_of_y[[length(list_of_y)]]
  
  for(i in unique(Rushmore_2cycles$TREE_NAME)){
    
    if(!is_empty(data_tree[data_tree$TREE_NAME==i,]$dbh_act_increment)){
      
      data_tree[data_tree$TREE_NAME==i,]$dbh_act_increment<-
             Rushmore_2cycles[Rushmore_2cycles$TREE_NAME== i & Rushmore_2cycles$Cycle==cycle1,]$DBH_INC
      
      list_of_y[[1]][list_of_y[[1]]$TREE_NAME==i,]$dbh_act_increment<-
             Rushmore_2cycles[Rushmore_2cycles$TREE_NAME== i & Rushmore_2cycles$Cycle==cycle1,]$DBH_INC/years
    }
    
    
  }
  list_of_y[[length(list_of_y)]]<-data_tree
  list_of_y[[length(list_of_y)]]$EST_increment<-list_of_y[[length(list_of_y)]]$dbhcm-list_of_y[[length(list_of_y)]]$Initial_dbhcm
  list_of_y[[length(list_of_y)]]$error<-NA
  list_of_y[[1]]$error_Carbware<-NA
  list_of_y[[length(list_of_y)]]$error <- list_of_y[[length(list_of_y)]]$dbh_act_increment-list_of_y[[length(list_of_y)]]$EST_increment
  list_of_y[[1]]$error_Carbware <- list_of_y[[1]]$dbh_act_increment -list_of_y[[1]]$DBH_Increment
  
  list_of_y[[1]]$average_est_inc<-NA
  for(i in list_of_y[[1]]$TREE_NAME){
    list_of_y[[1]][list_of_y[[1]]$TREE_NAME==i,]$average_est_inc <-  list_of_y[[length(list_of_y)]][list_of_y[[length(list_of_y)]]$TREE_NAME==i,]$EST_increment/length(list_of_y)
  }
  
  return(list_of_y)
}




###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################



#Monivea

Monivea_AFI_pre<-dbh_inc_yrs(Monivea_AFI,5,3,2)
View(Monivea_AFI_pre[[1]])
#Mellory
#Mellory_AFI<-AFI_Version3(Mellory,2011,dataset_AFI,"Mellory")
Mellory_AFI<-Mellory_AFI[-c(38,382),]
Mellory_AFI<-Mellory_AFI[!is.na(Mellory_AFI$dbhcm),]
Mellory_AFI_pre<-dbh_inc_yrs(Mellory_AFI,5,3,2)
#Knockrath

Knockrath_AFI_pre<-dbh_inc_yrs(Knockrath_AFI,5,3,2)
#stourhead
Stourhead_AFI_pre<-dbh_inc_yrs(Stourhead_AFI,5,4,3)
#rushmore
Rushmore_AFI<-Rushmore_AFI[Rushmore_AFI$TREE_NAME!="1 102",]
Rushmore_AFI_pre<-dbh_inc_yrs(Rushmore_AFI,5,2,1)
# berth ddu
Berth_Ddu_AFI_pre<-dbh_inc_yrs(Berth_Ddu_AFI,5,2,1)
View(Berth_Ddu_AFI_pre[[1]])
# Baronscourt_AFI
# Baronscourt_AFI$YC<-NA
# Baronscourt_AFI<-Baronscourt_AFI[!is.na(Baronscourt_AFI$idplots),]
# Baronscourt_AFI<-Baronscourt_AFI[!is.na(Baronscourt_AFI$dbhcm),]
# Baronscourt_AFI<-Baronscourt_AFI[!is.na(Baronscourt_AFI$species),]
# Baronscourt_AFI<-Baronscourt_AFI[!is.na(Baronscourt_AFI$idplots),]
# Baronscourt_AFI_pre<-dbh_inc_yrs(Baronscourt_AFI,4,2,1)

##################################################
# 1,2,3 cycle
Cranborne_208_isn$Limit

# Cranborne_208_isn<-Cranborne_208_isn[!(Cranborne_208_isn$TREE_NAME %in% neg_inc),]

Cranborne_208_p_1<-dbh_inc_yrs(Cranborne_208_isn[Cranborne_208_isn$Cycle!=3,],5,2,1)
Cranborne_208_p_2<-dbh_inc_yrs(Cranborne_208_isn[Cranborne_208_isn$Cycle!=1,],4,3,2)
# add cycle 3 
cycle_3<-Cranborne_208_isn[Cranborne_208_isn$Cycle==3,]
#stage_3<-stage_3[stage_3$Limit==0,]
cycle_3<-cycle_3[!is.na(cycle_3$dbhcm),]

not_included<- cycle_3[cycle_3$Limit==1,]

cycle_3_preproc<- Preprocessing_FERS(cycle_3[cycle_3$Limit==0,])
d<-NROW(cycle_3_preproc)
if(NROW(not_included)>0){
  cycle_3_preproc[(d+1):(d+nrow(not_included)),] <- NA#
  cycle_3_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
}

cycle_3_preproc$Initial_dbhcm<-NA
cycle_3_preproc$dbh_act_increment<-NA
cycle_3_preproc$error_Carbware_dbh_inc <-NA
cycle_3_preproc$error_Carbware_average_inc<-NA
cycle_3_preproc$average_est_inc<- NA



Cranborne_208_final<-rbind(Cranborne_208_p_1[[1]],Cranborne_208_p_2[[1]],cycle_3_preproc)
#View(Cranborne_208_final)
neg_inc<-Cranborne_208_final[Cranborne_208_final$dbh_act_increment<=0,"TREE_NAME"]
##################################################

# 1,2 cycle
Cranborne_37c_isn$Date_measured
# Cranborne_37c_isn<-Cranborne_37c_isn[!(Cranborne_37c_isn$TREE_NAME %in% neg_inc),]

#View(Cranborne_37c_isn[Cranborne_37c_isn$Cycle==2 & Cranborne_37c_isn$Limit==0,])

Cranborne_37c_p<-dbh_inc_yrs(Cranborne_37c_isn,5,2,1)

cycle_2<-Cranborne_37c_isn[Cranborne_37c_isn$Cycle==2,]
#stage_3<-stage_3[stage_3$Limit==0,]
cycle_2<-cycle_2[!is.na(cycle_2$dbhcm),]

not_included<- cycle_2[cycle_2$Limit==1,]

cycle_2_preproc<- Preprocessing_FERS(cycle_2[cycle_2$Limit==0,])
d<-NROW(cycle_2_preproc)
if(NROW(not_included)>0){
  cycle_2_preproc[(d+1):(d+nrow(not_included)),] <- NA#
  cycle_2_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
}
cycle_2_preproc$Initial_dbhcm<-NA
cycle_2_preproc$dbh_act_increment<-NA
cycle_2_preproc$error_Carbware_dbh_inc <-NA
cycle_2_preproc$error_Carbware_average_inc<-NA
cycle_2_preproc$average_est_inc<- NA

Cranborne_37c_final<-rbind(Cranborne_37c_p[[1]],cycle_2_preproc)
#View(Cranborne_37c_final)
rm(neg_inc)
neg_inc<-Cranborne_37c_final[Cranborne_37c_final$dbh_act_increment<=0,"TREE_NAME"]

#################################################################################################
# 1,2 cycle
Rushmore_l3_isn$Date_measured
# Rushmore_l3_isn<-Rushmore_l3_isn[!(Rushmore_l3_isn$TREE_NAME %in% neg_inc),]


Rushmore_l3_p<-dbh_inc_yrs(Rushmore_l3_isn,4,2,1)


cycle_2<-Rushmore_l3_isn[Rushmore_l3_isn$Cycle==2,]
#stage_3<-stage_3[stage_3$Limit==0,]
cycle_2<-cycle_2[!is.na(cycle_2$dbhcm),]

not_included<- cycle_2[cycle_2$Limit==1,]

cycle_2_preproc<- Preprocessing_FERS(cycle_2[cycle_2$Limit==0,])
d<-NROW(cycle_2_preproc)
if(NROW(not_included)>0){
  cycle_2_preproc[(d+1):(d+nrow(not_included)),] <- NA#
  cycle_2_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
}
cycle_2_preproc$Initial_dbhcm<-NA
cycle_2_preproc$dbh_act_increment<-NA
cycle_2_preproc$error_Carbware_dbh_inc <-NA
cycle_2_preproc$error_Carbware_average_inc<-NA
cycle_2_preproc$average_est_inc<- NA

Rushmore_l3_final<-rbind(Rushmore_l3_p[[1]],cycle_2_preproc)
View(Rushmore_l3_final)
rm(neg_inc)
neg_inc<-Rushmore_l3_final[Rushmore_l3_final$dbh_act_increment<=0,"TREE_NAME"]

############################################################################################
#1,2 cycle
View(Rushmore_m9_isn)
# Rushmore_m9_isn<-Rushmore_m9_isn[!(Rushmore_m9_isn$TREE_NAME %in% neg_inc),]

Rushmore_m9_p<-dbh_inc_yrs(Rushmore_m9_isn,4,2,1)


cycle_2<-Rushmore_m9_isn[Rushmore_m9_isn$Cycle==2,]
#stage_3<-stage_3[stage_3$Limit==0,]
cycle_2<-cycle_2[!is.na(cycle_2$dbhcm),]

not_included<- cycle_2[cycle_2$Limit==1,]

cycle_2_preproc<- Preprocessing_FERS(cycle_2[cycle_2$Limit==0,])
d<-NROW(cycle_2_preproc)
if(NROW(not_included)>0){
  cycle_2_preproc[(d+1):(d+nrow(not_included)),] <- NA#
  cycle_2_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
}
cycle_2_preproc$Initial_dbhcm<-NA
cycle_2_preproc$dbh_act_increment<-NA
cycle_2_preproc$error_Carbware_dbh_inc <-NA
cycle_2_preproc$error_Carbware_average_inc<-NA
cycle_2_preproc$average_est_inc<- NA


Rushmore_m9_final<-rbind(Rushmore_m9_p[[1]],cycle_2_preproc)
View(Rushmore_m9_final)

rm(neg_inc)
neg_inc<-Rushmore_m9_final[Rushmore_m9_final$dbh_act_increment<=0,"TREE_NAME"]
#############################################################################################
#1,2 cycle
Rushmore_b2_isn
# Rushmore_b2_isn<-Rushmore_b2_isn[!(Rushmore_b2_isn$TREE_NAME %in% neg_inc),]

Rushmore_b2_p<-dbh_inc_yrs(Rushmore_b2_isn,4,2,1)

cycle_2<-Rushmore_b2_isn[Rushmore_b2_isn$Cycle==2,]
#stage_3<-stage_3[stage_3$Limit==0,]
cycle_2<-cycle_2[!is.na(cycle_2$dbhcm),]

not_included<- cycle_2[cycle_2$Limit==1,]

cycle_2_preproc<- Preprocessing_FERS(cycle_2[cycle_2$Limit==0,])
d<-NROW(cycle_2_preproc)
if(NROW(not_included)>0){
  cycle_2_preproc[(d+1):(d+nrow(not_included)),] <- NA#
  cycle_2_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
}
cycle_2_preproc$Initial_dbhcm<-NA
cycle_2_preproc$dbh_act_increment<-NA
cycle_2_preproc$error_Carbware_dbh_inc <-NA
cycle_2_preproc$error_Carbware_average_inc<-NA
cycle_2_preproc$average_est_inc<- NA


Rushmore_b2_final<-rbind(Rushmore_b2_p[[1]],cycle_2_preproc)
View(Rushmore_b2_final)
rm(neg_inc)
neg_inc<-Rushmore_b2_final[Rushmore_b2_final$dbh_act_increment<=0,"TREE_NAME"]



#1,2 cycle
Crichel_down_isn
# Crichel_down_isn<-Crichel_down_isn[!(Crichel_down_isn$TREE_NAME %in% neg_inc),]

Crichel_p<-dbh_inc_yrs(Crichel_down_isn,3,2,1)

cycle_2<-Crichel_down_isn[Crichel_down_isn$Cycle==2,]
#stage_3<-stage_3[stage_3$Limit==0,]
cycle_2<-cycle_2[!is.na(cycle_2$dbhcm),]

not_included<- cycle_2[cycle_2$Limit==1,]

cycle_2_preproc<- Preprocessing_FERS(cycle_2[cycle_2$Limit==0,])
d<-NROW(cycle_2_preproc)
if(NROW(not_included)>0){
  cycle_2_preproc[(d+1):(d+nrow(not_included)),] <- NA#
  cycle_2_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
}
cycle_2_preproc$Initial_dbhcm<-NA
cycle_2_preproc$dbh_act_increment<-NA
cycle_2_preproc$error_Carbware_dbh_inc <-NA
cycle_2_preproc$error_Carbware_average_inc<-NA
cycle_2_preproc$average_est_inc<- NA


Crichel_p_final<-rbind(Crichel_p[[1]],cycle_2_preproc)
View(Crichel_p_final)

rm(neg_inc)
neg_inc<-Crichel_p_final[Crichel_p_final$dbh_act_increment<=0,"TREE_NAME"]
##########################################################################################################################
#1,2 cycle
llchester_47_isn
# llchester_47_isn<-llchester_47_isn[!(llchester_47_isn$TREE_NAME %in% neg_inc),]

llchester_47_p<-dbh_inc_yrs(llchester_47_isn,4,2,1)


cycle_2<-llchester_47_isn[llchester_47_isn$Cycle==2,]
#stage_3<-stage_3[stage_3$Limit==0,]
cycle_2<-cycle_2[!is.na(cycle_2$dbhcm),]

not_included<- cycle_2[cycle_2$Limit==1,]

cycle_2_preproc<- Preprocessing_FERS(cycle_2[cycle_2$Limit==0,])
d<-NROW(cycle_2_preproc)
if(NROW(not_included)>0){
  cycle_2_preproc[(d+1):(d+nrow(not_included)),] <- NA#
  cycle_2_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
}
cycle_2_preproc$Initial_dbhcm<-NA
cycle_2_preproc$dbh_act_increment<-NA
cycle_2_preproc$error_Carbware_dbh_inc <-NA
cycle_2_preproc$error_Carbware_average_inc<-NA
cycle_2_preproc$average_est_inc<- NA


llchester_47_final<-rbind(llchester_47_p[[1]],cycle_2_preproc)
View(llchester_47_final)

rm(neg_inc)
neg_inc<-llchester_47_final[llchester_47_final$dbh_act_increment<=0,"TREE_NAME"]
##################################################################################
ISN_Final_Data1<- rbind(llchester_47_final,
                       Crichel_p_final,
                       Rushmore_b2_final,
                       Rushmore_m9_final,
                       Rushmore_l3_final,
                       Cranborne_37c_final,
                       Cranborne_208_final)
View(ISN_Final_Data)
View(ISN_Final_Data1)

View(Cranborne_208_final[Cranborne_208_final$idplots==1,])
ISN_Final_Data_old
##################################################################################
#

# AFI ISN mart dataset creation
#

####################################################################################

llchester_47_isn$cycle
Stourhead_AFI_pre[[1]]$error
Knockrath_AFI_pre[[1]]


AFI_pre<-rbind(Monivea_AFI_pre[[1]],
               Mellory_AFI_pre[[1]],
               Stourhead_AFI_pre[[1]],
Knockrath_AFI_pre[[1]],
Rushmore_AFI_pre[[1]],
Berth_Ddu_AFI_pre[[1]])
AFI_pre$data_type<- "AFI"


ISN_pre<-rbind( Cranborne_208_p_1[[1]],Cranborne_208_p_2[[1]],
Cranborne_37c_p[[1]],
Rushmore_l3_p[[1]],
Rushmore_m9_p[[1]],
Rushmore_b2_p[[1]],
Crichel_p[[1]],
llchester_47_p[[1]])
ISN_pre$data_type<-"ISN"
ISN_AFI_data<-rbind(AFI_pre,ISN_pre[,-which(names(ISN_pre) %in% c("SpeciesGroup"))])
ISN_AFI_data$dbhcm

colnames(AFI_pre) %in% colnames(ISN_pre)
colnames(ISN_pre) %in% colnames(AFI_pre)

ISN_pre[,-which(names(ISN_pre) %in% c("SpeciesGroup"))]
which()



ISN_AFI_data<-ISN_AFI_data[ISN_AFI_data$species!="2021",]

View(ISN_AFI_data)
write.csv2(ISN_AFI_data,"ISN_AFI_data1")



sum(ISN_AFI_data[ISN_AFI_data$Site_code=="208, 213a Boulsbury" & ISN_AFI_data$Cycle==2,]$RepreBAHA)



table(ISN_AFI_data$species)


Stikaspruce<-ISN_AFI_data[ISN_AFI_data$species %in% c("Sitka spruce","SS","Epic?a C"),]
plot(lm(Stikaspruce$dbh_act_increment~Stikaspruce$DBH_Increment))


Stikaspruce<-dbh_class1(Stikaspruce,tarif9)


df1<-Stikaspruce[,c("dbhclass","DBH_Increment","dbh_act_increment")]

df2<- melt(df1,id.vars = "dbhclass" )

ggplot(df2, aes(x=as.factor(dbhclass), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+ labs(x = "DBH class (cm)", y = "DBH increment", title= "DBh Actual increment vs Estimated")


tarif9<- data.frame(dbhclass =seq(5,110,5)) 

Stikaspruce[Stikaspruce$dbh_act_increment<0 & !is.na(Stikaspruce$dbh_act_increment),]

harvested<-Stikaspruce[is.na(Stikaspruce$dbh_act_increment),]

Stikaspruce<-Stikaspruce[Stikaspruce$dbh_act_increment>0 & !is.na(Stikaspruce$dbh_act_increment),]


df1<-Stikaspruce[,c("dbhclass","DBH_Increment","dbh_act_increment")]

df2<- melt(df1,id.vars = "dbhclass" )

ggplot(df2, aes(x=as.factor(dbhclass), y=value, fill=variable)) +
  geom_col( position='dodge')+ labs(x = "DBH class (cm)", y = "DBH increment", title= "DBh Actual increment vs Estimated")



boxplot(Stikaspruce[Stikaspruce$dbhclass==25,]$dbh_act_increment)
adjbox(Stikaspruce[Stikaspruce$dbhclass==25,]$dbh_act_increment)



View(Stikaspruce[Stikaspruce$dbhclass==25,])



plot(lm(Stikaspruce$dbh_act_increment~Stikaspruce$DBH_Increment))

summary(lm(Stikaspruce$dbh_act_increment~Stikaspruce$DBH_Increment))

plot(Stikaspruce$dbh_act_increment~Stikaspruce$DBH_Increment,col =Stikaspruce$col,xlim=c(0,2.5), ylim=c(0,2.5))
lines(0:2,0:2)


plot(Stikaspruce[Stikaspruce$data_type=="AFI",]$dbh_act_increment~Stikaspruce[Stikaspruce$data_type=="AFI",]$DBH_Increment,xlim=c(0,2.5), ylim=c(0,2.5))
lines(0:2,0:2)

plot(Stikaspruce[Stikaspruce$data_type=="ISN",]$dbh_act_increment~Stikaspruce[Stikaspruce$data_type=="ISN",]$DBH_Increment,xlim=c(0,2.5), ylim=c(0,2.5))
lines(0:2,0:2)


Stikaspruce$col<-NA
Stikaspruce[Stikaspruce$data_type=="AFI",]$col<-1
Stikaspruce[Stikaspruce$data_type=="ISN",]$col<-2

Stikaspruce<-Stikaspruce[!(Stikaspruce$dbhclass==25 & Stikaspruce$dbh_act_increment>1.7),]

mean(Stikaspruce[Stikaspruce$dbhclass==25,]$dbh_act_increment)





means_stika<- data.frame(dbhclass=unique(Stikaspruce$dbhclass))
means_stika$dbh_act_increment<-NA
means_stika$dbh_increment<-NA
means_stika$N<-NA
for( i in means_stika$dbhclass){
  means_stika[means_stika$dbhclass==i,]$dbh_act_increment<-mean(Stikaspruce[Stikaspruce$dbhclass==i,]$dbh_act_increment)
  means_stika[means_stika$dbhclass==i,]$dbh_increment<-mean(Stikaspruce[Stikaspruce$dbhclass==i,]$DBH_Increment)
  means_stika[means_stika$dbhclass==i,]$N<-NROW(Stikaspruce[Stikaspruce$dbhclass==i,])

}

means_stika<-means_stika[order(means_stika$dbhclass),]
df2<- melt(means_stika[,c("dbh_increment","dbh_act_increment")],id.vars = "dbhclass" )

df2<- df2[order(df2$dbhclass),]
ggplot(df2, aes(x=as.factor(dbhclass), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+ labs(x = "DBH class (cm)", y = "DBH increment",
                                    title= "DBh Actual increment vs Estimated") + 
  geom_text(aes(label=n_forplot))

n_forplot<-c(12,12,22,22,35,35,50,50,65,65,33,33,16,16,4,4,7,7,1,1,2,2)


View(Stikaspruce[Stikaspruce$dbhclass==45,])
boxplot(Stikaspruce[Stikaspruce$dbhclass==55,]$DBH_Increment)
adjbox(Stikaspruce[Stikaspruce$dbhclass==55,]$dbh_act_increment)


table(ISN_AFI_data$species)



boxplot(Stikaspruce[Stikaspruce$dbhclass==40,]$dbh_act_increment)
adjbox(Stikaspruce[Stikaspruce$dbhclass==40,]$dbh_act_increment)


## data.frame of summary if plots
# age of plot 
# basal area
# average DBH
#top height
#stocking
#mean height
#developmental stage
Stourhead_AFI_pre[[1]]
Knockrath_AFI_pre[[1]]
Rushmore_AFI_pre[[1]]
Berth_Ddu_AFI_pre[[1]]
Cranborne_208_p_1[[1]]
Cranborne_208_p_2[[1]]
Cranborne_37c_p[[1]]
Rushmore_l3_p[[1]]
Rushmore_m9_p[[1]]
Rushmore_b2_p[[1]]
Crichel_p[[1]]
llchester_47_p[[1]]

Plot_Data<-data.frame(Forest=NA,
           Plot = NA,
           Specie = NA,
           Age= NA,
           Basal_Area = NA,
           Average_DBH=NA,
           Top_Height= NA,
           Mean_Height = NA,
           Stocking=NA,
           Developmental_Stage = NA)



Forest_name<- strsplit(deparse(substitute(Stourhead_AFI_pre)), "_")[[1]][1]

plot_data_fun<-function(Stourhead_AFI_pre){
  
  Plotid<- unique(Stourhead_AFI_pre[[1]]$idplots)
  Plotid<-Plotid[order(Plotid)]
  Developmental_Stage<-Stourhead_AFI_pre[[1]]$Development.stage[1]


  info_plot<-data.frame(Forest=NA,
                      Plot = NA,
                      Specie = NA,
                      Age= NA,
                      Basal_Area = NA,
                      Average_DBH=NA,
                      Top_Height= NA,
                      Mean_Height = NA,
                      Stocking=NA,
                      Developmental_Stage = NA)
  for (i in Plotid){
    species<-unique(Stourhead_AFI_pre[[1]][Stourhead_AFI_pre[[1]]$idplots==i ,]$species)
    for(f in species){
    
      BASAL<-sum(Stourhead_AFI_pre[[1]][Stourhead_AFI_pre[[1]]$idplots==i & Stourhead_AFI_pre[[1]]$species==f,]$RepreBAHA)
      mean_height<-mean(Stourhead_AFI_pre[[1]][Stourhead_AFI_pre[[1]]$idplots==i & Stourhead_AFI_pre[[1]]$species==f,]$Height_m,na.rm=T)
      stocking_ha<- sum(Stourhead_AFI_pre[[1]][Stourhead_AFI_pre[[1]]$idplots==i & Stourhead_AFI_pre[[1]]$species==f,]$expansionfactor)
      mean_DBH<- mean(Stourhead_AFI_pre[[1]][Stourhead_AFI_pre[[1]]$idplots==i & Stourhead_AFI_pre[[1]]$species==f,]$dbhcm)
      info_plot_specie<- data.frame(Forest=Stourhead_AFI_pre[[1]]$Site_code[1],
             Plot = i,
             Specie = f,
             Age= NA,
             Basal_Area = BASAL,
             Average_DBH=mean_DBH,
             Top_Height= NA,
             Mean_Height = mean_height,
             Stocking_ha=stocking_ha,
             Developmental_Stage = Developmental_Stage)
     info_plot<-rbind(info_plot,info_plot_specie)
   }
  }
return(info_plot)
  
}

Stourhead_AFI_pre1<-Stourhead_AFI_pre
Stourhead_AFI_pre<-Stourhead_AFI_pre1[[1]]

plot_data_fun<-function(Stourhead_AFI_pre){
  
  Plotid<- unique(Stourhead_AFI_pre$idplots)
  Plotid<-Plotid[order(Plotid)]
  Developmental_Stage<-Stourhead_AFI_pre$Development.stage[1]
  
  
  info_plot<-data.frame(Forest=NA,
                        Plot = NA,
                        Specie = NA,
                        Age= NA,
                        Basal_Area = NA,
                        Average_DBH=NA,
                        Top_Height= NA,
                        Mean_Height = NA,
                        Stocking_ha=NA,
                        Developmental_Stage = NA)
  for (i in Plotid){
    species<-unique(Stourhead_AFI_pre[Stourhead_AFI_pre$idplots==i ,]$species)
    for(f in species){
      
      BASAL<-sum(Stourhead_AFI_pre[Stourhead_AFI_pre$idplots==i & Stourhead_AFI_pre$species==f,]$RepreBAHA)
      mean_height<-mean(Stourhead_AFI_pre[Stourhead_AFI_pre$idplots==i & Stourhead_AFI_pre$species==f,]$Height_m,na.rm=T)
      
      top_height_data<- Stourhead_AFI_pre[Stourhead_AFI_pre$idplots==i & Stourhead_AFI_pre$species==f ,]
      top_height<-  top_height_data[max.col(t(top_height_data$dbhcm)),]$Height_m 
      
      stocking_ha<- sum(Stourhead_AFI_pre[Stourhead_AFI_pre$idplots==i & Stourhead_AFI_pre$species==f,]$expansionfactor)
      mean_DBH<- mean(Stourhead_AFI_pre[Stourhead_AFI_pre$idplots==i & Stourhead_AFI_pre$species==f,]$dbhcm)
      info_plot_specie<- data.frame(Forest=Stourhead_AFI_pre$Site_code[1],
                                    Plot = i,
                                    Specie = f,
                                    Age= NA,
                                    Basal_Area = BASAL,
                                    Average_DBH=mean_DBH,
                                    Top_Height= top_height,
                                    Mean_Height = mean_height,
                                    Stocking_ha=stocking_ha,
                                    Developmental_Stage = Developmental_Stage)
      info_plot<-rbind(info_plot,info_plot_specie)
    }
  }
  return(info_plot)
  
}
View(Berth_Ddu_AFI_pre[[1]][Berth_Ddu_AFI_pre[[1]]$idplots==1,])
  
forest_info<-list(Monivea_AFI_pre[[1]],
                  Mellory_AFI_pre[[1]],
                  Stourhead_AFI_pre[[1]],
Knockrath_AFI_pre[[1]],
Rushmore_AFI_pre[[1]],
Berth_Ddu_AFI_pre[[1]],
Cranborne_208_p_1[[1]],
Cranborne_208_p_2[[1]],
Cranborne_37c_p[[1]],
Rushmore_l3_p[[1]],
Rushmore_m9_p[[1]],
Rushmore_b2_p[[1]],
Crichel_p[[1]],
llchester_47_p[[1]],
Baronscourt_AFI_pre[[1]])

t<-lapply(forest_info,plot_data_fun)
#t
for (i in 1:length(t)){
  t[[i]]<-t[[i]][rowSums(is.na(t[[i]])) != ncol(t[[i]]), ]
}
# add age
t[[1]]$Age<-"1952"
t[[2]]$Age<-"1967"
t[[3]]$Age<-"Irr"
t[[5]]$Age<-"47"
t[[6]]$Age<-"24"
t[[7]]$Age<-"50"
t[[8]]$Age<-"55"
t[[8]]$Forest<-"208, 213a Boulsbury (cycle 2)"
t[[9]]$Age<-"42"
t[[10]]$Age<-"56"
t[[11]]$Age<-"65"
t[[12]]$Age<-"54"
t[[13]]$Age<-"23"
t[[14]]$Age<-"44"

ISN_AFI_plot_summary<-t[[1]]
for(i in 2:length(t)){
  ISN_AFI_plot_summary<-rbind(ISN_AFI_plot_summary,t[[i]])
}



Forest_name<- strsplit(deparse(substitute(Stourhead_AFI_pre)), "_")[[1]][1]  
plot_data_fun(Knockrath_AFI_pre,Forest_name)




library(openxlsx)
write.xlsx(ISN_AFI_data, 'ISN_AFI_Forest_data.xlsx')

View(ISN_AFI_plot_summary)



######
#
#
#
##   addition of arbres to AFI
#
#########

Arbres<-read_excel("D:/FERS Project/STVI/DATA/Arbres.xlsx",sheet = "Arbres")


#View(Arbres)


###############################################################################################################################################################
#67 stourhead_data

# find which cycles are where 
stourhead_data$Cycle # 3 and 4 
Arbres[Arbres$NumDisp==67,]$Cycle # 1,2 and 3

#View(AFI_ISN_Data[AFI_ISN_Data$Site_code==67 & AFI_ISN_Data$idplots==12 & AFI_ISN_Data$Cycle %in% c(2,3) & AFI_ISN_Data$cohort_name=="Slow-growing broadleaves",])
# store data in data.frames to work with
stourhead_cycle_1_2_3<-Arbres[Arbres$NumDisp==67,]
colnames(stourhead_cycle_1_2_3)[which(colnames(stourhead_cycle_1_2_3)=="Placette" )]<-"NumPlac"

# run initial preprocessing 

NA_coord<-stourhead_cycle_1_2_3[is.na(stourhead_cycle_1_2_3$Azimut),c("NumPlac","NumArbre")]
NA_coord_2<-stourhead_cycle_1_2_3[is.na(stourhead_cycle_1_2_3$Distance),c("NumPlac","NumArbre")]
NA_coord_id<-paste(NA_coord$NumPlac,NA_coord$NumArbre)
NA_coord_2_id<-paste(NA_coord_2$NumPlac,NA_coord_2$NumArbre)

stourhead_cycle_1_2_3$id<-paste(stourhead_cycle_1_2_3$NumPlac,stourhead_cycle_1_2_3$NumArbre)

for( i in NA_coord_2_id){
  Azi<- stourhead_cycle_1_2_3[stourhead_cycle_1_2_3$id==i,]$Azimut
  Dis<-stourhead_cycle_1_2_3[stourhead_cycle_1_2_3$id==i,]$Distance
  if(!is_empty(Azi[!is.na(Azi)])){
    stourhead_cycle_1_2_3[stourhead_cycle_1_2_3$id==i,]$Azimut<-unique(Azi[!is.na(Azi)])
  }
  if(!is_empty(Dis[!is.na(Dis)])){
    stourhead_cycle_1_2_3[stourhead_cycle_1_2_3$id==i,]$Distance<-unique(Dis[!is.na(Dis)])
  }
}
stourhead_cycle_1_2_3<-stourhead_cycle_1_2_3[!is.na(stourhead_cycle_1_2_3$Azimut),]


stourhead_cycle_123_AFI<- AFI_Version2(stourhead_cycle_1_2_3,2006,dataset_AFI_raw,"204 Dropping Gutter")

#combine data
stourhead_cycle_123_AFI_PRE<-rbind(stourhead_cycle_123_AFI[stourhead_cycle_123_AFI$Cycle<=2,],Stourhead_AFI)
########################
stourhead_model
stourhead_cycle_123_AFI_PRE$coeff_a<-NA
stourhead_cycle_123_AFI_PRE$coeff_b<-NA
for( i in stourhead_model$cohort_name){
  stourhead_cycle_123_AFI_PRE[stourhead_cycle_123_AFI_PRE$cohort_name==i,]$coeff_a<-stourhead_model[stourhead_model$cohort_name==i,"coeff_a"]
  stourhead_cycle_123_AFI_PRE[stourhead_cycle_123_AFI_PRE$cohort_name==i,]$coeff_b<-stourhead_model[stourhead_model$cohort_name==i,"coeff_b"]
}

View(stourhead_cycle_123_AFI_PRE)



########################
#remove trees from qc check that was done in code below 
#stourhead_cycle_123_AFI_PRE<-stourhead_cycle_123_AFI_PRE[!(stourhead_cycle_123_AFI_PRE$TREE_NAME %in%Trees_to_remove),]
########################
#add correct years
View(stourhead_cycle_123_AFI_PRE[stourhead_cycle_123_AFI_PRE$Limit==0 & is.na(stourhead_cycle_123_AFI_PRE$dbhcm),])

stourhead_cycle_123_AFI_PRE[stourhead_cycle_123_AFI_PRE$Cycle==2,]$Date_measured<-2011
stourhead_cycle_123_AFI_PRE[stourhead_cycle_123_AFI_PRE$Cycle==3,]$Date_measured<-2016
stourhead_cycle_123_AFI_PRE[stourhead_cycle_123_AFI_PRE$Cycle==4,]$Date_measured<-2021

# run dbh increment function on data for each cycle
test1<-dbh_inc_yrs(stourhead_cycle_123_AFI_PRE[stourhead_cycle_123_AFI_PRE$Cycle %in% c(1,2),],5,2,1)
test2<-dbh_inc_yrs(stourhead_cycle_123_AFI_PRE[stourhead_cycle_123_AFI_PRE$Cycle %in% c(2,3),],5,3,2)
test3<-dbh_inc_yrs(stourhead_cycle_123_AFI_PRE[stourhead_cycle_123_AFI_PRE$Cycle %in% c(4,3),],5,4,3)
 


# add last cycle to data as this will not be included
stage_4<-stourhead_cycle_123_AFI_PRE[stourhead_cycle_123_AFI_PRE$Cycle==4,]
stage_4<-stage_4[stage_4$Limit==0,]
stage_4<-stage_4[!is.na(stage_4$dbhcm),]
not_included<- stage_4[stage_4$Limit==1,]

  stage_4_preproc<- Preprocessing_FERS(stage_4[stage_4$Limit==0,])
  d<-NROW(stage_4_preproc)
  if(NROW(not_included)>0){
    stage_4_preproc[(d+1):(d+nrow(not_included)),] <- NA#
    stage_4_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
  }



colnames(test3[[1]])[!c(colnames(test3[[1]]) %in% colnames(stage_4_preproc))]

# add columns "Initial_dbhcm"     "dbh_act_increment" "error"  so it can be binded together
stage_4_preproc$Initial_dbhcm<-NA
stage_4_preproc$dbh_act_increment<-NA
stage_4_preproc$error_Carbware_dbh_inc <-NA
stage_4_preproc$error_Carbware_average_inc<-NA
stage_4_preproc$average_est_inc<- NA
########
Finalized_stourhead_67<-rbind(test1[[1]],test2[[1]],test3[[1]],stage_4_preproc)

########
#qc the data to ensure there are no bad entries
View(Finalized_stourhead_67)

Trees_with_NA_xcoord<-Finalized_stourhead_67[is.na(Finalized_stourhead_67$X),"TREE_NAME"]
for (i in Trees_with_NA_xcoord){
 xcoord_value<- Finalized_stourhead_67[Finalized_stourhead_67$TREE_NAME==i,"X"]
 if(!is_empty(xcoord_value[!is.na(xcoord_value)])){
Finalized_stourhead_67[Finalized_stourhead_67$TREE_NAME==i,"X"]<-unique(xcoord_value[!is.na(xcoord_value)])
}

ycoord_value<- Finalized_stourhead_67[Finalized_stourhead_67$TREE_NAME==i,"Y"]
if(!is_empty(ycoord_value[!is.na(ycoord_value)])){
Finalized_stourhead_67[Finalized_stourhead_67$TREE_NAME==i,"Y"]<-unique(ycoord_value[!is.na(ycoord_value)])
}
}

Trees_to_remove<-Finalized_stourhead_67[is.na(Finalized_stourhead_67$X),"TREE_NAME"]
negative_incr<-Finalized_stourhead_67[Finalized_stourhead_67$dbh_act_increment<=0 & !is.na(Finalized_stourhead_67$dbh_act_increment),"TREE_NAME"]
Trees_to_remove<-c(Trees_to_remove,negative_incr)
rm(Trees_to_remove)

###############################################################################################################################################################
#102
Monivea$Cycle # 2 and 3
Arbres[Arbres$NumDisp==102,]$Cycle # 1

Monivea_cycle_1<-Arbres[Arbres$NumDisp==102,]
colnames(Monivea_cycle_1)[which(colnames(Monivea_cycle_1)=="Placette" )]<-"NumPlac"
Monivea_cycle_1_AFI<- AFI_Version2(Monivea_cycle_1,2011,dataset_AFI_raw,"Monivea")
View(Monivea_cycle_1_AFI)
Monivea_cycle_1_PRE<-rbind(Monivea_cycle_1_AFI,Monivea_AFI)
########################
#remove trees 
#Monivea_cycle_1_PRE<-Monivea_cycle_1_PRE[!(Monivea_cycle_1_PRE$TREE_NAME %in% negative_incr_M),]
########################
#dates
Monivea_cycle_1_PRE[Monivea_cycle_1_PRE$Cycle==2,]$Date_measured<-2016
Monivea_cycle_1_PRE[Monivea_cycle_1_PRE$Cycle==3,]$Date_measured<-2021

# add coeff a and b for estimate height model
Monivea_cycle_1_PRE$coeff_a<-NA
Monivea_cycle_1_PRE$coeff_b<-NA
for( i in Monivea_model$cohort_name){
  Monivea_cycle_1_PRE[Monivea_cycle_1_PRE$cohort_name==i,]$coeff_a<-Monivea_model[Monivea_model$cohort_name==i,"coeff_a"]
  Monivea_cycle_1_PRE[Monivea_cycle_1_PRE$cohort_name==i,]$coeff_b<-Monivea_model[Monivea_model$cohort_name==i,"coeff_b"]
}
test1<-dbh_inc_yrs(Monivea_cycle_1_PRE[Monivea_cycle_1_PRE$Cycle %in% c(1,2),],5,2,1)
test2<-dbh_inc_yrs(Monivea_cycle_1_PRE[Monivea_cycle_1_PRE$Cycle %in% c(2,3),],5,3,2)

# add last cycle to data as this will not be included
stage_3<-Monivea_cycle_1_PRE[Monivea_cycle_1_PRE$Cycle==3,]
#stage_3<-stage_3[stage_3$Limit==0,]
stage_3<-stage_3[!is.na(stage_3$dbhcm),]

not_included<- stage_3[stage_3$Limit==1,]

stage_3_preproc<- Preprocessing_FERS(stage_3[stage_3$Limit==0,])
d<-NROW(stage_3_preproc)
if(NROW(not_included)>0){
  stage_3_preproc[(d+1):(d+nrow(not_included)),] <- NA#
  stage_3_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
}




stage_3_preproc$Initial_dbhcm<-NA
stage_3_preproc$dbh_act_increment<-NA
stage_3_preproc$error_Carbware_dbh_inc <-NA
stage_3_preproc$error_Carbware_average_inc<-NA
stage_3_preproc$average_est_inc<- NA
#
Finalized_Monivea_102<-rbind(test1[[1]],test2[[1]],stage_3_preproc)
View(Finalized_Monivea_102)
# qc 
negative_incr_M<-Finalized_Monivea_102[Finalized_Monivea_102$dbh_act_increment<=0 & !is.na(Finalized_Monivea_102$dbh_act_increment),"TREE_NAME"]
rm(negative_incr_M)


###############################################################################################################################################################
#103
Mellory$Cycle # 2 and 3
Arbres[Arbres$NumDisp==103,]$Cycle # 1

Mellory_cycle_1_PRE<-Mellory_cycle_1_PRE[Mellory_cycle_1_PRE$TREE_NAME!=i,]

Mellory_cycle_1<-Arbres[Arbres$NumDisp==103,]
colnames(Mellory_cycle_1)[which(colnames(Mellory_cycle_1)=="Placette" )]<-"NumPlac"

Mellory_cycle_1_AFI<- AFI_Version2(Mellory_cycle_1,2011,dataset_AFI_raw,"Mellory")
Mellory_cycle_1_PRE<-rbind(Mellory_cycle_1_AFI,Mellory_AFI)
########################
#remove
#Mellory_cycle_1_PRE<-Mellory_cycle_1_PRE[!(Mellory_cycle_1_PRE$TREE_NAME %in% negative_incr_M),]
########################
#dates
Mellory_cycle_1_PRE[Mellory_cycle_1_PRE$Cycle==2,]$Date_measured<-2016
Mellory_cycle_1_PRE[Mellory_cycle_1_PRE$Cycle==3,]$Date_measured<-2021

#
add_cr_after<-Mellory_cycle_1_PRE[Mellory_cycle_1_PRE$cr<0 & !is.na(Mellory_cycle_1_PRE$cr),c("TREE_NAME","cr")]

Mellory_cycle_1_PRE[Mellory_cycle_1_PRE$cr<0 & !is.na(Mellory_cycle_1_PRE$cr),c("cr")]<-NA

Mellory_cycle_1_PRE[Mellory_cycle_1_PRE$Height_m>200 & !is.na(Mellory_cycle_1_PRE$Height_m),]$Height_m<-NA

test1<-dbh_inc_yrs(Mellory_cycle_1_PRE[Mellory_cycle_1_PRE$Cycle %in% c(1,2),],5,2,1)

test1[[1]][test1[[1]]$TREE_NAME %in% add_cr_after$TREE_NAME,]$cr<-add_cr_after$cr

# add coeff a and b for estimate height model
Mellory_cycle_1_PRE$coeff_a<-NA
Mellory_cycle_1_PRE$coeff_b<-NA
for( i in Mellory_model$cohort_name){
  Mellory_cycle_1_PRE[Mellory_cycle_1_PRE$cohort_name==i,]$coeff_a<-Mellory_model[Mellory_model$cohort_name==i,"coeff_a"]
  Mellory_cycle_1_PRE[Mellory_cycle_1_PRE$cohort_name==i,]$coeff_b<-Mellory_model[Mellory_model$cohort_name==i,"coeff_b"]
}
##

test2<-dbh_inc_yrs(Mellory_cycle_1_PRE[Mellory_cycle_1_PRE$Cycle %in% c(2,3),],5,3,2)

# add last cycle to data as this will not be included
stage_3<-Mellory_cycle_1_PRE[Mellory_cycle_1_PRE$Cycle==3,]
#stage_3<-stage_3[stage_3$Limit==0,]
stage_3<-stage_3[!is.na(stage_3$dbhcm),]

not_included<- stage_3[stage_3$Limit==1,]

stage_3_preproc<- Preprocessing_FERS(stage_3[stage_3$Limit==0,])
d<-NROW(stage_3_preproc)
if(NROW(not_included)>0){
  stage_3_preproc[(d+1):(d+nrow(not_included)),] <- NA#
  stage_3_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
}





stage_3_preproc$Initial_dbhcm<-NA
stage_3_preproc$dbh_act_increment<-NA
stage_3_preproc$error_Carbware_dbh_inc <-NA
stage_3_preproc$error_Carbware_average_inc<-NA
stage_3_preproc$average_est_inc<- NA
#
Finalized_Mellory_103<-rbind(test1[[1]],test2[[1]],stage_3_preproc)
View(Finalized_Mellory_103)
# qc 
negative_incr_M<-Finalized_Mellory_103[Finalized_Mellory_103$dbh_act_increment<=0 & !is.na(Finalized_Mellory_103$dbh_act_increment),"TREE_NAME"]
negative_incr_M<-c(negative_incr_M,Finalized_Mellory_103[Finalized_Mellory_103$dbh_act_increment>1000 & !is.na(Finalized_Mellory_103$dbh_act_increment),"TREE_NAME"])
rm(negative_incr_M)
###############################################################################################################################################################
#106
Knockrath$Cycle # 2 and 3
Knockrath<-Knockrath[!is.na(Knockrath$Cycle),]
Arbres[Arbres$NumDisp==106,]$Cycle # 1
Knockrath_cycle_1<-Arbres[Arbres$NumDisp==106,]
colnames(Knockrath_cycle_1)[which(colnames(Knockrath_cycle_1)=="Placette" )]<-"NumPlac"

Knockrath_AFI<-AFI_Version3(Knockrath,2011,dataset_AFI_raw,"19/20 Knockrath")

Knockrath_cycle_1_AFI<-AFI_Version2(Knockrath_cycle_1,2011,dataset_AFI_raw,"19/20 Knockrath")
Knockrath_cycle_1_PRE<-rbind(Knockrath_cycle_1_AFI,Knockrath_AFI)

Knockrath_cycle_1_PRE[Knockrath_cycle_1_PRE$Cycle==2,]$Date_measured<-2016
Knockrath_cycle_1_PRE[Knockrath_cycle_1_PRE$Cycle==3,]$Date_measured<-2021
########################
#remove
#Knockrath_cycle_1_PRE<-Knockrath_cycle_1_PRE[!(Knockrath_cycle_1_PRE$TREE_NAME %in% negative_incr_M),]
########################
# add coeff a and b for estimate height model
Knockrath_cycle_1_PRE$coeff_a<-NA
Knockrath_cycle_1_PRE$coeff_b<-NA
for( i in Knockrath_model$cohort_name){
  Knockrath_cycle_1_PRE[Knockrath_cycle_1_PRE$cohort_name==i,]$coeff_a<-Knockrath_model[Knockrath_model$cohort_name==i,"coeff_a"]
  Knockrath_cycle_1_PRE[Knockrath_cycle_1_PRE$cohort_name==i,]$coeff_b<-Knockrath_model[Knockrath_model$cohort_name==i,"coeff_b"]
}
##
test1<-dbh_inc_yrs(Knockrath_cycle_1_PRE[Knockrath_cycle_1_PRE$Cycle %in% c(1,2),],5,2,1)
test2<-dbh_inc_yrs(Knockrath_cycle_1_PRE[Knockrath_cycle_1_PRE$Cycle %in% c(2,3),],5,3,2)
# add last cycle to data as this will not be included
stage_3<-Knockrath_cycle_1_PRE[Knockrath_cycle_1_PRE$Cycle==3,]
#stage_3<-stage_3[stage_3$Limit==0,]
stage_3<-stage_3[!is.na(stage_3$dbhcm),]

not_included<- stage_3[stage_3$Limit==1,]

stage_3_preproc<- Preprocessing_FERS(stage_3[stage_3$Limit==0,])
d<-NROW(stage_3_preproc)
if(NROW(not_included)>0){
  stage_3_preproc[(d+1):(d+nrow(not_included)),] <- NA#
  stage_3_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
}


stage_3_preproc$Initial_dbhcm<-NA
stage_3_preproc$dbh_act_increment<-NA
stage_3_preproc$error_Carbware_dbh_inc <-NA
stage_3_preproc$error_Carbware_average_inc<-NA
stage_3_preproc$average_est_inc<- NA
#
Finalized_Knockrath_106<-rbind(test1[[1]],test2[[1]],stage_3_preproc)
View(Finalized_Knockrath_106)
plot(Finalized_Knockrath_106$Height_m,Finalized_Knockrath_106$Estim_Ht,xlab="Recorded Height",ylab="Estimated height",main="Recorded vs estimated height with line of slope = 1.")
abline(coef=c(0,1))
# qc 
negative_incr_M<-Finalized_Knockrath_106[Finalized_Knockrath_106$dbh_act_increment<=0 & !is.na(Finalized_Knockrath_106$dbh_act_increment),"TREE_NAME"]
rm(negative_incr_M)
table(Finalized_Knockrath_106$cohort_name)
###############################################################################################################################################################

#rushmore
#Rushmore_AFI<-Rushmore_AFI[Rushmore_AFI$TREE_NAME!="1 102",]
Rushmore_AFI[Rushmore_AFI$Cycle==1,]$Date_measured<-2012
Rushmore_AFI[Rushmore_AFI$Cycle==2,]$Date_measured<-2017
########################
#remove
#Rushmore_AFI<-Rushmore_AFI[!(Rushmore_AFI$TREE_NAME %in% neg_inc), ]
########################
# add coeff a and b for estimate height model
Rushmore_AFI$coeff_a<-NA
Rushmore_AFI$coeff_b<-NA
for( i in Rushmore_model$cohort_name){
  Rushmore_AFI[Rushmore_AFI$cohort_name==i,]$coeff_a<-Rushmore_model[Rushmore_model$cohort_name==i,"coeff_a"]
  Rushmore_AFI[Rushmore_AFI$cohort_name==i,]$coeff_b<-Rushmore_model[Rushmore_model$cohort_name==i,"coeff_b"]
}
##
Rushmore_AFI_pre<-dbh_inc_yrs(Rushmore_AFI,5,2,1)


# add last cycle to data as this will not be included
stage_2<-Rushmore_AFI[Rushmore_AFI$Cycle==2,]
#stage_3<-stage_3[stage_3$Limit==0,]
stage_2<-stage_2[!is.na(stage_2$dbhcm),]

not_included<- stage_2[stage_2$Limit==1,]

stage_2_preproc<- Preprocessing_FERS(stage_2[stage_2$Limit==0,])
d<-NROW(stage_2_preproc)
if(NROW(not_included)>0){
  stage_2_preproc[(d+1):(d+nrow(not_included)),] <- NA#
  stage_2_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
}


stage_2_preproc$Initial_dbhcm<-NA
stage_2_preproc$dbh_act_increment<-NA
stage_2_preproc$error_Carbware_dbh_inc <-NA
stage_2_preproc$error_Carbware_average_inc<-NA
stage_2_preproc$average_est_inc<- NA

Rushmore_AFI_final<-rbind(Rushmore_AFI_pre[[1]],stage_2_preproc)
View(Rushmore_AFI_final)
neg_inc<-Rushmore_AFI_pre[[1]][Rushmore_AFI_pre[[1]]$dbh_act_increment<=0 & !is.na(Rushmore_AFI_pre[[1]]$dbh_act_increment),"TREE_NAME"]
rm(neg_inc)
###############################################################################################################################################################
# berth ddu
Berth_Ddu_AFI<-AFI_Version3(Berth_Ddu,2015,dataset_AFI_raw,"2/3 Berth Ddu")

Berth_Ddu_AFI[Berth_Ddu_AFI$Cycle==1,]$Date_measured<-2015
Berth_Ddu_AFI[Berth_Ddu_AFI$Cycle==2,]$Date_measured<-2021
########################
#remove
#Berth_Ddu_AFI<-Berth_Ddu_AFI[!(Berth_Ddu_AFI$TREE_NAME %in% neg_inc),]
########################
# add coeff a and b for estimate height model
Berth_Ddu_AFI$coeff_a<-NA
Berth_Ddu_AFI$coeff_b<-NA
for( i in Berth_Ddu_model$cohort_name){
  Berth_Ddu_AFI[Berth_Ddu_AFI$cohort_name==i,]$coeff_a<-Berth_Ddu_model[Berth_Ddu_model$cohort_name==i,"coeff_a"]
  Berth_Ddu_AFI[Berth_Ddu_AFI$cohort_name==i,]$coeff_b<-Berth_Ddu_model[Berth_Ddu_model$cohort_name==i,"coeff_b"]
}
##
Berth_Ddu_AFI_pre<-dbh_inc_yrs(Berth_Ddu_AFI,6,2,1)


# add last cycle to data as this will not be included
stage_2<-Berth_Ddu_AFI[Berth_Ddu_AFI$Cycle==2,]
#stage_2<-stage_2[stage_2$Limit==0,]
stage_2<-stage_2[!is.na(stage_2$dbhcm),]

not_included<- stage_2[stage_2$Limit==1,]

stage_2_preproc<- Preprocessing_FERS(stage_2[stage_2$Limit==0,])
d<-NROW(stage_2_preproc)
if(NROW(not_included)>0){
  stage_2_preproc[(d+1):(d+nrow(not_included)),] <- NA#
  stage_2_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
}

stage_2_preproc$Initial_dbhcm<-NA
stage_2_preproc$dbh_act_increment<-NA
stage_2_preproc$error_Carbware_dbh_inc <-NA
stage_2_preproc$error_Carbware_average_inc<-NA
stage_2_preproc$average_est_inc<- NA

Berth_Ddu_AFI_final<-rbind(Berth_Ddu_AFI_pre[[1]],stage_2_preproc)
View(Berth_Ddu_AFI_final)
neg_inc<-Berth_Ddu_AFI_pre[[1]][Berth_Ddu_AFI_pre[[1]]$dbh_act_increment<=0 & !is.na(Berth_Ddu_AFI_pre[[1]]$dbh_act_increment),"TREE_NAME"]


View(Berth_Ddu_AFI_pre[[1]])


colnames(Berth_Ddu_AFI_pre[[1]])

###############################################################################################################################################################
# barons court 
unique(Baronscourt_AFI$Cycle)
#2012 to 2016
plots_remove<-unique(Baronscourt_AFI[Baronscourt_AFI$cohort_name %in% c("Spruce","Douglas fir") & is.na(Baronscourt_AFI$YC),]$idplots)
Baronscourt_AFI<-Baronscourt_AFI[!(Baronscourt_AFI$idplots %in% plots_remove),]
unique(Baronscourt_AFI$idplots)

Baronscourt_AFI_pre<-dbh_inc_yrs(Baronscourt_AFI,5,2,1)
View(AFI_ISN_Data[AFI_ISN_Data$Site_code=="Baronscourt" & AFI_ISN_Data$idplots %in% plots_remove,])

stage_2<-Baronscourt_AFI[Baronscourt_AFI$Cycle==2,]
#stage_3<-stage_3[stage_3$Limit==0,]
stage_2<-stage_2[!is.na(stage_2$dbhcm),]

not_included<- stage_2[stage_2$Limit==1,]

stage_2_preproc<- Preprocessing_FERS(stage_2[stage_2$Limit==0,])
d<-NROW(stage_2_preproc)
if(NROW(not_included)>0){
  stage_2_preproc[(d+1):(d+nrow(not_included)),] <- NA#
  stage_2_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
}

stage_2_preproc$Initial_dbhcm<-NA
stage_2_preproc$dbh_act_increment<-NA
stage_2_preproc$error_Carbware_dbh_inc <-NA
stage_2_preproc$error_Carbware_average_inc<-NA
stage_2_preproc$average_est_inc<- NA




Baronscourt_AFI_pre_final<-rbind(Baronscourt_AFI_pre[[1]],stage_2_preproc)

Baronscourt_AFI_pre_final$Cycle

rm(plots_remove)

###############################################################################################################################################################
unique(Bryn_Arau_Duon_isn$Site_code)
unique(Bryn_Arau_Duon_isn$Cycle)
plots_remove<-unique(Bryn_Arau_Duon_isn[Bryn_Arau_Duon_isn$cohort_name %in% c("Spruce","Douglas fir") & is.na(Bryn_Arau_Duon_isn$YC),]$idplots)
Bryn_Arau_Duon_isn<-Bryn_Arau_Duon_isn[!(Bryn_Arau_Duon_isn$idplots %in% plots_remove),]


Bryn_Arau_Duon_isn$TREE_NAME<-paste(Bryn_Arau_Duon_isn$idplots,Bryn_Arau_Duon_isn$Tree_ID)


Bryn_Arau_Duon_isn_pre<-dbh_inc_yrs(Bryn_Arau_Duon_isn,8,2,1)



stage_2<-Bryn_Arau_Duon_isn[Bryn_Arau_Duon_isn$Cycle==2,]
#stage_3<-stage_3[stage_3$Limit==0,]
stage_2<-stage_2[!is.na(stage_2$dbhcm),]

not_included<- stage_2[stage_2$Limit==1,]

stage_2_preproc<- Preprocessing_FERS(stage_2[stage_2$Limit==0,])
d<-NROW(stage_2_preproc)
if(NROW(not_included)>0){
  stage_2_preproc[(d+1):(d+nrow(not_included)),] <- NA#
  stage_2_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
}

stage_2_preproc$Initial_dbhcm<-NA
stage_2_preproc$dbh_act_increment<-NA
stage_2_preproc$error_Carbware_dbh_inc <-NA
stage_2_preproc$error_Carbware_average_inc<-NA
stage_2_preproc$average_est_inc<- NA





Bryn_Arau_Duon_pre_final<-rbind(Bryn_Arau_Duon_isn_pre[[1]],stage_2_preproc)

table(Bryn_Arau_Duon_pre_final$species)
rm(plots_remove)
###############################################################################################################################################################
Llethir_Gwinau
Llethir_Gwinau$YC<-as.numeric(Llethir_Gwinau$YC)

plots_remove<-unique(Llethir_Gwinau[Llethir_Gwinau$cohort_name %in% c("Spruce","Douglas fir") & is.na(Llethir_Gwinau$YC),]$idplots)
Llethir_Gwinau<-Llethir_Gwinau[!(Llethir_Gwinau$idplots %in% plots_remove),]
unique(Llethir_Gwinau$YC)

Llethir_Gwinau$TREE_NAME<-paste(Llethir_Gwinau$idplots,Llethir_Gwinau$Tree_ID)

Llethir_Gwinau_pre<-dbh_inc_yrs(Llethir_Gwinau,4,2,1)

stage_2<-Llethir_Gwinau[Llethir_Gwinau$Cycle==2,]
#stage_3<-stage_3[stage_3$Limit==0,]
stage_2<-stage_2[!is.na(stage_2$dbhcm),]

not_included<- stage_2[stage_2$Limit==1,]

stage_2_preproc<- Preprocessing_FERS(stage_2[stage_2$Limit==0,])
d<-NROW(stage_2_preproc)
if(NROW(not_included)>0){
  stage_2_preproc[(d+1):(d+nrow(not_included)),] <- NA#
  stage_2_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
}

stage_2_preproc$Initial_dbhcm<-NA
stage_2_preproc$dbh_act_increment<-NA
stage_2_preproc$error_Carbware_dbh_inc <-NA
stage_2_preproc$error_Carbware_average_inc<-NA
stage_2_preproc$average_est_inc<- NA





Llethir_Gwinau_pre_final<-rbind(Llethir_Gwinau_pre[[1]],stage_2_preproc)


###############################################################################################################################################################
View(Nant_yr_Eira_isn)
for(i in unique(Nant_yr_Eira_isn$idplots)){
  Nant_yr_Eira_isn[Nant_yr_Eira_isn$idplots==i,]$YC<-Nant_yr_Eira_isn[Nant_yr_Eira_isn$idplots==i &
                                                                        !is.na(Nant_yr_Eira_isn$YC),]$YC[1]
  
}
plots_remove<-unique(Nant_yr_Eira_isn[Nant_yr_Eira_isn$cohort_name %in% c("Spruce","Douglas fir") & is.na(Nant_yr_Eira_isn$YC),]$idplots)
Nant_yr_Eira_isn<-Nant_yr_Eira_isn[!(Nant_yr_Eira_isn$idplots %in% plots_remove),]


Nant_yr_Eira_isn$TREE_NAME<-paste(Nant_yr_Eira_isn$idplots,Nant_yr_Eira_isn$Tree_ID)

Nant_yr_Eira_isn_pre<-dbh_inc_yrs(Nant_yr_Eira_isn,4,2,1)





stage_2<-Nant_yr_Eira_isn[Nant_yr_Eira_isn$Cycle==2,]
#stage_3<-stage_3[stage_3$Limit==0,]
stage_2<-stage_2[!is.na(stage_2$dbhcm),]

not_included<- stage_2[stage_2$Limit==1,]

stage_2_preproc<- Preprocessing_FERS(stage_2[stage_2$Limit==0,])
d<-NROW(stage_2_preproc)
if(NROW(not_included)>0){
  stage_2_preproc[(d+1):(d+nrow(not_included)),] <- NA#
  stage_2_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
}

stage_2_preproc$Initial_dbhcm<-NA
stage_2_preproc$dbh_act_increment<-NA
stage_2_preproc$error_Carbware_dbh_inc <-NA
stage_2_preproc$error_Carbware_average_inc<-NA
stage_2_preproc$average_est_inc<- NA





Nant_yr_Eira_isn_pre_final<-rbind(Nant_yr_Eira_isn_pre[[1]],stage_2_preproc)
Nant_yr_Eira_isn_pre_final$Site_code<-6
table(Baronscourt_AFI_pre_final$Site_code)
      table(Nant_yr_Eira_isn_pre_final$Site_code)
            table(Llethir_Gwinau_pre_final$Site_code)
                  table(Bryn_Arau_Duon_pre_final$Site_code)
###############################################################################################################################################################
                  ###############################################################################################################################################################
                  table(Nant_yr_Eira_INV$Species)
                  table(AFI_ISN_Data[AFI_ISN_Data$Site_code=="Nant yr Eira INV",]$species)
                  Nant_yr_Eira_INV_isn$Site_code<-"Nant yr Eira INV"
                  View(Nant_yr_Eira_INV_isn)
                  plots_remove<-unique(Nant_yr_Eira_INV_isn[Nant_yr_Eira_INV_isn$cohort_name %in% c("Spruce","Douglas fir") & is.na(Nant_yr_Eira_INV_isn$YC),]$idplots)
                  Nant_yr_Eira_INV_isn<-Nant_yr_Eira_INV_isn[!(Nant_yr_Eira_INV_isn$idplots %in% plots_remove),]
                  
                  
                  unique(Nant_yr_Eira_INV_isn$Date_measured)
                  Nant_yr_Eira_INV_isn$TREE_NAME<-paste(Nant_yr_Eira_INV_isn$idplots,Nant_yr_Eira_INV_isn$Tree_ID)
                  
                  Nant_yr_Eira_INV_isn_pre<-dbh_inc_yrs(Nant_yr_Eira_INV_isn,7,2,1)
                  
                  
                  
                  
                  
                  stage_2<-Nant_yr_Eira_INV_isn[Nant_yr_Eira_INV_isn$Cycle==2,]
                  #stage_3<-stage_3[stage_3$Limit==0,]
                  stage_2<-stage_2[!is.na(stage_2$dbhcm),]
                  
                  not_included<- stage_2[stage_2$Limit==1,]
                  
                  stage_2_preproc<- Preprocessing_FERS(stage_2[stage_2$Limit==0,])
                  d<-NROW(stage_2_preproc)
                  if(NROW(not_included)>0){
                    stage_2_preproc[(d+1):(d+nrow(not_included)),] <- NA#
                    stage_2_preproc[(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
                  }
                  
                  stage_2_preproc$Initial_dbhcm<-NA
                  stage_2_preproc$dbh_act_increment<-NA
                  stage_2_preproc$error_Carbware_dbh_inc <-NA
                  stage_2_preproc$error_Carbware_average_inc<-NA
                  stage_2_preproc$average_est_inc<- NA
                  
                  
                  
                  #View(Nant_yr_Eira_INV_isn_pre_final)
                  
                  Nant_yr_Eira_INV_isn_pre_final<-rbind(Nant_yr_Eira_INV_isn_pre[[1]],stage_2_preproc)
                  Nant_yr_Eira_INV_isn_pre_final$Site_code<-"Nant yr Eira INV"
                  
                  table(Baronscourt_AFI_pre_final$Site_code)
                  Nant_yr_Eira_isn_pre_final$Site_code<-"Nant yr Eira Wood"
                  
                  Llethir_Gwinau_pre_final$Site_code<-"Llethr Gwinau ISN"
                  table(Bryn_Arau_Duon_pre_final$Site_code)
                  Bryn_Arau_Duon_pre_final$Tree_
                  Nant_yr_Eira_INV_isn_pre_final$TreeName<-Nant_yr_Eira_INV_isn_pre_final$TREE_NAME
                  colnames(Bryn_Arau_Duon_pre_final)[!colnames(Bryn_Arau_Duon_pre_final) %in% colnames(Nant_yr_Eira_INV_isn_pre_final)]
                  ###############################################################################################################################################################
sdsdsd<-Rushmore


sdsdsd$Limit<-0
if(!is_empty(sdsdsd[sdsdsd$Diam1>30 & sdsdsd$Distance*0.03*100>sdsdsd$Diam1 & !is.na(sdsdsd$Diam1),]$Limit)){
  sdsdsd[sdsdsd$Diam1>30 & sdsdsd$Distance*0.03*100>sdsdsd$Diam1 & !is.na(sdsdsd$Diam1),]$Limit<-1
}

unique(AFI_ISN_Data$Site_code)
#bind all AFI data
AFI_Final_Data_new<- rbind(Finalized_stourhead_67,
                       Finalized_Monivea_102,
                       Finalized_Mellory_103,
                       Finalized_Knockrath_106,
                       Rushmore_AFI_final,
                       Berth_Ddu_AFI_final)


ISN_LRS_data<-      rbind(Nant_yr_Eira_isn_pre_final,
              Llethir_Gwinau_pre_final)

AFI_Final_Data$coeff_a

View(AFI_ISN_Data[AFI_ISN_Data$Site_code==102,])
#define type of data
AFI_Final_Data_new$Type<-"AFI"

ISN_Final_Data1$Type<-"ISN"
ISN_LRS_data$Type<-"ISN_LRS"
INV_data$Type<-"ISN_INV"
Baronscourt_AFI_pre_final$Type<-"ISN INV"
Nant_yr_Eira_INV_isn_pre_final$Type<-"ISN INV"
View(AFI_ISN_Data[AFI_ISN_Data$Site_code=="208, 213a Boulsbury" & AFI_ISN_Data$idplots==1,])
# extras data

Nant_yr_Eira_isn_pre_final$Type<-"ISN_LRS"
Llethir_Gwinau_pre_final$Type<-"ISN_LRS"
extra_Data<- rbind(Baronscourt_AFI_pre_final,
                   Nant_yr_Eira_isn_pre_final[,colnames(Nant_yr_Eira_isn_pre_final) %in% colnames(Baronscourt_AFI_pre_final)],
                   Llethir_Gwinau_pre_final[,colnames(Llethir_Gwinau_pre_final) %in% colnames(Baronscourt_AFI_pre_final)])
INV_data<- rbind(Nant_yr_Eira_INV_isn_pre_final,
                 Bryn_Arau_Duon_pre_final)



#bind all data
colnames(AFI_Final_Data_new ) %in% colnames(extra_Data)
INV_data1<-INV_data[,colnames(INV_data)[(colnames(INV_data) %in% colnames(AFI_Final_Data_new ))]]
#ISN_LRS_data<-ISN_LRS_data[,colnames(ISN_LRS_data)[(colnames(ISN_LRS_data) %in% colnames(AFI_Final_Data_new ))]]

###################################################################################
colnames(INV_data1)[!colnames(INV_data1) %in% colnames(AFI_Final_Data_new)]
extra_Data$SpeciesGroup<-extra_Data$cohort_name
extra_Data$Distance<-NA
colnames(AFI_Final_Data_new)[!colnames(AFI_Final_Data_new) %in% colnames(INV_data1)]

AFI_ISN_Data<-rbind(ISN_Final_Data1[,colnames(ISN_Final_Data1)[(colnames(ISN_Final_Data1) %in% colnames(AFI_Final_Data_new))]],
                    AFI_Final_Data_new,
                    INV_data1,
                    extra_Data[,colnames(extra_Data)[(colnames(extra_Data) %in% colnames(AFI_Final_Data_new))]])
unique(AFI_ISN_Data$Site_code)
AFI_ISN_Data<-AFI_ISN_Data[!(AFI_ISN_Data$Site_code %in% c("Nant yr Eira INV","Baronscourt")),]
AFI_ISN_Data<-rbind(AFI_ISN_Data,
                    Nant_yr_Eira_INV_isn_pre_final[,colnames(Nant_yr_Eira_INV_isn_pre_final)[(colnames(Nant_yr_Eira_INV_isn_pre_final) %in% colnames(AFI_ISN_Data))]],
                    Baronscourt_AFI_pre_final[,colnames(Baronscourt_AFI_pre_final)[(colnames(Baronscourt_AFI_pre_final) %in% colnames(AFI_ISN_Data))]])
###################################################################################

AFI_ISN_Data3<-rbind(ISN_Final_Data1[colnames(ISN_Final_Data1) %in% colnames(AFI_Final_Data_new)],
                    AFI_Final_Data_new)

colnames(ISN_Final_Data1)[!colnames(ISN_Final_Data1) %in% colnames(AFI_Final_Data_new)]
View(AFI_ISN_Data)
AFI_ISN_Data_0ld<-AFI_ISN_Data
colnames(AFI_ISN_Data)



order_of_cols<-c("Type","Date_measured" ,"Cycle","Site_code","idplots","Tree_ID","X",
 "Y","Plot.size..ha.","species","cohort_name","cr",
"dbhcm","expansionfactor","Height_m","YC","Development.stage",
 "Limit","TREE_NAME","Initial_dbhcm","Xi",
 "Density_ha","BASEL_M_HA","RepreBAHA","BAL","dbhcm_ties",
 "BAL_ties_adjusted_1" ,"BAL_ties_adjusted_2", "BAL_ties_adjusted_3", "plotba_m2_ha","Estim_Ht" ,         
"Height_imp","OGCD","OGCA","EP_OGCA","CCF",  
 "Logit_CR","C_R_est","C_R","DBH_Increment","dbh_act_increment","error_Carbware",
 "average_est_inc")


AFI_ISN_Data<-AFI_ISN_Data[,order_of_cols]
data.csv<-AFI_ISN_Data[AFI_ISN_Data$Site_code!="47/48/55 Llethr Gwinau",]
write.csv(data.csv,"AFI_ISN_Data_c")
??write.xlsx
AFI_ISN_Data$average_est_inc_error<-AFI_ISN_Data$dbh_act_increment-AFI_ISN_Data$average_est_inc
openxlsx:: write.xlsx(AFI_ISN_Data, 'AFI_ISN_Data.xlsx')



testin<-AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce" ,]
testing<-dbh_class_function(testin,tarif9)




ggplot()
means_stika<- data.frame(dbhclass=unique(Stikaspruce$dbhclass))
means_stika$dbh_act_increment<-NA
means_stika$dbh_increment<-NA
means_stika$N<-NA
for( i in means_stika$dbhclass){
  means_stika[means_stika$dbhclass==i,]$dbh_act_increment<-median(testing[testing$dbhclass==i,]$dbh_act_increment,na.rm=T)
  means_stika[means_stika$dbhclass==i,]$dbh_increment<-median(testing[testing$dbhclass==i,]$average_est_inc,na.rm=T)
  means_stika[means_stika$dbhclass==i,]$N<-NROW(testing[testing$dbhclass==i,])
  
}

means_stika<-means_stika[order(means_stika$dbhclass),]
df2<- melt(means_stika[,c("dbh_increment","dbh_act_increment","dbhclass")],id.vars = "dbhclass" )

df2<- df2[order(df2$dbhclass),]
ggplot(df2, aes(x=as.factor(dbhclass), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+ labs(x = "DBH class (cm)", y = "DBH increment",
                                                    title= "DBh Actual increment vs Estimated") #+ 
  geom_text(aes(label=n_forplot))

n_forplot<-c(12,12,22,22,35,35,50,50,65,65,33,33,16,16,4,4,7,7,1,1,2,2)


mean(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Douglas fir" ,]$error_Carbware,na.rm=T)
median(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Douglas fir" ,]$YC)

mean(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$dbh_act_increment,na.rm = T)
mean(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$DBH_Increment,na.rm = T)


median(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$dbh_act_increment,na.rm = T)
median(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$DBH_Increment,na.rm = T)


mean(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$ave_error,na.rm = T)
sd(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$ave_error,na.rm = T)
median(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$ave_error,na.rm = T)
max(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$ave_error,na.rm = T)
min(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$ave_error,na.rm = T)
lm_dbh<-lm(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$average_est_inc~AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$dbh_act_increment)
summary(lm_dbh)
plot(lm_dbh)


plot(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$RepreBAHA,AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$error_Carbware)


View(AFI_ISN_Data)


AFI_ISN_Data$ave_error<-AFI_ISN_Data$dbh_act_increment-AFI_ISN_Data$average_est_inc





####
# AIC()
#BIC()
####
























#stika scaling factors
# 16
# 19.3


#douglas scaling factors
# 24
#



plot(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$error_Carbware~AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$dbhcm)
line()

sds<-Rushmore_AFI
Rushmore_AFI<-Monivea_cycle_1_PRE[Monivea_cycle_1_PRE$Cycle %in% c(1,2),]
years<-5
cycle2<-2
cycle1<-1
# new and used sbh_inc_yr
dbh_inc_yrs<-function(Rushmore_AFI,years,cycle2,cycle1){
  
 # Rushmore_AFI<-Rushmore_AFI[Rushmore_AFI$Limit==0,]
  #Rushmore_AFI_2<- Rushmore_AFI[Rushmore_AFI$Limit==0,]
  Rushmore_AFI_Harvest<-Rushmore_AFI %>%
    group_by(TREE_NAME)%>%
    filter(n() == 1)
  
  Rushmore_2cycles<-Rushmore_AFI %>%
    group_by(TREE_NAME)%>%
    filter(n() > 1)
  
  Rushmore_2cycles_cycle1<-Rushmore_2cycles[Rushmore_2cycles$Cycle==cycle1,]
  Rushmore_2cycles_cycle2<-Rushmore_2cycles[Rushmore_2cycles$Cycle==cycle2,]
  
  Rushmore_2cycles_cycle1<-   Rushmore_2cycles_cycle1[!(duplicated(Rushmore_2cycles_cycle1$TREE_NAME) | duplicated(Rushmore_2cycles_cycle1$TREE_NAME, fromLast = TRUE)), ]
  Rushmore_2cycles_cycle2<-   Rushmore_2cycles_cycle2[!(duplicated(Rushmore_2cycles_cycle2$TREE_NAME) | duplicated(Rushmore_2cycles_cycle2$TREE_NAME, fromLast = TRUE)), ]
  
  Rushmore_2cycles<- rbind(Rushmore_2cycles_cycle1,Rushmore_2cycles_cycle2)
  
  Rushmore_2cycles<-Rushmore_2cycles %>%
    group_by(TREE_NAME)%>%
    filter(n() > 1)
  #cal dbh increment
  Rushmore_2cycles$DBH_INC<-NA
  for( i in unique(Rushmore_2cycles$TREE_NAME)){
    Rushmore_2cycles[Rushmore_2cycles$TREE_NAME== i & Rushmore_2cycles$Cycle==cycle1,]$DBH_INC<-  Rushmore_2cycles[Rushmore_2cycles$TREE_NAME== i & Rushmore_2cycles$Cycle==cycle2,]$dbhcm-
      Rushmore_2cycles[Rushmore_2cycles$TREE_NAME== i & Rushmore_2cycles$Cycle==cycle1,]$dbhcm
  }
  
  
  Rushmore_2cycles_1<-Rushmore_2cycles[!is.na(Rushmore_2cycles$cr),]
  
  Rushmore_2cycles_22<-Rushmore_2cycles_1[!is.na(Rushmore_2cycles_1$DBH_INC) ,]
  Rushmore_2cycles_22<-Rushmore_2cycles_22[Rushmore_2cycles_22$DBH_INC>=0 ,]
  
  Rushmore_2cycles_21<-Rushmore_2cycles_1[is.na(Rushmore_2cycles_1$DBH_INC) ,]
  
  #####
  Rushmore_cycle_1<-Rushmore_AFI[Rushmore_AFI$Cycle==cycle1,]
  #Rushmore_cycle_1<-Rushmore_cycle_1[Rushmore_cycle_1$Limit==0,]
  
  
  
  # run carbware model to find est dbh inc
  i <-2
  list_of_y<-list()
  Rushmore_cycle_1$Initial_dbhcm<-Rushmore_cycle_1$dbhcm
  
 not_included<- Rushmore_cycle_1[Rushmore_cycle_1$Limit==1,]#
  #only use included trees to est increment
  list_of_y[[1]]<-Preprocessing_FERS(Rushmore_cycle_1[Rushmore_cycle_1$Limit==0,])
  
  d<-nrow(list_of_y[[1]])#
  if(NROW(not_included)>0){
  list_of_y[[1]][(d+1):(d+nrow(not_included)),] <- NA#
  list_of_y[[1]][(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
  list_of_y[[1]][(d+1):(d+nrow(not_included)),]#
}
  
  
  list_of_y[[2]]<-list_of_y[[1]]
  list_of_y[[2]]$dbhcm<-list_of_y[[1]]$dbhcm+list_of_y[[1]]$DBH_Increment
  list_of_y[[2]]$Height_m<-NA
  
  while(i+1 <=years){
    
    not_included<- list_of_y[[i]][list_of_y[[i]]$Limit==1,]
    
    
    
    list_of_y[[i]]<-Preprocessing_FERS(list_of_y[[i]][list_of_y[[i]]$Limit==0,])
    d<-nrow(list_of_y[[i]])
    if(NROW(not_included)>0){
    list_of_y[[i]][(d+1):(d+nrow(not_included)),] <- NA#
    list_of_y[[i]][(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
    list_of_y[[i]][(d+1):(d+nrow(not_included)),]#
    }
    
    list_of_y[[i+1]]<-list_of_y[[i]]
    list_of_y[[i+1]]$dbhcm<-list_of_y[[i+1]]$dbhcm+list_of_y[[i+1]]$DBH_Increment
    
    
    i<-i+1
  }
  
  not_included<- list_of_y[[i]][list_of_y[[i]]$Limit==1,]
  list_of_y[[i]]<-Preprocessing_FERS(list_of_y[[i]][list_of_y[[i]]$Limit==0,])
  d<-nrow(list_of_y[[i]])
  if(NROW(not_included)>0){
  list_of_y[[i]][(d+1):(d+nrow(not_included)),] <- NA#
  list_of_y[[i]][(d+1):(d+nrow(not_included)),colnames(not_included)]<-not_included#
  list_of_y[[i]][(d+1):(d+nrow(not_included)),]#
  }
  
  
  list_of_y[[length(list_of_y)]]$dbh_act_increment<-NA
  list_of_y[[1]]$dbh_act_increment<-NA
  data_tree<-list_of_y[[length(list_of_y)]]
  
  for(i in unique(Rushmore_2cycles$TREE_NAME)){
    
    if(!is_empty(data_tree[data_tree$TREE_NAME==i,]$dbh_act_increment)){
      
      data_tree[data_tree$TREE_NAME==i,]$dbh_act_increment<-
        Rushmore_2cycles[Rushmore_2cycles$TREE_NAME== i & Rushmore_2cycles$Cycle==cycle1,]$DBH_INC
      
      list_of_y[[1]][list_of_y[[1]]$TREE_NAME==i,]$dbh_act_increment<-
        Rushmore_2cycles[Rushmore_2cycles$TREE_NAME== i & Rushmore_2cycles$Cycle==cycle1,]$DBH_INC/years
    }
    
    
  }
  list_of_y[[length(list_of_y)]]<-data_tree
  list_of_y[[length(list_of_y)]]$EST_increment<-list_of_y[[length(list_of_y)]]$dbhcm-list_of_y[[length(list_of_y)]]$Initial_dbhcm
  list_of_y[[length(list_of_y)]]$error<-NA
  list_of_y[[1]]$error_Carbware_dbh_inc<-NA
  list_of_y[[1]]$error_Carbware_average_inc<-NA
  list_of_y[[length(list_of_y)]]$error <- list_of_y[[length(list_of_y)]]$dbh_act_increment-list_of_y[[length(list_of_y)]]$EST_increment
  list_of_y[[1]]$error_Carbware_dbh_inc <- list_of_y[[1]]$dbh_act_increment -list_of_y[[1]]$DBH_Increment
  
  list_of_y[[1]]$average_est_inc<-NA
  for(i in list_of_y[[1]]$TREE_NAME){
    list_of_y[[1]][list_of_y[[1]]$TREE_NAME==i,]$average_est_inc <-  list_of_y[[length(list_of_y)]][list_of_y[[length(list_of_y)]]$TREE_NAME==i,]$EST_increment/length(list_of_y)
  }
  list_of_y[[1]]$error_Carbware_average_inc<-list_of_y[[1]]$dbh_act_increment -list_of_y[[1]]$average_est_inc
  
  return(list_of_y)
}


Rushmore_AFI$Limit

#rushmore
#Rushmore_AFI<-Rushmore_AFI[Rushmore_AFI$TREE_NAME!="1 102",]
Rushmore_AFI[Rushmore_AFI$Cycle==1,]$Date_measured<-2012
Rushmore_AFI[Rushmore_AFI$Cycle==2,]$Date_measured<-2017
########################
#remove
#Rushmore_AFI<-Rushmore_AFI[!(Rushmore_AFI$TREE_NAME %in% neg_inc), ]
########################

Rushmore_AFI_pre<-dbh_inc_yrs(Rushmore_AFI,5,2,1)

Rushmore_AFI_pre[[5]]$Limit
View(Rushmore_AFI_pre[[1]])

# add last cycle to data as this will not be included
stage_2<-Rushmore_AFI[Rushmore_AFI$Cycle==2,]
stage_2<-stage_2[stage_2$Limit==0,]
stage_2<-stage_2[!is.na(stage_2$dbhcm),]
stage_2_preproc<- Preprocessing_FERS(stage_2)
stage_2_preproc$Initial_dbhcm<-NA
stage_2_preproc$dbh_act_increment<-NA
stage_2_preproc$error_Carbware<-NA
stage_2_preproc$average_est_inc<- NA

Rushmore_AFI_final<-rbind(Rushmore_AFI_pre[[1]],stage_2_preproc)
View(Rushmore_AFI_final)








###################

#Height vs dbh model

plot(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$dbhcm,AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$Height_m,xlim=c(0,100),ylim=c(0,50))
axis(side=2, at=seq(0,50,2))
axis(side=1, at=seq(0,100,5))
?plot

plot(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Douglas fir",]$dbhcm,AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Douglas fir",]$Height_m,xlim=c(0,110),ylim=c(0,60))
axis(side=2, at=seq(0,50,2))
axis(side=1, at=seq(0,100,5))



plot(AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$dbhcm,AFI_ISN_Data[AFI_ISN_Data$cohort_name=="Spruce",]$Height_m,xlim=c(0,100),ylim=c(0,50))


View(AFI_ISN_Data)

max(C$Height_m,na.rm=T)



openxlsx:: write.xlsx(expansion_query, 'expansion_query.xlsx')


expansion_query

# flag for ingrowth and harvested

AFI_ISN_Data$Flag<-NA
table(AFI_ISN_Data$Flag)

for( i in unique(AFI_ISN_Data$Site_code)){
  if(!is_empty(AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==1 ,][
    !(AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==1,]$TREE_NAME %in%
      AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==2,]$TREE_NAME),]$Flag)){
    
  AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==1 ,][
        !(AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==1,]$TREE_NAME %in%
        AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==2,]$TREE_NAME),]$Flag <- "H"
  }
  
  if(!is_empty(AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==2,][
    !(AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==2,]$TREE_NAME %in%
      AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==1,]$TREE_NAME),]$Flag)){
  AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==2,][
                 !(AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==2,]$TREE_NAME %in%
                     AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==1,]$TREE_NAME),]$Flag <- "ING"
  }
}



for( i in unique(AFI_ISN_Data[AFI_ISN_Data$Cycle==3,]$Site_code)){
  if(!is_empty( AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==2,][
    !(AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==2,]$TREE_NAME %in%
      AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==3,]$TREE_NAME),]$Flag)){
    
    
  AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==2,][
                 !(AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==2,]$TREE_NAME %in%
                     AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==3,]$TREE_NAME),]$Flag <- "H"
  
  }
  
  AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==3 ,][
                 !(AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==3,]$TREE_NAME %in%
                     AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==2,]$TREE_NAME),]$Flag <- "ING"
}

for( i in unique(AFI_ISN_Data[AFI_ISN_Data$Cycle==4,]$Site_code)){
  AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==3 ,][
                 !(AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==3,]$TREE_NAME %in%
                     AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==4,]$TREE_NAME),]$Flag <- "H"
  AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==4 ,][
                 !(AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==4,]$TREE_NAME %in%
                     AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==3,]$TREE_NAME),]$Flag <- "ING"
}




write.csv(AFI_ISN_Data,"AFI_ISN_Data.20.03.23")

AFI_ISN_Data[AFI_ISN_Data$Site_code=="M9/10, M11, M13/15 Chase Wood" & AFI_ISN_Data$Cycle==2,]$Flag

AFI_ISN_Data3<-AFI_ISN_Data3[AFI_ISN_Data3$Site_code!="47/48/55 Llethr Gwinau",]
write.csv(AFI_ISN_Data3,"AFI_ISN_Data.new.DBH.INC")


View(AFI_ISN_Data[AFI_ISN_Data$Site_code=="102",])


i="Llethr Gwinau ISN"


AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==1 ,][
  !(AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==1,]$TREE_NAME %in%
      AFI_ISN_Data[AFI_ISN_Data$Site_code==i & AFI_ISN_Data$Cycle==2,]$TREE_NAME),]$Flag

table(AFI_ISN_Data[AFI_ISN_Data$Site_code=="Nant yr Eira INV",]$species)
