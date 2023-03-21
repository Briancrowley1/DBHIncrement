library(readxl)
chase_woods<-read_excel("D:/FERS Project/STVI/DATA/120-Chasse Woods - Rushmore Estate.xlsx",sheet = "Arbres")

Arbres<-Monivea_cycle_1
Arbres1<-Arbres
# AFI 
unique(Arbres1$Essence)
AFI_Version2(Monivea_cycle_1,2011,dataset_AFI_raw,"Monivea")
Site_Name<-"Monivea"
AFI_Version2<- function(Arbres,Date_measured,dataset_AFI,Site_Name){
  library(rlang)
  library(dplyr)
  # add cohorts to arbes
  Arbres$cohort_name<-NA
  Arbres<-Arbres[!is.na(Arbres$Essence),]
  Arbres[Arbres$Essence=="Douglas",]$cohort_name<-"Douglas fir"
  Arbres[Arbres$Essence=="Douglas fir",]$cohort_name<-"Douglas fir"
  
  Arbres[Arbres$Essence=="Epicéa S",]$cohort_name<-"Spruce"
  Arbres[Arbres$Essence=="Chêne P",]$cohort_name<-"Slow-growing broadleaves"
  Arbres[Arbres$Essence=="Mélèze J",]$cohort_name<-"Larch"
  Arbres[Arbres$Essence=="Aulne",]$cohort_name<-"Fast-growing broadleaves"
  Arbres[Arbres$Essence=="Frêne",]$cohort_name<-"Fast-growing broadleaves"
  Arbres[Arbres$Essence=="Erable S",]$cohort_name<-"Fast-growing broadleaves"
  Arbres[Arbres$Essence=="Epicéa C",]$cohort_name<-"Spruce"
  Arbres[Arbres$Essence=="Hêtre",]$cohort_name<-"Slow-growing broadleaves"
  Arbres[Arbres$Essence=="Tsuga",]$cohort_name<-"Other conifers"
  Arbres[Arbres$Essence=="Cyprès L",]$cohort_name<-"Other conifers"
  
  Arbres[Arbres$Essence=="Châtaignier",]$cohort_name<-"Slow-growing broadleaves"
  Arbres[Arbres$Essence=="Alder",]$cohort_name<-"Fast-growing broadleaves"
  Arbres[Arbres$Essence=="Ash",]$cohort_name<-"Fast-growing broadleaves"
  Arbres[Arbres$Essence=="Beech",]$cohort_name<-"Slow-growing broadleaves"
  Arbres[Arbres$Essence=="E Oak",]$cohort_name<-"Slow-growing broadleaves"
  Arbres[Arbres$Essence=="Japanese larch",]$cohort_name<-"Larch"
  Arbres[Arbres$Essence=="Norway spruce",]$cohort_name<-"Spruce"
  Arbres[Arbres$Essence=="Sitka spruce",]$cohort_name<-"Spruce"
  Arbres[Arbres$Essence=="Sweet chestnut",]$cohort_name<-"Slow-growing broadleaves"
  Arbres[Arbres$Essence=="Sycamore",]$cohort_name<-"Fast-growing broadleaves"
  Arbres[Arbres$Essence=="Western Hemlock",]$cohort_name<-"Other conifers"
  Arbres[Arbres$Essence=="Western Red Cedar",]$cohort_name<-"Other conifers"
  
  #  # get information from EIB excel sheet
  EIB_AFI<- EIB_Site_Specie_YC_DevStage_to_preporc(dataset_AFI)
  if(!is_empty(Arbres[Arbres$Diam1=="F" & !is.na(Arbres$Diam1),]$Diam1)){
  Arbres[Arbres$Diam1=="F" & !is.na(Arbres$Diam1),]$Diam1<-NA
  }
  if(!is_empty(Arbres[Arbres$Diam1=="Dead" & !is.na(Arbres$Diam1),]$Diam1)){
  Arbres[Arbres$Diam1=="Dead" & !is.na(Arbres$Diam1),]$Diam1<-NA
  }
  Arbres$Diam1<-as.numeric(Arbres$Diam1)
  #AFI_dataset<-AFI_dataset[AFI_dataset$Cycle==max(AFI_dataset$Cycle),]
  AFI_dataset<-Arbres
  AFI_dataset$Limit<-0
  if(!is_empty( AFI_dataset[(AFI_dataset$Diam1+AFI_dataset$Diam2)/2>17.5 &
                              AFI_dataset$Distance*0.03*100>(AFI_dataset$Diam1+AFI_dataset$Diam2)/2 & 
                                !is.na((AFI_dataset$Diam1+AFI_dataset$Diam2)/2),]$Limit)){
    
                                 AFI_dataset[(AFI_dataset$Diam1+AFI_dataset$Diam2)/2>=17.5 & 
                                  AFI_dataset$Distance*0.03*100>(AFI_dataset$Diam1+AFI_dataset$Diam2)/2 &
                                    !is.na((AFI_dataset$Diam1+AFI_dataset$Diam2)/2),]$Limit<-1
  }
  
  
  if(!is_empty(AFI_dataset[(AFI_dataset$Diam1+AFI_dataset$Diam2)/2<17.5 & 
                           AFI_dataset$Distance>10 & !is.na((AFI_dataset$Diam1+AFI_dataset$Diam2)/2),]$Limit)){
    AFI_dataset[(AFI_dataset$Diam1+AFI_dataset$Diam2)/2<17.5 & AFI_dataset$Distance>10 &
                  !is.na((AFI_dataset$Diam1+AFI_dataset$Diam2)/2),]$Limit<-1
  }
  AFI_dataset_initial<-data.frame(Date_measured=NA,species=AFI_dataset$Essence,
                                  Site_code=AFI_dataset$NumDisp,idplots=NA,Tree_ID=AFI_dataset$NumArbre,X=NA,Y=NA,Plot.size..ha.=NA,
                                  cohort_name=AFI_dataset$cohort_name,cr=(AFI_dataset$HautT-AFI_dataset$HautL)/AFI_dataset$HautT,dbhcm=(AFI_dataset$Diam1+AFI_dataset$Diam2)/2,
                                  expansionfactor=NA,Height_m=AFI_dataset$HautT,YC=NA,Development.stage= NA,Limit=AFI_dataset$Limit,Cycle=AFI_dataset$Cycle)

  for(i in 1:NROW(AFI_dataset_initial)){
   if(is.na(AFI_dataset_initial[i,"dbhcm"])){
    AFI_dataset_initial[i,"dbhcm"]<-AFI_dataset[i,]$Diam1
    }
  }
  
  
  AFI_dataset_initial$Development.stage <-  EIB_AFI[EIB_AFI$site_name==Site_Name,]$Development.stage[1]
  AFI_dataset_initial$Date_measured<-Date_measured
  AFI_dataset_initial$idplots<-AFI_dataset$NumPlac
  AFI_dataset_initial$X<- AFI_dataset$Distance*cos(AFI_dataset$Azimut)
  AFI_dataset_initial$Y<-AFI_dataset$Distance*sin(AFI_dataset$Azimut)
  AFI_dataset_initial$Plot.size..ha.<-1
  AFI_dataset_initial$TREE_NAME<- paste(AFI_dataset_initial$idplots,AFI_dataset_initial$Tree_ID)
  #AFI_dataset_initial<-AFI_dataset_initial[!is.na(AFI_dataset_initial$dbhcm),]
  
  
  
  # for(i in unique(AFI_dataset_initial$idplots)){
  #   AFI_dataset_initial[AFI_dataset_initial$idplots==i,]$Plot.size..ha.<-
  #     sum(abs(range(AFI_dataset_initial[AFI_dataset_initial$idplots==i,]$Y)))*sum(abs(range(AFI_dataset_initial[AFI_dataset_initial$idplots==i,]$X)))/10000
  # }
  
  
  
  
  
  
  
  # Yield class
  if(is_empty( AFI_dataset_initial[AFI_dataset_initial$species==EIB_AFI[EIB_AFI$site_name==Site_Name,]$species_major[1],]$YC)){
    AFI_dataset_initial$YC<-  as.integer(EIB_AFI[EIB_AFI$site_name==Site_Name,]$YC_major[1])
  }else{
    AFI_dataset_initial[AFI_dataset_initial$species==EIB_AFI[EIB_AFI$site_name==Site_Name,]$species_major[1],]$YC<-as.integer(EIB_AFI[EIB_AFI$site_name==Site_Name,]$YC_major[1])
    if(!is.na(EIB_AFI[EIB_AFI$site_name==Site_Name,]$species_second[1])){
      AFI_dataset_initial[AFI_dataset_initial$species==EIB_AFI[EIB_AFI$site_name==Site_Name,]$species_second[1],]$YC<-as.integer(EIB_AFI[EIB_AFI$site_name==Site_Name,]$YC_second[1])
    }
    AFI_dataset_initial[is.na(AFI_dataset_initial$YC),]$YC<-as.integer(EIB_AFI[EIB_AFI$site_name==Site_Name,]$YC_major[1])
    AFI_dataset_initial$YC<-as.integer( AFI_dataset_initial$YC)
  }
  AFI_dataset_initial[!(AFI_dataset_initial$cohort_name %in% c("Spruce","Douglas fir")),]$YC<-NA
  
  # expansion factor
  AFI_dataset_initial[AFI_dataset_initial$dbhcm<=17.5 & !is.na(AFI_dataset_initial$dbhcm),]$expansionfactor<-10000/pi/100
  
  AFI_dataset_initial[AFI_dataset_initial$dbhcm>17.5 & !is.na(AFI_dataset_initial$dbhcm),]$expansionfactor<-100000000*0.03^2/pi/(AFI_dataset_initial[AFI_dataset_initial$dbhcm>17.5 & !is.na(AFI_dataset_initial$dbhcm),]$dbhcm^2)
  # trees <27.5cm will have exp_fact of area of plot/ pi*100
  
  AFI_dataset_initial<-AFI_dataset_initial[!is.na(AFI_dataset_initial$cohort_name),]
  return(AFI_dataset_initial)
  
}

Stourhead_AFI<-AFI_dataset_Pre(Arbres,Date_measured,dataset_from_xlsx,Site_code,Site_Name)
Rush_AFI[Rush_AFI$cohort_name=="Douglas Fir",]$cohort_name<-"Douglas fir"
Rushmore_c1_c2_c3<-AFI_Version2(chase_woods,2018,dataset_AFI,Site_Name)

Rush_AFI$expansionfactor



Stourhead_AFI_pre<-Preprocessing_FERS(Stourhead_AFI)


# (DBH<=30){10000/pi/100,100000000*0.03^2/pi/dbh^2)}

Rush_AFI_pre[Rush_AFI_pre$idplots==1,]$RepreBAHA
Rush_AFI_pre[Rush_AFI_pre$idplots==1,]$expansionfactor


plot(Stourhead_AFI_pre[Stourhead_AFI_pre$idplots==3,]$X,Stourhead_AFI_pre[Stourhead_AFI_pre$idplots==3,]$Y)
Arbres$
  
  
  AFI_dataset$X<- AFI_dataset$Distance*cos(AFI_dataset$Azimut)
AFI_dataset$Y<-AFI_dataset$Distance*sin(AFI_dataset$Azimut)

testy<-AFI_dataset[AFI_dataset$Cycle==1,]
plot(testy[testy$Placette==3,]$X,testy[testy$Placette==3,]$Y)
Arbres<-Baronscourt
###############################################################################
AFI_Version3<- function(Arbres,Date_measured,dataset_AFI,Site_Name){
  library(rlang)
  library(dplyr)
  # add cohorts to arbes
  Arbres<-Arbres[!is.na(Arbres$Diam1),]
  Arbres<-Arbres[!is.na(Arbres$Species),]
  Arbres$TopHeight<-as.double(Arbres$TopHeight)
  Arbres$CrownHeight<- as.double(Arbres$CrownHeight)
  Arbres$cohort_name<-NA
  if(!is_empty(Arbres[Arbres$Species=="Douglas",]$cohort_name)){
  Arbres[Arbres$Species=="Douglas",]$cohort_name<-"Douglas fir"}
  if(!is_empty(Arbres[Arbres$Species=="Pacific silver fir",]$cohort_name)){
  Arbres[Arbres$Species=="Pacific silver fir",]$cohort_name <- "Other conifers"}
  if(!is_empty(Arbres[Arbres$Species=="Japanese larch",]$cohort_name)){
  Arbres[Arbres$Species=="Japanese larch",]$cohort_name <- "Larch"}
  if(!is_empty(Arbres[Arbres$Species=="Douglas fir",]$cohort_name)){
  Arbres[Arbres$Species=="Douglas fir",]$cohort_name<-"Douglas fir"}
  if(!is_empty(Arbres[Arbres$Species=="Elder",]$cohort_name)){
  Arbres[Arbres$Species=="Elder",]$cohort_name<-"Slow-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$Species=="Ash",]$cohort_name)){
  Arbres[Arbres$Species=="Ash",]$cohort_name<-"Fast-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$Species=="Birch",]$cohort_name)){
  Arbres[Arbres$Species=="Birch",]$cohort_name<-"Fast-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$Species=="Willow",]$cohort_name)){
  Arbres[Arbres$Species=="Willow",]$cohort_name<-"Fast-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$Species=="Beech",]$cohort_name)){
  Arbres[Arbres$Species=="Beech",]$cohort_name<-"Slow-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$Species=="Sessile oak",]$cohort_name)){
  Arbres[Arbres$Species=="Sessile oak",]$cohort_name<-"Slow-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$Species=="Birch",]$cohort_name)){
  Arbres[Arbres$Species=="Birch",]$cohort_name<-"Fast-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$Species=="Lawson cypress",]$cohort_name)){
  Arbres[Arbres$Species=="Lawson cypress",]$cohort_name<-"Other conifers"}
  if(!is_empty(Arbres[Arbres$Species=="Norway spruce",]$cohort_name)){
  Arbres[Arbres$Species=="Norway spruce",]$cohort_name<-"Spruce"}
  if(!is_empty(Arbres[Arbres$Species=="Sitka spruce",]$cohort_name)){
  Arbres[Arbres$Species=="Sitka spruce",]$cohort_name<-"Spruce"}
  if(!is_empty(Arbres[Arbres$Species=="Sitka",]$cohort_name)){
  Arbres[Arbres$Species=="Sitka",]$cohort_name<-"Spruce"}
 
  if(!is_empty(Arbres[Arbres$Species=="Lodgepole pine",]$cohort_name)){
    Arbres[Arbres$Species=="Lodgepole pine",]$cohort_name<-"Pine"} 
  if(!is_empty(Arbres[Arbres$Species=="Scots pine",]$cohort_name)){
    Arbres[Arbres$Species=="Scots pine",]$cohort_name<-"Pine"} 
  
  if(!is_empty(Arbres[Arbres$Species=="Holly",]$cohort_name)){
    Arbres[Arbres$Species=="Holly",]$cohort_name<-"Slow-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$Species=="Hornbeam",]$cohort_name)){
    Arbres[Arbres$Species=="Hornbeam",]$cohort_name<-"Slow-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$Species=="Oak",]$cohort_name)){
    Arbres[Arbres$Species=="Oak",]$cohort_name<-"Slow-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$Species=="Nothofagus",]$cohort_name)){
    Arbres[Arbres$Species=="Nothofagus",]$cohort_name<-"Slow-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$Species=="Wild cherry",]$cohort_name)){
    Arbres[Arbres$Species=="Wild cherry",]$cohort_name<-"Slow-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$Species=="Sweet chestnut",]$cohort_name)){
    Arbres[Arbres$Species=="Sweet chestnut",]$cohort_name<-"Slow-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$Species=="Horse chestnut",]$cohort_name)){
    Arbres[Arbres$Species=="Horse chestnut",]$cohort_name<-"Slow-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$Species=="Lime",]$cohort_name)){
    Arbres[Arbres$Species=="Lime",]$cohort_name<-"Slow-growing broadleaves"}
  
  if(!is_empty(Arbres[Arbres$Species=="Western hemlock",]$cohort_name)){
    Arbres[Arbres$Species=="Western hemlock",]$cohort_name <- "Other conifers"}
  if(!is_empty(Arbres[Arbres$Species=="Coast redwood",]$cohort_name)){
    Arbres[Arbres$Species=="Coast redwood",]$cohort_name <- "Other conifers"}
  if(!is_empty(Arbres[Arbres$Species=="Cedar",]$cohort_name)){
    Arbres[Arbres$Species=="Cedar",]$cohort_name <- "Other conifers"}
  if(!is_empty(Arbres[Arbres$Species=="Western red cedar",]$cohort_name)){
    Arbres[Arbres$Species=="Western red cedar",]$cohort_name <- "Other conifers"}
  
  if(!is_empty(Arbres[Arbres$Species=="Rowan",]$cohort_name)){
    Arbres[Arbres$Species=="Rowan",]$cohort_name<-"Fast-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$Species=="Sycamore",]$cohort_name)){
    Arbres[Arbres$Species=="Sycamore",]$cohort_name<-"Fast-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$Species=="Poplar",]$cohort_name)){
    Arbres[Arbres$Species=="Poplar",]$cohort_name<-"Fast-growing broadleaves"}
  if(!is_empty(Arbres[Arbres$Species=="Norway maple",]$cohort_name)){
    Arbres[Arbres$Species=="Norway maple",]$cohort_name<-"Fast-growing broadleaves"}
  
  if(!is_empty(Arbres[Arbres$Species=="Larch",]$cohort_name)){
    Arbres[Arbres$Species=="Larch",]$cohort_name<-"Larch"}
  #  # get information from EIB excel sheet
  
 
  
  
  
  
  EIB_AFI<- EIB_Site_Specie_YC_DevStage_to_preporc(dataset_AFI)
  EIB_data[16,]<-c("Barenscourt (NI)","Barenscourt","SS",NA,NA,NA,NA)
  
  #AFI_dataset<-AFI_dataset[AFI_dataset$Cycle==max(AFI_dataset$Cycle),]
  AFI_dataset<-Arbres
  AFI_dataset$Limit<-0
  if(!is_empty( AFI_dataset[(AFI_dataset$Diam1+AFI_dataset$Diam2)/2>17.5 &
                            AFI_dataset$Distance*0.03*100>(AFI_dataset$Diam1+AFI_dataset$Diam2)/2 & 
                            !is.na((AFI_dataset$Diam1+AFI_dataset$Diam2)/2),]$Limit)){
    AFI_dataset[(AFI_dataset$Diam1+AFI_dataset$Diam2)/2>=17.5 & 
                  AFI_dataset$Distance*0.03*100>(AFI_dataset$Diam1+AFI_dataset$Diam2)/2 & !is.na((AFI_dataset$Diam1+AFI_dataset$Diam2)/2),]$Limit<-1
  }
  if(!is_empty(AFI_dataset[(AFI_dataset$Diam1+AFI_dataset$Diam2)/2<17.5 & AFI_dataset$Distance>10 & !is.na((AFI_dataset$Diam1+AFI_dataset$Diam2)/2),]$Limit)){
    AFI_dataset[(AFI_dataset$Diam1+AFI_dataset$Diam2)/2<17.5 & AFI_dataset$Distance>10 & !is.na((AFI_dataset$Diam1+AFI_dataset$Diam2)/2),]$Limit<-1
  }
  
  AFI_dataset_initial<-data.frame(Date_measured=NA,species=AFI_dataset$Species,
                                  Site_code=AFI_dataset$StandNum,idplots=AFI_dataset$PlotNum,Tree_ID=AFI_dataset$TreeNum,X=NA,Y=NA,Plot.size..ha.=NA,
                                  cohort_name=AFI_dataset$cohort_name,cr=(as.double(AFI_dataset$TopHeight)-as.double(AFI_dataset$CrownHeight))/as.double(AFI_dataset$TopHeight),dbhcm=(AFI_dataset$Diam1+AFI_dataset$Diam2)/2,
                                  expansionfactor=NA,Height_m=AFI_dataset$TopHeight,YC=NA,Development.stage= NA,Limit=AFI_dataset$Limit,Cycle=AFI_dataset$Cycle)
  
  for(i in 1:NROW(AFI_dataset_initial)){
    if(is.na(AFI_dataset_initial[i,"dbhcm"])){
      AFI_dataset_initial[i,"dbhcm"]<-AFI_dataset[i,]$Diam1
    }
  }
  
  AFI_dataset_initial$Development.stage <-  EIB_AFI[EIB_AFI$site_name==Site_Name,]$Development.stage[1]
  AFI_dataset_initial$Date_measured<-Date_measured
  AFI_dataset_initial$TREE_NAME<- paste(AFI_dataset_initial$idplots,AFI_dataset_initial$Tree_ID)
  AFI_dataset_initial$X<- AFI_dataset$Distance*cos(AFI_dataset$Bearing)
  AFI_dataset_initial$Y<-AFI_dataset$Distance*sin(AFI_dataset$Bearing)
  AFI_dataset_initial$Plot.size..ha.<-1
  
  
  
  # for(i in unique(AFI_dataset_initial$idplots)){
  #   AFI_dataset_initial[AFI_dataset_initial$idplots==i,]$Plot.size..ha.<-
  #     sum(abs(range(AFI_dataset_initial[AFI_dataset_initial$idplots==i,]$Y)))*sum(abs(range(AFI_dataset_initial[AFI_dataset_initial$idplots==i,]$X)))/10000
  # }
  
  

  
  
  
  
  # Yield class
  if(!is.na(EIB_AFI[EIB_AFI$site_name==Site_Name,]$YC_major[1])){
  if(is_empty( AFI_dataset_initial[AFI_dataset_initial$species==EIB_AFI[EIB_AFI$site_name==Site_Name,]$species_major[1],]$YC)){
    AFI_dataset_initial$YC<-  as.integer(EIB_AFI[EIB_AFI$site_name==Site_Name,]$YC_major[1])
  }else{
    AFI_dataset_initial[AFI_dataset_initial$species==EIB_AFI[EIB_AFI$site_name==Site_Name,]$species_major[1],]$YC<-as.integer(EIB_AFI[EIB_AFI$site_name==Site_Name,]$YC_major[1])
    if(!is.na(EIB_AFI[EIB_AFI$site_name==Site_Name,]$species_second[1])){
      AFI_dataset_initial[AFI_dataset_initial$species==EIB_AFI[EIB_AFI$site_name==Site_Name,]$species_second[1],]$YC<-as.integer(EIB_AFI[EIB_AFI$site_name==Site_Name,]$YC_second[1])
    }
    AFI_dataset_initial[is.na(AFI_dataset_initial$YC),]$YC<-as.integer(EIB_AFI[EIB_AFI$site_name==Site_Name,]$YC_major[1])
    AFI_dataset_initial$YC<-as.integer( AFI_dataset_initial$YC)
  }
  if(!is_empty(AFI_dataset_initial[!(AFI_dataset_initial$cohort_name %in% c("Spruce","Douglas fir")),]$YC)){
  AFI_dataset_initial[!(AFI_dataset_initial$cohort_name %in% c("Spruce","Douglas fir")),]$YC<-NA
  }
  }
  
  # expansion factor
  AFI_dataset_initial[AFI_dataset_initial$dbhcm<=17.5,]$expansionfactor<-10000/pi/100
  
  AFI_dataset_initial[AFI_dataset_initial$dbhcm>17.5,]$expansionfactor<-100000000*0.03^2/pi/(AFI_dataset_initial[AFI_dataset_initial$dbhcm>17.5,]$dbhcm^2)
  # trees <27.5cm will have exp_fact of area of plot/ pi*100
  
  AFI_dataset_initial<-AFI_dataset_initial[!is.na(AFI_dataset_initial$cohort_name),]
  return(AFI_dataset_initial)
  
}

