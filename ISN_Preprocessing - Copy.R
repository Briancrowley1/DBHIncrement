# ISN preprocessing script
library(rlang)
library(readxl)
# what is needed to run fucntion
# ISN_dataset
#Date_measured
#EIB Research Data Database = dataset_from_xlsx
#Site_Name = site number_woodsName
#example, we will use cranborne, 37c daggons
# You may run the below code and manually select the excel sheet required or you may code it in like the comment below.
# cranborne<-read_excel("D:/FERS Project/STVI/DATA/ISN Cranborne 37c 1 PSC19.xls",sheet = "Trees")
# cranborne_date<-as.integer(colnames(read_excel("D:/FERS Project/STVI/DATA/ISN Cranborne 37c 1 PSC19.xls",sheet = "General",range = "E4:E4")))
Crichel<-read_excel("D:/FERS Project/STVI/DATA/Nant_yr_Eira_inventaires input data.xlsx",sheet = "Trees") # manually select excel sheet
Crichel_date<-as.integer(colnames(read_excel("D:/FERS Project/STVI/DATA/Nant_yr_Eira_inventaires input data.xlsx",sheet = "General",range = "E4:E4")))
Site_Name<-"208, 213a Boulsbury"

dataset_from_xlsx <- read_excel("D:/FERS Project/STVI/DATA/EIB Research Data Database 06-05-22 AP 05 07 22 (1).xlsx", sheet = "ISN", skip = 2)
dataset_from_xlsx<-dataset_from_xlsx[-7,]
EIB_data<- EIB_Site_Specie_YC_DevStage_to_preporc(dataset_from_xlsx)# using function from other script
# 
ISN_dataset<-Crichel

ISN_dataset_Pre<- function(ISN_dataset,Date_measured,dataset_from_xlsx,Site_Name){
  # get information from EIB excel sheet
  library(rlang)
  library(readxl)
  EIB_data<- EIB_Site_Specie_YC_DevStage_to_preporc(dataset_from_xlsx)
  
  
  ISN_dataset_initial<-data.frame(Date_measured=NA,
                                  Site_code=NA,idplots=NA,Tree_ID=NA,
                                  X=ISN_dataset$Distance*cos(ISN_dataset$Bering),Y=ISN_dataset$Distance*sin(ISN_dataset$Bering),
                                  Plot.size..ha.=NA,species=ISN_dataset$Species,
                                  cohort_name=NA,cr=NA,dbhcm=ISN_dataset$Diam,expansionfactor=ISN_dataset$`N/ha`, Height_m=NA,YC=NA,
                                  Development.stage= NA,SpeciesGroup=ISN_dataset$SpeciesGroup,Limit=ISN_dataset$Limit)
  
  ISN_dataset_initial$Development.stage <-  EIB_data[EIB_data$site_name==Site_Name,]$Development.stage[1]
  ISN_dataset_initial$Date_measured<-Date_measured
  ISN_dataset_initial$idplots<-ISN_dataset$Plot
  ISN_dataset_initial$Tree_ID <- paste(ISN_dataset$Distance,"_",ISN_dataset$Bering,sep="")
  ISN_dataset_initial$Plot.size..ha.<-1
  ISN_dataset_initial$Site_code<-Site_Name
  ISN_dataset_initial<-ISN_dataset_initial[!is.na(ISN_dataset_initial$dbhcm),]
  ISN_dataset_initial$TREE_NAME<-paste(ISN_dataset_initial$idplots,ISN_dataset_initial$Tree_ID)
  
  ISN_dataset<-ISN_dataset[ISN_dataset$`N/ha`!=0,]
  
  # for(i in unique(ISN_dataset_initial$idplots)){
  #   ISN_dataset_initial[ISN_dataset_initial$idplots==i,]$Plot.size..ha.<- max(ISN_dataset[ISN_dataset$Plot==i & ISN_dataset$Limit==0,]$Distance,na.rm=T)^2*pi /10000
  # }
  # if(!is_empty(ISN_dataset_initial[ISN_dataset_initial$Plot.size..ha.<0.03141593,]$Plot.size..ha.)){
  # ISN_dataset_initial[ISN_dataset_initial$Plot.size..ha.<0.03141593,]$Plot.size..ha.<-0.03141593
  # }
  # 
  
  
  # update cohort name
  ISN_dataset_initial<-ISN_dataset_initial[!is.na(ISN_dataset_initial$SpeciesGroup),]
 if(!is_empty(ISN_dataset_initial[ISN_dataset_initial$SpeciesGroup== "Other Con",]$cohort_name)){
   ISN_dataset_initial[ISN_dataset_initial$SpeciesGroup== "Other Con",]$cohort_name <- "Other conifers"
 }
  
  is_empty( grep("Western redcedar",CohortLookup$Value_spp,ignore.case=TRUE,value=TRUE))
  for(i in unique(ISN_dataset_initial$SpeciesGroup)){
    if(!is_empty(grep(i,CohortLookup$Value_spp,ignore.case=TRUE,value=TRUE))){
      val<-grep(i,CohortLookup$Value_spp,ignore.case=TRUE,value=TRUE)[1]
      ISN_dataset_initial[ISN_dataset_initial$SpeciesGroup==i,]$cohort_name<-CohortLookup[CohortLookup$Value_spp==val,]$`ID_Model-Cohort`
      
    }
    if(!is_empty(grep(i,CohortLookup$Value2,ignore.case=TRUE,value=TRUE))){
      val<- grep(i,CohortLookup$Value2,ignore.case=TRUE,value=TRUE)[1]
      ISN_dataset_initial[ISN_dataset_initial$SpeciesGroup==i,]$cohort_name<-CohortLookup[CohortLookup$Value2==val,]$`ID_Model-Cohort`
    }
    if(!is_empty(grep(i,CohortLookup$Value3,ignore.case=TRUE,value=TRUE))){
      val<- grep(i,CohortLookup$Value3,ignore.case=TRUE,value=TRUE)[1]
      ISN_dataset_initial[ISN_dataset_initial$SpeciesGroup==i,]$cohort_name<-CohortLookup[CohortLookup$Value3==val,]$`ID_Model-Cohort`
      
    }
  }
  
  
  #expansion factor
  # ISN_dataset_initial[ISN_dataset_initial$dbhcm>27.5,]$expansionfactor<-1
  # ISN_dataset_initial[ISN_dataset_initial$dbhcm<27.5,]$expansionfactor<-ISN_dataset_initial[ISN_dataset_initial$dbhcm<27.5,]$Plot.size..ha./0.03141593
  
  # Yield class
  ISN_dataset_initial[ISN_dataset_initial$species==EIB_data[EIB_data$site_name==Site_Name,]$species_major[1],]$YC<-as.integer(EIB_data[EIB_data$site_name==Site_Name,]$YC_major[1])
  if(!is.na(EIB_data[EIB_data$site_name==Site_Name,]$species_second[1])){
    ISN_dataset_initial[ISN_dataset_initial$species==EIB_data[EIB_data$site_name==Site_Name,]$species_second[1],]$YC<-as.integer(EIB_data[EIB_data$site_name==Site_Name,]$YC_second[1]) 
  }
  if(!is_empty(ISN_dataset_initial[is.na(ISN_dataset_initial$YC),]$YC)){
  ISN_dataset_initial[is.na(ISN_dataset_initial$YC),]$YC<-as.integer(EIB_data[EIB_data$site_name==Site_Name,]$YC_major[1])
  }
  
  ISN_dataset_initial$YC<-as.integer( ISN_dataset_initial$YC)
  if(!is_empty(ISN_dataset_initial[ISN_dataset_initial$cohort_name=="Douglas Fir",]$cohort_name)){
  ISN_dataset_initial[ISN_dataset_initial$cohort_name=="Douglas Fir",]$cohort_name<-"Douglas fir"
  }
  if(!is_empty(ISN_dataset_initial[!(ISN_dataset_initial$cohort_name %in% c("Spruce","Douglas fir")),]$YC)){
  ISN_dataset_initial[!(ISN_dataset_initial$cohort_name %in% c("Spruce","Douglas fir")),]$YC<-NA
  }
  
  return(ISN_dataset_initial)
  
}
ISN_dataset_Pre2<- function(ISN_dataset,Date_measured,dataset_from_xlsx,Site_Name){
  # get information from EIB excel sheet
  library(rlang)
  library(readxl)
  EIB_data<- EIB_Site_Specie_YC_DevStage_to_preporc(dataset_from_xlsx)
  
  
  ISN_dataset_initial<-data.frame(Date_measured=NA,
                                  Site_code=NA,idplots=NA,Tree_ID=NA,
                                  X=ISN_dataset$Dist*cos(ISN_dataset$Bearing),Y=ISN_dataset$Dist*sin(ISN_dataset$Bearing),
                                  Plot.size..ha.=NA,species=ISN_dataset$Species,
                                  cohort_name=NA,cr=NA,dbhcm=ISN_dataset$Diam1,expansionfactor=ISN_dataset$`N/ha`, Height_m=NA,YC=NA,
                                  Development.stage= NA,SpeciesGroup=ISN_dataset$SpeciesGroup,limit=ISN_dataset$Limit)
  
  ISN_dataset_initial$Development.stage <-  EIB_data[EIB_data$site_name==Site_Name,]$Development.stage[1]
  ISN_dataset_initial$Date_measured<-Date_measured
  ISN_dataset_initial$idplots<-ISN_dataset$Plot
  ISN_dataset_initial$Tree_ID <- paste(ISN_dataset$Dist,"_",ISN_dataset$Bearing,sep="")
  ISN_dataset_initial$Plot.size..ha.<-1
  ISN_dataset_initial$Site_code<-Site_Name
  ISN_dataset_initial<-ISN_dataset_initial[!is.na(ISN_dataset_initial$dbhcm),]
  
  
  ISN_dataset<-ISN_dataset[ISN_dataset$`N/ha`!=0,]
  
  # for(i in unique(ISN_dataset_initial$idplots)){
  #   ISN_dataset_initial[ISN_dataset_initial$idplots==i,]$Plot.size..ha.<- max(ISN_dataset[ISN_dataset$Plot==i & ISN_dataset$Limit==0,]$Distance,na.rm=T)^2*pi /10000
  # }
  # if(!is_empty(ISN_dataset_initial[ISN_dataset_initial$Plot.size..ha.<0.03141593,]$Plot.size..ha.)){
  # ISN_dataset_initial[ISN_dataset_initial$Plot.size..ha.<0.03141593,]$Plot.size..ha.<-0.03141593
  # }
  # 
  
  
  # update cohort name
  ISN_dataset_initial<-ISN_dataset_initial[!is.na(ISN_dataset_initial$SpeciesGroup),]
  if(!is_empty(ISN_dataset_initial[ISN_dataset_initial$SpeciesGroup== "Other Con",]$cohort_name)){
    ISN_dataset_initial[ISN_dataset_initial$SpeciesGroup== "Other Con",]$cohort_name <- "Other conifers"
  }
  
  is_empty( grep("Western redcedar",CohortLookup$Value_spp,ignore.case=TRUE,value=TRUE))
  for(i in unique(ISN_dataset_initial$SpeciesGroup)){
    if(!is_empty(grep(i,CohortLookup$Value_spp,ignore.case=TRUE,value=TRUE))){
      val<-grep(i,CohortLookup$Value_spp,ignore.case=TRUE,value=TRUE)[1]
      ISN_dataset_initial[ISN_dataset_initial$SpeciesGroup==i,]$cohort_name<-CohortLookup[CohortLookup$Value_spp==val,]$`ID_Model-Cohort`
      
    }
    if(!is_empty(grep(i,CohortLookup$Value2,ignore.case=TRUE,value=TRUE))){
      val<- grep(i,CohortLookup$Value2,ignore.case=TRUE,value=TRUE)[1]
      ISN_dataset_initial[ISN_dataset_initial$SpeciesGroup==i,]$cohort_name<-CohortLookup[CohortLookup$Value2==val,]$`ID_Model-Cohort`
    }
    if(!is_empty(grep(i,CohortLookup$Value3,ignore.case=TRUE,value=TRUE))){
      val<- grep(i,CohortLookup$Value3,ignore.case=TRUE,value=TRUE)[1]
      ISN_dataset_initial[ISN_dataset_initial$SpeciesGroup==i,]$cohort_name<-CohortLookup[CohortLookup$Value3==val,]$`ID_Model-Cohort`
      
    }
  }
  
  
  #expansion factor
  # ISN_dataset_initial[ISN_dataset_initial$dbhcm>27.5,]$expansionfactor<-1
  # ISN_dataset_initial[ISN_dataset_initial$dbhcm<27.5,]$expansionfactor<-ISN_dataset_initial[ISN_dataset_initial$dbhcm<27.5,]$Plot.size..ha./0.03141593
  
  # Yield class
  ISN_dataset_initial[ISN_dataset_initial$species==EIB_data[EIB_data$site_name==Site_Name,]$species_major[1],]$YC<-as.integer(EIB_data[EIB_data$site_name==Site_Name,]$YC_major[1])
  if(!is.na(EIB_data[EIB_data$site_name==Site_Name,]$species_second[1])){
    ISN_dataset_initial[ISN_dataset_initial$species==EIB_data[EIB_data$site_name==Site_Name,]$species_second[1],]$YC<-as.integer(EIB_data[EIB_data$site_name==Site_Name,]$YC_second[1]) 
  }
  if(!is_empty(ISN_dataset_initial[is.na(ISN_dataset_initial$YC),]$YC)){
    ISN_dataset_initial[is.na(ISN_dataset_initial$YC),]$YC<-as.integer(EIB_data[EIB_data$site_name==Site_Name,]$YC_major[1])
  }
  
  ISN_dataset_initial$YC<-as.integer( ISN_dataset_initial$YC)
  if(!is_empty(ISN_dataset_initial[ISN_dataset_initial$cohort_name=="Douglas Fir",]$cohort_name)){
    ISN_dataset_initial[ISN_dataset_initial$cohort_name=="Douglas Fir",]$cohort_name<-"Douglas fir"
  }
  if(!is_empty(ISN_dataset_initial[!(ISN_dataset_initial$cohort_name %in% c("Spruce","Douglas fir")),]$YC)){
    ISN_dataset_initial[!(ISN_dataset_initial$cohort_name %in% c("Spruce","Douglas fir")),]$YC<-NA
  }
  
  return(ISN_dataset_initial)
  
}
###########################################
s<-0
for(i in 1:10){
s[i]<-max(Rushmore_Pre[Rushmore_Pre$idplots==i,]$BAL)
}
s/10


Nant<-ISN_dataset_Pre2(Crichel,Crichel_date,dataset_from_xlsx,Site_Name)
Rushmore_1<-Rushmore_1[!is.na(Rushmore_1$expansionfactor),]
Rushmore_1<-Rushmore_1[Rushmore_1$expansionfactor>0,]
Rushmore_1[Rushmore_1$cohort_name=="Douglas Fir",]$cohort_name<-"Douglas fir"
Rushmore_Pre<-Preprocessing_FERS(Rushmore_1)
ISN_dataset<-Rushmore

Rushmore_Pre$cohort_name

sum(Rushmore_Pre$BASEL_M_HA)
Crichel_1$cycle<-1
Crichel_2$cycle<-2
Crichel_3$cycle<-3
Crichel_ISN<-rbind(Crichel_1,Crichel_2,Crichel_3)


write.csv(Crichel_ISN,"Cranborne_208_ISN")



