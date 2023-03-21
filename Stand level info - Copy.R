Stand_Level_INFO<-data.frame(Forest_ID=NA,Plot_ID=NA,Cycle=NA,Years_between_cycle=NA,Total_Trees=NA,Regen_Trees=NA,Ingrowth=NA,harvest=NA,
           BAsal_Area_Very_Large_wood=NA,BAsal_Area_Large_wood=NA,BAsal_Area_Medium_wood=NA,BAsal_Area_small_wood=NA,BAsal_Area_poles_wood=NA,
           BAsal_Area_INC_Very_Large_wood=NA,BAsal_Area_INC_Large_wood=NA,BAsal_Area_INC_Medium_wood=NA,BAsal_Area_INC_small_wood=NA,BAsal_Area_INC_poles_wood=NA)



AFI_ISN_Data$Size<-NA
AFI_ISN_Data[AFI_ISN_Data$dbhcm>65,]$Size<- "VLW"
AFI_ISN_Data[AFI_ISN_Data$dbhcm>47.5 & AFI_ISN_Data$dbhcm<=65,]$Size<-"LW"
AFI_ISN_Data[AFI_ISN_Data$dbhcm>27.5 & AFI_ISN_Data$dbhcm<=47.5,]$Size<-"MW"
AFI_ISN_Data[AFI_ISN_Data$dbhcm>17.5 & AFI_ISN_Data$dbhcm<=27.5,]$Size<-"SW"
AFI_ISN_Data[AFI_ISN_Data$dbhcm>7.5 & AFI_ISN_Data$dbhcm<=17.5,]$Size<-"P"


###########################################################################################################################

#function to extract stand level info

plot_data_fun_Rele<-function(AFI_ISN_Data2){
  
  AFI_ISN_Data2<-AFI_ISN_Data2[AFI_ISN_Data2$Limit==0,]
  Developmental_Stage<-AFI_ISN_Data2$Development.stage[1]
  
  # data frame set up
  info_plot<-data.frame(Forest=NA,Plot=NA,Developmental_Stage = NA,Age= NA,Cycle=NA,Years_between_cycle=NA,Cohort = NA,Total_Trees=NA,
                        Regen_Trees=NA,Ingrowth=NA,harvest=NA,Basal_Area = NA,BAsal_Area_Very_Large_wood=NA,
                        BAsal_Area_Large_wood=NA,BAsal_Area_Medium_wood=NA,BAsal_Area_small_wood=NA,
                        BAsal_Area_poles_wood=NA,BAsal_Area_INC=NA,BAsal_Area_INC_Very_Large_wood=NA,
                        BAsal_Area_INC_Large_wood=NA,BAsal_Area_INC_Medium_wood=NA,
                        BAsal_Area_INC_small_wood=NA,BAsal_Area_INC_poles_wood=NA,Harvest_Basal_Area=NA,
                        Harvest_Basal_Very_Large_wood=NA,Harvest_Basal_Large_wood=NA,Harvest_Basal_Medium_wood=NA,Harvest_Basal_small_wood=NA,
                        Harvest_Basal_poles_wood=NA,Ingrowth_Basal_Area=NA,Ingrowth_basal_area_Very_Large_wood=NA,Ingrowth_basal_area_Large_wood=NA,
                        Ingrowth_Basal_Medium_wood=NA,Ingrowth_Basal_small_wood=NA,Ingrowth_Basal_Pole_wood=NA)
  
 # for each cycle
  for (cyc in unique(AFI_ISN_Data2$Cycle)){
    AFI_ISN_Data1<-AFI_ISN_Data2[AFI_ISN_Data2$Cycle==cyc,]
   # extract plots in this cycle
    Plotid<- unique(AFI_ISN_Data1$idplots)
    Plotid<-Plotid[order(Plotid)]
    # run loop by plot
  for (i in Plotid){
    #get cohorts in the forest
    species<-unique(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i ,]$cohort_name)
    for(f in species){ # run for loop for species so I can extract by cohort
      #cal basal area by species for VLW LW MW SW Poles
      
      BASAL<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f,]$RepreBAHA,na.rm=T)/length(unique(AFI_ISN_Data1$idplots))

      
      BASAL_VLW<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$dbhcm>65,]$RepreBAHA,na.rm=T)/length(unique(AFI_ISN_Data1$idplots))
      BASAL_LW<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$dbhcm>47.5 & AFI_ISN_Data1$dbhcm<=65,]$RepreBAHA,na.rm = T)/length(unique(AFI_ISN_Data1$idplots))
      BASAL_MW<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$dbhcm>27.5 & AFI_ISN_Data1$dbhcm<=47.5,]$RepreBAHA,na.rm = T)/length(unique(AFI_ISN_Data1$idplots))
      BASAL_SW<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$dbhcm>17.5 & AFI_ISN_Data1$dbhcm<=27.5,]$RepreBAHA,na.rm = T)/length(unique(AFI_ISN_Data1$idplots))
      BASAL_P<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$dbhcm>7.5 & AFI_ISN_Data1$dbhcm<=17.5,]$RepreBAHA,na.rm = T)/length(unique(AFI_ISN_Data1$idplots))
      

      # Cal Sotcking and harvest rates for each forest by species
      stocking_ha<- sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$Limit==0,]$expansionfactor/length(unique(AFI_ISN_Data1$idplots)),na.rm = T)
      harvest<-0
      harvest<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1),]$expansionfactor/length(unique(AFI_ISN_Data1$idplots)))
      
      
    harvest_rel<-length(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>=17.5,]$expansionfactor)*2.25
    harvest_rel<-harvest_rel+
      sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA,na.rm=T)
      
      
        harvest_basal_area<-NROW(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & 
                                                 AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) &
                                                 AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=17.5,])*2.25
        
        harvest_basal_area<-harvest_basal_area+sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & 
                                                                   AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & 
                                                                   AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0 & 
                                                                   AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA,na.rm=T)
        
        harvest_basal_area<-harvest_basal_area/length(unique(AFI_ISN_Data1$idplots))
        
         harvest_basal_area_VLW<-length(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=65,]$RepreBAHA)*2.25/length(unique(AFI_ISN_Data1$idplots))
     
          harvest_basal_area_LW<-length(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0  & AFI_ISN_Data2$dbhcm>=47.5 & AFI_ISN_Data2$dbhcm<65,]$RepreBAHA)*2.25/length(unique(AFI_ISN_Data1$idplots))
     
           harvest_basal_area_MW<-length(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=27.5 & AFI_ISN_Data2$dbhcm<47.5,]$RepreBAHA)*2.25/length(unique(AFI_ISN_Data1$idplots))
    
             harvest_basal_area_SW<-length(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=17.5 & AFI_ISN_Data2$dbhcm<27.5,]$RepreBAHA)*2.25/length(unique(AFI_ISN_Data1$idplots))
    
               harvest_basal_area_P<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA/length(unique(AFI_ISN_Data1$idplots)))
     
               ingrowth<-0
      Ingrowth<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$Flag=="ING" & !is.na(AFI_ISN_Data1$Flag) & AFI_ISN_Data1$Cycle==cyc,]$expansionfactor/length(unique(AFI_ISN_Data1$idplots)))
      
      Ingrowth_basal_area<-length(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$Flag=="ING" & !is.na(AFI_ISN_Data1$Flag) & 
                                                  AFI_ISN_Data1$Cycle==cyc & AFI_ISN_Data1$Limit==0 & AFI_ISN_Data1$dbhcm>=17.5,]$RepreBAHA)*2.25/length(unique(AFI_ISN_Data1$idplots))
      
      Ingrowth_basal_area<-Ingrowth_basal_area+ sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & 
                                                                    AFI_ISN_Data1$Flag=="ING" & !is.na(AFI_ISN_Data1$Flag) & 
                                                                    AFI_ISN_Data1$Cycle==cyc & AFI_ISN_Data1$Limit==0 & AFI_ISN_Data1$dbhcm<17.5,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
      Ingrowth_basal_area_VLW<-length(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="ING" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=65,]$RepreBAHA)*2.25/length(unique(AFI_ISN_Data1$idplots))
      
      Ingrowth_basal_area_LW<-length(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="ING" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc) & AFI_ISN_Data2$Limit==0  & AFI_ISN_Data2$dbhcm>=47.5 & AFI_ISN_Data2$dbhcm<65,]$RepreBAHA)*2.25/length(unique(AFI_ISN_Data1$idplots))
      
      Ingrowth_basal_area_MW<-length(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="ING" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=27.5 & AFI_ISN_Data2$dbhcm<47.5,]$RepreBAHA)*2.25/length(unique(AFI_ISN_Data1$idplots))
      
      Ingrowth_basal_area_SW<-length(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="ING" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=17.5 & AFI_ISN_Data2$dbhcm<27.5,]$RepreBAHA)*2.25/length(unique(AFI_ISN_Data1$idplots))
      
      Ingrowth_basal_area_P<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="ING" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA/length(unique(AFI_ISN_Data1$idplots)))
      
       BAsal_Area_INC<-NA
      BAsal_Area_INC_VLW<-NA
      BAsal_Area_INC_LW<-NA
      BAsal_Area_INC_MW<-NA
      BAsal_Area_INC_SW<-NA
      BAsal_Area_INC_P<-NA
      # cal increment by species from cyc i-1 to i
      if (cyc>1){
       #  BAsal_Area_INC<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc,]$RepreBAHA,na.rm = T)-
       #                  sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) ,]$RepreBAHA,na.rm = T)
       # 
       #  BAsal_Area_INC_VLW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc & AFI_ISN_Data2$dbhcm>65 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
       #                      sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>65 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)
       #  
       # BAsal_Area_INC_LW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f &  AFI_ISN_Data2$Cycle==cyc & AFI_ISN_Data2$dbhcm>47.5 & AFI_ISN_Data2$dbhcm<=65 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
       #                    sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>47.5 & AFI_ISN_Data2$dbhcm<=65 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)
       # 
       # BAsal_Area_INC_MW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc & AFI_ISN_Data2$dbhcm>27.5 & AFI_ISN_Data2$dbhcm<=47.5 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
       #                    sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>27.5 & AFI_ISN_Data2$dbhcm<=47.5 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)
       # 
       # BAsal_Area_INC_SW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc & AFI_ISN_Data2$dbhcm>17.5 & AFI_ISN_Data2$dbhcm<=27.5 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
       #                    sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>17.5 & AFI_ISN_Data2$dbhcm<=27.5 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)
       # 
       # BAsal_Area_INC_P<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc & AFI_ISN_Data2$dbhcm>7.5 & AFI_ISN_Data2$dbhcm<=17.5 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
       #                   sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>7.5 & AFI_ISN_Data2$dbhcm<=17.5 & is.na(AFI_ISN_Data2$Flag) ,]$RepreBAHA,na.rm = T)
       # 
       #   calcuulate the tree names in each group and get the increment by tree  then add the total instead of doing it the abve way
        VLW_Trees<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="VLW",]$TREE_NAME
        LW_Trees<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="LW",]$TREE_NAME
        MW_Trees<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="MW",]$TREE_NAME
        SW_Trees<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="SW",]$TREE_NAME
        P_Trees<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="P",]$TREE_NAME
 
   
        
        
        
         # BAsal_Area_INC<-(sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
         #   sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm=T))
         # BAsal_Area_INC<-BAsal_Area_INC/length(unique(AFI_ISN_Data1$idplots))

         ################
        ##                 BASAL inc for reloscope is all trees greater than 17.5 counted and subtracted by previous cycle multiplied by 2.25
        ##############
        cyc_data<- AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f &
                                   AFI_ISN_Data2$Cycle==cyc & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & 
                                   AFI_ISN_Data2$dbhcm>=17.5,]
        Pre_cyc_data<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & 
                                      AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 &
                                      AFI_ISN_Data2$dbhcm>=17.5,]
        
        Pre_cyc_data<- Pre_cyc_data[Pre_cyc_data$TREE_NAME %in% cyc_data$TREE_NAME, ]
         BAsal_Area_INC<-(length(cyc_data$RepreBAHA)-
                                 length(Pre_cyc_data$RepreBAHA))*2.25
         
         BAsal_Area_INC<- BAsal_Area_INC+ 
                sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA,na.rm = T)-
                sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA,na.rm=T)
         BAsal_Area_INC<-BAsal_Area_INC/length(unique(AFI_ISN_Data1$idplots))
         

         BAsal_Area_INC_VLW<-(length(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% VLW_Trees & AFI_ISN_Data2$Cycle==cyc  & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA)-
                             length(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% VLW_Trees & AFI_ISN_Data2$Cycle==(cyc-1)  & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA))*2.25
         
         BAsal_Area_INC_VLW<-BAsal_Area_INC_VLW/length(unique(AFI_ISN_Data1$idplots))
          
        BAsal_Area_INC_LW<-(length(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% LW_Trees & AFI_ISN_Data2$Cycle==cyc   & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA)-
                           length(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% LW_Trees & AFI_ISN_Data2$Cycle==(cyc-1)  & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA))*2.25
        
        
         BAsal_Area_INC_LW<-BAsal_Area_INC_LW/length(unique(AFI_ISN_Data1$idplots))

        BAsal_Area_INC_MW<-(length(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% MW_Trees & AFI_ISN_Data2$Cycle==cyc   & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA)-
                           length(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% MW_Trees & AFI_ISN_Data2$Cycle==(cyc-1)& AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA  ))*2.25
        
        
         BAsal_Area_INC_MW<-BAsal_Area_INC_MW/length(unique(AFI_ISN_Data1$idplots))

        BAsal_Area_INC_SW<-(length(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% SW_Trees & AFI_ISN_Data2$Cycle==cyc   & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA)-
                           length(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% SW_Trees & AFI_ISN_Data2$Cycle==(cyc-1)  & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA))*2.25
        
        
         BAsal_Area_INC_SW<-BAsal_Area_INC_SW/length(unique(AFI_ISN_Data1$idplots))

        
        BAsal_Area_INC_P<- sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% P_Trees & AFI_ISN_Data2$Cycle==cyc  & AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA,na.rm = T)-
          sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% P_Trees & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA,na.rm = T)
        
         BAsal_Area_INC_P<-BAsal_Area_INC_P/length(unique(AFI_ISN_Data1$idplots))


      }
      
      #enter all info in df
      info_plot_specie<- data.frame(Forest=AFI_ISN_Data1$Site_code[1],
                                    Plot=i,
                                    Developmental_Stage = Developmental_Stage,
                                    Age= NA,
                                    Cycle=cyc,
                                    Years_between_cycle=NA,
                                    Cohort = f,
                                    Total_Trees=stocking_ha,
                                    Regen_Trees=NA,
                                    Ingrowth=Ingrowth,
                                    harvest=harvest,
                                    Basal_Area = BASAL,
                                    BAsal_Area_Very_Large_wood=BASAL_VLW,
                                    BAsal_Area_Large_wood=BASAL_LW,
                                    BAsal_Area_Medium_wood=BASAL_MW,
                                    BAsal_Area_small_wood=BASAL_SW,
                                    BAsal_Area_poles_wood=BASAL_P,
                                    BAsal_Area_INC=BAsal_Area_INC,
                                    BAsal_Area_INC_Very_Large_wood=BAsal_Area_INC_VLW,
                                    BAsal_Area_INC_Large_wood=BAsal_Area_INC_LW,
                                    BAsal_Area_INC_Medium_wood=BAsal_Area_INC_MW,
                                    BAsal_Area_INC_small_wood=BAsal_Area_INC_SW,
                                    BAsal_Area_INC_poles_wood=BAsal_Area_INC_P,
                                    Harvest_Basal_Area=harvest_basal_area,
                                    Harvest_Basal_Very_Large_wood=harvest_basal_area_VLW,
                                    Harvest_Basal_Large_wood=harvest_basal_area_LW,
                                    Harvest_Basal_Medium_wood=harvest_basal_area_MW,
                                    Harvest_Basal_small_wood=harvest_basal_area_SW,
                                    Harvest_Basal_poles_wood=harvest_basal_area_P,
                                    Ingrowth_Basal_Area=Ingrowth_basal_area,
                                    Ingrowth_basal_area_Very_Large_wood=Ingrowth_basal_area_VLW,
                                    Ingrowth_basal_area_Large_wood=Ingrowth_basal_area_LW,
                                    Ingrowth_Basal_Medium_wood=Ingrowth_basal_area_MW,
                                    Ingrowth_Basal_small_wood=Ingrowth_basal_area_SW,
                                    Ingrowth_Basal_Pole_wood=Ingrowth_basal_area_P)
        
        
      rm(BASAL,BASAL_VLW,BASAL_LW,BASAL_MW,BASAL_SW,BASAL_P)
      
   
      info_plot<-rbind(info_plot,info_plot_specie)
    }
  }
  }
  info_plot<-info_plot[-1,]
  return(info_plot)
  
}


###########################################################################################################################
###########################################################################################################################

#function to extract stand level info

plot_data_fun<-function(AFI_ISN_Data2){
  
  AFI_ISN_Data2<-AFI_ISN_Data2[AFI_ISN_Data2$Limit==0,]
  Developmental_Stage<-AFI_ISN_Data2$Development.stage[1]
  
  # data frame set up
  info_plot<-data.frame(Forest=NA,Plot=NA,Developmental_Stage = NA,Age= NA,Cycle=NA,Years_between_cycle=NA,Cohort = NA,Total_Trees=NA,
                        Regen_Trees=NA,Ingrowth=NA,harvest=NA,Basal_Area = NA,BAsal_Area_Very_Large_wood=NA,
                        BAsal_Area_Large_wood=NA,BAsal_Area_Medium_wood=NA,BAsal_Area_small_wood=NA,
                        BAsal_Area_poles_wood=NA,BAsal_Area_INC=NA,BAsal_Area_INC_Very_Large_wood=NA,
                        BAsal_Area_INC_Large_wood=NA,BAsal_Area_INC_Medium_wood=NA,
                        BAsal_Area_INC_small_wood=NA,BAsal_Area_INC_poles_wood=NA,Harvest_Basal_Area=NA,
                        Harvest_Basal_Very_Large_wood=NA,Harvest_Basal_Large_wood=NA,Harvest_Basal_Medium_wood=NA,Harvest_Basal_small_wood=NA,
                        Harvest_Basal_poles_wood=NA,Ingrowth_Basal_Area=NA,Ingrowth_basal_area_Very_Large_wood=NA,Ingrowth_basal_area_Large_wood=NA,
                        Ingrowth_Basal_Medium_wood=NA,Ingrowth_Basal_small_wood=NA,Ingrowth_Basal_Pole_wood=NA)
  
  # for each cycle
  for (cyc in unique(AFI_ISN_Data2$Cycle)){
    AFI_ISN_Data1<-AFI_ISN_Data2[AFI_ISN_Data2$Cycle==cyc,]
    # extract plots in this cycle
    Plotid<- unique(AFI_ISN_Data1$idplots)
    Plotid<-Plotid[order(Plotid)]
    # run loop by plot
    for (i in Plotid){
      #get cohorts in the forest
      species<-unique(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i ,]$cohort_name)
      for(f in species){ # run for loop for species so I can extract by cohort
        #cal basal area by species for VLW LW MW SW Poles
        
        BASAL<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f,]$RepreBAHA,na.rm=T)/length(unique(AFI_ISN_Data1$idplots))

        BASAL_VLW<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$dbhcm>65,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        BASAL_LW<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$dbhcm>47.5 & AFI_ISN_Data1$dbhcm<=65,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        BASAL_MW<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$dbhcm>27.5 & AFI_ISN_Data1$dbhcm<=47.5,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        BASAL_SW<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$dbhcm>17.5 & AFI_ISN_Data1$dbhcm<=27.5,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        BASAL_P<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$dbhcm>7.5 & AFI_ISN_Data1$dbhcm<=17.5,]$RepreBAHA,na.rm = T)/length(unique(AFI_ISN_Data1$idplots))
        
        
        # Cal Sotcking and harvest rates for each forest by species
        stocking_ha<- sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$Limit==0,]$expansionfactor/length(unique(AFI_ISN_Data1$idplots)),na.rm = T)
        harvest<-0
        harvest<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1),]$expansionfactor)/length(unique(AFI_ISN_Data1$idplots))
        
        
        harvest_rel<-length(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>=17.5,]$expansionfactor)*2.25
        harvest_rel<-harvest_rel+
          sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA,na.rm=T)
        
        
        harvest_basal_area<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & 
                                                                   AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & 
                                                                   AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0 ,]$RepreBAHA,na.rm=T)
        
        
        
        
        harvest_basal_area<-harvest_basal_area/length(unique(AFI_ISN_Data1$idplots))
        
        harvest_basal_area_VLW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=65,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        
        harvest_basal_area_LW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0  & AFI_ISN_Data2$dbhcm>=47.5 & AFI_ISN_Data2$dbhcm<65,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        
        harvest_basal_area_MW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=27.5 & AFI_ISN_Data2$dbhcm<47.5,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        
        harvest_basal_area_SW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=17.5 & AFI_ISN_Data2$dbhcm<27.5,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        
        harvest_basal_area_P<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA/length(unique(AFI_ISN_Data1$idplots)))
        
        ingrowth<-0
        Ingrowth<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$Flag=="ING" & !is.na(AFI_ISN_Data1$Flag) & AFI_ISN_Data1$Cycle==cyc,]$expansionfactor/length(unique(AFI_ISN_Data1$idplots)))
        
        Ingrowth_basal_area<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & 
                                                                      AFI_ISN_Data1$Flag=="ING" & !is.na(AFI_ISN_Data1$Flag) & 
                                                                      AFI_ISN_Data1$Cycle==cyc & AFI_ISN_Data1$Limit==0,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        Ingrowth_basal_area_VLW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="ING" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=65,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        
        Ingrowth_basal_area_LW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="ING" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc) & AFI_ISN_Data2$Limit==0  & AFI_ISN_Data2$dbhcm>=47.5 & AFI_ISN_Data2$dbhcm<65,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        
        Ingrowth_basal_area_MW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="ING" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=27.5 & AFI_ISN_Data2$dbhcm<47.5,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        
        Ingrowth_basal_area_SW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="ING" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=17.5 & AFI_ISN_Data2$dbhcm<27.5,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        
        Ingrowth_basal_area_P<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="ING" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA/length(unique(AFI_ISN_Data1$idplots)))
        
        BAsal_Area_INC<-NA
        BAsal_Area_INC_VLW<-NA
        BAsal_Area_INC_LW<-NA
        BAsal_Area_INC_MW<-NA
        BAsal_Area_INC_SW<-NA
        BAsal_Area_INC_P<-NA
        # cal increment by species from cyc i-1 to i
        if (cyc>1){
          #  BAsal_Area_INC<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc,]$RepreBAHA,na.rm = T)-
          #                  sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) ,]$RepreBAHA,na.rm = T)
          # 
          #  BAsal_Area_INC_VLW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc & AFI_ISN_Data2$dbhcm>65 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
          #                      sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>65 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)
          #  
          # BAsal_Area_INC_LW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f &  AFI_ISN_Data2$Cycle==cyc & AFI_ISN_Data2$dbhcm>47.5 & AFI_ISN_Data2$dbhcm<=65 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
          #                    sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>47.5 & AFI_ISN_Data2$dbhcm<=65 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)
          # 
          # BAsal_Area_INC_MW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc & AFI_ISN_Data2$dbhcm>27.5 & AFI_ISN_Data2$dbhcm<=47.5 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
          #                    sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>27.5 & AFI_ISN_Data2$dbhcm<=47.5 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)
          # 
          # BAsal_Area_INC_SW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc & AFI_ISN_Data2$dbhcm>17.5 & AFI_ISN_Data2$dbhcm<=27.5 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
          #                    sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>17.5 & AFI_ISN_Data2$dbhcm<=27.5 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)
          # 
          # BAsal_Area_INC_P<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc & AFI_ISN_Data2$dbhcm>7.5 & AFI_ISN_Data2$dbhcm<=17.5 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
          #                   sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>7.5 & AFI_ISN_Data2$dbhcm<=17.5 & is.na(AFI_ISN_Data2$Flag) ,]$RepreBAHA,na.rm = T)
          # 
          #   calcuulate the tree names in each group and get the increment by tree  then add the total instead of doing it the abve way
          VLW_Trees<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="VLW",]$TREE_NAME
          LW_Trees<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="LW",]$TREE_NAME
          MW_Trees<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="MW",]$TREE_NAME
          SW_Trees<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="SW",]$TREE_NAME
          P_Trees<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="P",]$TREE_NAME
          
          
          VLW_Trees2<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="VLW",]$TREE_NAME
          LW_Trees2<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="LW",]$TREE_NAME
          MW_Trees2<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="MW",]$TREE_NAME
          SW_Trees2<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="SW",]$TREE_NAME
          P_Trees2<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="P",]$TREE_NAME
          
          
          
          
          
          # BAsal_Area_INC<-(sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
          #   sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm=T))
          # BAsal_Area_INC<-BAsal_Area_INC/length(unique(AFI_ISN_Data1$idplots))
          
          
          BAsal_Area_INC<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f &
                                              AFI_ISN_Data2$Cycle==cyc & AFI_ISN_Data2$Limit==0 &
                                              is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
            
                          sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f &
                                              AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0 & 
                                              is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm=T)
          
          BAsal_Area_INC<-BAsal_Area_INC/length(unique(AFI_ISN_Data1$idplots))
          
          
          BAsal_Area_INC_VLW<-(sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% VLW_Trees2 & AFI_ISN_Data2$Cycle==cyc  & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA)-
                                 sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% VLW_Trees & AFI_ISN_Data2$Cycle==(cyc-1)  & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA))
          
          BAsal_Area_INC_VLW<-BAsal_Area_INC_VLW/length(unique(AFI_ISN_Data1$idplots))
          
          BAsal_Area_INC_LW<-(sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% LW_Trees2 & AFI_ISN_Data2$Cycle==cyc   & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA)-
                                sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% LW_Trees & AFI_ISN_Data2$Cycle==(cyc-1)  & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA))
          
          
          BAsal_Area_INC_LW<-BAsal_Area_INC_LW/length(unique(AFI_ISN_Data1$idplots))
          
          BAsal_Area_INC_MW<-(sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% MW_Trees2 & AFI_ISN_Data2$Cycle==cyc   & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA)-
                                sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% MW_Trees & AFI_ISN_Data2$Cycle==(cyc-1)& AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA  ))
          
          
          BAsal_Area_INC_MW<-BAsal_Area_INC_MW/length(unique(AFI_ISN_Data1$idplots))
          
          BAsal_Area_INC_SW<-(sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% SW_Trees2 & AFI_ISN_Data2$Cycle==cyc   & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA)-
                                sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% SW_Trees & AFI_ISN_Data2$Cycle==(cyc-1)  & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA))
          
          
          BAsal_Area_INC_SW<-BAsal_Area_INC_SW/length(unique(AFI_ISN_Data1$idplots))
          
          
          BAsal_Area_INC_P<- sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% P_Trees2 & AFI_ISN_Data2$Cycle==cyc  & AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA,na.rm = T)-
            sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% P_Trees & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA,na.rm = T)
          
          BAsal_Area_INC_P<-BAsal_Area_INC_P/length(unique(AFI_ISN_Data1$idplots))
          
          
        }
        
        #enter all info in df
        info_plot_specie<- data.frame(Forest=AFI_ISN_Data1$Site_code[1],
                                      Plot=i,
                                      Developmental_Stage = Developmental_Stage,
                                      Age= NA,
                                      Cycle=cyc,
                                      Years_between_cycle=NA,
                                      Cohort = f,
                                      Total_Trees=stocking_ha,
                                      Regen_Trees=NA,
                                      Ingrowth=Ingrowth,
                                      harvest=harvest,
                                      Basal_Area = BASAL,
                                      BAsal_Area_Very_Large_wood=BASAL_VLW,
                                      BAsal_Area_Large_wood=BASAL_LW,
                                      BAsal_Area_Medium_wood=BASAL_MW,
                                      BAsal_Area_small_wood=BASAL_SW,
                                      BAsal_Area_poles_wood=BASAL_P,
                                      BAsal_Area_INC=BAsal_Area_INC,
                                      BAsal_Area_INC_Very_Large_wood=BAsal_Area_INC_VLW,
                                      BAsal_Area_INC_Large_wood=BAsal_Area_INC_LW,
                                      BAsal_Area_INC_Medium_wood=BAsal_Area_INC_MW,
                                      BAsal_Area_INC_small_wood=BAsal_Area_INC_SW,
                                      BAsal_Area_INC_poles_wood=BAsal_Area_INC_P,
                                      Harvest_Basal_Area=harvest_basal_area,
                                      Harvest_Basal_Very_Large_wood=harvest_basal_area_VLW,
                                      Harvest_Basal_Large_wood=harvest_basal_area_LW,
                                      Harvest_Basal_Medium_wood=harvest_basal_area_MW,
                                      Harvest_Basal_small_wood=harvest_basal_area_SW,
                                      Harvest_Basal_poles_wood=harvest_basal_area_P,
                                      Ingrowth_Basal_Area=Ingrowth_basal_area,
                                      Ingrowth_basal_area_Very_Large_wood=Ingrowth_basal_area_VLW,
                                      Ingrowth_basal_area_Large_wood=Ingrowth_basal_area_LW,
                                      Ingrowth_Basal_Medium_wood=Ingrowth_basal_area_MW,
                                      Ingrowth_Basal_small_wood=Ingrowth_basal_area_SW,
                                      Ingrowth_Basal_Pole_wood=Ingrowth_basal_area_P)
        
        
        rm(BASAL,BASAL_VLW,BASAL_LW,BASAL_MW,BASAL_SW,BASAL_P)
        
        
        info_plot<-rbind(info_plot,info_plot_specie)
      }
    }
  }
  info_plot<-info_plot[-1,]
  return(info_plot)
  
}
#########################################################################################################################
info_plot[!(colnames(info_plot) %in% colnames(info_plot_specie))]


sum(AFI_ISN_Data[AFI_ISN_Data$Site_code=="123"&
                   AFI_ISN_Data$Cycle==2 & 
                   !is.na(AFI_ISN_Data$Flag) & 
                   AFI_ISN_Data$Limit==0,]$RepreBAHA)-


sum(AFI_ISN_Data[AFI_ISN_Data$Site_code=="123"&
                   AFI_ISN_Data$Cycle==1 & 
                   AFI_ISN_Data$Limit==0,]$RepreBAHA) + 
  
  sum(AFI_ISN_Data[AFI_ISN_Data$Site_code=="123"&
                     AFI_ISN_Data$Cycle==1 & 
                     !is.na(AFI_ISN_Data$Flag) & 
                     AFI_ISN_Data$Flag=="H",]$RepreBAHA,na.rm = T)







sum(out$`Bryn Arau Duon INV`[out$`Bryn Arau Duon INV`$idplots==i & out$`Bryn Arau Duon INV`$cohort_name==f & out$`Bryn Arau Duon INV`$Cycle==cyc ,]$RepreBAHA,na.rm = T)-
  sum(out$`Bryn Arau Duon INV`[out$`Bryn Arau Duon INV`$idplots==i & out$`Bryn Arau Duon INV`$cohort_name==f & out$`Bryn Arau Duon INV`$Cycle==(cyc-1) & is.na(out$`Bryn Arau Duon INV`$Flag),]$RepreBAHA,na.rm = T)
View(out$`Bryn Arau Duon INV`[out$`Bryn Arau Duon INV`$idplots==i ,])
 t$`102`
t$`103`
t$`106`
  t$`120`
  t$`123`
  t$`14b Chetterwood`
  t$`208, 213a Boulsbury`
  t$`37c Daggons`
  t$`47/48/55 Llethr Gwinau`
  t$`67`
  t$`B2/3 Farnham Wood`
  View(t$`Bryn Arau Duon INV`)
    t$`L3 Chase Wood`
    #plot 2 
    t$`Llethr Gwinau ISN`
    t$`M9/10, M11, M13/15 Chase Wood`
    t$`Nant yr Eira INV`$BAsal_Area_INC
    t$`Nant yr Eira Wood`

out$`L3 Chase Wood`

# implementing stand level function
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

Baronscourt_YC
# Normal method
out <- split( AFI_ISN_Data , f = AFI_ISN_Data$Site_code )
t<-lapply(out,plot_data_fun)


# Reloscope method
out <- split( AFI_ISN_Data , f = AFI_ISN_Data$Site_code )
t1<-lapply(out,plot_data_fun_Rele)
View(t1$`208, 213a Boulsbury`)
sum(t1$`208, 213a Boulsbury`[t1$`208, 213a Boulsbury`$Cycle==1,]$Basal_Area)
sum(t1$`208, 213a Boulsbury`[t1$`208, 213a Boulsbury`$Cycle==2,]$Basal_Area)
sum(t1$`208, 213a Boulsbury`[t1$`208, 213a Boulsbury`$Cycle==3,]$Basal_Area)
sum(t1$`208, 213a Boulsbury`$Harvest_Basal_Area,na.rm=T)

View(t1$`37c Daggons`)

(AFI_ISN_Data[AFI_ISN_Data$Site_code=="37c Daggons" & AFI_ISN_Data$Cycle==2,]$Limit)

View(AFI_ISN_Data[AFI_ISN_Data$Site_code=="37c Daggons" & AFI_ISN_Data$Cycle==1 & !is.na(AFI_ISN_Data$Flag) & AFI_ISN_Data$Flag=="H" & AFI_ISN_Data$dbhcm>=17.5,])

View(AFI_ISN_Data[AFI_ISN_Data$Site_code=="37c Daggons"  & AFI_ISN_Data$Cycle==1,])


dsf<-AFI_ISN_Data[AFI_ISN_Data$Site_code=="37c Daggons" & AFI_ISN_Data$Cycle==1 & !is.na(AFI_ISN_Data$Flag) & AFI_ISN_Data$Flag=="H" & AFI_ISN_Data$dbhcm>=17.5,]$TREE_NAME
Cranborne_37c_final[Cranborne_37c_final$Cycle==1 & Cranborne_37c_final$TREE_NAME %in%]


dsf
###################################################################################################
# Version 2 of individual tree basal inc combined
out <- split( AFI_ISN_Data , f = AFI_ISN_Data$Site_code )
t2<-lapply(out,plot_data_fun_2)
for(n in 1:length(out)){
  print(n)
  print(plot_data_fun_2(out[[n]]))
}
out[[2]]

t<-t1
t_bc<-plot_data_fun(Baronscourt_AFI_pre_final)

View(t_bc)
t[[length(t)+1]]<-t_bc
###################################################################################################
View(t1$`37c Daggons`)
sum(t1$`37c Daggons`[t1$`37c Daggons`$Cycle==2,]$Basal_Area)*9-
sum(t1$`37c Daggons`[t1$`37c Daggons`$Cycle==1,]$Basal_Area)*9+
#sum(t$`37c Daggons`$Ingrowth_Basal_Area,na.rm=T)*9+
sum(t1$`37c Daggons`$Harvest_Basal_Area,na.rm=T)*9


sum(AFI_ISN_Data[AFI_ISN_Data$Site_code=="37c Daggons" & AFI_ISN_Data$Cycle==2,]$RepreBAHA,na.rm=T)/
8

sum(AFI_ISN_Data[AFI_ISN_Data$Site_code=="37c Daggons" & AFI_ISN_Data$Cycle==1,]$RepreBAHA,na.rm=T)/
  9

NROW(AFI_ISN_Data[AFI_ISN_Data$Site_code=="37c Daggons" & AFI_ISN_Data$Flag=="H" & !is.na(AFI_ISN_Data$Flag),]$RepreBAHA)
#,
 #   na.rm=T)

for ( i in 1:9){
  print(
  length(AFI_ISN_Data[AFI_ISN_Data$Site_code=="37c Daggons" & AFI_ISN_Data$idplots==i &
                        AFI_ISN_Data$Flag=="H" & !is.na(AFI_ISN_Data$Flag),]$RepreBAHA)
  )
}
#################################################################################################
#barons court age
#################################################################################################
Baronscourt_YC
# for i in id plot 
# pyear = pyear
t$Baronscourt$Plant_Yr<-NA
for( i in unique(AFI_ISN_Data[AFI_ISN_Data$Site_code=="Baronscourt" ,]$idplots)){
t$Baronscourt[t$Baronscourt$Plot==i,]$Plant_Yr<- Baronscourt_YC[Baronscourt_YC$PLOT==i,]$`P-Year`
}
#################################################################################################
# Nant_yr_Eira INV
#################################################################################################
ISN_LRS_YC_NYE
ISN_LRS_YC_NYE<- read_excel("D:/FERS Project/STVI/DATA/ISN Wales YC & Dev Stage Data.xlsx",sheet = "Nant yr Eira INV")
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

    t$`Nant yr Eira INV`$Plant_Yr<-NA
    #loop each plot to find plot plant year
      for ( i in unique(t$`Nant yr Eira INV`$Plot)){
        # plant yr is by species
        for( n in unique(AFI_ISN_Data[AFI_ISN_Data$Site_code=="Nant yr Eira INV" ,]$species)){
          #make sure there is no empties 
          if(!is_empty(ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$`SP Nb.`==as.numeric(i) & ISN_LRS_YC_NYE$Species==n,]$P_year)){
            cohort_nye<- unique(AFI_ISN_Data[AFI_ISN_Data$species==n,]$cohort_name)
            t$`Nant yr Eira INV`[t$`Nant yr Eira INV`$Cohort==cohort_nye & t$`Nant yr Eira INV`$Plot==i,]$Plant_Yr<-ISN_LRS_YC_NYE[ISN_LRS_YC_NYE$`SP Nb.`==as.numeric(i) & ISN_LRS_YC_NYE$Species==n,]$P_year[1]
          }
        }
        # if specie plant yr isnt in data then find any plant yr for plot and store this as pyear 
          if(!is_empty(t$`Nant yr Eira INV`[ t$`Nant yr Eira INV`$Plot==i & is.na(t$`Nant yr Eira INV`$Plant_Yr),]$Plant_Yr)){
             t$`Nant yr Eira INV`[ t$`Nant yr Eira INV`$Plot==i & is.na(t$`Nant yr Eira INV`$Plant_Yr),]$Plant_Yr<-
                                                    t$`Nant yr Eira INV`[ t$`Nant yr Eira INV`$Plot==i & !is.na(t$`Nant yr Eira INV`$Plant_Yr),]$Plant_Yr[1]
           }
      }


#View(AFI_ISN_Data[AFI_ISN_Data$Site_code=="Nant yr Eira INV" & AFI_ISN_Data$idplots==10,])
#View(ISN_LRS_YC_NYE)


#################################################################################################
#`Bryn Arau Duon INV`

        ISN_LRS_YC_bry$P_year
#View(t$`Bryn Arau Duon INV`)
        t$`Bryn Arau Duon INV`$Plant_Yr<-NA
        
        for( i in unique(AFI_ISN_Data[AFI_ISN_Data$Site_code=="Bryn Arau Duon INV" ,]$idplots)){
          t$`Bryn Arau Duon INV`[t$`Bryn Arau Duon INV`$Plot==i,]$Plant_Yr<- ISN_LRS_YC_bry[ISN_LRS_YC_bry$`SP Nb.`==i,]$P_year
        }

       # View(AFI_ISN_Data[AFI_ISN_Data$Site_code=="Bryn Arau Duon INV" & AFI_ISN_Data$idplots==8,])
#################################################################################################
#rest of woods

#Nant yr Eira Wood
        t$`Nant yr Eira Wood`$Years_between_cycle<-4
t$`Nant yr Eira Wood`$Plant_Yr<-1969
 t$`Nant yr Eira Wood`[t$`Nant yr Eira Wood`$Cycle==1,]$Age<-2017-t$`Nant yr Eira Wood`$Plant_Yr[1]
 t$`Nant yr Eira Wood`[t$`Nant yr Eira Wood`$Cycle==2,]$Age<-2021-t$`Nant yr Eira Wood`$Plant_Yr[1]
 #Llethr Gwinau ISN
 t$`Llethr Gwinau ISN`$Years_between_cycle<-4
t$`Llethr Gwinau ISN`$Plant_Yr<-1973
t$`Llethr Gwinau ISN`[t$`Llethr Gwinau ISN`$Cycle==1,]$Age<-2017-t$`Llethr Gwinau ISN`$Plant_Yr[1]
t$`Llethr Gwinau ISN`[t$`Llethr Gwinau ISN`$Cycle==2,]$Age<-2021-t$`Llethr Gwinau ISN`$Plant_Yr[1]
#14b Chetterwood
t$`14b Chetterwood`$Years_between_cycle<-3
t$`14b Chetterwood`$Plant_Yr<-1995
t$`14b Chetterwood`[t$`14b Chetterwood`$Cycle==1,]$Age<-2018-t$`14b Chetterwood`$Plant_Yr[1]
t$`14b Chetterwood`[t$`14b Chetterwood`$Cycle==2,]$Age<-2021-t$`14b Chetterwood`$Plant_Yr[1]
#208, 213a Boulsbury
t$`208, 213a Boulsbury`[t$`208, 213a Boulsbury`$Cycle%in%c(1,2),]$Years_between_cycle<-5
t$`208, 213a Boulsbury`[t$`208, 213a Boulsbury`$Cycle==3,]$Years_between_cycle<-4

t$`208, 213a Boulsbury`$Plant_Yr<-1962
t$`208, 213a Boulsbury`[t$`208, 213a Boulsbury`$Cycle==1,]$Age<-2012-t$`208, 213a Boulsbury`$Plant_Yr[1]
t$`208, 213a Boulsbury`[t$`208, 213a Boulsbury`$Cycle==2,]$Age<-2017-t$`208, 213a Boulsbury`$Plant_Yr[1]
t$`208, 213a Boulsbury`[t$`208, 213a Boulsbury`$Cycle==3,]$Age<-2021-t$`208, 213a Boulsbury`$Plant_Yr[1]

#37c Daggons
t$`37c Daggons`$Years_between_cycle<-5
t$`37c Daggons`$Plant_Yr<-1970
t$`37c Daggons`[t$`37c Daggons`$Cycle==1,]$Age<-2012-t$`37c Daggons`$Plant_Yr[1]
t$`37c Daggons`[t$`37c Daggons`$Cycle==2,]$Age<-2017-t$`37c Daggons`$Plant_Yr[1]
#47/48/55 Llethr Gwinau
# t$`47/48/55 Llethr Gwinau`$Years_between_cycle<-5
# t$`47/48/55 Llethr Gwinau`$Plant_Yr<-1973
# t$`47/48/55 Llethr Gwinau`[t$`47/48/55 Llethr Gwinau`$Cycle==1,]$Age<-2017-t$`47/48/55 Llethr Gwinau`$Plant_Yr[1]
# t$`47/48/55 Llethr Gwinau`[t$`47/48/55 Llethr Gwinau`$Cycle==2,]$Age<-2021-t$`47/48/55 Llethr Gwinau`$Plant_Yr[1]
#B2/3 Farnham Wood
t$`B2/3 Farnham Wood`$Years_between_cycle<-5
t$`B2/3 Farnham Wood`$Plant_Yr<-1963
t$`B2/3 Farnham Wood`[t$`B2/3 Farnham Wood`$Cycle==1,]$Age<-2017-t$`B2/3 Farnham Wood`$Plant_Yr[1]
t$`B2/3 Farnham Wood`[t$`B2/3 Farnham Wood`$Cycle==2,]$Age<-2021-t$`B2/3 Farnham Wood`$Plant_Yr[1]
#L3 Chase Wood
t$`L3 Chase Wood`$Years_between_cycle<-5
t$`L3 Chase Wood`$Plant_Yr<-1961
t$`L3 Chase Wood`[t$`L3 Chase Wood`$Cycle==1,]$Age<-2017-t$`L3 Chase Wood`$Plant_Yr[1]
t$`L3 Chase Wood`[t$`L3 Chase Wood`$Cycle==2,]$Age<-2021-t$`L3 Chase Wood`$Plant_Yr[1]

#M9/10, M11, M13/15 Chase Wood
t$`M9/10, M11, M13/15 Chase Wood`$Years_between_cycle<-5
t$`M9/10, M11, M13/15 Chase Wood`$Plant_Yr<-1952
t$`M9/10, M11, M13/15 Chase Wood`[t$`M9/10, M11, M13/15 Chase Wood`$Cycle==1,]$Age<-2017-t$`M9/10, M11, M13/15 Chase Wood`$Plant_Yr[1]
t$`M9/10, M11, M13/15 Chase Wood`[t$`M9/10, M11, M13/15 Chase Wood`$Cycle==2,]$Age<-2021-t$`M9/10, M11, M13/15 Chase Wood`$Plant_Yr[1]

#102
t$`102`$Years_between_cycle<-5
t$`102`$Plant_Yr<-1952
t$`102`[t$`102`$Cycle==1,]$Age<-2011-t$`102`$Plant_Yr[1]
t$`102`[t$`102`$Cycle==2,]$Age<-2016-t$`102`$Plant_Yr[1]
t$`102`[t$`102`$Cycle==3,]$Age<-2021-t$`102`$Plant_Yr[1]
#103
t$`103`$Years_between_cycle<-5
t$`103`$Plant_Yr<-1967
t$`103`[t$`103`$Cycle==1,]$Age<-2011-t$`103`$Plant_Yr[1]
t$`103`[t$`103`$Cycle==2,]$Age<-2016-t$`103`$Plant_Yr[1]
t$`103`[t$`103`$Cycle==3,]$Age<-2021-t$`103`$Plant_Yr[1]
#106
t$`106`$Years_between_cycle<-5
t$`106`$Plant_Yr<-NA
#120
t$`120`$Years_between_cycle<-5
t$`120`$Plant_Yr<-1964
t$`120`[t$`120`$Cycle==1,]$Age<-2012-t$`120`$Plant_Yr[1]
t$`120`[t$`120`$Cycle==2,]$Age<-2017-t$`120`$Plant_Yr[1]
#123
t$`123`$Years_between_cycle<-6
t$`123`$Plant_Yr<-1991
t$`123`[t$`123`$Cycle==1,]$Age<-2015-t$`123`$Plant_Yr[1]
t$`123`[t$`123`$Cycle==2,]$Age<-2021-t$`123`$Plant_Yr[1]
#  67
t$`67`$Years_between_cycle<-5
t$`67`$Plant_Yr<- NA
# baronscourt
t$Baronscourt$Years_between_cycle<-5
t$Baronscourt[t$Baronscourt$Cycle==1,]$Age<-2015-as.numeric(t$Baronscourt[t$Baronscourt$Cycle==1,]$Plant_Yr)
t$Baronscourt[t$Baronscourt$Cycle==2,]$Age<-2020-as.numeric(t$Baronscourt[t$Baronscourt$Cycle==2,]$Plant_Yr)
# bryn arau duon INV
t$`Bryn Arau Duon INV`$Years_between_cycle<-8
t$`Bryn Arau Duon INV`[t$`Bryn Arau Duon INV`$Cycle==1,]$Age<-2009-as.numeric(t$`Bryn Arau Duon INV`[t$`Bryn Arau Duon INV`$Cycle==1,]$Plant_Yr)
t$`Bryn Arau Duon INV`[t$`Bryn Arau Duon INV`$Cycle==2,]$Age<-2017-as.numeric(t$`Bryn Arau Duon INV`[t$`Bryn Arau Duon INV`$Cycle==2,]$Plant_Yr)

#"Nant yr Eira INV" 
t$`Nant yr Eira INV`$Years_between_cycle<-7
t$`Nant yr Eira INV`[t$`Nant yr Eira INV`$Cycle==1,]$Age<-2012-as.numeric(t$`Nant yr Eira INV`[t$`Nant yr Eira INV`$Cycle==1,]$Plant_Yr)
t$`Nant yr Eira INV`[t$`Nant yr Eira INV`$Cycle==2,]$Age<-2019-as.numeric(t$`Nant yr Eira INV`[t$`Nant yr Eira INV`$Cycle==2,]$Plant_Yr)

# Nant_yr_Eira_isn_pre_final
# t_NYE<-plot_data_fun(Nant_yr_Eira_isn_pre_final)
# t[[length(t)+1]]<-t_NYE
# # Llethir_Gwinau_pre_final
# t_LG<-plot_data_fun(Llethir_Gwinau_pre_final)
# t[[length(t)+1]]<-t_LG
# # Bryn_Arau_Duon_pre_final
# t_BAD<-plot_data_fun(Bryn_Arau_Duon_pre_final)
# t[[length(t)+1]]<-t_BAD

#t$`Bryn Arau Duon INV`[t$`Bryn Arau Duon INV`$Plot==8,]
# next do this for lsr and baronscourt
Stand_level_data<-data.frame()
for(i in 1:length(t)){
  Stand_level_data<-rbind(Stand_level_data,t[[i]])
}
View(Stand_level_data)


unique(Stand_level_data[is.na(Stand_level_data$Years_between_cycle),]$Forest)
NROW(AFI_ISN_Data1[AFI_ISN_Data1$Flag=="H",])

View(t$Baronscourt)

Stand_level_data[!is.na(Stand_level_data$harvest) & is.na(Stand_level_data$Harvest_Basal_Area),]

Stand_level_data[!is.na(Stand_level_data$Ingrowth) & is.na(Stand_level_data$Ingrowth_Basal_Area),]

# gap between cycles

View(Stand_level_data_old)

sum(AFI_ISN_Data[AFI_ISN_Data$Site_code==102 & AFI_ISN_Data$idplots==4 & AFI_ISN_Data$Cycle==3 & AFI_ISN_Data$Limit==0,]$RepreBAHA)
sum(AFI_ISN_Data[AFI_ISN_Data$Site_code==102 & AFI_ISN_Data$idplots==4 & AFI_ISN_Data$Cycle==2 & AFI_ISN_Data$Limit==0,]$RepreBAHA)

write.csv(Stand_level_data,"Stand_level_data")


# QC
Stand_level_data$
unique(Stand_level_data[Stand_level_data$BAsal_Area_INC<=0 & Stand_level_data$harvest<=1 & !is.na(Stand_level_data$harvest) & !is.na(Stand_level_data$BAsal_Area_INC),]$Plot)


View(AFI_ISN_Data[AFI_ISN_Data$Site_code==67 & AFI_ISN_Data$idplots==1 & AFI_ISN_Data$Cycle %in% c(1,2) & AFI_ISN_Data$cohort_name=="Douglas fir",])

sum(AFI_ISN_Data[AFI_ISN_Data$Site_code==67 & AFI_ISN_Data$idplots==1 & AFI_ISN_Data$Cycle %in% c(2) & AFI_ISN_Data$cohort_name=="Douglas fir",]$RepreBAHA,na.rm=T)



sum(AFI_ISN_Data[AFI_ISN_Data$Site_code==67 & AFI_ISN_Data$idplots==1 & AFI_ISN_Data$Cycle %in% c(3) & AFI_ISN_Data$cohort_name=="Douglas fir",]$RepreBAHA,na.rm=T)



AFI_ISN_Data2$


rm(AFI_ISN_Data2)
  AFI_ISN_Data2<-AFI_ISN_Data[AFI_ISN_Data$Site_code==67,]




  sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==1 & AFI_ISN_Data2$cohort_name=="Douglas fir" & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(2-1),]$expansionfactor)



  AFI_ISN_Data[AFI_ISN_Data$Site_code==67 & AFI_ISN_Data$idplots==1 & AFI_ISN_Data$Cycle %in% c(1) & AFI_ISN_Data$cohort_name=="Douglas fir",]$TREE_NAME[
                      !(AFI_ISN_Data[AFI_ISN_Data$Site_code==67 & AFI_ISN_Data$idplots==1 & AFI_ISN_Data$Cycle %in% c(1) & AFI_ISN_Data$cohort_name=="Douglas fir",]$TREE_NAME %in% 
                          AFI_ISN_Data[AFI_ISN_Data$Site_code==67 & AFI_ISN_Data$idplots==1 & AFI_ISN_Data$Cycle %in% c(2) & AFI_ISN_Data$cohort_name=="Douglas fir",]$TREE_NAME)]



  View(Stand_level_data)


unique(Stand_level_data$Forest)



unique(AFI_ISN_Data[AFI_ISN_Data$Site_code==site1,]$cohort_name)

View(AFI_ISN_Data[AFI_ISN_Data$Site_code==site1,])



sum(Stand_level_data[Stand_level_data$Forest==site1 & 
                       Stand_level_data$Cohort==co & Stand_level_data$Cycle==cyc,]$BAsal_Area_Very_Large_wood)
    sum(Stand_level_data[Stand_level_data$Forest==site1 & 
                           Stand_level_data$Cohort==co & Stand_level_data$Cycle==cyc,]$BAsal_Area_Large_wood)
    sum(Stand_level_data[Stand_level_data$Forest==site1 & 
                           Stand_level_data$Cohort==co & Stand_level_data$Cycle==cyc,]$BAsal_Area_Medium_wood)
    sum(Stand_level_data[Stand_level_data$Forest==site1 & 
                           Stand_level_data$Cohort==co & Stand_level_data$Cycle==cyc,]$BAsal_Area_small_wood)  
    
cyc=2

co=  "Douglas fir"  
site1="37c Daggons"

unique(Stand_level_data$Forest)
unique(Stand_level_data$Cohort)
sum(Rushmore_m9_2[Rushmore_m9_2$SpeciesGroup== "Broadleaves"  & Rushmore_m9_2$Cat=="LW",]$`BA/ha`,na.rm=T)

# cranborne 208
#Douglas fir cyc 1
# Pine cyc 1 & 2 & 3
# other conifers cyc1
length(unique(AFI_ISN_Data[AFI_ISN_Data$Site_code==site1,]$idplots))

# crichel b 

# "M9/10, M11, M13/15 Chase Wood"
#cranborne 37c daggons


sum(Stand_level_data[Stand_level_data$Forest==site1 & Stand_level_data$Cycle==2 ,]$BAsal_Area_Large_wood)


#234-161
View(AFI_ISN_Data[AFI_ISN_Data$Site_code==site1 & AFI_ISN_Data$Cycle==2,])

View(AFI_ISN_Data[AFI_ISN_Data$Site_code==site1 & AFI_ISN_Data$Cycle==cyc &
                    AFI_ISN_Data$dbhcm>27.5 & AFI_ISN_Data$dbhcm<=47.5 & 
                   AFI_ISN_Data$Limit==0 & AFI_ISN_Data$cohort_name==co,])

View(AFI_ISN_Data[AFI_ISN_Data$Site_code==site1  & AFI_ISN_Data$idplots==6 & AFI_ISN_Data$cohort_name==co & AFI_ISN_Data$Cycle %in% c(1,2) & is.na(AFI_ISN_Data$Flag),])
  sum(AFI_ISN_Data[AFI_ISN_Data$Site_code==site1 & AFI_ISN_Data$Cycle==cyc-1 & AFI_ISN_Data$cohort_name==co,]$dbhcm)
  
  sum(AFI_ISN_Data[AFI_ISN_Data$Site_code==site1 & AFI_ISN_Data$Cycle==2& is.na(AFI_ISN_Data$Flag) ,]$RepreBAHA,na.rm=T)/9
  sum(AFI_ISN_Data[AFI_ISN_Data$Site_code==site1 & AFI_ISN_Data$Cycle==2 ,]$RepreBAHA,na.rm=T)/9
  
  sum(AFI_ISN_Data[AFI_ISN_Data$Site_code==site1 & AFI_ISN_Data$Cycle==cyc-1& is.na(AFI_ISN_Data$Flag) & AFI_ISN_Data$dbhcm>=17.5,]$RepreBAHA,na.rm=T)
  


View(Stand_level_data[Stand_level_data$Forest %in% unique(AFI_ISN_Data[AFI_ISN_Data$Type=="ISN",]$Site_code),])
sum(Stand_level_data[Stand_level_data$Forest==site1 ,]$Harvest_Basal_Area)

sum(Stand_level_data[Stand_level_data$Forest==site1 & Stand_level_data$Cycle==2 ,]$BAsal_Area_INC)+
sum(Stand_level_data[Stand_level_data$Forest==site1 & Stand_level_data$Cycle==2 ,]$Harvest_Basal_Area)+
  sum(Stand_level_data[Stand_level_data$Forest==site1 & Stand_level_data$Cycle==2 ,]$Ingrowth_Basal_Area)
  

NROW(AFI_ISN_Data[AFI_ISN_Data$Site_code=="L3 Chase Wood" & AFI_ISN_Data$Limit==0 & AFI_ISN_Data$Cycle==2 & AFI_ISN_Data$dbhcm>=17.5,])
NROW(AFI_ISN_Data[AFI_ISN_Data$Site_code=="L3 Chase Wood" & AFI_ISN_Data$Limit==0 & AFI_ISN_Data$Cycle==1 & AFI_ISN_Data$dbhcm>=17.5,])
View(AFI_ISN_Data[AFI_ISN_Data$Site_code=="L3 Chase Wood" & AFI_ISN_Data$Cycle==1 & AFI_ISN_Data$dbhcm>=17.5,])


View(Stand_level_data)
AFI_ISN_Data[AFI_ISN_Data$Site_code==site1 & AFI_ISN_Data$dbhcm>=47.5 & 
                    AFI_ISN_Data$Cycle==cyc & AFI_ISN_Data$cohort_name==co,]

View(AFI_ISN_Data[AFI_ISN_Data$Site_code==site1,])
& is.na(AFI_ISN_Data$Flag) & AFI_ISN_Data$Cycle==2,])


# chnge to less than or equal to in grouping of trees
# reloscope data must be baf*N 

for(i in 1:9){
  print(i)
  print(sum(Stand_level_data[Stand_level_data$Forest==site1 & Stand_level_data$Cycle==2 & Stand_level_data$Plot==i,]$Basal_Area)-
          sum(Stand_level_data[Stand_level_data$Forest==site1 & Stand_level_data$Cycle==1 & Stand_level_data$Plot==i ,]$Basal_Area))
}

sum(AFI_ISN_Data[AFI_ISN_Data$Site_code==site1 & AFI_ISN_Data$Flag=="H",]$RepreBAHA,na.rm=T)
View(AFI_ISN_Data[AFI_ISN_Data$Site_code==site1 & AFI_ISN_Data$Flag=="H" & !is.na(AFI_ISN_Data$Flag),])


length(AFI_ISN_Data[AFI_ISN_Data$Site_code==site1 & AFI_ISN_Data$dbhcm>=17.5 & AFI_ISN_Data$Cycle==2 & AFI_ISN_Data$Limit==0,]$RepreBAHA)*2.25+
sum(AFI_ISN_Data[AFI_ISN_Data$Site_code==site1 & AFI_ISN_Data$dbhcm<17.5 & AFI_ISN_Data$Cycle==2 & AFI_ISN_Data$Limit==0,]$RepreBAHA)-


length(AFI_ISN_Data[AFI_ISN_Data$Site_code==site1 & AFI_ISN_Data$dbhcm>=17.5 & AFI_ISN_Data$Cycle==1 & AFI_ISN_Data$Limit==0,]$RepreBAHA)*2.25-
sum(AFI_ISN_Data[AFI_ISN_Data$Site_code==site1 & AFI_ISN_Data$dbhcm<17.5 & AFI_ISN_Data$Cycle==1 & AFI_ISN_Data$Limit==0,]$RepreBAHA)





library(stringr)

as.numeric(str_extract(AFI_ISN_Data[AFI_ISN_Data$Type=="ISN",]$Tree_ID
, "[^_]+"))




AFI_ISN_Data$dist<-as.numeric(str_extract(AFI_ISN_Data$Tree_ID
                                                                     , "[^_]+"))

#IF((dbhcm>30 & dist*0.03*100>dbhcm))-> limit=0









View(AFI_ISN_Data[AFI_ISN_Data$Type=="AFI" & AFI_ISN_Data$dbhcm>30 & AFI_ISN_Data$dist*3>AFI_ISN_Data$dbhcm,])





tst<-stourhead_cycle_123_AFI_PRE[stourhead_cycle_123_AFI_PRE$Cycle==4,]
tst<-tst[!is.na(tst$dbhcm),]
tst$dist<- sqrt(tst$X^2 + tst$Y^2)

tst[tst$dbhcm>30 & tst$dist*3>tst$dbhcm  ,]$TREE_NAME


View(AFI_ISN_Data[AFI_ISN_Data$Site_code=="67" & AFI_ISN_Data$dbhcm>30 & AFI_ISN_Data$dist*3>AFI_ISN_Data$dbhcm & AFI_ISN_Data$Cycle>3,])


# convert limmit for last cycle to actual limit as you messed that up. 
# find why there is negative increments and resolve

# check the relescope method stand level info and ensure that the results are correct and match what was done in excel ( or close to it )


# fix AFI limit
AFI_ISN_Data[AFI_ISN_Data$Type=="AFI",]$Limit<-0
AFI_ISN_Data[AFI_ISN_Data$Type=="AFI" & AFI_ISN_Data$dbhcm>17.5 & AFI_ISN_Data$dist*3>AFI_ISN_Data$dbhcm,]$Limit<-1

# fix ISN 
unique(AFI_ISN_Data$Type)
AFI_ISN_Data[AFI_ISN_Data$Type!="AFI",]$Limit<-0
AFI_ISN_Data[AFI_ISN_Data$Type!="AFI" & AFI_ISN_Data$dbhcm>30 & AFI_ISN_Data$dist*3>AFI_ISN_Data$dbhcm,]$Limit<-1
AFI_ISN_Data[AFI_ISN_Data$Type!="AFI" & AFI_ISN_Data$dbhcm<30 & AFI_ISN_Data$dist*3>AFI_ISN_Data$dbhcm & AFI_ISN_Data$dist>10,]$Limit<-1
AFI_ISN_Data[AFI_ISN_Data$Type!="AFI" & AFI_ISN_Data$dbhcm<30 & AFI_ISN_Data$dist*3>AFI_ISN_Data$dbhcm & AFI_ISN_Data$dist>10,]$Limit

sdsd<-read.csv(file.choose())
AFI_ISN_Data[AFI_ISN_Data$Type=="AFI" & !(AFI_ISN_Data$dbhcm<17.5 & AFI_ISN_Data$dist>10),]$Limit

View(AFI_ISN_Data[AFI_ISN_Data$Type=="AFI" & AFI_ISN_Data$dbhcm<17.5 & AFI_ISN_Data$dist>10,])


AFI_dataset<-AFI_ISN_Data[AFI_ISN_Data$Type=="AFI",]


AFI_ISN_Data$lim_fac<-AFI_ISN_Data$dist*3











AFI_ISN_Data[AFI_ISN_Data$Site_code %in% c("120","123") & AFI_ISN_Data$Cycle==2,]$Limit


sdsd[sdsd$Site_code %in% c("67")& sdsd$Cycle==4,]$Limit
sum(AFI_ISN_Data[AFI_ISN_Data$Site_code %in% c("102","103","106") & AFI_ISN_Data$Cycle==3,]$Limit)


sum(AFI_ISN_Data[AFI_ISN_Data$Site_code %in% c("67") & AFI_ISN_Data$Cycle==4,]$Limit)






View(AFI_ISN_Data[AFI_ISN_Data$Limit==1,])

View(AFI_ISN_Data[is.na(AFI_ISN_Data$RepreBAHA),])








AFI_dataset[(AFI_dataset$dbhcm)>17.5 &
              AFI_dataset$dist*0.03*100>(AFI_dataset$dbhcm) & 
              !is.na((AFI_dataset$dbhcm)),]$Limit
AFI_dataset[(AFI_dataset$dbhcm)<17.5 & AFI_dataset$dist>10 &
              !is.na((AFI_dataset$dbhcm)),]$Limit




tst<-AFI_ISN_Data[AFI_ISN_Data$Site_code=="67",]

stourhead_cycle_1_2_3$tr_nme<-paste(stourhead_cycle_1_2_3$NumPlac,stourhead_cycle_1_2_3$NumArbre)


tst$xdist<-NA
tst$x.1<-NA
for(i in unique(stourhead_cycle_1_2_3$tr_nme)){
  if(!is_empty(tst[tst$TREE_NAME==i,]$xdist)){
tst[tst$TREE_NAME==i,]$xdist<-stourhead_cycle_1_2_3[stourhead_cycle_1_2_3$tr_nme==i,]$Distance[1]
  tst[tst$TREE_NAME==i,]$x.1<-stourhead_cycle_1_2_3[stourhead_cycle_1_2_3$tr_nme==i,]$X_1[1]}
}

tst$dist-(tst$xdist)

View(stourhead_cycle_1_2_3[stourhead_cycle_1_2_3$tr_nme==i,])

tst$X
stourhead_cycle_1_2_3$X_1<-stourhead_cycle_1_2_3$Distance*cos(stourhead_cycle_1_2_3$Azimut)



Monivea_cycle_1$Distance*cos(Monivea_cycle_1$Azimut)
Monivea_cycle_1_PRE$X

Finalized_Monivea_102


table(AFI_ISN_Data$Type,AFI_ISN_Data$Limit)
table(sdsd$Type,sdsd$Limit)

table(AFI_ISN_Data$Site_code,AFI_ISN_Data$Limit)
table(sdsd$Site_code,sdsd$Limit)



AFI_ISN_Data$dist<-sqrt(AFI_ISN_Data$X^2+AFI_ISN_Data$Y^2)
AFI_dataset<-AFI_ISN_Data
AFI_dataset[(AFI_dataset$dbhcm)<17.5 & AFI_dataset$dist>10 & !is.na(AFI_dataset$dbhcm),]

tstst[tstst$dist*3>tstst$dbhcm & tstst$dbhcm>17.5]

View(sdsd[(sdsd$dbhcm)<17.5 & sdsd$dist>10 & !is.na(sdsd$dbhcm),])
sdsd$dist<-sqrt(sdsd$X^2+sdsd$Y^2)

AFI_ISN_Data[AFI_ISN_Data$Site_code=="103",]


View(Stand_level_data[Stand_level_data$BAsal_Area_INC+Stand_level_data$Harvest_Basal_Area+Stand_level_data$Ingrowth_Basal_Area<0 &
                        !is.na(Stand_level_data$Ingrowth_Basal_Area) &
                        !is.na(Stand_level_data$BAsal_Area_INC),])

Stand_level_data[Stand_level_data$BAsal_Area_INC+Stand_level_data$Harvest_Basal_Area+Stand_level_data$Ingrowth_Basal_Area<0 &
                   !is.na(Stand_level_data$Ingrowth_Basal_Area) &
                 !is.na(Stand_level_data$BAsal_Area_INC),]
















###################################################################################################################
#####################################################################################################################
########################################################################################################################
AFI_ISN_Data2<-AFI_ISN_Data[AFI_ISN_Data$Site_code=="Baronscourt",]
plot_data_fun_2<-function(AFI_ISN_Data2){
  
  
  Developmental_Stage<-AFI_ISN_Data2$Development.stage[1]
  
  # data frame set up
  info_plot<-data.frame(Forest=NA,Plot=NA,Developmental_Stage = NA,Age= NA,Cycle=NA,Years_between_cycle=NA,Cohort = NA,Total_Trees=NA,
                        Regen_Trees=NA,Ingrowth=NA,harvest=NA,Basal_Area = NA,BAsal_Area_Very_Large_wood=NA,
                        BAsal_Area_Large_wood=NA,BAsal_Area_Medium_wood=NA,BAsal_Area_small_wood=NA,
                        BAsal_Area_poles_wood=NA,BAsal_Area_INC=NA,BAsal_Area_INC_Very_Large_wood=NA,
                        BAsal_Area_INC_Large_wood=NA,BAsal_Area_INC_Medium_wood=NA,
                        BAsal_Area_INC_small_wood=NA,BAsal_Area_INC_poles_wood=NA,Harvest_Basal_Area=NA,
                        Harvest_Basal_Very_Large_wood=NA,Harvest_Basal_Large_wood=NA,Harvest_Basal_Medium_wood=NA,Harvest_Basal_small_wood=NA,
                        Harvest_Basal_poles_wood=NA,Ingrowth_Basal_Area=NA,Ingrowth_basal_area_Very_Large_wood=NA,Ingrowth_basal_area_Large_wood=NA,
                        Ingrowth_Basal_Medium_wood=NA,Ingrowth_Basal_small_wood=NA,Ingrowth_Basal_Pole_wood=NA)
  
  # for each cycle
  for (cyc in unique(AFI_ISN_Data2$Cycle)){
    AFI_ISN_Data1<-AFI_ISN_Data2[AFI_ISN_Data2$Cycle==cyc,]
    # extract plots in this cycle
    Plotid<- unique(AFI_ISN_Data1$idplots)
    Plotid<-Plotid[order(Plotid)]
    # run loop by plot
    for (i in Plotid){
      #get cohorts in the forest
      species<-unique(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i ,]$cohort_name)
      for(f in species){ # run for loop for species so I can extract by cohort
        #cal basal area by species for VLW LW MW SW Poles
        
        BASAL<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f,]$RepreBAHA,na.rm=T)/length(unique(AFI_ISN_Data1$idplots))
        
        
        BASAL_VLW<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$dbhcm>65,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        BASAL_LW<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$dbhcm>47.5 & AFI_ISN_Data1$dbhcm<=65,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        BASAL_MW<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$dbhcm>27.5 & AFI_ISN_Data1$dbhcm<=47.5,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        BASAL_SW<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$dbhcm>17.5 & AFI_ISN_Data1$dbhcm<=27.5,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        BASAL_P<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$dbhcm>7.5 & AFI_ISN_Data1$dbhcm<=17.5,]$RepreBAHA,na.rm = T)/length(unique(AFI_ISN_Data1$idplots))
        
        
        # Cal Sotcking and harvest rates for each forest by species
        stocking_ha<- sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$Limit==0,]$expansionfactor/length(unique(AFI_ISN_Data1$idplots)),na.rm = T)
        harvest<-0
        harvest<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1),]$expansionfactor)/length(unique(AFI_ISN_Data1$idplots))
        
        
        harvest_rel<-length(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>=17.5,]$expansionfactor)*2.25
        harvest_rel<-harvest_rel+
          sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA,na.rm=T)
        
        
        harvest_basal_area<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & 
                                                AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & 
                                                AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0 ,]$RepreBAHA,na.rm=T)
        
        harvest_basal_area<-harvest_basal_area/length(unique(AFI_ISN_Data1$idplots))
        
        harvest_basal_area_VLW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=65,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        
        harvest_basal_area_LW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0  & AFI_ISN_Data2$dbhcm>=47.5 & AFI_ISN_Data2$dbhcm<65,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        
        harvest_basal_area_MW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=27.5 & AFI_ISN_Data2$dbhcm<47.5,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        
        harvest_basal_area_SW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=17.5 & AFI_ISN_Data2$dbhcm<27.5,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        
        harvest_basal_area_P<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="H" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA/length(unique(AFI_ISN_Data1$idplots)))
        
        ingrowth<-0
        Ingrowth<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & AFI_ISN_Data1$Flag=="ING" & !is.na(AFI_ISN_Data1$Flag) & AFI_ISN_Data1$Cycle==cyc,]$expansionfactor/length(unique(AFI_ISN_Data1$idplots)))
        
        Ingrowth_basal_area<-sum(AFI_ISN_Data1[AFI_ISN_Data1$idplots==i & AFI_ISN_Data1$cohort_name==f & 
                                                 AFI_ISN_Data1$Flag=="ING" & !is.na(AFI_ISN_Data1$Flag) & 
                                                 AFI_ISN_Data1$Cycle==cyc & AFI_ISN_Data1$Limit==0,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        Ingrowth_basal_area_VLW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="ING" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=65,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        
        Ingrowth_basal_area_LW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="ING" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc) & AFI_ISN_Data2$Limit==0  & AFI_ISN_Data2$dbhcm>=47.5 & AFI_ISN_Data2$dbhcm<65,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        
        Ingrowth_basal_area_MW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="ING" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=27.5 & AFI_ISN_Data2$dbhcm<47.5,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        
        Ingrowth_basal_area_SW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="ING" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm>=17.5 & AFI_ISN_Data2$dbhcm<27.5,]$RepreBAHA)/length(unique(AFI_ISN_Data1$idplots))
        
        Ingrowth_basal_area_P<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Flag=="ING" & !is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Cycle==(cyc) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA/length(unique(AFI_ISN_Data1$idplots)))
        
        BAsal_Area_INC<-NA
        BAsal_Area_INC_VLW<-NA
        BAsal_Area_INC_LW<-NA
        BAsal_Area_INC_MW<-NA
        BAsal_Area_INC_SW<-NA
        BAsal_Area_INC_P<-NA
        # cal increment by species from cyc i-1 to i
        if (cyc>1){
          #  BAsal_Area_INC<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc,]$RepreBAHA,na.rm = T)-
          #                  sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) ,]$RepreBAHA,na.rm = T)
          # 
          #  BAsal_Area_INC_VLW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc & AFI_ISN_Data2$dbhcm>65 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
          #                      sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>65 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)
          #  
          # BAsal_Area_INC_LW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f &  AFI_ISN_Data2$Cycle==cyc & AFI_ISN_Data2$dbhcm>47.5 & AFI_ISN_Data2$dbhcm<=65 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
          #                    sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>47.5 & AFI_ISN_Data2$dbhcm<=65 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)
          # 
          # BAsal_Area_INC_MW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc & AFI_ISN_Data2$dbhcm>27.5 & AFI_ISN_Data2$dbhcm<=47.5 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
          #                    sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>27.5 & AFI_ISN_Data2$dbhcm<=47.5 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)
          # 
          # BAsal_Area_INC_SW<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc & AFI_ISN_Data2$dbhcm>17.5 & AFI_ISN_Data2$dbhcm<=27.5 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
          #                    sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>17.5 & AFI_ISN_Data2$dbhcm<=27.5 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)
          # 
          # BAsal_Area_INC_P<-sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc & AFI_ISN_Data2$dbhcm>7.5 & AFI_ISN_Data2$dbhcm<=17.5 & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
          #                   sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm>7.5 & AFI_ISN_Data2$dbhcm<=17.5 & is.na(AFI_ISN_Data2$Flag) ,]$RepreBAHA,na.rm = T)
          # 
          #   calcuulate the tree names in each group and get the increment by tree  then add the total instead of doing it the abve way
          VLW_Trees<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="VLW",]$TREE_NAME
          LW_Trees<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="LW",]$TREE_NAME
          MW_Trees<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="MW",]$TREE_NAME
          SW_Trees<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="SW",]$TREE_NAME
          P_Trees<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag) & AFI_ISN_Data2$Limit==0 & AFI_ISN_Data2$Size=="P",]$TREE_NAME
          
          
          
          
          
          # BAsal_Area_INC<-(sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==cyc & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm = T)-
          #   sum(AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) & is.na(AFI_ISN_Data2$Flag),]$RepreBAHA,na.rm=T))
          # BAsal_Area_INC<-BAsal_Area_INC/length(unique(AFI_ISN_Data1$idplots))
          rm(BAsal_Area_INC)
          
          BAsal_Area_INC<-0
          dat1<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & AFI_ISN_Data2$Cycle==(cyc-1) &
                                is.na(AFI_ISN_Data2$Flag),]
          for ( tree_n in unique(dat1$TREE_NAME)){
            indi_tree_grwth<-(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME==tree_n & AFI_ISN_Data2$Cycle==cyc,]$BASEL_M_HA[1]-
                                AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME==tree_n & AFI_ISN_Data2$Cycle==(cyc-1),]$BASEL_M_HA[1])*
              AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME==tree_n & AFI_ISN_Data2$Cycle==(cyc-1),]$expansionfactor[1]
            if(!is.na(indi_tree_grwth)){
            BAsal_Area_INC<-indi_tree_grwth+BAsal_Area_INC}
            
          
            
          }
          extra_ba<-AFI_ISN_Data2[AFI_ISN_Data2$idplots==i & AFI_ISN_Data2$cohort_name==f & 
                                    AFI_ISN_Data2$Cycle==cyc & is.na(AFI_ISN_Data2$Flag) & !(AFI_ISN_Data2$TREE_NAME %in% dat1$TREE_NAME),]$RepreBAHA
          
          if(!is_empty(extra_ba)){
          BAsal_Area_INC<-BAsal_Area_INC+ extra_ba
          }
          rm(extra_ba)
          BAsal_Area_INC<-BAsal_Area_INC/length(unique(AFI_ISN_Data1$idplots))
          
          
          BAsal_Area_INC_VLW<-(sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% VLW_Trees & AFI_ISN_Data2$Cycle==cyc  & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA)-
                                 sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% VLW_Trees & AFI_ISN_Data2$Cycle==(cyc-1)  & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA))
          
          BAsal_Area_INC_VLW<-BAsal_Area_INC_VLW/length(unique(AFI_ISN_Data1$idplots))
          
          BAsal_Area_INC_LW<-(sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% LW_Trees & AFI_ISN_Data2$Cycle==cyc   & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA)-
                                sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% LW_Trees & AFI_ISN_Data2$Cycle==(cyc-1)  & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA))
          
          
          BAsal_Area_INC_LW<-BAsal_Area_INC_LW/length(unique(AFI_ISN_Data1$idplots))
          
          BAsal_Area_INC_MW<-(sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% MW_Trees & AFI_ISN_Data2$Cycle==cyc   & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA)-
                                sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% MW_Trees & AFI_ISN_Data2$Cycle==(cyc-1)& AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA  ))
          
          
          BAsal_Area_INC_MW<-BAsal_Area_INC_MW/length(unique(AFI_ISN_Data1$idplots))
          
          BAsal_Area_INC_SW<-(sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% SW_Trees & AFI_ISN_Data2$Cycle==cyc   & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA)-
                                sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% SW_Trees & AFI_ISN_Data2$Cycle==(cyc-1)  & AFI_ISN_Data2$dbhcm>=17.5,]$RepreBAHA))
          
          
          BAsal_Area_INC_SW<-BAsal_Area_INC_SW/length(unique(AFI_ISN_Data1$idplots))
          
          
          BAsal_Area_INC_P<- sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% P_Trees & AFI_ISN_Data2$Cycle==cyc  & AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA,na.rm = T)-
            sum(AFI_ISN_Data2[AFI_ISN_Data2$TREE_NAME %in% P_Trees & AFI_ISN_Data2$Cycle==(cyc-1) & AFI_ISN_Data2$dbhcm<17.5,]$RepreBAHA,na.rm = T)
          
          BAsal_Area_INC_P<-BAsal_Area_INC_P/length(unique(AFI_ISN_Data1$idplots))
          
          
        }
        
        #enter all info in df
        info_plot_specie<- data.frame(Forest=AFI_ISN_Data1$Site_code[1],
                                      Plot=i,
                                      Developmental_Stage = Developmental_Stage,
                                      Age= NA,
                                      Cycle=cyc,
                                      Years_between_cycle=NA,
                                      Cohort = f,
                                      Total_Trees=stocking_ha,
                                      Regen_Trees=NA,
                                      Ingrowth=Ingrowth,
                                      harvest=harvest,
                                      Basal_Area = BASAL,
                                      BAsal_Area_Very_Large_wood=BASAL_VLW,
                                      BAsal_Area_Large_wood=BASAL_LW,
                                      BAsal_Area_Medium_wood=BASAL_MW,
                                      BAsal_Area_small_wood=BASAL_SW,
                                      BAsal_Area_poles_wood=BASAL_P,
                                      BAsal_Area_INC=BAsal_Area_INC,
                                      BAsal_Area_INC_Very_Large_wood=BAsal_Area_INC_VLW,
                                      BAsal_Area_INC_Large_wood=BAsal_Area_INC_LW,
                                      BAsal_Area_INC_Medium_wood=BAsal_Area_INC_MW,
                                      BAsal_Area_INC_small_wood=BAsal_Area_INC_SW,
                                      BAsal_Area_INC_poles_wood=BAsal_Area_INC_P,
                                      Harvest_Basal_Area=harvest_basal_area,
                                      Harvest_Basal_Very_Large_wood=harvest_basal_area_VLW,
                                      Harvest_Basal_Large_wood=harvest_basal_area_LW,
                                      Harvest_Basal_Medium_wood=harvest_basal_area_MW,
                                      Harvest_Basal_small_wood=harvest_basal_area_SW,
                                      Harvest_Basal_poles_wood=harvest_basal_area_P,
                                      Ingrowth_Basal_Area=Ingrowth_basal_area,
                                      Ingrowth_basal_area_Very_Large_wood=Ingrowth_basal_area_VLW,
                                      Ingrowth_basal_area_Large_wood=Ingrowth_basal_area_LW,
                                      Ingrowth_Basal_Medium_wood=Ingrowth_basal_area_MW,
                                      Ingrowth_Basal_small_wood=Ingrowth_basal_area_SW,
                                      Ingrowth_Basal_Pole_wood=Ingrowth_basal_area_P)
        
        
        rm(BASAL,BASAL_VLW,BASAL_LW,BASAL_MW,BASAL_SW,BASAL_P)
        
        
        info_plot<-rbind(info_plot,info_plot_specie)
      }
    }
  }
  info_plot<-info_plot[-1,]
  return(info_plot)
  
}

View(info_plot)



View(AFI_ISN_Data2[AFI_ISN_Data2$idplots==3 & AFI_ISN_Data2$cohort_name=="Slow-growing broadleaves",])


sum(info_plot$BAsal_Area_INC,na.rm=T)



#######################################################################################################################################################
#
values_per_site_rel<-data.frame(Site=NA,BA_INC=NA)

#Stand_level_data_rel<-Stand_level_data
for ( i in unique(Stand_level_data_rel$Forest)){
 ba_inc<- sum(Stand_level_data_rel[Stand_level_data_rel$Cycle==2 & Stand_level_data_rel$Forest==i,]$Basal_Area)-
    sum(Stand_level_data_rel[Stand_level_data_rel$Cycle==1 & Stand_level_data_rel$Forest==i,]$Basal_Area)+
    #sum(t$`37c Daggons`$Ingrowth_Basal_Area,na.rm=T)*9+
    sum(Stand_level_data_rel[Stand_level_data_rel$Forest==i,]$Harvest_Basal_Area,na.rm=T)
 ba_inc<-ba_inc/Stand_level_data_method_1[Stand_level_data_method_1$Forest==i,]$Years_between_cycle[1]
 values_per_site_rel[NROW(values_per_site_rel)+1,]<-c(i,ba_inc)

 print(i)
 print(ba_inc)
 print("_____________________________________________________")
  }
for ( i in unique(Stand_level_data_rel$Forest)){
  ba_inc<- sum(Stand_level_data_rel[Stand_level_data_rel$Cycle==2 & Stand_level_data_rel$Forest==i,]$BAsal_Area_INC)+
  #sum(Stand_level_data_rel[Stand_level_data_rel$Cycle==2 & Stand_level_data_rel$Forest==i,]$Ingrowth_Basal_Area)+
    sum(Stand_level_data_rel[Stand_level_data_rel$Forest==i,]$Harvest_Basal_Area,na.rm=T)
  ba_inc<-ba_inc/Stand_level_data_method_1[Stand_level_data_method_1$Forest==i,]$Years_between_cycle[1]
  
  print(i)
  print(ba_inc)
  print("_____________________________________________________")
}

Stand_level_data_rel1<-Stand_level_data_rel

#######################################################################################################################################################
# for normal method 
values_per_site<-data.frame(Site=NA,BA_INC=NA)

for ( i in unique(Stand_level_data$Forest)){
  ba_inc<- sum(Stand_level_data[Stand_level_data$Cycle==2 & Stand_level_data$Forest==i,]$Basal_Area)-
    sum(Stand_level_data[Stand_level_data$Cycle==1 & Stand_level_data$Forest==i,]$Basal_Area)+
    #sum(t$`37c Daggons`$Ingrowth_Basal_Area,na.rm=T)*9+
    sum(Stand_level_data[Stand_level_data$Forest==i,]$Harvest_Basal_Area,na.rm=T)
  ba_inc<-ba_inc/Stand_level_data_method_1[Stand_level_data_method_1$Forest==i,]$Years_between_cycle[1]
  values_per_site[NROW(values_per_site)+1,]<-c(i,ba_inc)
  print(i)
  print(ba_inc)
  print("_____________________________________________________")
}


for ( i in unique(Stand_level_data_method_1$Forest)){
  ba_inc<- sum(Stand_level_data_method_1[Stand_level_data_method_1$Cycle==2 & Stand_level_data_method_1$Forest==i,]$BAsal_Area_INC)+
    #sum(Stand_level_data_method_1[Stand_level_data_method_1$Cycle==2 & Stand_level_data_method_1$Forest==i,]$Ingrowth_Basal_Area)+
    sum(Stand_level_data_method_1[Stand_level_data_method_1$Forest==i,]$Harvest_Basal_Area,na.rm=T)
  ba_inc<-ba_inc/Stand_level_data_method_1[Stand_level_data_method_1$Forest==i,]$Years_between_cycle[1]
  print(i)
  print(ba_inc)
  print("_____________________________________________________")
}
View(Stand_level_data)#[Stand_level_data$Forest=="102",])

for ( i in unique(Stand_level_data_method_2$Forest)){
  ba_inc<- sum(Stand_level_data_method_2[Stand_level_data_method_2$Cycle==2 & Stand_level_data_method_2$Forest==i,]$BAsal_Area_INC)+
   # sum(Stand_level_data_method_2[Stand_level_data_method_2$Cycle==2 & Stand_level_data_method_2$Forest==i,]$Ingrowth_Basal_Area)+
    sum(Stand_level_data_method_2[Stand_level_data_method_2$Forest==i,]$Harvest_Basal_Area,na.rm=T)
  ba_inc<-ba_inc/Stand_level_data_method_1[Stand_level_data_method_1$Forest==i,]$Years_between_cycle[1]
  
  print(i)
  print(ba_inc)
  print("_____________________________________________________")
}

Stand_level_data_correct<-Stand_level_data
Stand_level_data_rel
Stand_level_data_method_2
Stand_level_data<-Stand_level_data_correct


sum(Stand_level_data[Stand_level_data$Forest=="208, 213a Boulsbury"& 
                   Stand_level_data$Cycle==2 ,]$Harvest_Basal_Area)





View(AFI_ISN_Data[AFI_ISN_Data$Cycle==1 & 
               AFI_ISN_Data$Site_code== "208, 213a Boulsbury" & 
               AFI_ISN_Data$Flag=="H" & !is.na(AFI_ISN_Data$Flag),])





library(MASS)
x<-c(rep(58,32),
rep(54,54),
rep(50,89),
rep(46,131),
rep(42,187),
rep(38,254),
rep(34,324),
rep(30,443),
rep(26,559),
rep(22,750),
rep(18,1057),
rep(14,1323),
rep(10,2485))

fitdistr(x,"weibull")



values_per_site_rel<-values_per_site_rel[-1,]

values_per_site<-values_per_site[-1,]

values_per_site$rel_BA<-values_per_site_rel$BA_INC
plot(values_per_site$BA_INC,values_per_site$rel_BA)
abline(coef = c(0,1))

View(Stand_level_data)


lm(as.numeric(values_per_site$BA_INC)~as.numeric(values_per_site$rel_BA))

mean(as.numeric(values_per_site$BA_INC)/as.numeric(values_per_site$rel_BA))
median(as.numeric(values_per_site$BA_INC)/as.numeric(values_per_site$rel_BA))
mean(as.numeric(values_per_site$BA_INC)/as.numeric(values_per_site$rel_BA))
exp(mean(log((as.numeric(values_per_site$BA_INC)/as.numeric(values_per_site$rel_BA))
)))




View(AFI_ISN_Data[AFI_ISN_Data$Site_code=="208, 213a Boulsbury" & AFI_ISN_Data$Cycle==1 & !is.na(AFI_ISN_Data$Flag) & AFI_ISN_Data$Flag=="H",])


values_per_site$Dev.Stage<-NA

for( i in unique(AFI_ISN_Data$Site_code)){
  values_per_site[values_per_site$Site==i,]$Dev.Stage<- AFI_ISN_Data[AFI_ISN_Data$Site_code==i,]$Development.stage[1]
}



View(AFI_ISN_Data[AFI_ISN_Data$Site_code =="Nant yr Eira Wood",])



write.csv(AFI_ISN_Data,"AFI_ISN_Data_c1")



write.csv(Stand_level_data,"Stand_level_data_c1")
View(Stand_level_data[Stand_level_data$Forest=="Nant yr Eira Wood",])



values_per_site_c<-data.frame(Site=NA,plot=NA,BA_INC=NA)

Stand_level_data_c
for(cyc in 2:4){
  print("_____________________________________________________")
  print("_____________________________________________________")
  print("cycle")
  print(cyc)
  print("_____________________________________________________")
  print("_____________________________________________________")
  
  
for ( i in unique(Stand_level_data_c[Stand_level_data_c$Cycle==cyc,]$Forest)){
  for(n in unique(Stand_level_data_c[Stand_level_data_c$Cycle==cyc & Stand_level_data_c$Forest==i,]$Plot)){
  ba_inc<- sum(Stand_level_data_c[Stand_level_data_c$Cycle==cyc & 
                                    Stand_level_data_c$Forest==i &
                                    Stand_level_data_c$Plot==n,]$Basal_Area)-
    sum(Stand_level_data_c[Stand_level_data_c$Cycle==cyc-1 &
                             Stand_level_data_c$Forest==i &
                             Stand_level_data_c$Plot==n,]$Basal_Area)+
    #sum(t$`37c Daggons`$Ingrowth_Basal_Area,na.rm=T)*9+
    sum(Stand_level_data_c[Stand_level_data_c$Forest==i &
                             Stand_level_data_c$Plot==n,]$Harvest_Basal_Area,na.rm=T)
  ba_inc<-ba_inc/Stand_level_data_c[Stand_level_data_c$Forest==i,]$Years_between_cycle[1]
  values_per_site_c[NROW(values_per_site_c)+1,]<-c(i,n,ba_inc)
  print(i)
  print(ba_inc)
  print("_____________________________________________________")
  }
}
}


View(values_per_site_c[values_per_site_c$])

View(AFI_ISN_Data[AFI_ISN_Data$Site_code=="103" & AFI_ISN_Data$Cycle %in% c(3),])

AFI_ISN_Data[AFI_ISN_Data$Site_code=="67" ,]$idplots


NROW(AFI_ISN_Data[AFI_ISN_Data$Site_code=="103" & 
                   AFI_ISN_Data$Cycle %in% c(3) &
                   is.na(AFI_ISN_Data$Flag) & AFI_ISN_Data$Limit==0,]$RepreBAHA)-

  NROW(AFI_ISN_Data[AFI_ISN_Data$Site_code=="103" & 
                   AFI_ISN_Data$Cycle %in% c(2)&
                   is.na(AFI_ISN_Data$Flag) & AFI_ISN_Data$Limit==0,]$RepreBAHA)

NROW(AFI_ISN_Data[AFI_ISN_Data$Site_code=="103" & 
                   AFI_ISN_Data$Cycle %in% c(2) & 
                   !is.na(AFI_ISN_Data$Flag) &
                   AFI_ISN_Data$Flag=="H" & AFI_ISN_Data$Limit==0,]$RepreBAHA)

sum(Stand_level_data_c[Stand_level_data_c$Forest=="67" &
             Stand_level_data_c$Cycle==3 ,]$BAsal_Area_INC)


values_per_site_r<-data.frame(Site=NA,BA_INC=NA)

for(cyc in 2:4){
  print("_____________________________________________________")
  print("_____________________________________________________")
  print("cycle")
  print(cyc)
  print("_____________________________________________________")
  print("_____________________________________________________")
  
  
  for ( i in unique(Stand_level_data_r[Stand_level_data_r$Cycle==cyc,]$Forest)){
    ba_inc<- sum(Stand_level_data_r[Stand_level_data_r$Cycle==cyc & Stand_level_data_r$Forest==i,]$Basal_Area)-
      sum(Stand_level_data_r[Stand_level_data_r$Cycle==cyc-1 & Stand_level_data_r$Forest==i,]$Basal_Area)+
      #sum(t$`37c Daggons`$Ingrowth_Basal_Area,na.rm=T)*9+
      sum(Stand_level_data_r[Stand_level_data_r$Forest==i,]$Harvest_Basal_Area,na.rm=T)
    ba_inc<-ba_inc/Stand_level_data_r[Stand_level_data_r$Forest==i,]$Years_between_cycle[1]
    values_per_site_r[NROW(values_per_site_r)+1,]<-c(i,ba_inc)
    print(i)
    print(ba_inc)
    print("_____________________________________________________")
  }
}

values_per_site_c$BA_INC<-as.numeric(values_per_site_c$BA_INC)
values_per_site_r$BA_INC<-as.numeric(values_per_site_r$BA_INC)

mean(values_per_site_c$BA_INC/values_per_site_r$BA_INC,na.rm=T)

values_per_site_r


plot(values_per_site_c$BA_INC,values_per_site_r$BA_INC,xlim=c(0,2.5),ylim=c(0,2.5))
abline(coef = c(0,1))


0.9460955
unique(AFI_ISN_Data$Site_code)
No_Plots<-NA
for(i in unique(Stand_level_data$Forest)){
  rm(No_Plots)
  No_Plots<-NROW(unique(Stand_level_data[Stand_level_data$Forest==i,]$Plot))
  Stand_level_data[Stand_level_data$Forest==i,]$Basal_Area<-No_Plots*
      Stand_level_data[Stand_level_data$Forest==i,]$Basal_Area
  
  Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_INC<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_INC
  
  Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_Medium_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_Medium_wood
  
  Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_small_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_small_wood
  
  Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_INC_Medium_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_INC_Medium_wood
    
  Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_Large_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_Large_wood
    
  Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_poles_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_poles_wood
    
  Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_Very_Large_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_Very_Large_wood
    
  Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_INC_small_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_INC_small_wood
    
  Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_INC_Large_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_INC_Large_wood
    
  Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_INC_poles_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_INC_poles_wood
    
  Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_INC_Very_Large_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$BAsal_Area_INC_Very_Large_wood
    
  Stand_level_data[Stand_level_data$Forest==i,]$Harvest_Basal_Area<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$Harvest_Basal_Area
    
  Stand_level_data[Stand_level_data$Forest==i,]$Harvest_Basal_Very_Large_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$Harvest_Basal_Very_Large_wood
    
  Stand_level_data[Stand_level_data$Forest==i,]$Harvest_Basal_Large_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$Harvest_Basal_Large_wood
    
  Stand_level_data[Stand_level_data$Forest==i,]$Harvest_Basal_Medium_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$Harvest_Basal_Medium_wood
    
  Stand_level_data[Stand_level_data$Forest==i,]$Harvest_Basal_small_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$Harvest_Basal_small_wood
    
  Stand_level_data[Stand_level_data$Forest==i,]$Harvest_Basal_poles_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$Harvest_Basal_poles_wood
    
  Stand_level_data[Stand_level_data$Forest==i,]$Ingrowth_Basal_Area<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$Ingrowth_Basal_Area
    
  Stand_level_data[Stand_level_data$Forest==i,]$Ingrowth_basal_area_Very_Large_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$Ingrowth_basal_area_Very_Large_wood
    
  Stand_level_data[Stand_level_data$Forest==i,]$Ingrowth_basal_area_Large_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$Ingrowth_basal_area_Large_wood
    
  Stand_level_data[Stand_level_data$Forest==i,]$Ingrowth_Basal_Medium_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$Ingrowth_Basal_Medium_wood
    
  Stand_level_data[Stand_level_data$Forest==i,]$Ingrowth_Basal_small_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$Ingrowth_Basal_small_wood
    
  Stand_level_data[Stand_level_data$Forest==i,]$Ingrowth_Basal_Pole_wood<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$Ingrowth_Basal_Pole_wood

  Stand_level_data[Stand_level_data$Forest==i,]$Total_Trees<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$Total_Trees
  
  Stand_level_data[Stand_level_data$Forest==i,]$harvest<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$harvest
  
  Stand_level_data[Stand_level_data$Forest==i,]$Ingrowth<-No_Plots*
    Stand_level_data[Stand_level_data$Forest==i,]$Ingrowth
  
}





AFI_ISN_Data<-AFI_ISN_Data[AFI_ISN_Data$Site_code!="47/48/55 Llethr Gwinau",]
Stand_level_data<-Stand_level_data[Stand_level_data$Forest!="47/48/55 Llethr Gwinau",]
write.csv(AFI_ISN_Data,"AFI_ISN_Data_c3")
write.csv(Stand_level_data,"Stand_level_data_c3")

NROW(Stand_level_data)

unique(Stand_level_data$Forest)

Stand_level_data<-Stand_level_data[!(Stand_level_data$Forest %in% c("Baronscourt" ,"Nant yr Eira INV")),]

Stand_level_data<-rbind(Stand_level_data,t$Baronscourt,t$`Nant yr Eira INV`)


View(Stand_level_data[Stand_level_data$Forest=="Baronscourt",])

