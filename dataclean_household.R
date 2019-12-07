library(sas7bdat)
library(dplyr)

#origin data 
farmh<-read.sas7bdat("farmh_12.sas7bdat")

#data (>=2004)
farmh5<-farmh[which(farmh$WAVE>=2004),]

#Name Description (full)
farmhnames<-c("Any_Mem_Farm_for_regular_wage","Any_Mem_Works_on_COLLCTV/STATE/HH_Farm",
              "Specialized_Farm_Household","Number_of_mu_Land_Cultivated_lastyear",
              "Total_Income_From_Crops_lastyear","Value_of_Crops_Consumed_lastyear",
              "Yuan_Spent_Raising_Crops_lastyear","Year","Year_Land_last_redistributed",
              "Farm>oneyear","Farm<oneyear","Engage_in_farming_lastyear",
              "Outlying_Value","Outlying_Value_2","Outlying_Value_3","HHid",
              "CommID","Province","UR","CC","numComm","HHnum","HH_Consume_Crops_Grown")

#Delete Missing >40%
farmmis<-sapply(farmh5,function(x) sum(is.na(x))/8928)
name<-farmhnames[which(farmmis<=0.4)]
farmh5<-farmh5[,which(farmmis<=0.4)]
names(farmh5)<-name
name

#Pick up Complete cases and delete donotknowvalue(=-9999,99999)
farmh5_comp <- farmh5 %>% filter(complete.cases(.))
farmh5_comp <- farmh5_comp %>% 
  filter(Total_Income_From_Crops_lastyear!=-9999 &Total_Income_From_Crops_lastyear!=99999
         & Number_of_mu_Land_Cultivated_lastyear!=-99 & Number_of_mu_Land_Cultivated_lastyear!=999
         & Value_of_Crops_Consumed_lastyear!=-9999 & Value_of_Crops_Consumed_lastyear!=99999
         & Yuan_Spent_Raising_Crops_lastyear!=-999 & Yuan_Spent_Raising_Crops_lastyear!=99999)

# Drop Engage(always 1), CommID(community), CC, numComm, HHnum
farmh5_comp<-farmh5_comp[,-c(6,8,11,12,13)]

# workingdata
workingdata<-farmh5_comp

# Drop HouseholdID whose #measurement is smaller than 3
workingdata<-workingdata[-which(workingdata$HHid %in% names((which(table(workingdata$HHid)<3)))),]

# Build up Category
workingdata$UR<-as.factor(ifelse(workingdata$UR==1,c("Urban"),c("Rural")))

for (i in 1:nrow(workingdata)){
  if (workingdata$Province[i]==11){
    workingdata$Province[i]=c("Beijing")
  }
  else if(workingdata$Province[i]==21){
    workingdata$Province[i]=c("Liaoning")
  }
  else if(workingdata$Province[i]==23){
    workingdata$Province[i]=c("Heilongjiang")
  }
  else if(workingdata$Province[i]==31){
    workingdata$Province[i]=c("Shanghai")
  }
  else if(workingdata$Province[i]==32){
    workingdata$Province[i]=c("Jiangsu")
  }
  else if(workingdata$Province[i]==37){
    workingdata$Province[i]=c("Shandong")
  }
  else if(workingdata$Province[i]==41){
    workingdata$Province[i]=c("Henan")
  }
  else if(workingdata$Province[i]==42){
    workingdata$Province[i]=c("Hubei")
  }
  else if(workingdata$Province[i]==43){
    workingdata$Province[i]=c("Hunan")
  }
  else if(workingdata$Province[i]==45){
    workingdata$Province[i]=c("Guangxi")
  }
  else if(workingdata$Province[i]==52){
    workingdata$Province[i]=c("Guizhou")
  }
  else if(workingdata$Province[i]==55){
    workingdata$Province[i]=c("Chongqing")
  }
}

workingdata$Province<-as.factor(workingdata$Province)
summary(workingdata)

workingdata[,"Total_Value_From_Crops_lastyear"]<-
  workingdata$Total_Income_From_Crops_lastyear+workingdata$Value_of_Crops_Consumed_lastyear

workingdata[,c("CI")]<-c("Inland")
workingdata[which(workingdata$Province 
         %in% c("Jiangsu","Shandong","Zhejiang","Liaoning","Guangxi")),c("CI")]<-c("Coastal")

workingdata$CI<-as.factor(workingdata$CI)

rownames(workingdata)<-c(1:5171)
save(workingdata,file="workingdata.Rdata")

