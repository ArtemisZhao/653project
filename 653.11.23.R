
workingdata[,c("Year")]<-workingdata[,c("Year")]-2004

workingdata$time<-1
for (i in 1:nrow(workingdata)) {
if (workingdata$Year[i]==2){
  workingdata$time[i]<-2
}
if (workingdata$Year[i]==5){
  workingdata$time[i]<-3
}
if (workingdata$Year[i]==7){
  workingdata$time[i]<-4
}
if (workingdata$Year[i]==11){
  workingdata$time[i]<-5
}}

workingdata$ratio<-workingdata$Yuan_Spent_Raising_Crops_lastyear/
  (workingdata$Number_of_mu_Land_Cultivated_lastyear+0.01)
workingdata$logmu<-log(workingdata$Number_of_mu_Land_Cultivated_lastyear+0.1)
workingdata$logratio<-log(workingdata$ratio+0.1)
workingdata$Y<-log(workingdata$Total_Value_From_Crops_lastyear+0.1)
names(data)


### Mean model selection
### include year fixed+random


ff<-lmer(log(Total_Value_From_Crops_lastyear+0.1)~Year+(Year|HHid),data=workingdata,REML=F)


### all fixed + year random
ff1<-lmer(Y~Year+logmu+logratio+UR+CI+(Year|HHid),data=workingdata,REML=FALSE)

### residual
res<-(log(data[,9])-predict(ff1,data[,c(1,4,5,6,8,10,11)]))
plot(workingdata$Y,res)
plot(workingdata$logratio,res)

### add in interaction effect
ff2<-lmer(Y~Year+logmu+logratio+UR+CI+UR*logmu+CI*logmu+(Year|HHid),data=workingdata,REML=FALSE)

!!!!!!### based on AIC
ff3<-lmer(Y~Year+logmu+logratio+UR+CI+UR*logmu+(Year|HHid),data=workingdata,REML=FALSE)
ff3f<-lmer(Y~Year+logmu+logratio+UR+CI+UR*logmu+(1|HHid),data=workingdata,REML=FALSE)

###???singular? why
ff4<-lmer(Y~Year+logmu+logmu*Year+logratio+UR+CI+UR*logmu+(Year|HHid),data=workingdata,REML=FALSE)

### logmu random
ff5<-lmer(Y~Year+logmu+logratio+UR+CI+UR*logmu+(logmu+Year|HHid),data=workingdata,REML=FALSE)

?###logratio random  number of observations (=5171) <= number of random effects (=5440)
ff6<-lmer(Y~Year+logmu+logratio+UR+CI+UR*logmu+logratio+(logratio+logmu+Year|HHid),data=workingdata,REML=FALSE)

### UR/CI
ff7<-lmer(Y~Year+logmu+logratio+CI+(Year|HHid),data=workingdata[which(workingdata$UR=="Urban"),],REML=FALSE)
ff8<-lmer(Y~Year+logmu+logratio+CI+(Year|HHid),data=workingdata[which(workingdata$UR=="Rural"),],REML=FALSE)
ff9<-lmer(Y~Year+logmu+logratio+UR+(Year|HHid),data=workingdata[which(workingdata$CI=="Coastal"),],REML=FALSE)
ff10<-lmer(Y~Year+logmu+logratio+UR+(Year|HHid),data=workingdata[which(workingdata$CI=="Inland"),],REML=FALSE)

##Stratified
data2<-workingdata
data2$Year<-as.factor(data2$Year)


ff11<-lmer(Y~Year+I(Year^2)+logmu+logratio+UR+CI+UR*logmu+(Year|HHid),data=workingdata,REML=FALSE)

##########mle
Ctr<-lmeControl(maxIter=200,msMaxIter=100,tolerance = 1e-2,msTol = 1e-3,returnObject=TRUE)
summary(lme(Y~logmu+Year+logratio+UR+CI,
            random=~Year|HHid,correlation=corSymm(form=~1),data=workingdata,control =Ctr))         
  
