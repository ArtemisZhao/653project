#GEE and GLS
library(nlme)
library(gee)
library(geeM)
library(geepack)
#AR1, GLS, ML
GLSARML =  gls(Y~Year+logmu +logratio + CI+UR+Year*logmu + Year*logratio + logmu * CI + logratio*UR, data = workingdata, correlation = corAR1(form=~1|HHid), method = "ML")

#AR1, GLS, REML
GLSAR =  gls(Y~Year+logmu +logratio + CI+UR+Year*logmu + Year*logratio + logmu * CI + logratio*UR, data = workingdata, correlation = corAR1(form=~1|HHid), method = "REML")

#AR1, GEE
GEEAR = gee(Y~Year+logmu +logratio + CI+UR+Year*logmu + Year*logratio + logmu * CI + logratio*UR, HHid, data = workingdata, family = gaussian, corstr =  "AR-M", Mv = 1)

#Unstructured, GEE
GEEUN = gee(Y~Year+logmu +logratio + CI+UR+Year*logmu + Year*logratio + logmu * CI + logratio*UR, HHid, data = workingdata, family = gaussian, corstr =  "unstructured")

#AR1, GEEM
GEEMAR = geem(Y~Year+logmu +logratio + CI+UR+Year*logmu + Year*logratio + logmu * CI + logratio*UR, HHid, data = workingdata, family = gaussian, corstr =  "ar1")

#Unstructured, GEEM
GEEMUN = geem(Y~Year+logmu +logratio + CI+UR+Year*logmu + Year*logratio + logmu * CI + logratio*UR, HHid, data = workingdata, family = gaussian, corstr =  "unstructured")

#AR1, GEEPACK
GEEPAR = geeglm(Y~Year+logmu +logratio + CI+UR+Year*logmu + Year*logratio + logmu * CI + logratio*UR, data = workingdata, id = HHid, corstr = "ar1")

#Unstructured, GEEPACK
GEEPUN = geeglm(Y~Year+logmu +logratio + CI+UR+Year*logmu + Year*logratio + logmu * CI + logratio*UR, data = workingdata, id = HHid, corstr = "unstructured")

#For Cor of GEE, use GEEAR$working.correlation; 

#For Cor of GEEM, use GEEMUN$biggest.R.alpha

library(MuMIn)
QIC(GEEAR)
QIC(GEEMAR)
QIC(GEEMUN)
#For Cor of GEEP, use summary. 


