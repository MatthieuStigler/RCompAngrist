
########### DATA
library(hexView)

schooling<-readEViews("/home/fao/Dropbox/Documents/stats/R/RcompVerbeek2000/Datasets/schooling.wf1")
head(schooling)

pricing<-readEViews("/home/fao/Dropbox/Documents/stats/R/RcompVerbeek2000/Datasets/pricing.wf1")
head(pricing)

library(AER)
################ 5.1
mean(schooling$ED66)
mean(schooling$ED76)
schooling$AGE762<-schooling$AGE76^2

tab5_1<-lm(LWAGE76~ED76+EXP76+EXP762+BLACK+SMSA76+SOUTH76, data=schooling)  
summary(tab5_1)

tab5_2<-lm(ED76~AGE76+I(AGE76^2)+BLACK+SMSA76+SOUTH76+NEARC4, data=schooling)  
summary(tab5_2)

tab5_3<-ivreg(LWAGE76~ED76+EXP76+EXP762+BLACK+SMSA76+SOUTH76|NEARC4+AGE76+I(AGE76^2)+BLACK+SMSA76+SOUTH76, data=schooling)
summary(tab5_3)


## 
library(gmm)

##Manually
Z<-as.matrix(cbind(1,schooling[,c("NEARC4", "AGE76", "AGE762", "BLACK", "SMSA76","SOUTH76")]))
X<-as.matrix(cbind(1,schooling[,c("ED76", "EXP76", "EXP762", "BLACK", "SMSA76","SOUTH76")]))
y<-as.matrix(schooling$LWAGE76)

solve(t(X)%*%X)%*%t(X)%*%y
solve(t(Z)%*%X)%*%t(Z)%*%y

