library(RcompAngrist)

data(krug_dat)
krug <- krug_dat

library(lmtest)
library(sandwich)
library(plyr)

### compare data with AER
library(AER)

data(STAR)
dim(STAR)
head(STAR)

### REPLICATE Table 8.2.1

head(krug)
length(unique(krug$classid)) ## 318 clusters!
table(table(krug$classid))
N_class <- table(krug$classid)


## Problems in data: not all students per class entered: Angrist uses 

krug_mean <- ddply(krug, .(classid), function(x) c(cs=mean(x$cs),pscore=mean(x$pscore)))

krug_mean2 <- aggregate(krug, by=list(krug$classid), mean)
  ddply(krug, .(classid), function(x) c(cs=mean(x$cs),pscore=mean(x$pscore)))

krug_mean3 <- krug
krug_mean3$cs_mean <- ave(krug$cs, krug$classid)

head(krug_mean)


N_class2 <- krug_mean$cs

reg_krug <- lm(pscore~cs, data=krug)
coef(summary(reg_krug))[2,2]


#### Sandwich:
coeftest(reg_krug,vcov.=vcovHC(reg_krug, "HC1"))[2,2]

#### Clustered 1 (sandwich):
clx <- function(fm, dfcw=1, cluster) {
          library(sandwich)
          library(lmtest)
          M <- length(unique(cluster))
          N <- length(cluster)
          K <- fm$rank
          dfc <- (M/(M-1))*((N-1)/(N-K))
          u <- apply(estfun(fm),2,function(x) tapply(x,cluster,sum))
          vcovCL <- dfc*sandwich(fm,meat=crossprod(u)/N)*dfcw
          coeftest(fm,vcovCL)
}

clx(reg_krug, 1, cluster=factor(krug$classid))[2,2]



#### Clustered 1 (rms):
# library(rms)
# ols_reg_krug <- ols(pscore~cs, data=krug, x=TRUE, y=TRUE)
# rob_se <- robcov(ols_reg_krug, cluster=krug$classid)
# sqrt(rob_se$var[2,2])


### Block-bootstrap:
boot_se <- bootcov(ols_reg_krug, cluster=krug$classid, B=199, pr=TRUE)
sqrt(boot_se$var[2,2])

##### Moulton:
library(rms)
ins <- var(N_class)/mean(N_class) + mean(N_class)
ins2 <- var(N_class2)/mean(N_class2) + mean(N_class2)

rho_e <- deff(residuals(reg_krug), krug$classid)["rho"]
rho_x <- deff(jitter(krug$cs, factor=0.0001), krug$classid)["rho"]
mou <- sqrt(1+(ins2-1)*rho_e*rho_x)
mou*coef(summary(reg_krug))[2,2]

moulton(reg_krug, cluster=krug_dat$classid, trace=TRUE)

### GLS
reg_krug_gls <- lm(pscore~cs, data=krug_mean, weights=cs)
reg_krug_gls <- lm(pscore~cs_mean, data=krug_mean3)
coef(summary(reg_krug_gls))[2,2]

