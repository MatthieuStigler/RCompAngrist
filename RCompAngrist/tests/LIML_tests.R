
#####################
####### LIML high
#####################

library(AER)
library(RcompAngrist)
data("CigarettesSW")
CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)

res <- kclass(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
                   data = CigarettesSW)
res_kcl_2sls <- kclass(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
                            data = CigarettesSW, k=1)
res_kcl_ols <- kclass(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
                           data = CigarettesSW, k=0)
res_ivreg <- ivreg(log(packs) ~ log(rincome)+log(rprice) | log(rincome) + tdiff + I(tax/cpi),
                   data = CigarettesSW)
res_ols <- lm(log(packs) ~ log(rincome)+log(rprice) ,data = CigarettesSW)

res 
res_kcl_2sls
res_ivreg
res_kcl_ols
res_ols


all.equal(residuals(res_ols), residuals(res_kcl_ols), check=FALSE)
all.equal(residuals(res_ivreg), residuals(res_kcl_2sls), check=FALSE)

all.equal(coef(res_ols), coef(res_kcl_ols)[c(2,3,1)], check=FALSE)
all.equal(coef(res_ivreg), coef(res_kcl_2sls)[c(2,3,1)], check=FALSE)

vcov(object=res_kcl_ols)
vcov(object=res_ols)

summary(object=res_kcl_ols)
coef(summary(object=res_ols))
all.equal(coef(summary(object=res_ols)),summary(object=res_kcl_ols)[c(2,3,1),])

summary(object=res_kcl_2sls)
coef(summary(object=res_ivreg))
all.equal(coef(summary(object=res_ivreg)),summary(object=res_kcl_2sls)[c(2,3,1),])

## kclass_fit 
dt <- CigarettesSW
res_fit <- RcompAngrist:::kclass_fit(y=log(dt$packs), x_exo=cbind(1,log(dt$rincome)), x_endo= log(dt$rprice),
                      z=cbind(1, with(dt, tax/cpi), dt$tdiff, log(dt$rincome)))

res_fit_2sls <- RcompAngrist:::kclass_fit(y=log(dt$packs), x_exo=cbind(1,log(dt$rincome)), x_endo= log(dt$rprice),
                                     z=cbind(1, with(dt, tax/cpi), dt$tdiff, log(dt$rincome)), k=1)
all.equal(coef(res_ivreg), coef(res_fit_2sls)[c(2,3,1)], check=FALSE)

###### TESTING
res <- kclass(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
              data = CigarettesSW)
res_geig <- kclass(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
              data = CigarettesSW, eig="geigen")

res$call <- res_geig$call <- NULL
all.equal(res, res_geig)

## Stata example in R fashion:
hsng2 <- webuse(x="hsng2")
hsng2$regions <- model.matrix(~region, data=hsng2)[,-1]
res_stata <- kclass(rent ~ pcturban +hsngval| pcturban + faminc +region, data=hsng2)

hs_kclass_fit_liml <-RcompAngrist:::kclass_fit(y=hsng2$rent, x_endo=hsng2$hsngval,x_exo=cbind(1,hsng2$pcturban ), 
                                 z=cbind(hsng2[, c("pcturban", "faminc", "regions")],1))
hs_kclass_fit_liml


summary(res_stata, adj="df")
summary(res_stata, adj="n")

confint(res_stata)