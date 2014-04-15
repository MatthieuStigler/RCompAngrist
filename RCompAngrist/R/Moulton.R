#' Moulton correction for clustered standard errors
#' 
#' Corrects
#' @param lm Object of class \code{lm}
#' @param cluster The cluster variable
#' @param x Will be removed?
#' @param trace Print intermediate output
#' @param custom User defined value to control for
#' @return A matrix of coefficients, standard erros and t/p-values
#' @export
#' @import Hmisc


moulton <- function(lm,  cluster, x=NULL, trace=FALSE, custom=NULL){
  
  
  ## Compute mean N, var N
  N_class <- table(cluster)
  sameSize <- length(unique(N_class))==1
  ins <- if(sameSize) unique(N_class) else var(N_class)/mean(N_class) + mean(N_class)
  if(trace) cat("-Clus size: -mean: ", mean(N_class), "\n            -var: ", var(N_class), "\n")
  
  ## extract regresors, and residuals:
  X <- model.matrix(lm)
  has_intercep <- any(grepl("Intercept", colnames(X)))
  if(has_intercep) X <- X[,-grep("Intercept", colnames(X)), drop=FALSE] ## probably there is a more elegant solution...
  
  resids <- residuals(lm)
  if(!is.null(lm$na.action)){
    nact <- lm$na.action
    cluster <- cluster[-nact]
    warning("There were missing values in the regression data, tried to adjust the cluster variable correspondingly")
  }
  
  ## Compute ICC on Xs
  rho_X <- apply(X, 2,function(x) deff(x, cluster)["rho"])
  if(any(!is.finite(rho_X))) rho_X[!is.finite(rho_X)] <- 1 #workaround as deff does not work for perfect ICC!
  if(has_intercep) rho_X <- c(1, rho_X)
  print(rho_X)
  
  ## Compute ICC on resids:
  rho_e <- deff(resids, cluster)["rho"]
  
  if(trace) cat("-Rho_e: ", rho_e, "\n-Rho_x: ", rho_X, "\n")

  
  if(!is.null(x) && unique(aggregate(x, list(cluster), function(x) length(unique(x)))[,2])==1)
    warning("Arg 'x' provided but useless?")

  ## Moulton factor
  mou <- 1+(ins-1)*rho_e*rho_X
  if(trace) cat("-Moulton:", mou, "\n")
  if(!is.null(custom)) mou <- custom
  
  ## Adjust se, t val and p.val:
  coSum <- coef(summary(lm)) 
  coSum[,"Std. Error"] <- sqrt(mou)*coSum[,2]
  coSum[,"t value"] <-   coSum[,1]/coSum[,2] ## recompute se
  coSum[,4] <-   2 * pt(abs(coSum[,3]), df=lm$df.residual, lower.tail = FALSE)
  
  ## Return result
  coSum
}

## Moulton ICC estimator:

IC_moult <- function(x, cluster){

  ## Function for within cluster sum of prods:
  sum_clus <- function(x, x_mean){
    cov_i <- tcrossprod(x-x_mean )
    diag(cov_i) <- 0
    sum(cov_i, na.rm=TRUE)
  }
  
  ## get (the sum of) all within sums of prods :
  numer <- sum(tapply(x, cluster, sum_clus, x_mean=mean(x, na.rm=TRUE)))
  
  ## denomin:
  freqs <- table(cluster)
  denom <- var(x, na.rm=TRUE) * sum(freqs*(freqs-1))
  
  rho <- numer/denom
  rho
}

#### TEST: try to replicate Table 8.2.1 (p. 316) in Angrist Mostly Harm

if(FALSE){
  library(RcompAngrist)
  krug2 <- data(krug_dat)
  
  ols_reg_krug <- lm(pscore~cs, data=krug_dat)


  coef(summary(ols_reg_krug))
  moulton(lm=ols_reg_krug, cluster=krug_dat$classid)
  moulton(lm=ols_reg_krug, cluster=krug_dat$classid, trace=TRUE)
  
  ## Multiple Xs:
  ols_reg_krug_2 <- lm(pscore~cs+nwhite, data=krug_dat)
  moulton(lm=ols_reg_krug_2, cluster=krug_dat$classid, trace=TRUE)
  moulton(lm=ols_reg_krug_2, cluster=krug_dat$classid, trace=TRUE, custom=sqrt(6.368154))

  ## Compare ICC Moulton and deff():
  IC_moult(x=krug_dat$cs, cluster=krug_dat$classid)
  deff(y=krug_dat$cs, cluster=krug_dat$classid)["rho"]
  
  ## check discrepanc
  mean(table(krug_dat$cs))
  mean(table(krug_dat$classid))
  
  ## Check o noriginal dataset:
  library(MASS)
  library(Ecdat)
  data(Boston)
  data(Hedonic)
  
  dim(Boston)
  dim(Hedonic)
  
  head(Boston)
  head(Hedonic)
  colnames(Hedonic) <- gsub("zn", "lots", colnames(Hedonic))
  grep("townid", colnames(Hedonic))
  Hedonic2 <-Hedonic[, 1:14]
  Hedonic2[, "crim"] <- Hedonic2[, "crim"]/100
  Hedonic2[, "crim"] <- Hedonic2[, "crim"]/100
  
  Hedonic2 <-cbind(mv=Hedonic[, 1], Hedonic[, c(2:4, 6:14)]/100, Hedonic[, c(2:4, 6:14)])

#   Hedonic2 <- Hedonic[, -grep("townid", colnames(Hedonic))]
#   Hedonic2 <- 
    
  reg_M <- lm(mv~., data=Hedonic2)
  reg_M2 <- lm(mv~.-townid, data=Hedonic)
  round(coef(summary(reg_M)),4)
  round(coef(summary(reg_M2)),4)
  
  
  x<- rep(c(5,7), each=10)
  sum_clus(x[1:10], mean(x))
10*9*(5-6)^2
  
  length(residuals(ols_reg_krug_2))
  length(krug_dat$classid)
str(ols_reg_krug_2)  
  r <- ols_reg_krug_2
  r$na.action
  krug_dat[r$na.action,]
}