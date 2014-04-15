#' @title kclass_fit
#' K-class Instrumental Variable estimator, low-level function
#' 
#' Compute the k-class IV, by default the LIML
#' @param y vector with dependent variable
#' @param x regressor matrix, containing both exo and endo x
#' @param z instrument matrix (need not include the exogeneous x)
#' @param k The parameter of the k-class estimator. If missing, the LIML is computed.
#' @param x_exo vector/matrix containing only the exogeneous x
#' @param x_endo vector/matrix containing only the endogeneous x 
#' @param eig Whether to use the simple eigenvectors on inverted system, or the generalized eigenvectors
#' @return An object of class \code{kclass}
#' @seealso \code{\link{kclass}} for the high-level function with formula approach
#' @export
#' @import geigen
#' @examples
#' require(AER)
#' example(ivreg.fit, echo=FALSE)
#' x <- model.matrix(fm, component="regressors")
#' z2 <- model.matrix(fm, component="instruments")
#' 
#' ivreg.fit(x, as.matrix(y), z=z2)$coefficients
#' dt_sub <- subset(CigarettesSW,  year == "1995")
#' res_fit <- kclass_fit(y=with(dt_sub, log(packs)), x_exo=cbind(1,with(dt_sub, log(rincome))), x_endo= with(dt_sub, log(rprice)),
#'                       z=cbind(with(dt_sub, tax/cpi), dt_sub$tdiff), k=1)
#'                       
#' coef(res_fit)
#' all.equal(coef(res_fit)[c(2,1,3)], ivreg.fit(x, as.matrix(y), z2)$coefficients, check.attributes=FALSE)


kclass_fit <- function(y, x, z, k=NULL, x_exo, x_endo, eig=c("eigen", "geigen")){
    
  eig <- match.arg(eig)
  n <- NROW(y)
  n_ins <- NCOL(z)
  
  ##
  if(!is.matrix(z)) z <- as.matrix(z)
  if(missing(x_endo)) x_endo <- x
  if(!is.matrix(x_exo)) x_exo <- as.matrix(x_exo)
  if(!is.matrix(x_endo)) x_endo <- as.matrix(x_endo)
  if(!is.matrix(y)) y <- as.matrix(y)
  
  ## Matrix of full instruments:
  z <- cbind(x_exo, z)
  
  ## Residuals matrices
  xy <- cbind(y,x_endo)
  y_MZ_y <- crossprod(lm.fit(z, xy)$residuals)
  
  if(missing(x_exo)){
    y_MX_y <- crossprod(xy)
    X <- x_endo
  } else {
    X <- cbind(x_endo, x_exo)
    y_MX_y <- crossprod(lm.fit(x_exo, xy)$residuals)
  }
  
  
  ## determine k:  
  if(is.null(k)){
    
    ## as eigen:
    if(eig=="eigen"){
      k <- min(eigen(y_MX_y%*%solve(y_MZ_y))$values)
    } else {
      k <- min(geigen(y_MX_y, y_MZ_y, symmetric=TRUE, only.values=TRUE)$values)
    }
  }
  
  ## Coefs, given k:
  X_kM_X_inv <- solve(crossprod(X) - k* crossprod(lm.fit(z, X)$residuals))
  out <- y - k* lm.fit(z, y)$residuals
  val <- X_kM_X_inv%*%crossprod(X, out)
  val2 <- drop(val)
  names(val2) <- colnames(X)
  
  
  ## Vcov: (McKinnon, Davidson 1993, p. 650)
  df <- n - length(val2)
  fitted <- X%*%val
  resids <- drop(y - fitted)
  sig_res <- sqrt(crossprod(resids)/df)
  
  ## Return result
  res <- list()
  res$coefficients <- val2
  res$residuals <- resids
  res$fitted <- drop(fitted)
  res$sigma <- drop(sig_res)
  res$cov.unscaled <- X_kM_X_inv
  res$eigen <- k
  res$n <- n
  res$df <- df
  
  res
  
}

#' @rdname kclass_fit
#' @export
kclass_fit_QR <- function(y, x, z, k=NULL, x_exo, x_endo, eig=c("eigen", "geigen")){
  
  eig <- match.arg(eig)
  n <- NROW(y)
  n_ins <- NCOL(z)
  
  ##
  if(!is.matrix(z)) z <- as.matrix(z)
  if(missing(x_endo)) x_endo <- x
  if(!is.matrix(x_exo)) x_exo <- as.matrix(x_exo)
  if(!is.matrix(x_endo)) x_endo <- as.matrix(x_endo)
  
  if(!is.matrix(y)) y <- as.matrix(y)
  
  ## Dims
  K1 <- NCOL(x_exo)
  K2 <- NCOL(z)
  G <- NCOL(x_endo)
  
  ## Residuals matrices
  bigZ <- as.matrix(cbind(x_exo, z, x_endo, y ))
  QR_Z <- qr(bigZ)
  
  if(!all(QR_Z$pivot==1:NCOL(bigZ))) {
    warning("Oups, rank issue...")
    print(QR_Z$pivot)
    cat("Rank:\n")
    print(QR_Z$rank)
    max_piv <- which.max(QR_Z$pivot)
    out_piv <- QR_Z$pivot[(max_piv+1):length(QR_Z$pivot)]
    n_K1 <- 1:K1
    n_K2 <- K1 + (1:K2)
    n_G  <- (K1+K2) +(1:G)
    n_y  <- K1+K2+G+1
    if(any(out_piv%in%n_G)) G <- G -1
    
  }
  
  R_Z <- qr.R(QR_Z)
  
  ## decompos
  n_K1 <- 1:K1
  n_K2 <- K1 + (1:K2)
  n_G  <- (K1+K2) +(1:G)
  n_y  <- K1+K2+G+1
  #print(list(n_K1, n_K2, G, n_y))
  
  R_11 <- R_Z[n_K1, n_K1]
  R_13 <- R_Z[n_K1,n_G, drop=FALSE]
  R_14 <- R_Z[n_K1,n_y, drop=FALSE]
  
  R_23 <- R_Z[n_K2,n_G, drop=FALSE]
  R_24 <- R_Z[n_K2,n_y, drop=FALSE]
  
  R_33 <- R_Z[n_G,n_G, drop=FALSE]
  R_34 <- R_Z[n_G,n_y, drop=FALSE]
  
  ### LIML
  cp_R_33 <- crossprod(R_33)
  cp_R_23 <- crossprod(R_23)
  
  ## LIML
  if(is.null(k)){
    R_34_34 <- R_Z[c(n_G, n_y),c(n_G, n_y)]
    R_2_34 <- R_Z[n_K2,c(n_G, n_y)]
    
#     cp_R_34_34 <- crossprod(R_34_34)
    cp_R_2_34  <- crossprod(R_2_34)    
    R_34_34_inv <- tcrossprod(backsolve(R_34_34, diag(nrow(R_34_34))))
    eig <- eigen(R_34_34_inv%*%cp_R_2_34)$values
    k <- min(eig)+1

    # to benchmark:
#     solve(crossprod(R_34_34))
#     tcrossprod(solve(R_34_34))
#     require(geigen)
#     k <- min(geigen:::geigen(y_MX_y, y_MZ_y, symmetric=TRUE, only.values=TRUE)$values)
  }
  
  ## M
  M_up <- cbind(crossprod(R_13)+cp_R_23 + (1-k)* cp_R_33, crossprod(R_13, R_11))
  M_lo <- cbind(crossprod(R_11, R_13), crossprod(R_11))
  M <- rbind(M_up, M_lo)
#   print(head(M))
  M_inv <- solve(M)
  
  ## d
  d <- rbind(crossprod(R_13, R_14)+crossprod(R_23, R_24)+(1-k)*crossprod(R_33, R_34), 
             crossprod(R_11, R_14))
  
  coefs <- M_inv%*%d
  rownames(coefs) <- c(colnames(x_endo),colnames(x_exo))
  ##
  fitted <- bigZ[, c(n_G, n_K1)]%*%coefs
  resids <- drop(bigZ[,n_y]-fitted)
    
  
  ## Vcov: (McKinnon, Davidson 1993, p. 650)
  df <- n - length(coefs)
  sig_res <- sqrt(crossprod(resids)/df)
  
  ## Return result
  res <- list()
  res$coefficients <- drop(coefs)
  res$residuals <- resids
  res$fitted <- drop(fitted)
  res$sigma <- drop(sig_res)
  res$cov.unscaled <- M_inv
  res$eigen <- k
  res$n <- n
  res$df <- df
  
  res
  
}


## x_endo   Y
## x_exo    X_inclu X1
## z        x_exclu X2

##### OLD code:
if(FALSE){
 library(AER) 
 library(RcompAngrist) 
 example(ivreg.fit, echo=FALSE)
 x <- model.matrix(fm, component="regressors")
 z2 <- model.matrix(fm, component="instruments")
 
 ivreg.fit(x, as.matrix(y), z=z2)$coefficients
 dt_sub <- subset(CigarettesSW,  year == "1995")
 res_fit <- kclass_fit(y=with(dt_sub, log(packs)), x_exo=cbind(1,with(dt_sub, log(rincome))), x_endo= with(dt_sub, log(rprice)),
                                      z=cbind(1, with(dt_sub, tax/cpi), dt_sub$tdiff, log(dt_sub$rincome)), k=1)

 res_fit2 <- kclass_fit(y=log(dt_sub[,"packs", drop=FALSE]), x_exo=cbind(1,log(dt_sub[,"rincome", drop=FALSE])), x_endo= log(dt_sub[, "rprice", drop=FALSE]),
                       z=cbind(with(dt_sub, tax/cpi), dt_sub$tdiff), k=1)
 
 coef(res_fit)
 all.equal(coef(res_fit)[c(2,1,3)], ivreg.fit(x, as.matrix(y), z2)$coefficients, check.attributes=FALSE)
 
 res_fit_QR <- kclass_fit_QR(y=log(dt_sub[,"packs", drop=FALSE]), x_exo=cbind(1,log(dt_sub[,"rincome", drop=FALSE])), x_endo= log(dt_sub[, "rprice", drop=FALSE]),
                       z=cbind(with(dt_sub, tax/cpi), dt_sub$tdiff), k=1)
 coef(res_fit_QR)
 all.equal(coef(res_fit_QR)[c(2,1,3)], ivreg.fit(x, as.matrix(y), z2)$coefficients, check.attributes=FALSE)
 
 ## LIML
 res_fit_LI <- kclass_fit(y=log(dt_sub[,"packs", drop=FALSE]), x_exo=cbind(1,log(dt_sub[,"rincome", drop=FALSE])), x_endo= log(dt_sub[, "rprice", drop=FALSE]),
                       z=cbind(1, with(dt_sub, tax/cpi), dt_sub$tdiff, log(dt_sub$rincome)))
 coef( res_fit_LI)
 res_fit_LI$eigen
 
 res_fit_QR_LI_ext <- kclass_fit_QR(y=log(dt_sub[,"packs", drop=FALSE]), x_exo=cbind(1,log(dt_sub[,"rincome", drop=FALSE])), x_endo= log(dt_sub[, "rprice", drop=FALSE]),
                                z=cbind(with(dt_sub, tax/cpi), dt_sub$tdiff), k=res_fit_LI$eigen)
 coef(res_fit_QR_LI_ext)
 res_fit_QR_LI <- kclass_fit_QR(y=log(dt_sub[,"packs", drop=FALSE]), x_exo=cbind(1,log(dt_sub[,"rincome", drop=FALSE])), x_endo= log(dt_sub[, "rprice", drop=FALSE]),z=cbind(with(dt_sub, tax/cpi), dt_sub$tdiff))
 res_fit_QR_LI$eigen
 all.equal(res_fit_LI$eigen, res_fit_QR_LI$eigen)
}

##### OLD code:
if(FALSE){
  
  ### Old Projs:
  I_n <- diag(n)
  Z_inv <- try(solve(crossprod(z)), silent=TRUE)
  if(inherits(Z_inv, "try-error")) {
    require(MASS)
    Z_inv <- ginv(crossprod(z))
  }
  Pz <- z%*%Z_inv%*%t(z)
  Mz <- I_n - Pz
  
  if(!missing(x_exo)){
    My <-I_n - x_exo%*%solve(crossprod(x_exo))%*%t(x_exo)
    X <- cbind(x_exo, x_endo)
  } else {
    My <- I_n
    X <- x_endo
  }
  
  mat <- t(xy)%*%My%*%xy
  mat2 <- t(xy)%*%Mz%*%xy
  ## Coefs, given k:
  X_kM_inv <- solve(t(X)%*%(I_n-k*Mz)%*%X)
  val <- X_kM_inv%*%(t(X)%*%(I_n-k*Mz)%*%y)
}