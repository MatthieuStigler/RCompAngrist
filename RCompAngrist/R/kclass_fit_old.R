kclass_fit_old <- function(y, x, z, k=NULL, x_exo, x_endo, eig=c("eigen", "geigen")){
    
  eig <- match.arg(eig)
  n <- NROW(y)
  n_ins <- NCOL(z)
  
  ##
  if(!is.matrix(z)) z <- as.matrix(z)
  if(missing(x_endo)) x_endo <- x
  
  ## Projection matrices
  require(Matrix)
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
  
  ## determine k:  
  if(is.null(k)){
    if(NCOL(x_endo)==1){
      f <- function(p) (t(y-x_endo*p)%*%Pz%*%(y-x_endo*p))/crossprod(y-x_endo*p)  
      k <- nlm(f, 1)$minimum 
    }
    
    ## as eigen:
    xy <- cbind(y,x_endo)
    mat <<- t(xy)%*%My%*%xy
    mat2 <<- t(xy)%*%Mz%*%xy
    if(eig=="eigen"){
      k <- min(eigen(mat%*%solve(mat2))$values)
    } else {
      require(geigen)
      cat("Geig!\n")
      k <- min(geigen:::geigen(mat, mat2, symmetric=TRUE, only.values=TRUE)$values)
      k2 <- min(geigen:::geigen(mat2, mat, symmetric=TRUE, only.values=TRUE)$values)
      print(k2)
    }
  }
  
  ## Coefs, given k:
  X_kM_inv <- solve(t(X)%*%(I_n-k*Mz)%*%X)
  val <- X_kM_inv%*%(t(X)%*%(I_n-k*Mz)%*%y)
  val2 <- as.numeric(val)
  names(val2) <- colnames(X)
  
  ## Vcov: (McKinnon, Davidson 1993, p. 650)
  df <- n - length(val2)
  resids <- as.numeric(y - X%*%val)
  sig_res <- sqrt(as.numeric(crossprod(resids))/df)
  
  ## Return result
  res <- list()
  res$coefficients <- val2
  res$residuals <- resids
  res$sigma <- sig_res
  res$cov.unscaled <- X_kM_inv
  res$eigen <- k
  res$n <- n
  res$df <- df
  
  res
  
}