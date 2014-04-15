#' K-class Instrumental Variable estimator
#' 
#' Compute the k-class IV, by default the LIML
#' @param formula formula specification(s) of the regression relationship and the instruments 
#' with  three parts as in \code{y ~ x1 + x2 | z1 + z2 + z3}. See \code{\link{ivreg}} for details
#' @param instruments Eventually instruments if not in formula
#' @param data The (optional) data for the formula
#' @param k The parameter of the k-class estimator. If missing, the LIML is computed.
#' @param model,x,y If \code{TRUE} the corresponding components of
#' the fit (the model frame, the model matrices , the response) are returned.
#' @param subset an optional vector specifying a subset of observations to be used in the fitting process.
#' @param useQR if low-level function kclass_fit_QR should be called. 
#' @param \ldots Further argumetns passed to coeftest
#' @return An object of class \code{kclass}
#' @seealso \code{\link{kclass_fit}} for the low-level function
#' @export
#' @import AER
#' @import Formula
#' @import MASS
#' @examples
#' require(AER)
#' 
#' # Example from ivreg:
#' data("CigarettesSW")
#' CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
#' CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
#' CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)
#' 
#' ## Use ivreg:
#' fm <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
#' data = subset(CigarettesSW, year == "1995"))
#' summary(fm)
#' 
#' ## Use k-class:
#' fm_kcl <- kclass(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
#' data = subset(CigarettesSW, year == "1995"))
#' summary(fm_kcl)
kclass <- function (formula, instruments, data, subset, 
                         model = TRUE, y = TRUE, x = FALSE, k=NULL, useQR=TRUE,
                         ...) 
{
  
  ## cancel som egeneral speciications:
  
  cl <- match.call()
  if (missing(data)) 
    data <- environment(formula)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset"),
             names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  if (!missing(instruments)) {
    formula <- as.Formula(formula, instruments)
    cl$instruments <- NULL
    cl$formula <- formula(formula)
  }
  else {
    formula <- as.Formula(formula)
  }
  stopifnot(length(formula)[1] == 1L, length(formula)[2] %in% 
              1:2)
  has_dot <- function(formula) inherits(try(terms(formula), 
                                            silent = TRUE), "try-error")
  if (has_dot(formula)) {
    f1 <- formula(formula, rhs = 1)
    f2 <- formula(formula, lhs = 0, rhs = 2)
    if (!has_dot(f1) & has_dot(f2)) 
      formula <- as.Formula(f1, update(formula(formula, 
                                               lhs = 0, rhs = 1), f2))
  }
  mf$formula <- formula
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  Y <- model.response(mf, "numeric")
  mt <- terms(formula, data = data)
  mtX <- terms(formula, data = data, rhs = 1)
  X <- model.matrix(mtX, mf)
  if (length(formula)[2] < 2L) {
    mtZ <- NULL
    Z <- NULL
  }
  else {
    mtZ <- delete.response(terms(formula, data = data, rhs = 2))
    Z <- model.matrix(mtZ, mf)
  }
  
  ## My stuff:
#   mtX_1 <- terms(formula, data = data, rhs = 1, lhs=0)
#   mtX_2 <- terms(formula, data = data, rhs = 2, lhs=0)
#   
#   x_exo <- attr(mtX_1, "term.labels")[attr(mtX_1, "term.labels")%in%attr(mtX_2, "term.labels")]
#   x_endo <- attr(mtX_1, "term.labels")[!attr(mtX_1, "term.labels")%in%attr(mtX_2, "term.labels")]
#   z <- attr(mtX_2, "term.labels")[!attr(mtX_2, "term.labels")%in%x_exo]
#   if(length(x_endo)>1) x_endo <- paste(x_endo, collapse="+")
#   if(length(x_exo)>1) x_exo <- paste(x_exo, collapse="+")
#   if(length(z)>1) z <- paste(z, collapse="+")
#   mtX_exo <- update(formula(formula, rhs=1), paste(". ~ . -", x_endo))
#   mtX_endo <- update(formula(formula, rhs=1), paste(". ~ . -1-", x_exo))
#   mtZ <- update(formula(formula, rhs=2), paste(". ~ .  -", x_exo), evaluate=FALSE)
#   
#   Z <- model.matrix(mtZ, data)
#   Z <- model.matrix(formula(formula, rhs=2), data)
#   X_exo <- model.matrix(mtX_exo, data)
#   X_endo <- model.matrix(mtX_endo, data)
  
  ### Achim way
  x_all <- model.matrix(formula, data = data, lhs = 0, rhs = 1)
  z_all <- model.matrix(formula, data = data, lhs = 0, rhs = 2)
  
  ## sub-matrices for exogenous, endogenous, and instrument variables
  which_X_exo <- colnames(x_all) %in% colnames(z_all)
  which_ins <- !(colnames(z_all) %in% colnames(x_all))
  which_X_endo <- !(colnames(x_all) %in% colnames(z_all))
    
  X_exo <- x_all[, which_X_exo, drop = FALSE]
  ins <- z_all[, which_ins, drop = FALSE]
  X_endo <- x_all[, which_X_endo, drop = FALSE]
  
  if(useQR){
    rval <- kclass_fit_QR(y=as.matrix(Y), x_exo=as.matrix(X_exo), x_endo=as.matrix(X_endo), z=as.matrix(ins), k=k,...)
  } else {
    rval <- kclass_fit(y=as.matrix(Y), x_exo=as.matrix(X_exo), x_endo=as.matrix(X_endo), z=as.matrix(ins), k=k,...)
  }
  
  
  rval$x_exo <- colnames(x_all)[which_X_exo]
  rval$x_endo <- colnames(x_all)[which_X_endo]
  rval$z <- colnames(z_all)[which_ins]
  
  rval$call <- cl
  rval$formula <- formula(formula)
  rval$terms <- list(regressors = mtX, instruments = mtZ, full = mt)
  rval$levels <- .getXlevels(mt, mf)
  if (model) 
    rval$model <- mf
  if (y) 
    rval$y <- Y
  if (x) 
    rval$x <- list(regressors = x_all, instruments = z_all, projected = rval$x)
  else rval$x <- NULL
  class(rval) <- c("kclass", "ivreg")
  return(rval)
}

#' @S3method print kclass
print.kclass <- function(x,...){
  cat("Exo: ", x$x_exo, "\n")
  cat("Endo: ", x$x_endo, "\n")
  cat("Z: ", x$z, "\n")
  AER:::print.ivreg(x)
  
  cat("k: ", x$eigen, "\n")
}


#' @S3method vcov kclass
vcov.kclass <- function (object, adj=c("df", "n"), ...) {
  adj <- match.arg(adj)
  sig <- object$sigma^2
  if(adj=="n") sig <- (object$df/object$n)*sig
  sig * object$cov.unscaled
}

#' @S3method summary kclass
summary.kclass <- function (object, adj=c("df", "n"), ...) {
  adj <- match.arg(adj)
  
  N_adj <- if(adj=="df") object$df else object$n
  est <- coef(object)
  vc <- vcov(object, adj=adj) 
  ses <- sqrt(diag(vc))
  tval <- est/ses
  pval <- 2 * pt(abs(tval), N_adj, lower.tail = FALSE)
  
  
  ans <- list()
  coefficients <- matrix(NA, length(est), 4L)
  
  coefficients <- cbind(est, ses, tval, pval)  
  
  dimnames(coefficients) <- list(names(est), 
                                 c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
  coefficients
  
}


#### OLD STUFF
if(FALSE){
  
  
  
}