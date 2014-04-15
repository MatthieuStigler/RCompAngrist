\name{kclass_fit}
\alias{kclass_fit}
\alias{kclass_fit_QR}
\title{kclass_fit
K-class Instrumental Variable estimator, low-level function

Compute the k-class IV, by default the LIML}
\usage{
kclass_fit(y, x, z, k = NULL, x_exo, x_endo, eig = c("eigen", "geigen"))

kclass_fit_QR(y, x, z, k = NULL, x_exo, x_endo, eig = c("eigen", "geigen"))
}
\arguments{
  \item{y}{vector with dependent variable}

  \item{x}{regressor matrix, containing both exo and endo
  x}

  \item{z}{instrument matrix (need not include the
  exogeneous x)}

  \item{k}{The parameter of the k-class estimator. If
  missing, the LIML is computed.}

  \item{x_exo}{vector/matrix containing only the exogeneous
  x}

  \item{x_endo}{vector/matrix containing only the
  endogeneous x}

  \item{eig}{Whether to use the simple eigenvectors on
  inverted system, or the generalized eigenvectors}
}
\value{
An object of class \code{kclass}
}
\description{
kclass_fit K-class Instrumental Variable estimator,
low-level function

Compute the k-class IV, by default the LIML
}
\examples{
require(AER)
example(ivreg.fit, echo=FALSE)
x <- model.matrix(fm, component="regressors")
z2 <- model.matrix(fm, component="instruments")

ivreg.fit(x, as.matrix(y), z=z2)$coefficients
dt_sub <- subset(CigarettesSW,  year == "1995")
res_fit <- kclass_fit(y=with(dt_sub, log(packs)), x_exo=cbind(1,with(dt_sub, log(rincome))), x_endo= with(dt_sub, log(rprice)),
                      z=cbind(with(dt_sub, tax/cpi), dt_sub$tdiff), k=1)

coef(res_fit)
all.equal(coef(res_fit)[c(2,1,3)], ivreg.fit(x, as.matrix(y), z2)$coefficients, check.attributes=FALSE)
}

