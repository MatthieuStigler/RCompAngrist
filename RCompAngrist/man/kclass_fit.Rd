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
}

