\name{kclass}
\alias{kclass}
\title{K-class Instrumental Variable estimator}
\usage{
  kclass(formula, instruments, data, subset, model = TRUE,
    y = TRUE, x = FALSE, k = NULL, ...)
}
\arguments{
  \item{formula}{formula specification(s) of the regression
  relationship and the instruments with three parts as in
  \code{y ~ x1 + x2 | z1 + z2 + z3}. See
  \code{\link{ivreg}} for details}

  \item{instruments}{Eventually instruments if not in
  formula}

  \item{data}{The (optional) data for the formula}

  \item{k}{The parameter of the k-class estimator. If
  missing, the LIML is computed.}

  \item{model,x,y}{If \code{TRUE} the corresponding
  components of the fit (the model frame, the model
  matrices , the response) are returned.}

  \item{\ldots}{Further argumetns passed to coeftest}
}
\value{
  An object of class \code{kclass}
}
\description{
  Compute the k-class IV, by default the LIML
}
\examples{
require(AER)

# Example from ivreg:
data("CigarettesSW")
CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)

## Use ivreg:
fm <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
data = subset(CigarettesSW, year == "1995"))
summary(fm)

## Use k-class:
fm_kcl <- kclass(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
data = subset(CigarettesSW, year == "1995"))
summary(fm_kcl)
}

