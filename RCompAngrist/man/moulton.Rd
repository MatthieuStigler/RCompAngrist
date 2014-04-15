\name{moulton}
\alias{moulton}
\title{Moulton correction for clustered standard errors}
\usage{
  moulton(lm, cluster, x = NULL, trace = FALSE,
    custom = NULL)
}
\arguments{
  \item{lm}{Object of class \code{lm}}

  \item{cluster}{The cluster variable}

  \item{x}{Will be removed?}

  \item{trace}{Print intermediate output}

  \item{custom}{User defined value to control for}
}
\value{
  A matrix of coefficients, standard erros and t/p-values
}
\description{
  Corrects
}

