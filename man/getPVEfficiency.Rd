\name{getPVEfficiency}
\alias{getPVEfficiency}
\title{Get PV temperature, eletrical and thermal power}
\usage{
  getPVEfficiency(env.town, cfg)
}
\arguments{
  \item{env.town}{a \code{data.frame} holding a
  \code{time}, \code{place}, \code{tempEsterna} and
  \code{irraggiamento}}

  \item{cfg}{a \code{list} holding several cfg infos (to be
  expanded...)}
}
\description{
  The relative PV cells efficiency and the energy they can
  potentially generate.  Several models are available to
  estimate working temperature of PV cells based on
  environmental conditions. The simplest thermal model is
  based on the assumption that the ratio between the
  difference between cell temperature (Tc) and air
  temperature (Te) over sun irradiance on the cell (G) is
  constant: \eqn{\frac{T_c-T_e}{G} \approx k}. This model
  is reasonable as far as Italy is concerned due to the
  absence of abrupt changes in irradiance over the course
  of the day and thermal inertia of PV systems can be
  neglected.
}
\author{
  Marco Bascietto \email{marco@bascietto.name}
}
\references{
  L’effetto della temperatura sull’efficienza dei moduli
  fotovoltaici: cosa sapere sul NOCT, 2011,
  \url{http://www.energyhunters.it/content/l'effetto-della-temperatura-sull'efficienza-dei-moduli-fotovoltaici-cosa-sapere-sul-noct}
  retrieved on 2012/10/09
}

