#' Get PV temperature, eletrical and thermal power
#'
#' The relative PV cells efficiency and the energy they can potentially generate.  Several models are available to estimate working temperature of PV cells based on environmental conditions. 
#' The simplest thermal model is based on the assumption that the ratio between the difference between cell temperature (Tc) and air temperature (Te) over sun irradiance on the cell (G) is constant:
#' \deqn{\frac{T_c-T_e}{G} \approx k}. This model is reasonable as far as Italy is concerned due to the absence of abrupt changes in irradiance over the course of the day and thermal inertia of PV systems can be neglected.
#' The technical sheets of PV cells provide the NOCT value, that is the working temperature of the PV cell when air temperature is 20 \eqn{^\circ}C, sun irradiance is 800 \eqn{W/m^2} and wind speed is 1 \eqn{m/s}.
#' By coupling NOCT (Nominal Operating Cell Temperature) definition and the equation provided by the simple model we can estimate the PV cell temperature:
#' \deqn{T_c = T_e + \frac{NOCT - 20}{800}G}
#' By using \eqn{\gamma}, a power coefficient usually provided by technical sheets, the efficiency of the PV cell can be easily estimated:
#' \deqn{\eta = \eta_{std}(1-\gamma(T_c-T_{c,std}))}
#' where \eqn{\gamma} is the percentual decrease of power per Celsius degree, \eqn{\eta_{std}} is the power coefficient in standard conditions, \eqn{T_{c,std}} is PV cell temperature in standard conditions. A PV cell in standard conditions has \eqn{T_c = 25 ^\circ}C, \eqn{G = 1000 W/m^2}, wind speed \eqn{1 m/s}. 
#' 
#' Parameter \code{cfg} is a \code{list} holding those parameters needed to
#' \enumerate{
#'   \item estimate \eqn{T_c}, \eqn{\eta},
#'   \item estimate electrical and thermal power corrected for the cell tilt and for the PV system losses.
#' }
#' As for the first set \code{cfg} must hold:
#' \itemize{
#'   \item \code{NOCT} Nominal Operating Cell Temperature in \eqn{^\circ}C, usually around 45\eqn{^\circ}C for mono-crystalline and poly-crystalline Si systems, much lower for amorphous Si systems and Cadmium telluride systems
#'   \item \code{etaStd} \eqn{\gamma_{std}} in \eqn{\%}, typically around \eqn{15\%}
#'   \item \code{gamma} \eqn{\gamma} in \eqn{\%}, a positive numeric percentual decrease of power per Celsius degree typically around 0.4--0.5\eqn{\%}
#' }
#' As for the second set \code{cfg} must hold:
#' \itemize{
#'   \item \code{geoCoord} a matrix of two columns: longitude and latitude for each location
#'   \item \code{tilt} the tilt angle of PV cells (\eqn{0^\circ} for an horizontal plane, \eqn{90^\circ} for a vertical plane)
#'   \item \code{PVlosses} PV system losses in \eqn{\%}, typically around \eqn{30\%}
#' }
#' 
#' @references F. Roveda, \emph{L'effetto della temperatura sulla efficienza dei moduli fotovoltaici: cosa sapere sul NOCT}, 2011, \url{http://www.energyhunters.it}
#' retrieved on 2012/10/09
#' @references E. Skoplaki and J.A. Palyvos, \emph{Operating temperature of photovoltaic modules: A survey of pertinent correlations}, 2009, Renewable Energy 34, 1: 23--29.
#' @references W. Maranda and M. Piotrowicz, \emph{Extraction of thermal model parameters for field-installed photovoltaic module}, 2010, 27th International Conference on Microelectronics Proceedings (MIEL) (presented at the 2010 27th International Conference on Microelectronics Proceedings (MIEL), IEEE, 2010), 153-156.
#'
#' @param env.town a \code{data.frame} holding a \code{time}, \code{place}, \code{Te} and \code{G}
#' @param cfg a \code{list} holding several cfg infos, see Details
#' @export
#' @author Marco Bascietto \email{marco@@bascietto.name}
getPVEfficiency <- function(env.town, cfg) {
  # Calcolo azimuth e altezza del sole per ogni ora disponibile
  tmp.sun <- with(env.town, getSunCoordinates(cfg$geoCoord[as.character(place),], time))
  
  env.town <- within(env.town, {
    # Correzione per l'inclinazione dei pannelli (eq. derivata a naso da me, va verificata)
    tiltedCorrection <- sin((tmp.sun[, "height"] + cfg$tilt) * pi / 180)
    # W/m^2 irraggiamento corretto per l'inclinazione dei pannelli
    tiltedPanelG <- G * tiltedCorrection
    # Temperatura pannello, eq. citata in "L'effetto della temperatura sull’eta dei moduli fotovoltaici: cosa sapere sul NOCT"
    Tc       <- Te + ((cfg$NOCT - 20) / 800) * G
    # Temperatura pannello inclinato 
    tiltedTc <- Te + ((cfg$NOCT - 20) / 800) * tiltedPanelG
    # Efficienza, eq. citata in "L'effetto della temperatura sull’efficienza dei moduli fotovoltaici: cosa sapere sul NOCT"
    eta       <- (cfg$etaStd / 100 * (1 - cfg$gamma / 100 * (Tc - 25)))
    # Efficienza, pannello inclinato
    tiltedEta <- (cfg$etaStd / 100 * (1 - cfg$gamma / 100 * (tiltedTc - 25)))
    # W/m^2 Potenza elettrica teorica
    ElectricPower    <- G * eta
    # W/m^2 Potenza elettrica corretta per l'inclinazione dei pannelli
    tiltedEPower     <- ElectricPower * tiltedCorrection
    # W/m^2 Potenza elettrica corretta per l'inclinazione dei pannelli e per le perdite di carico
    lossesTiltedEPower <- tiltedEPower / (1 + cfg$PVlosses / 100)
    # W/m^2 Potenza termica teorica, assumendo che tutto ciò che non si trasforma in energia elettrica si trasforma in energia termica
    thermalPower <- G * (1 - eta)
    # W/m^2 Potenza termica corretta per l'inclinazione dei pannelli
    tiltedTPower <- thermalPower * tiltedCorrection 
  })
  
  return(env.town)
}