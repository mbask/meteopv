#' Get PV temperature, eletrical and thermal power
#'
#' The relative PV cells efficiency and the energy they can potentially generate.  Several models are available to estimate working temperature of PV cells based on environmental conditions. 
#' The simplest thermal model is based on the assumption that the ratio between the difference between cell temperature (Tc) and air temperature (Te) over sun irradiance on the cell (G) is constant: \eqn{\frac{T_c-T_e}{G} \approx k}. This model is reasonable as far as Italy is concerned due to the absence of abrupt changes in irradiance over the course of the day and thermal inertia of PV systems can be neglected.
#' 
#' @references L’effetto della temperatura sull’efficienza dei moduli fotovoltaici: cosa sapere sul NOCT, 2011, \url{http://www.energyhunters.it/content/l'effetto-della-temperatura-sull'efficienza-dei-moduli-fotovoltaici-cosa-sapere-sul-noct}
#' retrieved on 2012/10/09
#'
#' @param env.town a \code{data.frame} holding a \code{time}, \code{place}, \code{tempEsterna} and \code{irraggiamento}
#' @param cfg a \code{list} holding several cfg infos (to be expanded...)
#' @export
#' @author Marco Bascietto \email{marco@@bascietto.name}
getPVEfficiency <- function(env.town, cfg) {
  # Calcolo azimuth e altezza del sole per ogni ora disponibile
  tmp.sun <- with(env.town, getSunCoordinates(cfg$geoCoord[as.character(place),], time))
  
  env.town <- within(env.town, {
    # Correzione per l'inclinazione dei pannelli (eq. derivata a naso da me, va verificata)
    inclCorrection      <- sin((tmp.sun[, "height"] + cfg$angoloPannelli) * pi / 180)
    # W/m^2 irraggiamento corretto per l'inclinazione dei pannelli
    #irraggiamentoIncl   <- irraggiamento * inclCorrection
    # Temperatura pannello, eq. citata in "L'effetto della temperatura sull’efficienza dei moduli fotovoltaici: cosa sapere sul NOCT"
    tempPannello     <- tempEsterna + ((cfg$NOCT - 20) / 800) * irraggiamento
    # Temperatura pannello inclinato 
    #tempPannelloIncl <- tempEsterna + ((cfg$NOCT - 20) / 800) * irraggiamentoIncl
    # Efficienza, eq. citata in "L'effetto della temperatura sull’efficienza dei moduli fotovoltaici: cosa sapere sul NOCT"
    efficienza      <- (cfg$effCondStand_eta / 100 * (1 - cfg$coeffPotenza_gamma / 100 * (tempPannello - 25)))
    # Efficienza, pannello inclinato
    #efficienzaIncl  <- (cfg$effCondStand_eta / 100 * (1 - cfg$coeffPotenza_gamma / 100 * (tempPannelloIncl - 25)))
    # W/m^2 Potenza elettrica teorica
    potElettrica         <- irraggiamento * efficienza
    # W/m^2 Potenza elettrica corretta per l'inclinazione dei pannelli
    #potElettricaIncl     <- potElettrica * inclCorrection
    # W/m^2 Potenza elettrica corretta per l'inclinazione dei pannelli e per le perdite di carico
    #potElettricaInclPerd <- potElettricaIncl / (1 + cfg$perditeSistemaFV)
    # W/m^2 Potenza termica teorica, assumendo che tutto ciò che non si trasforma in energia elettrica si trasforma in energia termica
    # QUESTA `E LA EQUAZIONE CRITICA DELLA CONVERSIONE IN CALORE DELL'ENERGIA DI SCARTO
    potTermica     <- irraggiamento * (1 - efficienza)
    # W/m^2 Potenza termica corretta per l'inclinazione dei pannelli
    #potTermicaIncl <- potTermica * inclCorrection 
  })
  
  return(env.town)
}