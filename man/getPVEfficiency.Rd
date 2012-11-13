\name{getPVEfficiency}
\alias{getPVEfficiency}
\title{Get PV temperature, eletrical and thermal power}
\usage{
  getPVEfficiency(env.town, cfg)
}
\arguments{
  \item{env.town}{a \code{data.frame} holding a
  "\code{time}", "\code{place}", "\code{tempEsterna}" and
  "\code{irraggiamento}"}

  \item{cfg}{a \code{list} holding several cfg infos (to be
  expanded...)}
}
\description{
  Equazioni da "L’effetto della temperatura sull’efficienza
  dei moduli fotovoltaici: cosa sapere sul NOCT"
  disponibile su
  http://www.energyhunters.it/content/l’effetto-della-temperatura-sull’efficienza-dei-moduli-fotovoltaici-cosa-sapere-sul-noct
  consultato in data 2012/10/09 Diversi modelli sono
  disponibili per stimare la temperatura di funzionamento
  delle celle sulla base delle condizioni ambientali, al
  fine di conoscere l’efficienza effettiva e quindi
  l’energia realmente producibile dai moduli nel contesto
  di installazione. Il modello termico piu` semplice ed
  utilizzato dai produttori di moduli è basato
  sull’assunzione che sia costante il rapporto tra la
  differenza di temperatura Tc − Te e l’irraggiamento G a
  cui `e sottoposta la cella. Questo approccio si dimostra
  ragionevole per l’Italia, paese in cui, tendenzialmente,
  l’irraggiamento non subisce variazioni brusche nell’arco
  della giornata ed `e lecito trascurare l’inerzia termica
  dei sistemi fotovoltaici. Ricorrendo a questa
  semplificazione, (Tc − Te) / G = costante dove le
  temperature sono espresse in °C e l’irraggiamento in
  W/m2.
}
\author{
  Marco Bascietto \email{marco@bascietto.name}
}

