% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{conso_temp}
\alias{conso_temp}
\title{Consommation d'électricité}
\format{
A data frame with 365 observations on the following 8 variables.
\describe{ \item{Date}{a factor with levels \code{01/01/2003}
\code{01/02/2003} \code{01/03/2003} \code{01/04/2003} \code{01/05/2003}
\code{01/06/2003} \code{01/07/2003} \code{01/08/2003} \code{01/09/2003}
\code{01/10/2003} \code{01/11/2003} \code{01/12/2003} \code{02/01/2003}
\code{02/02/2003} \code{02/03/2003} \code{02/04/2003} \code{02/05/2003}
\code{02/06/2003} \code{02/07/2003} \code{02/08/2003} \code{02/09/2003}
\code{02/10/2003} \code{02/11/2003} \code{02/12/2003} \code{03/01/2003}
\code{03/02/2003} \code{03/03/2003} \code{03/04/2003} \code{03/05/2003}
\code{03/06/2003} \code{03/07/2003} \code{03/08/2003} \code{03/09/2003}
\code{03/10/2003} \code{03/11/2003} \code{03/12/2003} \code{04/01/2003}
\code{04/02/2003} \code{04/03/2003} \code{04/04/2003} \code{04/05/2003}
\code{04/06/2003} \code{04/07/2003} \code{04/08/2003} \code{04/09/2003}
\code{04/10/2003} \code{04/11/2003} \code{04/12/2003} \code{05/01/2003}
\code{05/02/2003} \code{05/03/2003} \code{05/04/2003} \code{05/05/2003}
\code{05/06/2003} \code{05/07/2003} \code{05/08/2003} \code{05/09/2003}
\code{05/10/2003} \code{05/11/2003} \code{05/12/2003} \code{06/01/2003}
\code{06/02/2003} \code{06/03/2003} \code{06/04/2003} \code{06/05/2003}
\code{06/06/2003} \code{06/07/2003} \code{06/08/2003} \code{06/09/2003}
\code{06/10/2003} \code{06/11/2003} \code{06/12/2003} \code{07/01/2003}
\code{07/02/2003} \code{07/03/2003} \code{07/04/2003} \code{07/05/2003}
\code{07/06/2003} \code{07/07/2003} \code{07/08/2003} \code{07/09/2003}
\code{07/10/2003} \code{07/11/2003} \code{07/12/2003} \code{08/01/2003}
\code{08/02/2003} \code{08/03/2003} \code{08/04/2003} \code{08/05/2003}
\code{08/06/2003} \code{08/07/2003} \code{08/08/2003} \code{08/09/2003}
\code{08/10/2003} \code{08/11/2003} \code{08/12/2003} \code{09/01/2003}
\code{09/02/2003} \code{09/03/2003} \code{09/04/2003} \code{09/05/2003}
\code{09/06/2003} \code{09/07/2003} \code{09/08/2003} \code{09/09/2003}
\code{09/10/2003} \code{09/11/2003} \code{09/12/2003} \code{10/01/2003}
\code{10/02/2003} \code{10/03/2003} \code{10/04/2003} \code{10/05/2003}
\code{10/06/2003} \code{10/07/2003} \code{10/08/2003} \code{10/09/2003}
\code{10/10/2003} \code{10/11/2003} \code{10/12/2003} \code{11/01/2003}
\code{11/02/2003} \code{11/03/2003} \code{11/04/2003} \code{11/05/2003}
\code{11/06/2003} \code{11/07/2003} \code{11/08/2003} \code{11/09/2003}
\code{11/10/2003} \code{11/11/2003} \code{11/12/2003} \code{12/01/2003}
\code{12/02/2003} \code{12/03/2003} \code{12/04/2003} \code{12/05/2003}
\code{12/06/2003} \code{12/07/2003} \code{12/08/2003} \code{12/09/2003}
\code{12/10/2003} \code{12/11/2003} \code{12/12/2003} \code{13/01/2003}
\code{13/02/2003} \code{13/03/2003} \code{13/04/2003} \code{13/05/2003}
\code{13/06/2003} \code{13/07/2003} \code{13/08/2003} \code{13/09/2003}
\code{13/10/2003} \code{13/11/2003} \code{13/12/2003} \code{14/01/2003}
\code{14/02/2003} \code{14/03/2003} \code{14/04/2003} \code{14/05/2003}
\code{14/06/2003} \code{14/07/2003} \code{14/08/2003} \code{14/09/2003}
\code{14/10/2003} \code{14/11/2003} \code{14/12/2003} \code{15/01/2003}
\code{15/02/2003} \code{15/03/2003} \code{15/04/2003} \code{15/05/2003}
\code{15/06/2003} \code{15/07/2003} \code{15/08/2003} \code{15/09/2003}
\code{15/10/2003} \code{15/11/2003} \code{15/12/2003} \code{16/01/2003}
\code{16/02/2003} \code{16/03/2003} \code{16/04/2003} \code{16/05/2003}
\code{16/06/2003} \code{16/07/2003} \code{16/08/2003} \code{16/09/2003}
\code{16/10/2003} \code{16/11/2003} \code{16/12/2003} \code{17/01/2003}
\code{17/02/2003} \code{17/03/2003} \code{17/04/2003} \code{17/05/2003}
\code{17/06/2003} \code{17/07/2003} \code{17/08/2003} \code{17/09/2003}
\code{17/10/2003} \code{17/11/2003} \code{17/12/2003} \code{18/01/2003}
\code{18/02/2003} \code{18/03/2003} \code{18/04/2003} \code{18/05/2003}
\code{18/06/2003} \code{18/07/2003} \code{18/08/2003} \code{18/09/2003}
\code{18/10/2003} \code{18/11/2003} \code{18/12/2003} \code{19/01/2003}
\code{19/02/2003} \code{19/03/2003} \code{19/04/2003} \code{19/05/2003}
\code{19/06/2003} \code{19/07/2003} \code{19/08/2003} \code{19/09/2003}
\code{19/10/2003} \code{19/11/2003} \code{19/12/2003} \code{20/01/2003}
\code{20/02/2003} \code{20/03/2003} \code{20/04/2003} \code{20/05/2003}
\code{20/06/2003} \code{20/07/2003} \code{20/08/2003} \code{20/09/2003}
\code{20/10/2003} \code{20/11/2003} \code{20/12/2003} \code{21/01/2003}
\code{21/02/2003} \code{21/03/2003} \code{21/04/2003} \code{21/05/2003}
\code{21/06/2003} \code{21/07/2003} \code{21/08/2003} \code{21/09/2003}
\code{21/10/2003} \code{21/11/2003} \code{21/12/2003} \code{22/01/2003}
\code{22/02/2003} \code{22/03/2003} \code{22/04/2003} \code{22/05/2003}
\code{22/06/2003} \code{22/07/2003} \code{22/08/2003} \code{22/09/2003}
\code{22/10/2003} \code{22/11/2003} \code{22/12/2003} \code{23/01/2003}
\code{23/02/2003} \code{23/03/2003} \code{23/04/2003} \code{23/05/2003}
\code{23/06/2003} \code{23/07/2003} \code{23/08/2003} \code{23/09/2003}
\code{23/10/2003} \code{23/11/2003} \code{23/12/2003} \code{24/01/2003}
\code{24/02/2003} \code{24/03/2003} \code{24/04/2003} \code{24/05/2003}
\code{24/06/2003} \code{24/07/2003} \code{24/08/2003} \code{24/09/2003}
\code{24/10/2003} \code{24/11/2003} \code{24/12/2003} \code{25/01/2003}
\code{25/02/2003} \code{25/03/2003} \code{25/04/2003} \code{25/05/2003}
\code{25/06/2003} \code{25/07/2003} \code{25/08/2003} \code{25/09/2003}
\code{25/10/2003} \code{25/11/2003} \code{25/12/2003} \code{26/01/2003}
\code{26/02/2003} \code{26/03/2003} \code{26/04/2003} \code{26/05/2003}
\code{26/06/2003} \code{26/07/2003} \code{26/08/2003} \code{26/09/2003}
\code{26/10/2003} \code{26/11/2003} \code{26/12/2003} \code{27/01/2003}
\code{27/02/2003} \code{27/03/2003} \code{27/04/2003} \code{27/05/2003}
\code{27/06/2003} \code{27/07/2003} \code{27/08/2003} \code{27/09/2003}
\code{27/10/2003} \code{27/11/2003} \code{27/12/2003} \code{28/01/2003}
\code{28/02/2003} \code{28/03/2003} \code{28/04/2003} \code{28/05/2003}
\code{28/06/2003} \code{28/07/2003} \code{28/08/2003} \code{28/09/2003}
\code{28/10/2003} \code{28/11/2003} \code{28/12/2003} \code{29/01/2003}
\code{29/03/2003} \code{29/04/2003} \code{29/05/2003} \code{29/06/2003}
\code{29/07/2003} \code{29/08/2003} \code{29/09/2003} \code{29/10/2003}
\code{29/11/2003} \code{29/12/2003} \code{30/01/2003} \code{30/03/2003}
\code{30/04/2003} \code{30/05/2003} \code{30/06/2003} \code{30/07/2003}
\code{30/08/2003} \code{30/09/2003} \code{30/10/2003} \code{30/11/2003}
\code{30/12/2003} \code{31/01/2003} \code{31/03/2003} \code{31/05/2003}
\code{31/07/2003} \code{31/08/2003} \code{31/10/2003} \code{31/12/2003}. Date.}
\item{conso}{a numeric vector. Consommation d'électricité en MWH.}
\item{t}{a numeric vector. Numéro du jour.}
\item{nom_jour}{a factor with levels \code{Dimanche} \code{Jeudi}
\code{Lundi} \code{Mardi} \code{Mercredi} \code{Samedi} \code{Vendredi}. Nom du jour de la semaine.}
\item{mois}{a factor with levels \code{Août} \code{Avril}
\code{Décembre} \code{Février} \code{Janvier} \code{Juillet} \code{Juin}
\code{Mai} \code{Mars} \code{Novembre} \code{Octobre} \code{Septembre}. Nom du mois.}
\item{temp}{a numeric vector. Température en degrés Celsius.}
\item{ejp}{a numeric vector. Statut du jour EDF (spécial = 1 ; non spécial = 0).}
\item{ferie}{a factor with levels \code{non} \code{oui}. Statut du jour annuel (férié ou non).}
}
}
\usage{
conso_temp
}
\description{
Consommation d'électricité journalière en France de l'année 2003?
}
\examples{

data(conso_temp)
str(conso_temp)
plot(conso_temp$conso)

}
\keyword{datasets}
