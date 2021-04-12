#' Population du Canada par classes d’âge et provinces et territoires en 2020
#'
#' Répartition en classes d’âge de la population des provinces et des territoires du Canada en 2020. Statistique Canada. Tableau 17-10-0005-01  Estimations de la population au 1er juillet, par âge et sexe.
#'
#' @docType data
#' @format Un jeu de données avec 21 observations de 13 variables.
#' \describe{
#'   \item{Terre.Neuve.et.Labrador}{}
#'   \item{Île.du.Prince.Édouard}{}
#'   \item{Nouvelle.Écosse}{}
#'   \item{Nouveau.Brunswick}{}
#'   \item{Québec}{}
#'   \item{Ontario}{}
#'   \item{Manitoba}{}
#'   \item{Saskatchewan}{}
#'   \item{Alberta}{}
#'   \item{Colombie.Britannique}{}
#'   \item{Yukon}{}
#'   \item{TerritoiresduNord.Ouest}{}
#'   \item{Nunavut}{}
#' }
#' @references \url{https://doi.org/10.25318/1710000501-fra}
"AgevsProter_Canada_full"
#> [1] "AgevsProter_Canada_full"

#' Population du Canada par classes d’âge et sexe en 2020
#'
#' Répartition en classes d’âge et sexe de la population du Canada en 2020. Statistique Canada. Tableau 17-10-0005-01  Estimations de la population au 1er juillet, par âge et sexe.
#'
#' @docType data
#' @format Un jeu de données avec 21 observations de 2 variables.
#' \describe{
#'   \item{Hommes}{}
#'   \item{Femmes}{}
#' }
#' @references \url{https://doi.org/10.25318/1710000501-fra}
"AgevsSexe_Canada_full"
#> [1] "AgevsSexe_Canada_full"

#' Taux d'emploi en % de la classe d'age
#'
#' Le taux d'emploi d'une classe d'âge se mesure en fonction du nombre des actifs occupés d'un âge donné rapporté à l'effectif total de cette classe d'âge. Les actifs occupés sont les personnes de 15 ans et plus qui, durant la semaine de référence, déclarent avoir effectué un travail rémunéré pendant une heure au moins ou avoir occupé un emploi dont elles étaient absentes. Les taux d'emploi sont présentés pour quatre classes d'âge : les personnes âgées de 15 à 64 ans (personnes en âge de travailler); les personnes âgées de 15 à 24 ans sont celles qui font leur entrée sur le marché du travail à l'issue de leur scolarité, les personnes âgées de 25 à 54 ans sont celles qui sont au plus fort de leur activité professionnelle, et les personnes âgées de 55 à 64 ans sont celles qui ont dépassé le pic de leur carrière professionnelle et approchent de l'âge de la retraite. Cet indicateur est désaisonnalisé et est mesuré en pourcentage de l'effectif total de la classe d'âge. OCDE (2021), Taux d'emploi par groupe d'âge (indicateur).
#'
#' @docType data
#' @format Un jeu de données avec 35 observations de 6 variables.
#' \describe{
#'   \item{Etats.membres}{}
#'   \item{Partiel_Ens}{}
#'   \item{Partiel_H}{}
#'   \item{Partiel_F}{}
#'   \item{Salariés}{}
#'   \item{NonSalariés}{}
#' }
#' @references \url{https://doi.org/10.1787/b01db125-fr}
"Europe"
#> [1] "Europe"

#' Personnes hospitalisées atteintes de la Covid 19 (21/02/2021)
#'
#' Répartition par région française du nombre de personnes hospitalisées et atteintes du Covid 19 le 21 février 2021.
#'
#' @docType data
#' @format Un jeu de données avec 19 observations de 12 variables.
#' \describe{
#'   \item{Région}{}
#'   \item{Tous.âges}{}
#'   \item{X0.9}{}
#'   \item{X11.19}{}
#'   \item{X20.29}{}
#'   \item{X30.39}{}
#'   \item{X40.49}{}
#'   \item{X50.59}{}
#'   \item{X60.69}{}
#'   \item{X70.79}{}
#'   \item{X80.89}{}
#'   \item{X90.}{}
#' }
"HospitFull"
#> [1] "HospitFull"

#' Prix de vente en fonction de la marque, format large ou dépilé
#'
#' Valeurs d'articles de qualité équivalente en fonction de leur marque.
#'
#' @docType data
#' @format Un jeu de données avec 30 observations de 3 variables.
#' \describe{
#'   \item{Marque.1}{}
#'   \item{Marque.2}{}
#'   \item{Marque.3}{}
#' }
"Marque.Valeur.large"
#> [1] "Marque.Valeur.large"

#' Prix de vente en fonction de la marque, format long ou empilé
#'
#' Valeurs d'articles de qualité équivalente en fonction de leur marque.
#'
#' @docType data
#' @format Un jeu de données avec 90 observations de 2 variables.
#' \describe{
#'   \item{Marque}{}
#'   \item{Valeur}{}
#' }
"Marque.Valeur"
#> [1] "Marque.Valeur"

#' Nombre de personnes dans un foyer
#'
#' Ménages par taille du ménage en 2017, source INSEE.
#'
#' @docType data
#' @format Un jeu de données avec 6 observations de 2 variables `xi` et `ni`.
#' \describe{
#'   \item{xi}{Nombres de personnes dans un foyer}
#'   \item{ni}{Nombres de foyers}
#' }
"Personnes_Foyer"
#> [1] "Personnes_Foyer"

#' Population du Canada par classes d’âge et provinces et territoires en 2020
#'
#' Répartition en provinces et territoires et sexe de la population du Canada en 2020. Statistique Canada. Tableau 17-10-0005-01  Estimations de la population au 1er juillet, par âge et sexe.
#'
#' @docType data
#' @format Un jeu de données avec 13 observations de 2 variables.
#' \describe{
#'   \item{Hommes}{}
#'   \item{Femmes}{}
#' }
#' @references \url{https://doi.org/10.25318/1710000501-fra}
"ProtervsSexe_Canada"
#> [1] "ProtervsSexe_Canada"

#' Personnes en réanimation atteintes de la Covid 19 (21/02/2021)
#'
#' Répartition par région française du nombre de personne en réanimation et atteintes de la Covid 19 le 21 février 2021.
#'
#' @docType data
#' @format Un jeu de données avec 19 observations de 12 variables.
#' \describe{
#'   \item{Région}{}
#'   \item{Tous.âges}{}
#'   \item{X0.9}{}
#'   \item{X11.19}{}
#'   \item{X20.29}{}
#'   \item{X30.39}{}
#'   \item{X40.49}{}
#'   \item{X50.59}{}
#'   \item{X60.69}{}
#'   \item{X70.79}{}
#'   \item{X80.89}{}
#'   \item{X90.}{}
#' }
"ReaFull"
#> [1] "ReaFull"

#' Part du revenu national total équivalent	en Euro en 2019
#'
#' Répartitition du revenu par quantiles - enquêtes EU-SILC et PCM (ILC_DI01).
#'
#' @docType data
#' @format Un jeu de données avec 10 observations de 36 variables.
#' \describe{
#'   \item{"Déciles"}{}
#'   \item{"Belgique"}{}
#'   \item{"Bulgarie"}{}
#'   \item{"Tchéquie"}{}
#'   \item{"Danemark"}{}
#'   \item{"Allemagne..jusqu.en.1990..ancien.territoire.de.la.RFA."}{}
#'   \item{"Estonie"}{}
#'   \item{"Irlande"}{}
#'   \item{"Grèce"}{}
#'   \item{"Espagne"}{}
#'   \item{"France"}{}
#'   \item{"Croatie"}{}
#'   \item{"Italie"}{}
#'   \item{"Chypre"}{}
#'   \item{"Lettonie"}{}
#'   \item{"Lituanie"}{}
#'   \item{"Luxembourg"}{}
#'   \item{"Hongrie"}{}
#'   \item{"Malte"}{}
#'   \item{"Pays.Bas"}{}
#'   \item{"Autriche"}{}
#'   \item{"Pologne"}{}
#'   \item{"Portugal"}{}
#'   \item{"Roumanie"}{}
#'   \item{"Slovénie"}{}
#'   \item{"Slovaquie"}{}
#'   \item{"Finlande"}{}
#'   \item{"Suède"}{}
#'   \item{"Islande"}{}
#'   \item{"Norvège"}{}
#'   \item{"Suisse"}{}
#'   \item{"Royaume.Uni"}{}
#'   \item{"Monténégro"}{}
#'   \item{"Macédoine.du.Nord"}{}
#'   \item{"Serbie"}{}
#'   \item{"Turquie"}{}
#' }
"Richesse"
#> [1] "Richesse"

#' Emploi par secteur et par pays dans les pays de l'OCDE en 2020-Q3.
#'
#' Emploi par secteur d'activité et par pays (indicateur). OCDE (2021). [doi: 10.1787/6b2fff89-fr](https://doi.org/10.1787/6b2fff89-fr). (Consulté le 11 février 2021).
#' `INDUSCONSTR désigne l'activité industrielle *AVEC* la construction.
#'
#' @docType data
#' @format Un jeu de données avec 34 observations de 6 variables .
#' \describe{
#'   \item{PAYS}{Nombres de personnes dans un foyer}
#'   \item{AGR}{Nombres de foyers}
#'   \item{CONSTR}{Nombres de foyers}
#'   \item{INDUSCONSTR}{Nombres de foyers}
#'   \item{MFG}{Nombres de foyers}
#'   \item{SERV}{Nombres de foyers}
#' }
"Secteur"
#> [1] "Secteur"

#' Nombre de sièges et de voix dans l'Union européenne
#'
#' Nombre de sièges et de voix dans l'Union européenne.
#'
#' @docType data
#' @format Un jeu de données avec 27 observations de 4 variables.
#' \describe{
#'   \item{Etats.Membres}{}
#'   \item{Date.entrée}{}
#'   \item{Sièges.au.parlement}{}
#'   \item{Voix.au.conseil}{}
#' }
"Sieges_Voix"
#> [1] "Sieges_Voix"

#' Emploi total par pays dans les pays de l'OCDE en 2020-Q3.
#'
#' Emploi total par pays (indicateur). OCDE (2021). [doi: 10.1787/6b2fff89-fr](https://doi.org/10.1787/6b2fff89-fr). (Consulté le 11 février 2021).
#'
#' @docType data
#' @format Un jeu de données avec 34 observations de 2 variables.
#' \describe{
#'   \item{NameX}{Acronyme du pays}
#'   \item{Effectif}{Nombres de personnes}
#' }
"Total_Pays"
#> [1] "Total_Pays"

#' Emploi total par secteur dans les pays de l'OCDE en 2020-Q3.
#'
#' Emploi total par secteur d'activité (indicateur). OCDE (2021). [doi: 10.1787/6b2fff89-fr](https://doi.org/10.1787/6b2fff89-fr). (Consulté le 11 février 2021).
#' L'industrie (`INDUSwithoutCONSTR`) désigne l'activité industrielle *SANS* la construction.
#'
#' @docType data
#' @format Un jeu de données avec 5 observations de 3 variables.
#' \describe{
#'   \item{Secteur}{Acronyme du secteur d'actvité}
#'   \item{NameX}{Nom du secteur d'actvité}
#'   \item{Effectif}{Nombres de personnes}
#' }
"Total_Secteur"
#> [1] "Total_Secteur"


