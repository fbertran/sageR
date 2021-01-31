#' Title
#'
#' @param cutoff
#'
#' @return
#' @export
#'
#' @examples
filter_down <- function(cutoff = 30) {
  dplyr::filter(swiss, Agriculture < cutoff)
}
