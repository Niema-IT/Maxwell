#' Vitesse de la lumière dans le vide
#' @format Un nombre, en m/s.
#' @export
c0 <- 299792458

#' Perméabilité magnétique du vide
#'
#' @format Un nombre, en H/m (henry par mètre).
#' @export
mu0 <- 4 * pi * 1e-7

#' Permittivité électrique du vide
#'
#' Déduite de la relation c0^2 = 1 / (mu0 * eps0).
#'
#' @format Un nombre, en F/m (farad par mètre).
#' @export
eps0 <- 1 / (mu0 * c0^2)
