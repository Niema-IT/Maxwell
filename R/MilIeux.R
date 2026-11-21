#' Vitesse de phase dans un milieu
#'
#' Calcule la vitesse de propagation d'une onde électromagnétique
#' dans un milieu, à partir de sa permittivité et de sa perméabilité relatives.
#'
#' @param eps_r Permittivité relative du milieu (1 pour le vide).
#' @param mu_r Perméabilité relative du milieu (1 pour le vide).
#' @return La vitesse en m/s.
#' @examples
#' vitesse_phase()
#' vitesse_phase(eps_r = 4)
#' @export
vitesse_phase <- function(eps_r = 1, mu_r = 1) {
  1 / sqrt(mu0 * mu_r * eps0 * eps_r)
}
vitesse_phase <- function(eps_r = 1, mu_r = 1) {
  1 / sqrt(mu0 * mu_r * eps0 * eps_r)
}
#' Indice de réfraction
#'
#' Rapport entre la vitesse de la lumière dans le vide et celle dans le milieu.
#'
#' @inheritParams vitesse_phase
#' @return L'indice n (sans unité).
#' @examples
#' indice_refraction(eps_r = 4)
#' @export
indice_refraction <- function(eps_r = 1, mu_r = 1) {
  sqrt(eps_r * mu_r)
}
#' Impédance intrinsèque d'un milieu
#'
#' @inheritParams vitesse_phase
#' @return L'impédance en ohms (environ 377 ohms dans le vide).
#' @examples
#' impedance()
#' @export
impedance <- function(eps_r = 1, mu_r = 1) {
  sqrt((mu0 * mu_r) / (eps0 * eps_r))
}
#' Atténuation dans un milieu conducteur
#'
#' @param f Fréquence en Hz.
#' @param sigma Conductivité du milieu en S/m.
#' @inheritParams vitesse_phase
#' @return Le coefficient d'atténuation alpha en Np/m.
#' @examples
#' attenuation(f = 1e9, sigma = 0.01)
#' @export
attenuation <- function(f, sigma, eps_r = 1, mu_r = 1) {
  w <- 2 * pi * f
  mu <- mu0 * mu_r
  eps <- eps0 * eps_r
  w * sqrt(mu * eps / 2) * sqrt(sqrt(1 + (sigma / (w * eps))^2) - 1)
}
#' Perte en espace libre (FSPL)
#'
#' @param d Distance en mètres.
#' @param f Fréquence en Hz.
#' @return La perte en dB.
#' @examples
#' fspl_db(d = 100, f = 2.4e9)
#' @export
fspl_db <- function(d, f) {
  20 * log10(d) + 20 * log10(f) + 20 * log10(4 * pi / c0)
}

