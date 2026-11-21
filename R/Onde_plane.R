#' Onde plane monochromatique
#'
#' Calcule le champ électrique E et le champ magnétique B d'une onde plane
#' se propageant selon z.
#'
#' @param z Positions en mètres (vecteur).
#' @param t Instant en secondes.
#' @param E0 Amplitude du champ électrique en V/m.
#' @param f Fréquence en Hz.
#' @inheritParams vitesse_phase
#' @return Un data.frame avec les colonnes z, E et B.
#' @examples
#' onde_plane(z = seq(0, 3, 0.1), E0 = 5, f = 1e8)
#' @export
onde_plane <- function(z, t = 0, E0 = 1, f = 1e9, eps_r = 1, mu_r = 1) {
  v <- vitesse_phase(eps_r, mu_r)
  w <- 2 * pi * f
  k <- w / v
  E <- E0 * cos(k * z - w * t)
  B <- (E0 / v) * cos(k * z - w * t)
  data.frame(z = z, E = E, B = B)
}
#' Tracer le champ électrique
#'
#' @param x Le data.frame renvoyé par \code{onde_plane}.
#' @return \code{x}, de façon invisible.
#' @examples
#' tracer_onde(onde_plane(z = seq(0, 6, 0.01), E0 = 5, f = 1e8))
#' @export
tracer_onde <- function(x) {
  graphics::plot(x$z, x$E, type = "l", col = "blue", lwd = 2,
                 xlab = "z (m)", ylab = "E (V/m)",
                 main = "Champ electrique de l'onde plane")
  invisible(x)
}
