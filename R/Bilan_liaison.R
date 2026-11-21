#' Puissance reçue (bilan de liaison)
#'
#' @param Pt_dbm Puissance d'émission en dBm.
#' @param Gt_db,Gr_db Gains des antennes d'émission et de réception en dB.
#' @param d Distance en mètres.
#' @param f Fréquence en Hz.
#' @param pertes_db Pertes supplémentaires en dB (câbles, obstacles).
#' @return La puissance reçue en dBm.
#' @examples
#' puissance_recue(20, 2, 2, d = 100, f = 2.4e9)
#' @export
puissance_recue <- function(Pt_dbm, Gt_db, Gr_db, d, f, pertes_db = 0) {
  Pt_dbm + Gt_db + Gr_db - fspl_db(d, f) - pertes_db
}
#' Marge de liaison
#'
#' Écart entre la puissance reçue et la sensibilité du récepteur.
#' Une marge positive signifie que la liaison fonctionne.
#'
#' @inheritParams puissance_recue
#' @param sensibilite_dbm Sensibilité du récepteur en dBm.
#' @return La marge en dB.
#' @examples
#' marge_liaison(20, 2, 2, d = 100, f = 2.4e9, sensibilite_dbm = -80)
#' @export
marge_liaison <- function(Pt_dbm, Gt_db, Gr_db, d, f,
                          sensibilite_dbm, pertes_db = 0) {
  puissance_recue(Pt_dbm, Gt_db, Gr_db, d, f, pertes_db) - sensibilite_dbm
}
#' Distance maximale de la liaison
#'
#' @inheritParams marge_liaison
#' @return La distance maximale en mètres, en espace libre.
#' @examples
#' distance_max(20, 2, 2, f = 2.4e9, sensibilite_dbm = -80)
#' @export
distance_max <- function(Pt_dbm, Gt_db, Gr_db, f,
                         sensibilite_dbm, pertes_db = 0) {
  fspl_max <- Pt_dbm + Gt_db + Gr_db - pertes_db - sensibilite_dbm
  10^((fspl_max - 20 * log10(f) - 20 * log10(4 * pi / c0)) / 20)
}
