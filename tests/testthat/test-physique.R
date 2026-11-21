test_that("la vitesse dans le vide vaut c0", {
  expect_equal(vitesse_phase(), c0)
})

test_that("l'indice est la racine de eps_r", {
  expect_equal(indice_refraction(4, 1), 2)
})

test_that("l'impedance du vide vaut environ 377 ohms", {
  expect_equal(impedance(), 376.73, tolerance = 1e-3)
})

test_that("dans le vide, E = c * B", {
  x <- onde_plane(z = seq(0, 1, 0.1), E0 = 5, f = 1e8)
  expect_equal(x$E, c0 * x$B)
})

test_that("la perte FSPL augmente de 20*log10(2) quand d double", {
  expect_equal(fspl_db(200, 1e9) - fspl_db(100, 1e9), 20 * log10(2))
})

test_that("pas d'attenuation si sigma = 0", {
  expect_equal(attenuation(1e9, 0), 0)
})

test_that("la marge est nulle a la distance maximale", {
  dm <- distance_max(20, 2, 2, f = 2.4e9, sensibilite_dbm = -80)
  m <- marge_liaison(20, 2, 2, d = dm, f = 2.4e9, sensibilite_dbm = -80)
  expect_equal(m, 0)
})
