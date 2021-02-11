test_that("Cria a base", {
  expect_known_output(consolidar_base(), "data/base.rds")
})
