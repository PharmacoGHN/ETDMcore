

expected_pop_file <- data.frame(
  id = 1:10,
  CL = c(94.393, 94.811, 91.090, 104.113, 120.238, 85.278, 92.506, 109.999, 120.182, 114.097),
  V1 = c(484.138, 37.946, 66.865, 240.854, 493.916, 1511.888, 1641.528, 48.005, 467.128, 189.840),
  Q2 = rep(180, 10),
  V2 = rep(2890, 10),
  Q3 = rep(10.6, 10),
  V3 = rep(2610, 10),
  KA = rep(0.259, 10),
  WT = rep(70, 10)
)

test_that("multiplication works", {
  library(mrgsolve)
  model <- mread(test_path("testdata/test_model.mod"))
  pop_data <- makePopulationData(model, n = 10, seed = TRUE)
  expect_equal(pop_data, expected_pop_file)
})
