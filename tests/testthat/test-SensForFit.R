test_that("multiplication works", {
  expect_error(SensForFit(refMes, NULL, "Patient", "Age", "MeanSens", 10))
  expect_error(SensForFit(refMes, "Examtype", NULL, "Age", "MeanSens", 10))
  expect_error(SensForFit(refMes, "Examtype", "Patient", NULL, "MeanSens", 10))
  expect_error(SensForFit(refMes, "Examtype", "Patient", "Age", NULL, 10))
})
