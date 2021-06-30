test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that("pdf gives output",{
  expect_output(btld_pdf(runif(1000),3,3,0.3,0.8))
})