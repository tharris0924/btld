test_that("valid cdf 1", {
  max<-vector()

  for (i in seq_len(1000)){
    vals<-rtridist(1000,0.3)
    vals<-vals[order(vals)]
    max[i]<-max(tridist_cdf(vals,0.3))

  }
  boot_max<- mean(max)
  expect_equal(round(boot_max), 1)
})

test_that("valid cdf 2", {
  min<-vector()
  for (i in seq_len(1000)){
    vals<-rtridist(1000,0.3)
    vals<-vals[order(vals)]
    min[i]<-min(tridist_cdf(vals,0.3))
  }
  boot_min<- mean(min)
  expect_equal(round(boot_min), 0)
})