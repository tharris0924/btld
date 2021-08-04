test_that("valid cdf 1", {
  # does the limit of F(x) approach 1 as x tends to infinity?
  max<-c()
  for (i in seq_len(1000)){
    vals<-rbtld(1000,c(3,3),c(0.3,0.8))
    vals<-vals[order(vals)]
    max[i]<-max(pbtld(vals,c(3,3),c(0.3,0.8)))
  }
  boot_max<- mean(max)
  expect_equal(round(boot_max), 1)
})

test_that("valid cdf 2", {
  # does the limit of F(x) approach 0 as x tends to negative infinity?
  min<-c()
  for (i in seq_len(1000)){
    vals<-rbtld(1000,c(3,3),c(0.3,0.8))
    vals<-vals[order(vals)]
    min[i]<-min(pbtld(vals,c(3,3),c(0.3,0.8)))
  }
  boot_min<- mean(min)
  expect_equal(round(boot_min), 0)
})