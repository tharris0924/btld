test_that("integral of pdf is one",{
  lower_x<-0
  upper_x<-1
  width<-abs(upper_x-lower_x) # distance on x plane
  x1<-runif(1000, lower_x, upper_x) # uniform random variables on x plane
  z<-dtridist(x1, theta=0.3) # function values giving values on z plane
  height <- mean(z) # distance on z plane
  area_mcmc <- width*height
  area_mcmc
  expect_equal(round(area_mcmc), 1)
})

