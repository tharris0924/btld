
# btld

<!-- badges: start -->
<!-- badges: end -->

The goal of btld is to have functions to generate the pdfs, cdfs and icdfs of the triangular and bimodal triangular linked distributions (btld).
These functions are used to generate the multivariate distribution which uses a Gaussian copula. Functions to do that may be included later. 

## Installation

You can install the released version of btld from [GitHub](https://github.com/tharris0924/btld) with:

``` r
devtools::install_github("tharris0924/btld")
```

## Example

This is a basic example which shows you how to generate values from a btld as well as determine the pdf of the distribution. A histogram overlay of the pdf and kde is shown. 

``` r
library(btld)
## basic example code


rvs <-rbltd(1000, theta1 = 0.3, theta2 = 0.8, alpha1 = 3, alpha2 =3) # generate random variables from BTLD
# distribution)
vals<-vals[order(vals)] # filter and order
pdf <- dbtld(vals,  theta1 = 0.3, theta2 = 0.8, alpha1 = 3, alpha2 =3) # determine pdf values


# plot histogram
hist(vals, main="Histogram of BTLD with KDE and PDF overlay", xlab = "x", freq=F, col = "green",breaks = 30)
lines(density(vals),col = "red")
par(new=T)
plot(vals,pdfs,type = "l", axes = F,ann = F, col = "blue")
legend(0,3,c("KDE", "PDF"),col = c("red","blue"),lty = 1)
```

