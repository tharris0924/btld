
# btld: Bimodal Triangular Linked Distributions and Extensions

<!-- badges: start -->
<!-- badges: end -->

The goal of btld is to have functions to generate the pdfs, cdfs and icdfs of the triangular and bimodal triangular linked distributions (btld).
These functions are used to generate the multivariate distribution which uses a Gaussian copula. Functionality to generate the multivariate distribution has been implemented. <br>
This package was built in a Linux system and tested using R 4.1.3 in Manjaro Gnome 15.5 and R 4.0.5 on Windows 10. 

## Installation

You can install the released version of btld from [GitHub](https://github.com/tharris0924/btld) with:
```r
install_github('tharris2409/btld', dependencies = TRUE)
```
## Example

This is a basic example which shows you how to generate values from a btld as well as determine the pdf of the distribution. A histogram overlay of the pdf and kde is shown. 
```r
library(btld)

alpha<-c(5,1)
theta<-c(0.3,0.7)
rvs <-rbtld(size=1000, alpha = alpha, theta =theta) # generate random variables from BTLD
# distribution)
vals<-rvs[order(rvs)]
 # generate corresponding pdf values for the rvs
pdf <- dbtld(vals,  alpha = alpha, theta =theta)

# plot histogram
hist(vals, main="Histogram of BTLD with KDE and PDF overlay", xlab = "x", freq=F, col = "green",breaks = 30)
lines(density(vals),col = "red")
par(new=T)
plot(vals,pdf,type = "l", axes = F,ann = F, col = "blue")
legend(0,3,c("KDE", "PDF"),col = c("red","blue"),lty = 1)
```
## Example 2

This example shows the calculation of goodness of fit for the btld taking values from the above example
```r
alpha<-c(5,1)
theta<-c(0.3,0.7)
rvs <-rbtld(size=1000, alpha = alpha, theta =theta) 
vals<-rvs[order(rvs)]
cdf<-pbtld(vals, alpha = alpha, theta =theta)
n<-length(cdf)

ks.test(rvs,"pbtld", alpha=alpha,theta=theta)
goftest::cvm.test(rvs,"pbtld",alpha=alpha,theta=theta,estimated=TRUE)
ecdf <- rep(1/n,n)
ecdf<- cumsum(ecdf)
plot(vals,cdf,type = "l", lty="dashed",col="Blue", main = "Generated CDF vs Emperical CDF", xlab = "X", ylab="Cumulative Density")
par(new=T)
plot(vals,ecdf, col="Red", type="l", ann = F, axes = F)


legend(0.8, 0.2, c("Generated CDF", "Emperical CDF"), col = c("Blue", "Red"),
       lty = c("dashed", "solid"),
       bg = "gray90")

```
