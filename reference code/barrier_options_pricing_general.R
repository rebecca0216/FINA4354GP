
##################################################################
## 
## Using a "general-purpose" approach, this program illustrates the
## pricing of down-and-out barrier options. In particular, we use
## Monte Carlo simulation, combined with some ideas from functional
## programming and the "magic formula" from the lecture notes on
## "Barrier Options."
## 
## Note that we could also directly use Monte Carlo simulation without
## using our "magic formula" to price arbitrary barrier
## options. However, here we are using a combination of an analytical
## solution (i.e. our "magic formula") and a numerical solution
## (i.e. Monte Carlo simulation for the pricing of the "chopped-off"
## simple claim) to show that it is possible to combine both
## approaches seamlessly.
## 
## Our combined solution (analytical plus numerical) is more efficient
## than a "pure" Monte Carlo solution, since we do not have to
## simulate the whole price path. Instead, we only need to simulate
## the stock price at time 'T'. This is so since our "magic formula"
## allows us to reduce the problem of pricing a *path-dependent* claim
## to that of pricing a *simple* claim!
## 
##################################################################



## Variables about the economy: risk-free interest rate, stock price
## volatility, and current stock price.
r <- 0.01
sigma <- 0.3
s0 <- 50

## Variables about the option: barrier and time to expiration.
L <- 30
T <- 1



## This function returns the contract function that is "chopped off"
## below 'L'. (Yes, indeed, this function accepts a function as its
## function argument, and then returns another function. That's why R
## is called a 'functional' programming language, because you can do
## all sort of tricks with functions in R.) Note that 'phi' is the
## original contract function, while 'L' is the barrier below which
## 'phi' is chopped off.
phi.sub.L <- function(phi, L) {
  g <- function(s) {
    if (s <= L) return(0)               # Get rid of easy case.
    return(phi(s))
  }
  return(g)
}

## This function determines the price of any *simple* claim 'phi' by
## means of Monte Carlo Simulation. It assumes that the economy
## follows a Black-Scholes model.
price <- function(s0, sigma, r, T, phi, trials = 1e+04) {
  set.seed(8)                           # Get reproducible results.
  payoff <- rep(0, trials)
  for (j in seq_len(trials)) {
    dW <- sqrt(T) * rnorm(1)
    sT <- s0 * exp((r - (1/2) * sigma^2) * T + sigma * dW)
    payoff[j] <- phi(sT)
  }
  return(exp(-r * T) * mean(payoff))
}

## This function determines the price of any down-and-out contract for
## the contract function 'phi' by applying our "magic formula." The
## function assumes that if 's0>L', the barrier has not yet been hit.
price.DO <- function(s0, sigma, r, T, phi, L, trials = 1e+04) {
  if (s0 <= L) return(0)                # Get rid of easy case.
  phi.cut.off <- phi.sub.L(phi, L)      # 'phi.cut.off' is a function!
  adjustment.factor <- (L / s0)^(2 * (r - (1/2) * sigma^2) / sigma^2)
  price.basic <- price(s0, sigma, r, T, phi.cut.off, trials)
  price.adjusted <- price(L^2 / s0, sigma, r, T, phi.cut.off, trials)
  return(price.basic - adjustment.factor * price.adjusted)
}



##################################################################
##
## For illustration, let's price a down-and-out contract on the
## stock. We do this only for illustration, since for this particular
## contract we could actually solve it using an analytical solution
## (as we have seen in the class on 'Barrier Options'). However, we do
## this here for illustration only (we could also price any other
## claim), and furthermore, it is interesting to compare the
## Monte-Carlo solution to the analytical solution that we know
## already.
##
## Keep in mind that in principle, we can now price *any* barrier
## option using the methods shown here in this R code.
##
##################################################################



## Define the simple claim, which in our case is just the stock price.
ST <- function(s) s

## Consistency check: The price of 'ST' as calculated via Monte Carlo
## simulation should approximately be equal to 's0'.
all.equal(price(s0, sigma, r, T, ST), s0)

## Define the set of stock prices to print. When printing below, we
## use 'sapply' to calculate the option price for each given stock
## price. For our case, you can think of 'sapply' being a fancy way of
## running a for-loop to calculate the option prices, and you don't
## have to worry about the details. (If you're interested, you can
## always type '?sapply' for more detailed information.)
s <- seq(from = L, to = L + 40, length.out = 50)

## Plot the price of the down-and-out contract. Note that when the
## stock price hits 'L', the option price should go to zero. At the
## same time, when the stock price is far away from 'L', the option
## price should approximate the price of the simple claim (to be
## plotted below for comparison).
plot(
    s,
    sapply(s, price.DO, sigma = sigma, r = r, T = T, phi = ST, L = L),
    col = "blue")

## For comparison, we add the plot of the simple contract's price. In
## our case, it should be a straight line, since the the price of the
## stock is just the stock price (i.e. a 45 degree line).
lines(
    s,
    sapply(s, price, sigma = sigma, r = r, T = T, phi = ST),
    col = "black")



##################################################################
## Here let's examine the European call when K<L. This is the opposite
## case of L<K that we have examined in closed form before.
K <- 30
L <- 50
s <- seq(from = K - 10, to = L + 30, length.out = 50)
C <- function(s) max(s - K, 0)
plot(
    s,
    sapply(s, price.DO, sigma = sigma, r = r, T = T, phi = C, L = L),
    col = "blue",
    ylab = 'Option Price',
    xlab = "Price of Underlying")
lines(
    s,
    sapply(s, price, sigma = sigma, r = r, T = T, phi = C),
    col = "black")



##################################################################
## You can even use a completely "crazy" payoff function such as the
## sine curve. It doesn't make much economic sense, but it shows that
## our machinery can even deal with this.
s <- seq(from = K - 10, to = L + 30, length.out = 2000) # Need finer granularity here for plotting.
plot(
    s,
    sapply(s, price.DO, sigma = sigma, r = r, T = T, phi = sin, L = L),
    type = 'l',
    col = "blue",
    ylab = 'Option Price',
    xlab = "Price of Underlying")
lines(
    s,
    sapply(s, price, sigma = sigma, r = r, T = T, phi = sin),
    col = "black")
