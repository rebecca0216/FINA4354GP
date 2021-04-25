## Stock payoff function.
ST <- function(s) s

## Stock price function.
F_ST <- function(t, s) s

############################################################


## Cash-or-nothing call payoff function.
H <- function(s) if (s > L) 1 else 0

## Cash-or-nothing call price function.
F_H <- function(t, s) {
  d2 <- (log(s/L)+(r-(1/2)*sigma^2)*(T-t)) / (sigma*sqrt(T-t))
  exp(-r * (T - t)) * pnorm(d2)
}


############################################################


## European call payoff function.
C <- function(s) max(s - K, 0)

## European call price function (i.e. Black-Scholes formula).
F_C <- function(t, s, K) {
  d1 <- (log(s/K)+(r+(1/2)*sigma^2)*(T-t)) / (sigma*sqrt(T-t))
  d2 <- d1 - sigma * sqrt(T - t)
  pnorm(d1) * s - pnorm(d2) * K * exp(-r * (T - t))
}


############################################################

## Price of down-and-out contract on European call.
F_C_DO <- function(t, s, K, L) {
  if (s <= L) return(0)
  const <- (L / s)^(2 * (r - (1/2) * sigma^2) / sigma^2)
  if (L < K) return(F_C(t, s, K) - const * F_C(t, L^2 / s, K))
  price <- F_C(t, s, L) + (L - K) * F_H(t, s)
  price - const*(F_C(t, L^2 / s, L)+(L-K)*F_H(t, L^2 / s))
}

## Price of down-and-out contract on European call.
F_C_DI <- function(t, s, K, L) {
  price <- F_C(t, s, K) - F_C_DO(t, s, K, L)
}


############################################################


## Variables about the economy.
r <- 0.01
sigma <- 0.3

## Variables about the option.
K <- 40
L <- 30
T <- 1

## Define stock prices for plotting.
s <- seq(from = L, to = K + 10, length.out = 50)


############################################################


## Plot down-and-out contract on call and visually compare 
## it with the contract that does *not* have a barrier.
price_DO <- sapply(s, F_C_DO, t = 0, K = K, L = L)
price_DI <- sapply(s, F_C_DI, t = 0, K = K, L = L)
price <- sapply(s, F_C, t = 0, K = K)
plot(s, price_DO, main="Comparison", ylab = "price", type = "l", col = "blue")
lines(s, price_DI, col = "red")
lines(s, price, col = "black")
legend("topleft", c("DO call","DI call","vallina call"), fill=c("blue","red", "black"))

############################################################

