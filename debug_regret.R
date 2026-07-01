payoff <- matrix(c(100,40,-20,60,60,60,20,100,80), nrow=3, byrow=TRUE)
print(payoff)
best <- apply(payoff, 2, max)
print(best)
regret <- best - payoff
print(regret)
