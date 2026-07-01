# Test scripts for the 5 apps

library(ggplot2)

# ===== NPV test =====
npv <- function(flows, rate) sum(flows / (1 + rate)^(seq_along(flows) - 1))
A <- c(-250, 100, 100, 100, 50, 50, 50)
B <- c(-100, 30, 30, 60, 60, 60, 60)
cat("NPV A @10%:", npv(A, 0.10), "\n")
cat("NPV B @10%:", npv(B, 0.10), "\n")

# ===== Risk test =====
# Benefit
prior <- c(0.2, 0.5, 0.3)
payoff_b <- matrix(c(100, 40, -20,
                     60, 60, 60,
                     20, 100, 80), nrow=3, byrow=TRUE)
emv_b <- rowSums(payoff_b * matrix(prior, nrow=3, ncol=3, byrow=TRUE))
cat("\nBenefit EMV:", emv_b, "; best:", which.max(emv_b), "\n")
evpi_b <- sum(prior * apply(payoff_b, 2, max)) - max(emv_b)
cat("EVPI benefit:", evpi_b, "\n")
regret_b <- apply(payoff_b, 2, max) - payoff_b
eol_b <- rowSums(regret_b * matrix(prior, nrow=3, ncol=3, byrow=TRUE))
cat("EOL benefit:", eol_b, "; min EOL:", min(eol_b), "\n")

# Cost
payoff_c <- matrix(c(20, 50, 80,
                     40, 40, 40,
                     80, 30, 20), nrow=3, byrow=TRUE)
ec_c <- rowSums(payoff_c * matrix(prior, nrow=3, ncol=3, byrow=TRUE))
cat("\nCost EC:", ec_c, "; best:", which.min(ec_c), "\n")
evpi_c <- min(ec_c) - sum(prior * apply(payoff_c, 2, min))
cat("EVPI cost:", evpi_c, "\n")
regret_c <- payoff_c - apply(payoff_c, 2, min)
eol_c <- rowSums(regret_c * matrix(prior, nrow=3, ncol=3, byrow=TRUE))
cat("EOL cost:", eol_c, "; min EOL:", min(eol_c), "\n")

# ===== Uncertainty test =====
# Benefit
payoff_u_b <- matrix(c(100, 40, -20,
                       60, 60, 60,
                       20, 100, 80), nrow=3, byrow=TRUE)
alpha <- 0.6
cat("\nUncertainty benefit:")
cat("Maximax:", apply(payoff_u_b, 1, max), "\n")
cat("Maximin:", apply(payoff_u_b, 1, min), "\n")
cat("Laplace:", rowMeans(payoff_u_b), "\n")
cat("Hurwicz:", alpha*apply(payoff_u_b,1,max)+(1-alpha)*apply(payoff_u_b,1,min), "\n")
regret_u_b <- apply(payoff_u_b, 2, max) - payoff_u_b
cat("Minimax regret:", apply(regret_u_b,1,max), "\n")

# Cost
payoff_u_c <- matrix(c(20, 50, 80,
                       40, 40, 40,
                       80, 30, 20), nrow=3, byrow=TRUE)
cat("\nUncertainty cost:")
cat("Minimin:", apply(payoff_u_c, 1, min), "\n")
cat("Minimax:", apply(payoff_u_c, 1, max), "\n")
cat("Laplace:", rowMeans(payoff_u_c), "\n")
cat("Hurwicz cost:", alpha*apply(payoff_u_c,1,min)+(1-alpha)*apply(payoff_u_c,1,max), "\n")
regret_u_c <- payoff_u_c - apply(payoff_u_c, 2, min)
cat("Minimax regret:", apply(regret_u_c,1,max), "\n")

# ===== AHP test =====
ri_table <- c("1"=0,"2"=0,"3"=0.58,"4"=0.90,"5"=1.12,"6"=1.24,"7"=1.32,"8"=1.41,"9"=1.45)
ahp_weights <- function(mat){
  n <- nrow(mat)
  gm <- apply(mat,1,function(x) prod(x)^(1/length(x)))
  w <- gm/sum(gm)
  aw <- mat %*% w
  lambda <- mean(aw/w)
  ci <- (lambda-n)/(n-1)
  ri <- ri_table[as.character(n)]
  cr <- if(is.na(ri)||ri==0) 0 else ci/ri
  list(weights=w,lambda=lambda,ci=ci,cr=cr)
}
M1 <- matrix(c(1,2,2, 0.5,1,1, 0.5,1,1), nrow=3, byrow=TRUE)
r1 <- ahp_weights(M1)
cat("\nAHP consistent:\n")
print(r1)
M2 <- matrix(c(1,5,9, 1/5,1,7, 1/9,1/7,1), nrow=3, byrow=TRUE)
r2 <- ahp_weights(M2)
cat("\nAHP inconsistent:\n")
print(r2)
V <- matrix(c(0.6,0.2,0.2,
              0.3,0.5,0.2,
              0.1,0.3,0.6), nrow=3, byrow=TRUE)
W <- c(0.5,0.25,0.25)
cat("\nTotal scores:", V %*% W, "\n")

# ===== Markov test =====
mat_power <- function(P,n){ if(n==0) return(diag(nrow(P))); result<-P; if(n==1) return(result); for(i in 2:n) result<-result%*%P; result }
P <- matrix(c(0.7,0.3, 0.2,0.8), nrow=2, byrow=TRUE)
cat("\nMarkov pi1:", c(1,0) %*% P, "\n")
cat("Markov pi2:", c(1,0) %*% mat_power(P,2), "\n")
A <- t(P)-diag(2); A <- rbind(A, rep(1,2)); b <- c(0,0,1)
ss <- qr.solve(A,b); ss <- pmax(ss,0); ss <- ss/sum(ss)
cat("Steady state:", ss, "\n")

# Absorption
P_abs <- matrix(c(0.5,0.3,0.2, 0.2,0.5,0.3, 0,0,1), nrow=3, byrow=TRUE)
absorbing <- which(abs(diag(P_abs)-1)<1e-6 & abs(rowSums(P_abs)-1)<1e-6)
cat("Absorbing states:", absorbing, "\n")
nonabs <- setdiff(1:3, absorbing)
Q <- P_abs[nonabs, nonabs, drop=FALSE]
R <- P_abs[nonabs, absorbing, drop=FALSE]
N <- solve(diag(length(nonabs))-Q)
B <- N %*% R
t <- N %*% rep(1, length(nonabs))
cat("N:\n"); print(N)
cat("B:", B, "\n")
cat("t:", t, "\n")
