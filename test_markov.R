mat_power <- function(P,n){ if(n==0) return(diag(nrow(P))); result<-P; if(n==1) return(result); for(i in 2:n) result<-result%*%P; result }
P <- matrix(c(0.7,0.3, 0.2,0.8), nrow=2, byrow=TRUE)
cat("pi1:", c(1,0)%*%P, "\n")
cat("pi2:", c(1,0)%*%mat_power(P,2), "\n")
A <- t(P)-diag(2); A <- rbind(A, rep(1,2)); b <- c(0,0,1)
ss <- qr.solve(A,b); ss <- pmax(ss,0); ss <- ss/sum(ss)
cat("steady:", ss, "\n")

P_abs <- matrix(c(0.5,0.3,0.2, 0.2,0.5,0.3, 0,0,1), nrow=3, byrow=TRUE)
absorbing <- which(abs(diag(P_abs)-1)<1e-6)
nonabs <- setdiff(1:3, absorbing)
Q <- P_abs[nonabs, nonabs, drop=FALSE]
R <- P_abs[nonabs, absorbing, drop=FALSE]
N <- solve(diag(length(nonabs))-Q)
B <- N%*%R
t <- N%*%rep(1,length(nonabs))
cat("N:\n"); print(N)
cat("B:", B, "\n")
cat("t:", t, "\n")
