# Test Risk app logic with sample cases

validate_probs <- function(p, name = "概率") {
  p <- suppressWarnings(as.numeric(p))
  if (any(is.na(p))) return(list(valid = FALSE, msg = "NA", p = p))
  if (any(p < 0)) p <- pmax(p, 0)
  s <- sum(p)
  if (s == 0) return(list(valid = FALSE, msg = "sum 0", p = p))
  if (abs(s - 1) > 1e-6) p <- p / s
  list(valid = TRUE, msg = NULL, p = p)
}

# Benefit sample
prior <- c(0.2, 0.5, 0.3)
payoff <- matrix(c(100, 40, -20,
                   60, 60, 60,
                   20, 100, 80), nrow = 3, byrow = TRUE)
n_a <- nrow(payoff); n_s <- ncol(payoff)
prior <- validate_probs(prior)$p
prior_mat <- matrix(prior, nrow = n_a, ncol = n_s, byrow = TRUE)
expected <- rowSums(payoff * prior_mat)
cat("Benefit EMV:", expected, "best:", which.max(expected), "\n")
perfect <- sum(prior * apply(payoff, 2, max))
evpi <- perfect - max(expected)
cat("EVPI:", evpi, "\n")
regret <- sweep(matrix(apply(payoff, 2, max), nrow = n_a, ncol = n_s, byrow = TRUE), 1:2, payoff, "-")
eol <- rowSums(regret * prior_mat)
cat("EOL:", eol, "min EOL:", min(eol), "\n")

# Cost sample
payoff_c <- matrix(c(20, 50, 80,
                     40, 40, 40,
                     80, 30, 20), nrow = 3, byrow = TRUE)
expected_c <- rowSums(payoff_c * prior_mat)
cat("\nCost EC:", expected_c, "best:", which.min(expected_c), "\n")
perfect_c <- sum(prior * apply(payoff_c, 2, min))
evpi_c <- min(expected_c) - perfect_c
cat("EVPI cost:", evpi_c, "\n")
regret_c <- sweep(payoff_c, 2, apply(payoff_c, 2, min), "-")
eol_c <- rowSums(regret_c * prior_mat)
cat("EOL cost:", eol_c, "min EOL:", min(eol_c), "\n")
