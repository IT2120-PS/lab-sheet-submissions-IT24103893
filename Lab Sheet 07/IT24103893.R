setwd("C://Users//New//OneDrive//Desktop//IT24103976")
##Q1

prob_q1 <- (25 - 10) / (40 - 0)
cat("Q1: Probability train arrives between 8:10 and 8:25 =", prob_q1, "\n")

# Q2

lambda <- 1/3
prob_q2 <- pexp(2, rate=lambda)
cat("Q2: Probability update takes at most 2 hours =", prob_q2, "\n")

# Q3

mean_iq <- 100
sd_iq <- 15

prob_q3_i <- 1 - pnorm(130, mean=mean_iq, sd=sd_iq)
cat("Q3(i): Probability IQ > 130 =", prob_q3_i, "\n")

iq_95 <- qnorm(0.95, mean=mean_iq, sd=sd_iq)
cat("Q3(ii): 95th percentile IQ score =", iq_95, "\n")


