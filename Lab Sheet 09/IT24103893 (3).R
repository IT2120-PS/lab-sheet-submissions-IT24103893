setwd("C:\\Users\\OneDrive\\Desktop\\PS Lab 9")

set.seed(123)

mu <- 45     
sigma <- 2   
n <- 25      

sample_data <- rnorm(n, mean = mu, sd = sigma)
sample_data

t_test_result <- t.test(sample_data, mu = 46, alternative = "less")
t_test_result

if(t_test_result$p.value < 0.05){
  print("Reject H0: The average baking time is less than 46 minutes.")
} else {
  print("Fail to reject H0: Not enough evidence to say average baking time is less than 46 minutes.")
}
