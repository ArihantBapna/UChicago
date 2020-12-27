gh$ghs <- 0
vector <- c()

for(i in 1:10000){
  print(i)
  for(i in 1:nrow(gh)){
    loc <- gh[i,]
    den <- runif(1,loc$LowerLimit,loc$UpperLimit)
    loc$ghs <- (den * 1000000) * loc$`Cropland in mill. Acres`
    gh[i,] <- loc
  }
  
  vector <- c(vector,sum(gh$ghs) / (1000000 * sum(gh$`Cropland in mill. Acres`)))
}
confidence_interval(vector,0.99)
vector

confidence_interval <- function(vector, interval) {
  # Standard deviation of sample
  vec_sd <- sd(vector)
  # Sample size
  n <- length(vector)
  # Mean of sample
  vec_mean <- mean(vector)
  # Error according to t distribution
  error <- qt((interval + 1)/2, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  result <- c("lower" = vec_mean - error, "upper" = vec_mean + error)
  return(result)
}
vec_sd <- sd(vector)
n <- length(vector)
vec_mean <- mean(vector)
error <- qt((0.99 + 1)/2, df = n - 1) * vec_sd / sqrt(n)
print(vec_mean)
print(error)
