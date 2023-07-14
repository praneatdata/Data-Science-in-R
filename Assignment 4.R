#Question 1
n <- 1000
m <- 1000
A <- matrix(runif(n*m), nrow = n, ncol = m)
#A <- as.numeric(A)
column_norms_l <- numeric(length = 1000)
system.time({
for (i in 1:1000) {
  column_norms_l[i] <- norm(A[, i], type = "2")
}
})
system.time({
column_norms_s <- sapply(A, function (x) norm)
})
#Loop runned faster
#Question 2
n <- 1000
m <- 1000

A <- matrix(runif(n*m), nrow = n, ncol = m)
B <- matrix(rnorm(n*m), nrow = n, ncol = m)
x <- runif(m)

ABtx <- crossprod(A, B) %*% x

#Question 3
load("ques3.Rdata")
det <- prod(eigen(mat)$values)
trace <- sum(eigen(mat)$values)
p=10
ans <- (det^(1/p)) * factorial(p) * (2.7^p) / (p * p * trace)

#Question 4
n <- 50
m <- 1e3
A <- matrix(runif(n*m), nrow = n, ncol = m)
# p_vec will store p_i eventually
p_vec <- numeric(length = m)
# running a loop for each column
# to find the norm (the numerator)
#for(k in 1:m)
#{
  #p_vec[k] <- norm(A[ ,k], type = "2")
#}
p_vec <- sapply(seq_len(ncol(A)), function(i) norm(A[, i], type = "2"))
# divide by the sum
p_vec <- p_vec/sum(p_vec)
# choosing column
chosen <- sample(1:m, size = 1, prob = p_vec)

#Question 5
# n = integer
# rho = number in (-1,1)
autoreg <- function(n, rho)
{
  out <- 0
  for(t in 2:n)
  {
    error <- rnorm(1)
    error <- rho*out[t-1] + error
    out <- c(out, error)
  }
  return(out)
}

#This function generates a sequence of n random numbers, rho determines the range of random values, if rho =0 than randomness would not be there.
