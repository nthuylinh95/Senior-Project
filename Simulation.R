##

m <- 5
M <- 5*17^10

# Creating a sample space

sample.space <- c(m,M)

# Probability of getting m

p <- 0.6

# Number of trials

N <- 100

#values <- sample(sample.space, size = N, replace = TRUE, prob = c(p, 1 - p))
#values

# Create a vector with positions

pos <- 0:(N-1)
#pos

#CM <- sum(((pos)*values))/sum(values)
#CM

# Creating data sets

B <- 10^3-1  # set number of times to repeat this process
result <- numeric(B)

for(i in 1:B)
{
  index <- sample(sample.space, size = N, replace = TRUE, prob = c(p,1-p))
  result[i] <- sum(((pos)*index))/sum(index)
}

#result
#mean(result)
s <- sd(result)
s