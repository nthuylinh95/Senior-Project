# Center of Mass: Discrete Case, One-dimensional

# Setup

# Mass values of m and M
m <- 1
M <- 10

# Creating a sample space
sample.space <- c(m,M)

# Probability of getting m
p <- 0.1

# Number of nodes
N <- 10^3

# Create a vector with positions
pos <- 0:N

# Simulation
# Use the sample() function to sample from a vector of masses m and M.

B <- 10^3-1  # Set number of times to repeat this process
result <- numeric(B)

# Creating data sets
for(i in 1:B)
{
  index <- sample(sample.space, size = N+1, replace = TRUE, prob = c(p,1-p))
  result[i] <- sum(((pos)*index))/sum(index)
}

#m <- mean(result)

# Standard deviation of the center of mass from simulations
s <- sd(result)

# Calculating approximations for standard deviation
sdd <- function(M,m,p,N){
  L <- function(pow,i,M,m,p){
    alpha <- (((m^i)*p)+(M^i)*(1-p))^pow
    return(alpha)
  }
  # Approximation a1
  sd.tay <- sqrt((1/12)*(N*(N+2)*(L(1,2,M,m,p)-L(2,1,M,m,p)))/(L(1,2,M,m,p)+N*L(2,1,M,m,p)))
  # Approximation a2
  sd.form <- sqrt((1/12)*((N*(N+2)*(L(1,2,M,m,p)-L(2,1,M,m,p)))/((N+1)*L(2,1,M,m,p))))
  paste("Taylor/Approx: sd(CM)=", signif(sd.tay), "Formula/Approx.: sd(CM)=", signif(sd.form))
}

# Calculating the exact value formula
k <- c(0:N+1)
var.totM <- ((N-1)*(3*N+2)/12) - ((N^2)/4) + sum(((choose(N+1,k)*(p^k)*(1-p)^(N+1-k))/(((k*m+(N+1-k)*M))^2))*((N+1)*(N+2)/12)*(k*(m^2)+(N+1-k)*(M^2)))
sd.totM <- sqrt(var.totM)

paste("sd(CM)=", signif(s))
#sdd(M,m,p,N)

paste("Proven Formula: sd(CM)=", signif(sd.totM))