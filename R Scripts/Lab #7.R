# Create simulated data
dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)

apply(dat, MARGIN = 1, FUN = min)
apply(dat, MARGIN = 1, FUN = max)

moths = read.csv(here("data", "moths.csv"))
head(moths)

m = 10000

# numeric() creates an vector of length m with all values initialized to zero
result = numeric(m)
head(result)
m = 10000

# numeric() creates an vector of length m with all values initialized to zero
result = numeric(m)
head(result)
mean(result)
quantile(result,c(0.025,0.975))

install.packages("boot")
require(boot)
boot(data, statistic, R)
boot_mean = function(x, i)
  

{
  return(mean(x[i], na.rm = TRUE))
  myboot = 
    boot(
      data = moths$anst,
      statistic = boot_mean,
      R = 10000)
  print(myboot)
  
  str(myboot)
  
  mean(moths$anst)
  myboot$t0
  mean(myboot$t) - myboot$t0
  sd(myboot$t)
  
  quantile(
    myboot$t,
    c(0.025, 0.975))
  
}

moth_dat = moths[,-1]
head(moth_dat)

n = nrow(moth_dat) #number of rows or sample observations
m = 100 #number of bootstrap iterations
moth_result = matrix(
  nrow = m,
  ncol = n)
data[sample(...), ]

n = nrow(moth_dat) #number of rows or sample observations

m = 100 #number of bootstrap iterations

moth_result = matrix(
  nrow = m,
  ncol = n)


# The outer loop: runs once for each bootstrap iteration.  index variable is i
for(i in 1:m)
{
  # The inner loop: simulates increasing sampling intensity
  # Sampling intensity ranges from 1 site to the complete count of sites (24)
  # index variable is j
  for(j in 1:n)
  {
    # sample the input data row indices, with replacement
    rows_j = sample(n, size = j, replace=TRUE)
    
    # Creates a new data matrix from the resampled rows.
    t1 = moth_dat[rows_j, ]
    
    # Calculates the column sums of the new data matrix.
    t2 = apply(t1, 2, sum)
    
    # Counts the number of columns in which any moths were observed
    moth_result[i, j] = sum(t2 > 0)
  }
}

head(moth_result)

rarefaction_sampler = function(input_dat, n_iterations)
{
  n = nrow(moth_dat) #number of rows or sample observations
  m = 100 #number of bootstrap iterations
  
  moth_result = matrix(
    nrow = m,
    ncol = n)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:m)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of sites (24)
    # index variable is j
    for(j in 1:n)
    {
      
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = moth_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      moth_result[i, j] = sum(t2 > 0)
    }
  }
  
  return(moth_result)
}

# This clears the current R session's environment
rm(list = ls())

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    for(j in 1:n_input_rows)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n_input_rows, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

# rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

require(here)
moths = read.csv(here("data", "moths.csv"))

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)
rarefact

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness',
  main='Rarefaction Curve')

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))

require(palmerpenguins)
View(penguins)

gentoos <- subset(penguins, penguins$species == "Gentoo")
gentoos <- subset(gentoos, gentoos$bill_length_mm != "NA")
length(gentoos$bill_length_mm)

sd(gentoos$bill_length_mm)

df <-length(gentoos$bill_length_mm) - 1
qt(0.05, df, lower.tail = FALSE)

sse_mean = function(x){
  x <- na.omit(x)
  stdev <- sd(x, na.rm = TRUE)
  sqrtn <- sqrt(length(x))
  SSE <- stdev/sqrtn
  return(SSE)
  }
sse_mean(gentoos$bill_length_mm)

xbar <- mean(gentoos$bill_length_mm)
gen2sse <- sse_mean(gentoos$bill_length_mm)
tcrit <-qt(0.05/2, df, lower.tail = FALSE)
CI
CI <- c(xbar + gen2sse*tcrit, xbar - gen2sse*tcrit)

require(boot)

boot_mean <- function(x,i)
{
return(mean(X[i],na.rm  = TRUE))
}

mean(result)

quantile(result,c(0.025,0.975))

matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness',
  main='Rarefaction Curve')

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))