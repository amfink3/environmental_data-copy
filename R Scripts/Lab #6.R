require(palmerpenguins)
require(here)



sse_mean = function(x) 
  { na.x = is.na(x)
  x2 = x[!na.x]
  sd.x2 = sd(x2)
  n = length(x2)
    sd.x2/(sqrt(n))}

sse_mean(penguins$bill_depth_mm)

boxplot(flipper_length_mm ~ species, data = penguins)
dat_pen = subset(penguins, species != "Gentoo")
boxplot(flipper_length_mm ~ species, data = dat_pen)
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(flipper_length_mm ~ species, data = dat_pen)
}

# for reproducibility
set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1, 2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")

t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
# Reset the random number generator state for reproduceablility
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)

boxplot(flipper_shuffled ~ dat_pen$species)

t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1

t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test

t_test$estimate

diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

table(dat_pen$species)
n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

print(c(observed = diff_observed, simulated = diff_simulated))

x = dat_pen$flipper_length_mm
n_1 = 68
n_2 = 152

dat_1 = sample(x, n_1, replace = TRUE)
dat_2 = sample(x, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
# My Function
two_group_resample=function(x, n_1, n_2)
{
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  diff_simulated = 
    mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  return(diff_simulated)
  
}

two_group_resample(penguins$bill_length_mm, 68, 152)

n = 200
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)

sum(abs(mean_differences) >= diff_observed)

t_test = t.test(flipper_shuffled ~ dat_pen$species)

str(t_test)

t_test$estimate

sse_mean = function(x) 

  rm(list = ls())

sse_mean = function(x) 
{ na.x = is.na(x)
x2 = x[!na.x]
sd.x2 = sd(x2)
n = length(x2)
sd.x2/(sqrt(n))}

sse_mean(penguins$body_mass_g)

dat_pen = droplevels(subset(penguins, species != "Gentoo"))
n = 200
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)
sse_mean(mtcars$mpg)

set.seed(54321)
two_group_resample(dat_pen$flipper_length_mm, 68, 152)

n = 200
mean_differences = c()

ab
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)

hm = (mean_differences > 5.8)
abs()


boxplot(penguins$bill_depth_mm)

dat_pen = subset(penguins, species != "Gentoo")
boxplot(bill_depth_mm ~ species, data = dat_pen, col = "blue")

agg_means = aggregate(
  bill_depth_mm ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

t_test = t.test(dat_pen$bill-depth_mm ~ dat_pen$species)
t_test

t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
# Reset the random number generator state for reproduceablility
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)

t.test(dat_pen$bill_depth_mm ~ dat_pen$species)

 diff_crit
 mean_differences >diff_crit

 
