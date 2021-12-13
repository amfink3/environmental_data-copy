require(here)
bird.sub.csv = read.csv(here("data", "bird.sub.csv"))
hab.sub.csv = read.csv(here("data" , "hab.sub.csv"))

birdhab = merge(bird.sub.csv, hab.sub.csv)

dim(birdhab)

plot(data = birdhab, BRCR ~ ls)

fit_1 = 
  lm(
    formula = BRCR ~ ls,
    data = birdhab)
abline(fit_1)

summary(fit_1)

linear = function(x,y_int,slope)
{
  y_int + slope *x
  
}
linear(x = 1, y_int = 1, slope = 1)
linear(x = 3:5, y_int = 1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 0.01)

linear_simulator = function(x,y_int,slope,st_dev)

{
  n = length(x)

 (y_int + slope * x) + rnorm(n,mean = 0,st_dev)  
}
Simnulator Function

n = 200

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x,
    linear_simulator(x, y_int = 1, slope = 4.5, st_dev = 0.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
  
  n = 400
  
  par(mfrow = c(2, 2))
  for (i in 1:4)
  {
    x = runif(n = n)
    plot(
      x, linear_simulator(x, y_int = 10, slope = -6.5, st_dev = 1.1),
      main = "", xlab = "x", ylab = "y",
      pch = 16, col = rgb(0, 0.2, 0, 0.2))
  }

  
  
  fit_1_coefs = coefficients(fit_1)
  str(fit_1_coefs)

  fit_1_summary = summary(fit_1)
  str(fit_1_summary)
  fit_1_summary$sigma
 summary(fit_1)
 
 int_obs = 0.0991039
 slope_obs = 0.0058405
 sd_obs = 0.1412668
 
 
 plot(
   x = birdhab$ls, 
   y = linear_simulator(
     x = birdhab$ls,
     y_int = int_obs,
     slope = slope_obs,
     st_dev = sd_obs
   ),
   main = "Simulated Data",
   xlab = "late-successional forest",
   ylab = "Brown Creeper Abundance")
 
 plot(
   birdhab$ls, birdhab$BRCR, 
   xlab = "late-successional forest extent",
   ylab = "Brown Creeper abundance",
   pch = 19)
 
 points(
   x = birdhab$ls, 
   y = linear_simulator(
     x = birdhab$ls,
     y_int = int_obs,
     slope = slope_obs,
     st_dev = sd_obs
   ),
   col = adjustcolor("red", alpha = 0.3),
   pch = 16)
 
 legend(
   "topleft",
   legend = c("data", "simulation"),
   pch = 16,
   col = c(1, adjustcolor("red", alpha = 0.3)))
 
 
 
 y_sim = linear_simulator(
   x = birdhab$ls,
   y_int = int_obs,
   slope = slope_obs,
   st_dev = sd_obs
 )
 
 fit_sim = lm(y_sim ~ birdhab$ls)
 summary(fit_sim)
 
 sum_1 = summary(fit_sim)
 sum_1$coefficients
 
sum_1$coefficients[2, 4]

Repeated Simulations
n_sims = 1000
p_vals = numeric(n_sims)
alpha = 0.05
for(i in 1:n_sims)
{
  y_sim = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  )
  fit_sim = lm(y_sim ~ birdhab$ls)
  
  p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
}
sum(p_vals < alpha) / n_sims

  Simlulation loop Function
linear_sim_fit = function(x, y, slope, y_int, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  fit_sim = lm(y_sim ~ x)
  return(fit_sim)
}
Simulating effect sizes
alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes_1 = seq(-.01, .01, length.out = n_effect_sizes)

effect_size_powers = numeric(n_effect_sizes)

for(j in 1:n_effect_sizes)
{
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = effect_sizes_1[j],
      st_dev = sd_obs
    )
    
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  effect_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_effect_size = 
  data.frame(
    power       = effect_size_powers,
    effect_size = effect_sizes_1)


plot(
  power ~ effect_size, data = sim_effect_size,
  type = 'l', xlab = 'Effect size', ylab = 'Power')
abline(v = slope_obs, lty = 2, col = 'red')

Simulating Sample Size
alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

sample_sizes = seq(5, 100)
sample_size_powers = numeric(length(sample_sizes))

for(j in 1:length(sample_sizes))
{
  x_vals = seq(0, 100, length.out = sample_sizes[j])
  
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sample_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_sample_size = 
  data.frame(
    power       = sample_size_powers,
    sample_size = sample_sizes)

plot(
  power ~ sample_size, data = sim_sample_size,
  type = 'l', xlab = 'Sample size', ylab = 'Power')
abline(v = nrow(birdhab), lty = 2, col = 'red')
 
Bivariate power analysis

alpha = 0.01
n_sims = 50

p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes = seq(-.01, .01, length.out = n_effect_sizes)

sample_sizes = seq(10, 50)

sim_output_2 = matrix(nrow = length(effect_sizes), ncol = length(sample_sizes))

for(k in 1:length(effect_sizes))
{
  effect_size = effect_sizes[k]
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, 100, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = effect_size,
        st_dev = sd_obs
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_2[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("computing effect size ", k," of ", length(effect_sizes)))
}

sim_n_effect_size = 
  list(
    power = sim_output_2,
    effect_size = effect_sizes,
    sample_size = sample_sizes
  )

image(
  sim_n_effect_size$power,
  xlab = "Effect size",
  ylab = "Sample Size",
  axes = FALSE)

# add x-axis labels
axis(
  1, 
  at = c(0, 0.5, 1), 
  labels = c(-.01, 0.0, .01))

# add y=axis labels
axis(
  2, 
  at = c(0, 1), 
  labels = c(sample_sizes[1], tail(sample_sizes, 1)))

contour(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "effect size",
  ylab = "sample size",
  main = "Contour Plot of Statistical Power",
  levels = seq(0, 1, length.out = 9),
  drawlabels = TRUE,
  # method = "simple")
  method = "edge")

persp(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

install.packages("rgl")
require(rgl)

persp3d(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')


save(
  sim_n_effect_size,
  file = here::here("data", "lab_11_n_effect_sizes.Rdata"))

load(file = here::here("data", "lab_11_n_effect_sizes.Rdata")))

require(here)


alpha = 0.05
n_sims = 100
p_vals = umeric(n_sims)

# What was the observed standard deviation?
sd_obs = 0.1412668

# specify the number of different standard deviation values to simulate:
n_sds = 20
pop_sds = seq(from = 0.01, to = 1.5, length.out = n_sds)

pop_sd_power = numeric(n_sds)

for(j in 1:length(pop_sds))
{
  pop_sd_j = ...
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(...)
    p_vals[i] = ...
  }
  pop_sd_power[j] = ...
}

sim_output_dispersion = data.frame(
  sd = ...,
  power = ...)

# You should save your simulation results so you don't have to run it every time.
save(
  sim_output_dispersion, 
  file = here("data", "lab_ll_dat_dispersion_sim.RData"))

# Line plot of standard deviation (x-axis) and statistical power (y-axis)
plot(...)

# Add a dotted vertical red line at the observed population standard deviation value.
abline(v = ...)

plot(
  power ~ dispersion, data = dispersion,
  type = 'l', xlab = 'dispersion', ylab = 'Power')
abline(v = nrow(birdhab), lty = 2, col = 'red')

#Q1


alpha = 0.05
n_sims = 200
p_vals = numeric(n_sims)
sd_obs
n_sds= 20
pop_sds=seq(from = 0.01, to = 1.5, length.out = n_sds)
pop_sd_power = numeric(n_sds)
sample_sizes = seq(5, 100)
sim_output_3 = matrix(0, nrow = n_sds, ncol = length(sample_sizes))

for(k in 1:length(pop_sds))
{
  pop_sd_k = pop_sds[k]
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, 100, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = effect_size,
        st_dev = pop_sd_k
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_3[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("computing blah ", k," of ", n_sds))
}

plot(
  power ~ sample_size, data = sim_sample_size,
  type = 'l', xlab = "Dispersion", ylab = "Statistical power",main = "Dispersion Line Plot")
abline(v = nrow(birdhab), lty = 2, col = 'red')

sim_DATA_3 = 
  list(
    power = sim_output_3,
    pop_sd = pop_sds,
    sample_size = sample_sizes
  )

#Q3
contour(
  x = sim_DATA_3$pop_sd,
  y = sim_DATA_3$sample_size,
  z = sim_DATA_3$power,
  xlab = "sample size",
  ylab = "population dispersion",
  main = "Contour Plot of population dspersion",
  levels = seq(0, 1, length.out = 9),
  drawlabels = TRUE,
  # method = "simple")
  method = "edge")

image(sim_DATA_3)

image(
  sim_DATA_3$power,
  xlab = "Effect size",
  ylab = "Sample Size",
  axes = FALSE)

persp(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'red',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

      