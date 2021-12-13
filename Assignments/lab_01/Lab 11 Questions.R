plot(
  power ~ effect_size, data = sim_effect_size,
  type = 'l', xlab = "Dispersion", ylab = "Statistical power",main = "Dispersion Line Plot")
abline(v = slope_obs, lty = 2, col = 'red')

contour(
  x = sim_3_dat$pop_sd,
  y = sim_3_dat$sample_size,
  z = sim_3_dat$power,
  xlab = "sample size",
  ylab = "population dispersion",
  main = "Contour Plot of population dspersion",
  levels = seq(0, 1, length.out = 9),
  drawlabels = TRUE,
  # method = "simple")
  method = "edge")


