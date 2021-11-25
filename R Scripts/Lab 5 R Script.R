ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")


pdf(file = here("images" , "Ricker Function"))
exp_fun = function(x, a, b)
{
    return(a * exp(-b * x))
}

curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")

error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")

error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)

par(mfrow = c(1, 2))
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_2, main = "Normally Distributed Errors\n Increasing Variance", xlab = "", ylab = "")

par(mfrow = c(3, 1))
plot(x_sim, y_observed)
plot(x_sim, y_observed_2)
plot(x_sim, y_observed_3)

par(mfrow = c(3, 1))
hist(y_observed - y_pred, main = "sim data 1", xlab = "observed y=values")
hist(y_observed_2 - y_pred, main = "sim data 2", xlab = "observed y=values")
hist(y_observed_3 - y_pred, main = "sim data 3", xlab = "observed y=values")



curve(exp_fun(x, 1.9, 0.1), add = FALSE, from = 0, to = 50,
      ann = FALSE, axes = TRUE, ylab = "f(x)", col=1, lty = 1)

curve(exp_fun(x, 1.9, 0.3), add = TRUE, from = 0, to = 50, ylab = "f(x)",
      col=1, lty = 2)

curve(exp_fun(x, 1.2, 0.2), add = TRUE, from = 0, to = 50, ylab = "f(x)",
      col=2, lty = 1)

curve(exp_fun(x, 1.2, 0.4), add = TRUE, from = 0, to = 50, ylab = "f(x)",
      col=2, lty = 2)


curve(
  ricker_fun(x, 25, 0.1),
  from = 0, to = 10, add = FALSE,
  main = "Single plot 6 Ricker curves",
  ylab = "f(x)", xlab = "x",
  col=1,
  lty=1)

curve(
  ricker_fun(x, 20, 0.2),
  from = 0, to = 10, add = TRUE,
  main = "Single plot 6 Ricker curves",
  ylab = "f(x)", xlab = "x",
  col=1,
  lty=2)

curve(
  ricker_fun(x, 10, 0.2),
  from = 0, to = 10, add = TRUE,
  main = "Single plot 6 Ricker curves",
  ylab = "f(x)", xlab = "x",
  col=1,
  lty=2)

curve(
  ricker_fun(x, 75, 0.3),
  from = 0, to = 10, add = TRUE,
  main = "Single plot 6 Ricker curves",
  ylab = "f(x)", xlab = "x",
  col=2,
  lty=1)

curve(
  ricker_fun(x, 50, 0.3),
  from = 0, to = 10, add = TRUE,
  main = "Single plot 6 Ricker curves",
  ylab = "f(x)", xlab = "x",
  col=2,
  lty=2)

curve(
  ricker_fun(x, 40, 0.3),
  from = 0, to = 10, add = TRUE,
  main = "Single plot 6 Ricker curves",
  ylab = "f(x)", xlab = "x",
  col=2,
  lty=2)



  line_point_slope(x, 75, 0.3),
  from = 0, to = 10, add = TRUE,
  main = "Single plot 6 Ricker curves",
  ylab = "f(x)", xlab = "x",
  col=2,
  lty=1)

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept =
    function(x1, y1, slope)
      return(-(x1 * slope) + y1)
  
  
  linear =
    function(x, yint, slope)
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, vy1, slope), slope))
}


