install.packages("psych")
require(psych)
pairs.panels(iris)

require(here)
dat_bird = read.csv(
  here("data", "bird.sta.csv"))
head(dat_bird)

dat_habitat = read.csv(
  here("data", "hab.sta.csv"))
head(dat_habit)

dat_all = merge(dat_habitat, dat_bird)
plot(ba.tot ~ elev, data = dat_all)

sample(dat_all$CEWA, 100)
my_vec = rep(1:3, 5)
my_vec == 3
my_vec > 1
as.numeric(my_vec > 1)
as.numeric(cewa_present_absent.)

sample(dat_all$CEWA, 100)
cewa_vec = dat_all$CEWA >0
cewa_present_absent = as.numeric(cewa_vec >0)
plot(x = dat_all$elev, y = cewa_present_absent)

# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)

pairs.panels(dat_all[ , c("elev", "slope", "aspect", "ba.tot")])

sample(dat_all$AMGO, 100)
amgo_vec = dat_all$AMGO >0
amgo_present_absent = as.numeric(amgo_vec >0)
plot(x = dat_all$ba.tot, y = amgo_present_absent, xlab = "Basal Area", ylab = "amgo present absent", main = "AMGO Logistic Function Plot Present/Absence", col = "green")

plot(x = dat_all$bat.tot, y = amgo_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

sample(dat_all$BHCO, 100)
bhco_vec = dat_all$BHCO >0
bhco_present_absent = as.numeric(bhco_vec >0)
plot(x = dat_all$ba.tot, y = bhco_present_absent, xlab = "Basal Area", ylab = "bhco present absent", main = "BHCO Logistic Function Plot Present/Absence", col = "blue")

plot(x = dat_all$bat.tot, y = amgo_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

total_GRJA = dat_all$GRJA
sum(total_GRJA)

as.numeric(cewa_vec >0)
sum(as.numeric)

grja_vec = dat_all$GRJA >0
grja_present_absent = as.numeric(grja_vec >0)
sum(grja_present_absent)
