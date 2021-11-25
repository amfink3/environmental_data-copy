require("here")
catrate = read.csv(here("catrate.csv"))
head(catrate)                       
summary(catrate)
hist(catrate$cat.rate, xlab = "catastrophe rate", main = "Histogram of Slamander Catastrophe Rate", col = "red")

shapiro.test(catrate$cat.rate)

t.test(catrate$cat.rate, mu = 2/7)

wilcox.test(catrate$cat.rate, mu = 2/7)

summary(penguin_dat)
boxplot(
  flipper_length_mm ~ species, 
  data = penguin_dat,
  ylab = "Flipper Length (mm)")

# Extract the Adelie penguin data
dat_adelie = subset(penguin_dat, species == "Adelie")
shapiro.test(flipper_length_mm$Adelie)

levels(penguin_dat$species)

hist(dat_adelie$flipper_length_mm, xlab =  "flipper length in mm", main = "Historgam of differnce in penguin flipper length", col = "purple")
hist(dat_chin$flipper_length_mm, xlab =  "flipper length in mm", main = "Historgam of differnce in penguin flipper length", col = "purple")

t.test(dat_adelie$flipper_length_mm, dat_chin$flipper_length_mm)
