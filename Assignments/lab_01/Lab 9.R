catrate = read.csv(here("data", "catrate.csv"))
head(catrate)

n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(n_success, n_years)

binom.test(n_success, n_years, p = 5/7) 

binom.test(
  n_success,
  n_years,
  p = 5/7,
  alternative='less')

veg = read.csv(here("data", "vegdata.csv"))
head(veg)


var.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

shapiro.test(veg$pine[veg$treatment=="control"])

shapiro.test(veg$pine[veg$treatment=="clipped"])

fligner.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

bartlett.test(pine ~ treatment, data=veg)

fligner.test(pine ~ treatment, data = veg)

t.test(pine~treatment,data=veg,subset=treatment %in% c('control','clipped'), conf.int=TRUE)

wilcox.test(pine~treatment,data=veg,subset=treatment %in% c('control','clipped'), conf.int=TRUE)

control = veg$pine[veg$treatment=='control']
clipped = veg$pine[veg$treatment=='clipped']

t.test(control, clipped, paired=TRUE)

wilcox.test(control, clipped, paired=TRUE)

disp = read.csv(here("data", "dispersal.csv"))
disp

plot(disp$disp.rate.ftb, disp$disp.rate.eb)

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs')

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')



plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)
plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)plot(
    ecdf(disp$disp.rate.ftb),
    verticals=TRUE)
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)

ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

prop.test(c(4,16),c(40,250))

owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq.test(owls)

fisher.test(owls)

birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(birds, hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
table(birdhab$s.edge, birdhab$BRCR > 0)

# set the presence to be in the first column
br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]

require(palmerpenguins)

fit_fl_sp = 
  lm(
    formula = flipper_length_mm ~ species,
    data = penguins)

fit_species = 
  lm(
    formula = body_mass_g ~ species,
    data = penguins)

fit_sex = 
  lm(
    formula = body_mass_g ~ sex,
    data = penguins)

fit_both = 
  lm(
    formula = body_mass_g ~ sex, species,
    data = penguins)


boxplot(
  rnorm(100) ~ rbinom(100, 1, 0.5), 
  main = "Plot of \n 100 Random Numbers", 
  names = c("Adelie\nfemale", "Gentoo\nmale"))
 
boxplot(
  rnorm(100) ~ rbinom(100, 1, 0.5), 
  main = "Plot of \n 100 Random Numbers", 
  names = c("Adelie\nfemale", "Chinstrap\nfemale"))

boxplot(
  rnorm(100) ~ rbinom(100, 1, 0.5), 
  main = "Plot of \n 100 Random Numbers", 
  names = c("Adelie\nmale", "Chinstrap\nmale"))

boxplot(
  rnorm(100) ~ rbinom(100, 1, 0.5), 
  main = "Plot of \n 100 Random Numbers", 
  names = c("Adelie\nmale"))

boxplot(
  rnorm(100) ~ rbinom(100, 1, 0.5), 
  main = "Plot of \n 100 Random Numbers", 
  names = c("Adelie\nfemale"))

boxplot(
  rnorm(100) ~ rbinom(100, 1, 0.5), 
  main = "Plot of \n 100 Random Numbers", 
  names = c("Chinstrap\nmale"))

boxplot(
  rnorm(100) ~ rbinom(100, 1, 0.5), 
  main = "Plot of \n 100 Random Numbers", 
  names = c("chinstrap\nfemale"))

boxplot(
  rnorm(100) ~ rbinom(100, 1, 0.5), 
  main = "Plot of \n 100 Random Numbers", 
  names = c("gentoo\nmale"))

boxplot(
  rnorm(100) ~ rbinom(100, 1, 0.5), 
  main = "Plot of \n 100 Random Numbers", 
  names = c("gentoo\nfemale"))

boxplot(body_mass_g ~ sex * species, data = penguins)

boxplot(body_mass_g ~ sex * species, data = penguins,las = 2,ylab = "body mass(g)", main = "Boxplot of penguins by sex and species",
      names = c("Adelie\nfemale", 
                "Adelie\nmale", 
                "Chinstrap\nfemale", 
                "Chinstrap\nmale", 
                "Gentoo\nfemale", 
                "Gentoo\nmale")  )

boxplot(body_mass_g ~ species, data = penguins, ylab = "body mass (g)", main = "Boxplot of penguins by species")

boxplot(body_mass_g ~ sex , data = penguins,xlab = "sex", ylab = "body mas (g)", main = "Boxplot of pengins by sex")

bartlett.test(body_mass_g ~ species, data=penguins)
bartlett.test(body_mass_g~ sex, data=penguins)
bartlett.test(body_mass_g~ , data=penguins)

dat_groups = aggregate(
  body_mass_g ~ species * sex,
  data = penguins,
  FUN = c)
str(dat_groups)

bartlett.test(dat_groups$body_mass_g, c(dat_groups$species, dat_groups$sex), data = penguins)
bartlett.test(dat_groups$body_mass_g)
