# Generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)

require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main = "Histogram of Penguin Body Mass",
  xlab = "Body Mass (g)")

mean(penguins$body_mass_g, na.rm = TRUE)
sd(penguins$body_mass_g, na.rm = TRUE)
nrow(penguins)

mean(penguins$body_mass_g, na.rm = FALSE)
sd(penguins$body_mass_g, na.rm =FALSE)
nrow(penguins)

dat_1 = rnorm(n = 344, mean = 4202, sd = 802)
dat_2 = rnorm(n = 344, mean = 4202, sd = 802)
dat_3 = rnorm(n = 344, mean = 4202, sd = 802)
dat_4 = rnorm(n = 344, mean = 4202, sd = 802)

par(mfrow = c(2, 2))

hist(dat_1) 
hist(dat_2)
hist(dat_3)
hist(dat_4)

set.seed(12)
dat_unif = runif(n = 27, min = 0, max = 4)
hist(dat_unif)

set.seed(1)
dat_unif_1 = runif(n = 270, min = 0, max = 4)
set.seed(1)
dat_unif_2 = runif(n = 270, min = 0, max = 4)

par(mfrow = c(1, 2))
hist(dat_unif_1)
hist(dat_unif_2)

set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

guess_x = 6
guess_y = 0
guess_slope = 0.1

plot(y_observed ~ x, data = dat, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

line_point_slope(dat$x, guess_x, guess_y, guess_slope)

sum(dat$resids)

norm_17 = rnorm(n = 17, mean = 10.4, sd = 2.4 )
norm_30 = rnorm(n = 30 , mean = 10.4 , sd = 2.4)
norm_300 = rnorm(n = 300 , mean = 10.4 , sd = 2.4)
norm_3000 = rnorm(n = 3000 , mean = 10.4 , sd = 2.4)

mean = pop_mean = 10.4
 sd= pop_sd = 2.4
 
 norm_17 = rnorm(n = 17 , mean = pop_mean , sd = pop_sd)
 norm_30 = rnorm(n = 30 , mean = pop_mean ,sd = pop_sd)
 norm_300 = rnorm(n = 300 , mean = pop_mean , sd = pop_sd)
norm_3000 = rnorm(n = 3000 , mean = pop_mean ,sd =  pop_sd)

par(mfrow = c(1, 2))
hist(dat_unif_1)
hist(dat_unif_2)

par(mfrow = c(2,2) )
  hist(norm_17 ,xlab = "random data"  , ylab = "frequency" , main  = "histogram norm 17" ,col = "red")
  hist(norm_30 ,xlab = "random data"  , ylab = "frequency" , main  = "histogram norm 30" ,col = "blue")
  hist(norm_300 ,xlab = "random data"  , ylab = "frequency" , main  = "histogram norm 300" ,col = "green")
  hist(norm_3000 ,xlab = "random data"  , ylab = "frequency" , main  = "histogram norm 3000" ,col = "yellow")
  dev.off()
        
       
           
        
        # Generate a vector of x-values
        x = seq(-6, 6, length.out = 1000)
        y = dnorm(x)
        
        plot(x, y, main = " Normal PDF: mean=10.4 sd=2.4", type = "l", xlim = c(-3, 3))
        abline(h = 0)

        # Generate a vector of x-values
        x = seq(4, 17, length.out = 1000)
        y = dnorm(x,mean = 10.4, sd = 2.4)
        
        plot(x, y, main = " Normal PDF: mean=10.4 sd=2.4", type = "l", xlim = c(4,17))
        abline(h = 0)
        
        image_file  = "norm histogram.png"
        require(here)
        
        {
          png(filename = 
            here("Images" , "lab_04_hist_01.png"),
            width = 1500, height = 1600)
          par(mfrow = c(2,2) )
          hist(norm_17 ,xlab = "random data"  , ylab = "frequency" , main  = "histogram norm 17" ,col = "red")
          hist(norm_30 ,xlab = "random data"  , ylab = "frequency" , main  = "histogram norm 30" ,col = "blue")
          hist(norm_300 ,xlab = "random data"  , ylab = "frequency" , main  = "histogram norm 300" ,col = "green")
          hist(norm_3000 ,xlab = "random data"  , ylab = "frequency" , main  = "histogram norm 3000" ,col = "yellow")
          dev.off()
        
        }
        
       pdf(file = here("images" , "norm_1.pdf"))
        
        x = seq(4, 17, length.out = 1000)
        y = dnorm(x,mean = 10.4, sd = 2.4)
        
        plot(x, y, main = " Normal PDF: mean=10.4 sd=2.4", type = "l", xlim = c(4,17))
        abline(h = 0)
        
        n_pts = 3
        x_min = 15
        x_max = 21
        x = runif(n = n_pts, min = x_min, max = x_max)
        dat = data.frame(x = x, y_observed = rnorm(n_pts))
        dat
     rnorm(3)
     set.seed(3)
     
     hist(dat$y_observed , main = "Random Data" , col = "steelblue")
     dev.off()
     
     pdf(file = here("images" , "Random Data Histo.pdf"))
         hist(dat$y_observed , main = "Random Data" , col = "steelblue")
         dev.off()
         
         plot(dat , xlab = "Data Points", ylab = "frequency", main = "Data Scat Plot", col = "red")
        
         
         # Calculates the value of y for a linear function, given the coordinates
         # of a known point (x1, y1) and the slope of the line.
         line_point_slope = function(x, x1, y1, slope)
         {
           get_y_intercept = 
             function(x1, y1, slope) 
               return(-(x1 * slope) + y1)
           
           linear = 
             function(x, yint, slope) 
               return(yint + x * slope)
           
           return(linear(x, get_y_intercept(x1, y1, slope), slope))
         }
         guess_x = 6
         guess_y = 0
         guess_slope = 0.1
         
         plot(y_observed ~ x, data = dat, pch = 8 , col = "olivedrab")
         curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
         dev.off()
         
         pdf(file = here("images" , "plotwithline.pdf"))
         
         plot(dat , xlab = "Data Points", ylab = "frequency", main = "Data Scat Plot", col = "red")
         dev.off()
          
          pdf(file = here("Images" , "Data Scat Plot.pdf"))
         plot(dat , xlab = "Data Points", ylab = "frequency", main = "Data Scat Plot", col = "red")
         
         boxplot(dat , xlab = "Values" , ylab = "data" , main = "boxplot" , col = "yellow")
      dev.off()
        
      pdf(file = here("images" , "boxplot.pdf"))
      
      boxplot(dat , xlab = "Values" , ylab = "data" , main = "boxplot" , col = "yellow")
      dev.off()
      
      plot(dat , xlab = "More Data Points", ylab = "frequency 2", main = "Data Scat Plot #2", col = "red")
      dev.off()
      
      barplot(dat , xlab ="" , ylab = "" , main = "Barplot" , col = "dark green")
    
      plot(dat,xlab = "DATA" , ylab = "FREQUENCY" , main = "lineplot", col="pink")
      
      barplot(dat$y_observed,main = "barplot" , xlab = "data points" , ylab = "y_observed" ,col = "cadetblue")
pdf(file = here("images" , "barpolt.pdf"))        

line_point_slope(dat$x, guess_x, guess_y, guess_slope)

dat$y_predicted = line_point_slope(dat$x,guess_x,guess_y,guess_slope)
dat
dat$resids = (dat$y_observed - dat$y_predicted) 


hist(dat$resids , 
     main = "residual plot" , 
     xlab = "values", 
     ylab = "residuals" , 
     col = "orange" , 
     border = "brown")

pdf(file = here("images" , "resid histogram.pdf"))

plot(dat$y_predicted, dat$residuals, main = "residuals", xlab = "predicted values",  ylab = "residuals", col = "plum4")
dat
plot(dat)
pdf(file = here("images" , "resid scatterplot.pdf"))
dev.off()


