require(here)
here() 
dat_catrate <- read.csv(here("data", "catrate .csv"))
catrate <- data.frame(read.csv(here("data", "catrate .csv")))

dat_delomys.<- read.csv(here("data", "delomys.csv"))
delomys <-data.frame(read.csv(here("data", "delomys.csv")))

dat_rope <- read.csv(here("data" , "rope .csv"))
rope <- data.frame(read.csv(here("data", "rope .csv" )))

hist(dat_catrate$pond, xlab = "Pond", ylab = "success" , main = "Alex Historgram of Pond and success" , col = "re")
