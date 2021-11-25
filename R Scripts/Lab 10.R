require(rope.csv)
rope_ <- read_csv("data/rope .csv")

rope =read_csv("data/rope .csv")
rope$rope.type = factor(rope$rope.type)
levels(rope$rope.type)


summary(rope$rope.type)
data.frame(table(rope$rope.type))   
nrow((table(rope$rope.type)) )
length(rope$rope.type)
sum((rope$p.cut - mean(rope$p.cut))^2)
n_obs -1

aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type), 
  FUN = mean)

agg_sq_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) sum((x - mean(x))^2))

str(agg_sq_resids)
agg_sq_resids$x
sum(agg_sq_resids$x)

n_obs - n_groups

ss_tot-ss_within

n_groups-1

ss_within/df_within

ss_among/df_among 

ms_among/ms_within 

ss_within
  
  ms_within

df_within

n_groups

nrow(rope)

ss_tot
df_tot

ms_among
                        
ms_within                                

 
pf(f_ratio, df_within, df_among)

Template:
n_obs = nrow(rope)
data.frame(table(rope$rope.type)) 

 n_groups = nrow((table(rope$rope.type))) 
 length(rope$rope.type)
                        
  ss_tot = sum((rope$p.cut - mean(rope$p.cut))^2)
  df_tot = n_obs -1
                        
 agg_sq_resids  = agg_sq_resids = aggregate(
x = rope$p.cut,
by = list(rope$rope.type),
FUN = function(x) sum((x - mean(x))^2)) 
                      
ss_within = sum(agg_sq_resids$x)
df_within = n_obs - n_groups
                        
ss_among = ss_tot-ss_within
df_among = n_groups-1
                        
ms_within = ss_within/df_within
ms_among  =  ss_among/df_among 
                        
f_ratio = ms_among/ms_within 
f_pval = pf(f_ratio, df_within, df_among)