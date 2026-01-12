setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/Extreme heat and cold")

library(tidyverse)
library(readr)
library(dlnm)
library(mgcv)

first_stage_LAD_age_sepcific = read_csv("data/processed/first_stage_LAD_age_sepcific.csv")

#########################################################
#Set up data (Day of week + time trend)
df  = first_stage_LAD_age_sepcific %>%
  mutate(
    dow = factor(as.character(wday(date, label=TRUE, week_start=1)),
                 levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),
    ym = format(date, "%Y-%m"),
    strata = interaction(ym, dow, drop=TRUE)
  )




##########################################################
#made age group as factor 
df = df %>% 
  mutate(age_group = factor(age_group, levels = c("0-64",
                                                  "65-74",
                                                  "75-84",
                                                  "85+")))

age_levels = levels(df$age_group)


#########################################################
#initiates a list to store the model results 
fit_mod = vector("list", length(age_levels))

names(fit_mod) = age_levels

########################################################
#to create the time shifted copies for lags
cbs  = vector("list", length(age_levels))  # store crossbasis used in each fit
names(cbs) =  age_levels

########################################################
#calucalte the degree of freedom
df_per_year = 7
n_years = as.numeric(difftime(max(df$date), min(df$date), units = "days")) / 365.25
df_time = ceiling(df_per_year * n_years)


########################################################

#data driven knots
#This gives the spline flexibility across Birmingham’s typical temperature range, including the cooler and warmer tails,
# which helps capture the non-linear risk shape without overfitting the very extremes

temp_knots = quantile(df$mean_air_temp, probs = c(0.10, 0.50, 0.90), na.rm = TRUE)

# lag knots 
#Using logknots(lag_max, 3) is the gold standard for temperature.
#The log-scale puts more "statistical attention" (knots) on the early days (0, 1, 2) and spreads them out as you get toward day 21.
#puts more knots at the beginning (where the mortality effect changes quickly, like during a heatwave) and 
#fewer knots at the end (where the effect of a cold spell stays more constant)
lag_knots = logknots(21, 2)


########################################################

for (age in age_levels){
  data = df %>% 
    filter(age_group == age)
  
  #cross basis allows us to estimate non-linear effect of temp and delayed effect over time 
  cb = crossbasis(
    data$mean_air_temp,
    lag= c(0,21), 
    argvar = list(fun ="ns",df=4),
    arglag = list(fun="ns",knots=lag_knots)
  )
  
  fit = gnm(count ~ cb, eliminate = strata,
            family = quasipoisson(), data = data)
  
  fit_mod[[age]] = fit
  cbs[[age]] = cb
  
}

##############################################################################



ag = "85+"
d  = df[df$age_group == ag, ]

summary(d$mean_air_temp)
quantile(d$mean_air_temp, c(.01,.05,.50,.95,.99), na.rm=TRUE)
table(cut(d$mean_air_temp, breaks=c(-10,0,5,10,15,20,25,40)))
# 
# 


cb  <- cbs[[ag]]          # use the crossbasis you fitted with
fit <- fit_mod[[ag]]      # fitted glm

# Predict the effect relative to a reference temperature (e.g., 15°C)
pred <- crosspred(cb, fit, by = 0.1, cen = median(d$mean_air_temp, na.rm = TRUE), cumul = TRUE)

#Restrict to 1st–99th percentile temperature range (paper-style)
t_rng <- quantile(d$mean_air_temp, probs = c(0.05, 0.975), na.rm = TRUE)

keep <- pred$predvar >= t_rng[1] & pred$predvar <= t_rng[2]

mmt <- pred$predvar[keep][ which.min(pred$allRRfit[keep]) ]

pred2 <- crosspred(cb, fit, by = 0.1, cen = mmt, cumul = TRUE)
# Plot the "Overall" effect (sum of all lags)
plot(pred2, "overall", xlab="Temperature", ylab="RR", main="Overall Effect for 0-64")

#the graph is not what we expected, this could due to uncontrained dlnm 
#many cross-basis and hard to follow every small wiggle
#highly correlated across several days, an unconstrained model might assign a negative coefficient to one lag and a positive to another 
#just to "fit" the noise, resulting in this non-sensical dip.
