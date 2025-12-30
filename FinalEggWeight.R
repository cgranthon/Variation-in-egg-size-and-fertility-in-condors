# ------------------------------------------
# Title: "Egg Weight Model"
# Paper: "Senescence and individual variation drive egg size and fertility of captive Gymnogyps californianus (California Condors)"
# Author: "Carolina Granthon"
# Date: "2025-12-30"
# ------------------------------------------

# 1. Load packages
library(dplyr)
library(ggplot2)
library(brms)
library(bayesplot)
library(tidyr)

# 2. Read data

condors <- read.csv("Data/ch1finaldata.csv")

#Response Variable:
#weight = estimated fresh weight of egg at lay

#Fixed Effects:
#fage = age of female when the egg was laid
#mage = age of male when the egg was laid
#year = year the egg was laid (numerical)
#drear = how the dam was reared (Parent, Artificial)
#srear = how the sire was reared (Parent, Artificial)
#pair = how long (in years) had the pair been together when the egg was laid
#clutch = seasonal lay order (1, 2)
#double = did the female lay more than one egg the previous year (Yes, No)
#damF = inbreeding coefficient for the female
#sireF = inbreeding coefficient for the male
#relatedness = pairwise kinship for the pair producing the egg

#Random Effects
#dam = female of the pair
#sire = male of the pair
#yearcat = year the egg was laid (categorical)


# 3. Data preparation

condorstat <- condors %>%
  filter(weight>200) %>% #remove runt eggs
  filter(weight!=is.na(weight)) %>% #remove eggs with missing weights
  filter(mage!=is.na(mage)) %>% #remove eggs laid by unpaired females
  select(year,weight,mage,fage,clutch,sire,srear,dam,drear,pair,double,damF,sireF,relatedness) #select variables for analysis

condorstat <- condorstat %>% 
  group_by(dam) %>% 
  filter(n()>=3) #remove females with less than 3 eggs in the dataset

condorstat$yearcat <- as.factor(condorstat$year) #add variable for year as a category

condorstat$clutch <- as.factor(condorstat$clutch) #transform character variables to factors
condorstat$dam <- as.factor(condorstat$dam)
condorstat$sire <- as.factor(condorstat$sire)
condorstat$drear <- as.factor(condorstat$drear)
condorstat$srear <- as.factor(condorstat$srear)
condorstat$double <- as.factor(condorstat$double)

condorsc <- condorstat
condorsc$year <- scale(condorsc$year) #scale numerical predictors
condorsc$fage <- scale(condorsc$fage)
condorsc$mage <- scale(condorsc$mage)
condorsc$pair <- scale(condorsc$pair)
condorsc$damF <- scale(condorsc$damF)
condorsc$sireF <- scale(condorsc$sireF)
condorsc$relatedness <- scale(condorsc$relatedness)

# 4. Model

priors <- set_prior(horseshoe(1),class="b") #set up horseshoe prior on fixed effects

modp <- brm(weight~fage+I(fage^2)+mage+year+drear+srear+pair+clutch+double+damF+sireF+relatedness+(1|dam)+(1|sire)+(1|yearcat),data=condorsc,prior=priors,family=gaussian(),chains=4,iter=2000,control = list(adapt_delta=0.9)) 
#model includes the quadratic effect of female age: I(fage^2)
#increased adapt delta to improve convergence

# 5. Output

summary(modp, prob = 0.90)
mcmc_plot(modp, variable = "^b_", regex=TRUE) #fixed effects
mcmc_plot(modp,variable=c("b_fage","b_IfageE2","b_mage","b_year","b_drearParent","b_srearParent","b_pair","b_clutch2","b_doubleY","b_damF","b_sireF","b_relatedness"))+#no intercept
  scale_y_discrete(labels=c("b_year"="Year","b_relatedness"="Pairwise kinship","b_pair"="Years paired","b_doubleY"="Prior-year double laying (True)","b_clutch2"="Seasonal laying order (2nd egg)","b_sireF"="Male inbreeding","b_damF"="Female inbreeding","b_srearParent"="Male rearing (Parent)","b_drearParent"="Female rearing (Parent)","b_mage"="Male age","b_IfageE2"="Female age2","b_fage"="Female age")) #fixed effects, no intercept, labeled

mcmc_plot(modp,variable=c("sd_dam__Intercept","sd_sire__Intercept","sd_yearcat__Intercept"))+ #random effects, labeled
  scale_y_discrete(labels=c("sd_dam__Intercept"="Female","sd_sire__Intercept"="Male","sd_yearcat__Intercept"="Year"))

mcmc_plot(modp,variable=c("r_dam")) #by female
mcmc_plot(modp,variable=c("r_sire")) #by male
mcmc_plot(modp,variable="r_year") #by year