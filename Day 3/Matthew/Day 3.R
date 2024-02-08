# ARDC Summer School 2024
# HASS/Indigenous Stream
# Analysing GeoSocial Survey Data in R

# # Install Packages (only needed once)
# install.packages("tidyverse")
# install.packages("stargazer")
# install.packages("psych")
# install.packages("marginaleffects")


# Load Packages
library(tidyverse)
library(stargazer)
library(psych)
library(marginaleffects)
library(summarytools) 


# Working directory: make sure the correct location
#Load data: Read in a CSV file
sdata <- read_csv("ardc_ss3.csv")


# To read data in Stata format (.dta), use the haven package with read_dta() function
# install.packages("haven")
# library(haven)
# mydata <- read_dta("StataFile.dta")





#######################################
# Step 1 - Explore and clean the data #
#######################################

head(sdata)


# Recoding variables

# Weekly earnings: wkp2019
typeof(sdata$wkp2019)
summary(sdata$wkp2019)



# What does the distribution of earnings look like? (skewed right)
# Histogram function in base R -- hist() -- can we guess what the code should look like?
# hist(...)

# Using ggplot to plot the density -- while filtering out the missing values of wkp2019
sdata %>% 
  filter(wkp2019>=0 & !is.na(wkp2019)) %>% 
  ggplot(aes(x = wkp2019)) + 
  geom_density(alpha = 0.5) + 
  labs(y = "Density", x = "Weekly earnings")


# Log transformation: 'start' log at positive value (technically unnecessary with our data)
# Creating a new data object, so leaving sdata unchanged, and working with 'edata' now
edata <- sdata %>% 
  mutate(lnwkp2019 = log(wkp2019 + 1))


# What does the distribution of log(earnings) look like?
edata %>% 
  ggplot(aes(x = lnwkp2019)) + 
  geom_density() + 
  labs(y = "Density", x = "Log weekly earnings")



##########################
# Demographic background #
##########################

# Our variables of interest: 
# Demographic background
# - gender
# - language spoken at home (English vs. other)
# - family structure (living with 2 biological parents vs. other)
# 
# Socioeconomic background
# - parents education
# - parental occupational status (SEI - socioeconomic index)
# - wealth (measured by an index ranging from 0-4)
# 
# School experiences
# - ever missed 2 months of school
# - educational attainment by 2019

# Area-level characteristics from the Geosocial tool
# - proportion of households that speak a language other than English
# - employment-to-population ratio for adult men
# - median household income



# Gender
table(edata$sex)

# What data type is the sex variable? How can we figure this out?
typeof(edata$sex)

# Create a factor variable
edata <- edata %>% 
  mutate(female = factor(sex, 
                         levels = c("2 Male", "1 Female"), 
                         labels = c("Male", "Female")))


typeof(edata$female)
table(edata$sex, edata$female, useNA = "always")


# Home language: currently character, change to factor variable (typeof = integer)
table(edata$hlang)
typeof(edata$hlang)

edata <- edata %>% 
  mutate(hlang_f = factor(hlang, labels = c("English only", "Non-English")))

typeof(edata$hlang_f)
table(edata$hlang, edata$hlang_f)


# Family household structure (collapse factor levels using dplyr::fct_collapse)
table(edata$famstruc)

edata <- edata %>% 
  mutate(famhh  = fct_collapse(famstruc, 
                         "Other" = c("1 Single parent family", "3 Mixed"), 
                         "2 bio parents" = "2 Nuclear family"))

table(edata$famstruc, edata$famhh, useNA = "ifany")


# Alternatively, could use use ifelse()...
# edata <- edata %>% 
#   mutate(famhh_f = factor(ifelse(famstruc %in% c("2 Nuclear family"), "2 bio parents", "Other"), 
#                           levels = c("Other", "2 bio parents")))
# 
# table(edata$famstruc, edata$famhh_f)




############################
# Socioeconomic background #
############################

# Parental education: reduce categories
table(edata$hisced)

edata <- edata %>% 
  mutate(ped = fct_collapse(hisced, 
    "< HS" = c("1 ISCED 1 - Primary school only", 
               "2 ISCED 2 - Some secondary but didn't complete Yr 10", 
               "3 ISCED 3B, 3C - Yr 10 or 11"),
    "HS/Cert" = "4 ISCED 3A - Year 12, ISCED 4 - TAFE Training Certificate",
    "TAFE Dip" = "5 ISCED 5B - TAFE Diploma",
    "Uni" =  "6 ISCED 5A - Uni degree, ISCED 6 - PhD or equivalent"))

table(edata$hisced, edata$ped, useNA = "ifany")


# Parental SEI (a measure of occupational status)
typeof(edata$hisei)
summary(edata$hisei)

#Histogram and denisty plots in base R (plot(density()) requires no missing values)
hist(edata$hisei)
plot(density(edata$hisei))



# Wealth index
typeof(edata$wealth)
summary(edata$wealth)

hist(edata$wealth)
plot(density(edata$wealth))


# ggplot density plot alternative
edata %>% 
  ggplot(aes(x = wealth)) + 
  geom_density() + 
  labs(y = "Density", x = "Wealth Index")





######################
# School experiences #
######################

# Ever missed 2 months of school?
# Combine measures from primary and secondary school: m2m_p & m2m_s
table(edata$m2m_p, edata$m2m_s, useNA = "ifany")

edata <- edata %>% 
  mutate(m2m = if_else(m2m_p=="1 No, never" & m2m_s=="1 No, never", "Never missed 2 months", "Missed 2 months")) %>% 
  mutate(m2m = factor(m2m, levels = c("Never missed 2 months", "Missed 2 months")))

table(edata$m2m)
typeof(edata$m2m)


# Educational attainment: need to combine 'school' and 'post-school qualifications'
# "HS" will be the reference category, but that means I'll make it 'out of order'

table(edata$hsl2019, useNA = "ifany")
table(edata$hel2019, useNA = "ifany")

edata <- edata %>% 
  mutate(educ = case_when(hsl2019 %in% c("2 Year 11", "3 Year 10", "4 Year 9 or below") ~ "< HS", 
                          hsl2019 == "1 Year 12" & hel2019 == "No qual" ~ "HS", 
                          hel2019 %in% c("Cert I/II", "Cert III/IV/Dip") ~ "VET", 
                          hel2019 %in% c("BA", "Post-grad") ~ "Uni")) %>% 
  mutate(educ = factor(educ, levels = c("HS", "< HS", "VET", "Uni")))


table(edata$educ)




###################################
# Area-level Geosocial indicators #
###################################

# Proportion of households that speak a language other than English
# Adult male employment:population ratio
# Median household income


# Language other than English spoken at home expressed as a proportion
edata <- edata %>% 
  mutate(noneng_p = lsh_oth_lan_2011_ce_p / (lsh_eng_only_2011_ce_p + lsh_oth_lan_2011_ce_p))

summary(edata$noneng_p)
sum(!is.na(edata$noneng_p)) # This counts the number of non-missing values of noneng_p



# Median household income
edata <- edata %>% 
  mutate(hhi_p50 = med_tot_hh_inc_wee_c2011)

summary(edata$hhi_p50)
describe(edata$hhi_p50, skew="F") # psych::describe(), the function here, is another easy way to get summary stats



# Male employment:population ratio (expressed in percentage, hence multiplication by 100)
describe(edata$lfs_tot_lforc_c11_m)
describe(edata$n_lforc_c11_m)
describe(edata$lfs_unem_lfw_c11_m)

describe(edata[c("lfs_tot_lforc_c11_m", "lfs_unem_lfw_c11_m", "n_lforc_c11_m")], skew = "F")

# Create our employment-to-population variable
edata <- edata %>% 
  mutate(emp_pop_m = 100 * (lfs_tot_lforc_c11_m - lfs_unem_lfw_c11_m) / (lfs_tot_lforc_c11_m + n_lforc_c11_m))

describe(edata$emp_pop_m, skew="F")



#########################
# Variables of interest #
#########################

# Outcome variables #
# lnwkp2019; wkp2019

# Covariates #
# Demographic background: female; hlang_f; famhh; 
# SES background: ped; hisei; wealth
# School: m2m; educ
# Geosocial: emp_pop_m; noneng_p

# Let's create some vectors for our different types of variables
idvars <- c("stid", "sa3")
numvars <- c("wkp2019", "hisei", "wealth", "noneng_p", "hhi_p50", "emp_pop_m")
facvars <- c("female", "hlang_f", "famhh", "ped", "m2m", "educ")




####################
# Sample Selection #
####################

# Are there cases that we want to drop from our analysis?

#Full-time study
table(edata$fts2019)


# Density plot of weekly earnings by full-time study status

# First, create a variable for FT students vs. else
edata <- edata %>% 
  mutate(fts = fct_collapse(fts2019, 
                            "Not FT Student" = c("2 Part-time", "3 Not studying"), 
                            "FT Student" = "1 Full-time")) %>% 
  mutate(fts=factor(fts, levels = c("Not FT Student", "FT Student")))


table(edata$fts)

# Alternatively, using ifelse()
# edata <- edata %>% 
#   mutate(fts_ifelse = ifelse(fts2019 == "1 Full-time", "FT student", "Not FT student")) %>%
#   mutate(fts_ifelse = factor(fts_f, levels = c("Not FT student", "FT student")))
# 
# table(edata$fts_ifelse)


#Density plot of logged earnings by full-time student status           
edata %>% 
  filter(!is.na(lnwkp2019)) %>% 
  ggplot(aes(x=lnwkp2019, fill=fts)) + 
  geom_density() 

#Let's customise to make the density plots translucent
edata %>% 
  filter(!is.na(lnwkp2019)) %>% 
  ggplot(aes(x=lnwkp2019, fill=fts)) + 
  geom_density(alpha = 0.5) 


# Compare that to educational and gender differences (Note I made the fill lighter by reducing alpha=0.25)
edata %>% 
  filter(!is.na(lnwkp2019)) %>% 
  ggplot(aes(x=lnwkp2019, fill=educ)) + 
  geom_density(alpha=0.25)

edata %>% 
  filter(!is.na(lnwkp2019)) %>% 
  ggplot(aes(x=lnwkp2019, fill=female)) + 
  geom_density(alpha=0.25)


# Filter to keep only non-full-time students;
# Select variables we want; ensure non-missing on those variables
adata <- edata %>% 
  filter(fts=="Not FT Student") %>% 
  select(all_of(c(idvars, numvars, facvars))) %>% 
  na.omit


# # In 2 more transparent steps (but less efficient coding):
# # Filtering to disregard full-time students in 2019
# adata <- edata %>% 
#   filter(fts_f=="Not FT student")
# 
# # Filtering to ensure non-missing on all variables we will include in analysis
# adata <- adata %>% 
#   filter(!is.na(wkp2019) & 
#            !is.na(female) & 
#            !is.na(hlang) & 
#            !is.na(famhh) & 
#            !is.na(ped) & 
#            !is.na(hisei) & 
#            !is.na(wealth) & 
#            !is.na(m2m) & 
#            !is.na(educ), etc., etc.
#   )


################################################
# Descriptive Statistics for Numeric Variables #
################################################

# Several ways to report descriptive statistics in R

# Extracting descriptive statistics for numeric variables using dplyr::summarize()
adata %>% 
  summarize(mean_wkpay = mean(wkp2019), 
             sd_wkpay = sd(wkp2019), 
             n_wkpay = n(), 
             min_wkpay = min(wkp2019), 
             max_wkpay = max(wkp2019))

# Base R: summary()
summary(adata$wkp2019)


# Select multiple columns, then use summary()
adata %>% 
  select(c("wkp2019", "hisei")) %>% 
  summary()

# Selecting all numeric variables
adata %>%
  select_if(is.numeric) %>%
  summary()

# # Selecting all the variables in our numvars vector
# adata %>%
#   select(any_of(numvars)) %>%
#   summary()

#Create a vector of variables, then use psych::describe to easily print descriptives
describe(adata[c("wkp2019", "hisei")], skew=F)

# ...or use our vector of numeric variables
describe(adata[numvars], skew = F)

# ...or select all numeric variables (but will include ID vars if numeric)
adata %>%
  select_if(is.numeric) %>%
  describe(skew=F)




###############################################
# Descriptive Statistics for Factor Variables #
###############################################

# What happens if we try to summarise factor variables? We don't get meaningful values
describe(adata[facvars])


# Categorical variables one by one...
freq(adata$female)
freq(adata$hlang_f)
freq(adata$famhh)
freq(adata$ped)


# Or, using lapply() to loop over our facvars vector
lapply(adata[facvars], freq)




#############################
# Linear Regresssion Models #
#############################


# Linear regression models: demographic background
lm1 <- lm(wkp2019 ~ female + hlang_f + famhh, data = adata) 
summary(lm1)


# View results using stargazer (can also output to Latex or html)
stargazer(lm1, type="text")


# Socioeconomic background
lm2 <- lm(wkp2019 ~ female + hlang_f + famhh + 
            ped + hisei + wealth, data = adata)
stargazer(lm1, lm2, type="text")


# Geosocial factors
lm3 <- lm(wkp2019 ~ female + hlang_f + famhh + 
            ped + hisei + wealth + 
            noneng_p + emp_pop_m + hhi_p50, data = adata)
stargazer(lm1, lm2, lm3, type="text")

# Education
lm4 <- lm(wkp2019 ~ female + hlang_f + famhh + 
            ped + hisei + wealth + 
            noneng_p + emp_pop_m + hhi_p50 + 
            m2m + educ, data = adata)
stargazer(lm1, lm2, lm3, lm4, type="text")


########################
# Marginal predictions #
########################

# Predictions based on Model 4
p_lm4 <- predictions(lm4, 
                     type = "response", 
                     by = "educ", 
                     newdata = datagrid(educ = unique(adata$educ), 
                                        grid_type = "counterfactual"))

p_lm4

#fct_relevel() changes the order of the educ categories so that "< HS" comes first
p_lm4 %>% 
  mutate(educ = fct_relevel(educ, "< HS")) %>% 
  ggplot(aes(y= estimate , x = educ)) +
  geom_col(position = "dodge", fill = "purple") +
  geom_errorbar(aes(ymin = conf.low , ymax = conf.high), position = "dodge") +
  labs(y = "Adjusted weekly labour income", x = "Educational attainment") +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(size = 11)
  )



########################
# Interaction efffects #
########################

# What if the effect of education is different for men and women?

# Education * gender interaction: categorical * categorical
lm5 <- lm(wkp2019 ~ female*educ + hlang_f + famhh + 
            ped + hisei + wealth + 
            noneng_p + emp_pop_m + hhi_p50 + 
            m2m, data = adata)

stargazer(lm4, lm5, type = "text")


################################################
# Marginal predictions for Interaction Models #
################################################


# Marginal predictions from lm5 (interaction of gender*educ)
p_lm5 <- predictions(
  lm5,
  type = "response",
  by = c("educ", "female"),
  newdata = datagrid(educ = unique(adata$educ), female = unique(adata$female), grid_type = "counterfactual"))

summary(p_lm5)

p_lm5 %>%
  mutate(educ = fct_relevel(educ, "< HS", "HS", "VET", "Uni")) %>%   
  ggplot(aes(y= estimate , x = educ, fill = female)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = conf.low , ymax = conf.high), position = "dodge") +
  labs(y = "Adjusted weekly earnings", x = "Educational attainment") +
  scale_fill_manual(values = c("grey", "purple")) +
  guides(fill = guide_legend(title = "Gender", title.position = "top")) + 
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(size = 11)
  )


##########################################################################
# Interacting area-level income with individual-level household language #
##########################################################################

# Does the effect of household language differ by neighborhood SES?
# household language * area-level income: continuous * categorical
lm6 <- lm(wkp2019 ~ female + hlang_f*hhi_p50 + famhh + 
            ped + hisei + wealth + 
            noneng_p + emp_pop_m + hhi_p50 + 
            m2m + educ, data = adata)

stargazer(lm4, lm6, type = "text")


##########################################################################
# Marginal predictions for Interaction Models with a Continuous Variable #
##########################################################################

# We'll need to choose some values of the continuous variable to plug into our regression equation
describe(adata$hhi_p50, skew=F)


# Choose $1,000 and $2,000 per week for values of area-level median weekly household income to plot
p_lm6 <- predictions(
  lm6, 
  type="response", 
  by=c("hlang_f", "hhi_p50"), 
  newdata = datagrid(hlang_f = unique(adata$hlang_f), hhi_p50=c(1000, 2000), grid_type = "counterfactual")
)

p_lm6
# Graphing in ggplot with a line graph (geom_line() rather than geom_col())
p_lm6 %>% 
  ggplot(aes(y=estimate, x=hhi_p50, color=hlang_f)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .25) + 
  geom_line() + 
  scale_color_manual(values = c("black", "purple")) + 
  labs(y = "Adjusted weekly earnings", x = "SA3 Median household income")





#######################################
# Binary Outcome: Logistic Regression #
#######################################

# Let's make 2 income categories at half the median income to measure poverty
median(adata$wkp2019) / 2
adata <- adata %>% 
  mutate(poor = ifelse(wkp2019 < 612.4375, "Poor", "Non-poor")) %>% 
  mutate(poor = factor(poor, levels = c("Non-poor", "Poor")))

table(adata$poor)



# logit model with interaction between gender and education
l1 <- glm(poor ~ female*educ + hlang_f + famhh + 
            ped + hisei + wealth + 
            noneng_p + emp_pop_m + hhi_p50 + 
            m2m, 
          family=binomial(link = "logit"), data=adata)

#coefficients are in log odds -- very opaque
summary(l1)

#need to exponentiate to get odds ratios
exp(cbind(OR = coef(l1), confint(l1)))

#In stargazer: coefficients in log odds
stargazer(l1, type = "text")

#Exponentiated (odds ratios), plus the untransformed t-statistics with p-value stars
stargazer(l1, apply.coef=exp, t.auto=F, p.auto=F, report = "vc*t", type="text")


#Marginal predictions
p_l1 <- predictions(l1, 
                    type = "response",
                    by = c("educ", "female"),
                    newdata = datagrid(educ = unique(adata$educ), 
                                       female = unique(adata$female), grid_type = "counterfactual"))


summary(p_l1)


# Graph the adjusted predictions
p_l1 %>% 
  mutate(educ = fct_relevel(educ, "< HS", "HS", "VET", "Uni")) %>%   
  ggplot(aes(y= estimate , x = educ, fill = female)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = conf.low , ymax = conf.high), position = "dodge") +
  ggtitle("Predicted probability of poverty by gender and education") + 
  labs(y = "Predicted probability", x = "Educational attainment") +
  scale_fill_manual(values = c("grey", "purple")) +
  guides(fill = guide_legend(title = "Gender", title.position = "top")) + 
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(size = 11)
  )


# Now let's flip the x-axis and fill variables
p_l1 %>% 
mutate(educ = fct_relevel(educ, "< HS", "HS", "VET", "Uni")) %>%   
  ggplot(aes(y= estimate , x = female, fill = educ)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = conf.low , ymax = conf.high), position = "dodge") + 
  ggtitle("Predicted probability of poverty by gender and education") + 
  labs(y = "Predicted probability", x = "") +
  scale_fill_manual(values = c("grey", "purple", "blue", "seagreen")) +
  guides(fill = guide_legend(title = "Gender", title.position = "top")) + 
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(size = 11)
  )




