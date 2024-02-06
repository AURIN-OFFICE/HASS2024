# ARDC Summer School 2024: Introduction to data wrangling in R #
# Day 1 #

##############################
# Base R
##############################

##############################
## vector ##
### Create a numeric vector and save it as "mv1"
mv1 <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)

### Print an object
print(mv1)

### In most cases, it is enough to type just the name of the object 
mv1

### other ways to create vectors -- what will each of these do?
mv2 <- c(1:20) 
mv3 <- mv2/10
mv4 <- c(1:4, 20:24, 1:11)
mv5 <- rep(1:4,5) # hint: Before we execute this, let's look at the 'Help' tab to figure out what rep() does
mv6 <- c(mv2,mv5)

### check the data type
typeof(mv1)
typeof(mv2)
typeof(mv3)

# double and integer data types
typeof(1)  #double
typeof(1L) #integer


## Other data types
### logical

mv6 <- mv4 == mv5 # Are corresponding values in mv4 and mv5 equal?

#print mv4 and mv5
mv4 
mv5

#what will mv6 say?
mv6

# TRUE (T), FALSE (F)
c(TRUE, FALSE)
c(T,F)


### characters
mv7 <- rep(c("a","b","c","d","e"),4)
mv7 

### Coercion 
mv8 <- c(1,3,4,"a")
mv8 # Note that all numbers are stored as characters
as.numeric(mv8) # Converting the vector to numeric will replace non-numbers to NA (missing)
as.character(mv1) # Converting to character

### adding NA to a vector
mv9 <- c(1,3,4,NA) 
mv9

# what happens when performing mathematical operations on an 'NA'?
mv9 + 1



##############################
## create a data.frame
mdf <- data.frame(mv2, mv4, mv7, y = c(25:44)) # Note that all vectors must be of equal length (in this case 20 elements long).

mdf

# We have now created our first data set in the traditional sense, with multiple rows and columns

# Back to PowerPoint #




##############################
# Installing and loading packages
##############################

#install.packages("tidyverse") # Required only once -- and we don't need to run this in this environment
library(tidyverse) # Required for every new R session

##############################
# Loading data files
##############################

# Load higher education data
edu <- read_csv("ARDC_SS_2023_edu.csv")



# Inspect the data
str(edu) # the structure of the data
head(edu) # first rows
print(edu)
print(edu, n = 16, width = Inf) # print more rows and all columns
view(edu) # all elements in an excel-like view (view() is part of the tibble package)
names(edu) # return list of column names (it is a character vector)


# Back to Powerpoint #


##############################
# Subsetting/filtering rows
##############################

# Select individuals who graduated in 2011
table(edu$hied_2011_completion_flag)

filter(edu, hied_2011_completion_flag == "1")

# Select individuals who graduated from a BA course (hied_2011_level==9 | 10) in 2011 and save the new data.frame as edu2
edu2 <- filter(edu, hied_2011_completion_flag == "1" & hied_2011_level %in% c(9,10))

nrow(edu) # check the number of rows
nrow(edu2)
rm(edu2) # Remove edu2


##############################
# Subsetting/selecting variables
##############################

# selection by index
select(edu, 7:12)
select(edu, 7,1,3)
select(edu, 7,1:3)

# Select columns by names
select(edu, MADIP_ID_04, hied_2011_completion_flag, hied_2011_completion_field) # We can change the order

select(edu, MADIP_ID_04:hied_2011_completion_flag)

select(edu, MADIP_ID_04, hied_2011_completion_field:hied_2011_completion_flag)

# Dropping variables
select(edu, -1:-6)
select(edu, -1:-6,-hied_2011_field:-hied_yr_arrival)

select(edu, starts_with("hied_2011_"))

# other useful select commands
select(edu, ends_with("field"))
select(edu, contains("2011"))
select(edu, c(ends_with("field"),contains("2011")))
select(edu, ends_with("field") | contains("2011"))
select(edu, ends_with("field") & contains("2011"))
select(edu,where(is.numeric))

## other options
# https://dplyr.tidyverse.org/reference/select.html - e.g. num_range(), group_cols()


# HE data - keep relevant variables only
select(edu, MADIP_ID_04, gender, hied_cob_2dig, hied_disability_ind, hied_indigenous_ind, hied_language_ind, hied_yr_arrival, starts_with("hied_2011_"))

##############################
# Chaining - Pipe
##############################

edu2 <- edu %>%
  filter(hied_2011_completion_flag == "1" & hied_2011_level %in% c(9,10)) %>%
  select(MADIP_ID_04, gender, hied_cob_2dig, hied_disability_ind, hied_indigenous_ind, hied_language_ind, hied_yr_arrival, starts_with("hied_2011_"))

head(edu2)

##############################
# Creating/modifying a variable
##############################

# create a broad field of study variable
typeof(edu2$hied_2011_completion_field)
head(edu2$hied_2011_completion_field)

## coerce the existing variable to numeric 
edu2 <- edu2 %>%
  mutate(hied_2011_completion_field = as.numeric(hied_2011_completion_field))

head(edu2$hied_2011_completion_field)
typeof(edu2$hied_2011_completion_field)

## create a new variable with a 2-digit broad field code. floor() will round down
edu2 <- edu2 %>%
  mutate(broad_FoS = floor(hied_2011_completion_field/10000))

## check the results
table(edu2$hied_2011_completion_field, edu2$broad_FoS)

## Remove no information and mixed field programmes
edu2 <- edu2 %>%
  filter(broad_FoS > 0 & broad_FoS < 12)

## label the new variable
edu2 <- edu2 %>%
  mutate(broad_FoS = factor(broad_FoS, levels = c(1:10), labels = c("Natural and Physical Sciences",
                                                                    "Information Technology",
                                                                    "Engineering and Related Technologies",
                                                                    "Architecture and Building",
                                                                    "Agriculture, Environmental\nand Related Studies",
                                                                    "Health",
                                                                    "Education",
                                                                    "Management and Commerce",
                                                                    "Society and Culture",
                                                                    "Creative Arts")))

# recode the disability variable (3 is no information)
table(edu2$hied_disability_ind, exclude = NULL)

edu2 <- edu2 %>%
  mutate(hied_disability_ind_rec = replace(hied_disability_ind, hied_disability_ind == 3, 2))

table(edu2$hied_disability_ind, edu2$hied_disability_ind_rec, exclude = NULL)

# modifying multiple variables - across()
edu2 %>%
  mutate(across(c(hied_disability_ind, hied_indigenous_ind, hied_language_ind), ~replace(.,. == 3, 2))) %>%
  select(hied_disability_ind, hied_indigenous_ind, hied_language_ind) %>%
  summary()

# creating multiple new variables
edu2 <- edu2 %>%
  mutate(across(c(hied_disability_ind, hied_indigenous_ind, hied_language_ind), list(rec = ~replace(.,. == 3, 2), mat = ~.+1)))

edu2 %>%
  select(contains(c("dis", "lang", "indi"))) %>%
  head()


############## ############## ##############  
############## add income data ##############
############## ############## ##############  


##############################
# Prepare income data
##############################

# read data
inc <- read_csv("ARDC_SS_2023_income.csv")

head(inc)

# extract ID column and columns with salary_wages
inc <- inc %>%
  select(MADIP_ID_04, contains(c("salary_wages")))

head(inc)

# change format to long
inc_long <- inc %>%
  pivot_longer(cols = -MADIP_ID_04, 
               names_to = c("fin_year"), 
               names_pattern = "pit_(.*)_salary_wages",
               values_to = "salary_wages") 

head(inc_long)

##############################
# Join data
##############################

edu_inc <- left_join(edu2, inc_long)
edu_inc <- left_join(edu2, inc_long, by = "MADIP_ID_04")
edu_inc <- left_join(edu2, inc_long, by = c("MADIP_ID_04" = "MADIP_ID_04"))

# how many observations per ID?
edu_inc <- edu_inc %>% 
  group_by(MADIP_ID_04) %>% 
  mutate(nobs = n())

table(edu_inc$nobs)

edu_inc <- edu_inc %>% 
  select(-nobs)


##############################
# Collapse/ summarise data
##############################

names(edu_inc)

# calculate average income and the number of observations for female and male graduates of each field in each year
# na.rm = T removes any NAs
agg_data <- edu_inc %>%
  group_by(broad_FoS, fin_year, gender) %>%
  summarise(av_inc = mean(salary_wages, na.rm = T),
            count = n())
  
print(agg_data)

# change format to wide - 1 observation per id_cols(), multiple columns by gender values
agg_data <- agg_data %>%
  pivot_wider(id_cols = c("broad_FoS", "fin_year"), names_from = gender, values_from = c("av_inc", "count")) 

agg_data

# calculate the feminisation rate and gender pay gap
agg_data <- agg_data %>%
  mutate(total = count_F+count_M,
         fem_rate = count_F/total,
         gpg = (av_inc_M - av_inc_F)/av_inc_M)

agg_data

# plot the results

agg_data %>%
  filter(fin_year %in% c("1011","1516")) %>%
  ggplot(aes(x = av_inc_M, y = gpg, colour = factor(broad_FoS), size = total)) +
  geom_point() +
  facet_wrap(~factor(fin_year), ncol = 2L) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy=1), limits = c(-0.05,0.35), expand = c(0,0)) +
  ylab("Gender pay gap") +
  xlab("Average income among men") +
  guides(colour = guide_legend(ncol = 2)) +
  scale_size_continuous(name="N observations") +
  scale_colour_discrete(name="Field of study") +
  theme(
    axis.line = element_line(colour = "grey"),
    legend.position="bottom",
    legend.key.width = unit(1,"cm"),
    panel.spacing = unit(1.5, "lines"),
    legend.box = "horizontal",
    legend.direction = "vertical"
  ) 

