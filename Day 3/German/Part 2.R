############### ---------------- Workshop -------------- ##############
###### ------ Clean variables ----- #########
rm(list=ls())
###### ------ Install libraries ----- #########
# install.packages(c('sf','tidyverse','ggplot2','leaflet.extras'))
library("sf")
library("tidyverse")
library("ggplot2")
library("leaflet.extras")

##### ------ Definition path: Data------ #####
setwd("/HASS2024/Day 3/German")

################ ---------------- Part 2: Integration methods -------------- ####################
##### -------- Clean environment --------- ########
rm(list=setdiff(ls(),c('path')))
#### --------- Work with Geospatial data ------- ####
### ---- Read the data ---- ####
Education = readRDS("data/education.rds")
#### --- Inspect the data --- ####
head(Education)

###### ----------- Definition -------- ########
# ‘School leavers’ are students who attained a Year 12 qualification in 2015 in any 
# State/ Territory through the completion of one or more Year 12 courses; 
# may include (unless noted otherwise below) adult students, 
# part-time students and students doing one 
# or more subjects to improve their overall score (repeating students).
Education = Education[,c('lga_code','lga_name','scl_leaver_part_hier_edu_2016_pc_hier_edu','scl_leaver_part_hier_edu_2016_enrld_hier_edu')]
colnames(Education) = c('lga_code','lga_name', 'School Leaver Participation In Higher Education 2016 % in higher education','School Leaver Participation In Higher Education 2016 Enrolled in higher education')
head(Education)

########### ---------- Geolocate: Join: ID ---------- #########
##### ---------- LGA information ------ #######
LGA = readRDS('data/lga.rds')
##### ---------- Transform ------ #######
LGA = LGA %>% st_set_crs(4283) 
LGA$geometry = st_transform(LGA$geometry, 4326)
##### ------ Filter: Victoria ------ #######
LGA = LGA[LGA$state_name_2016 =='Victoria',]
##### ---------- Simplification ------ #######
LGA = st_simplify(LGA, preserveTopology = TRUE, dTolerance = 1000)
#### ---- Map as image ----- #####
ggplot() + geom_sf(aes(), data=LGA$geometry)
dev.off()
#### --- Inspect the data --- ####
head(LGA)
#### ---- Filter only the important variables ---- #####
LGA = LGA[,c('lga_code_2016','geometry','state_name_2016')]
#### ----- Change the name ---- ####
colnames(LGA) = c('lga_code','geometry','state')

##### ------ Join using ID: LGA code ------ #######
Education = merge(LGA,Education,by='lga_code')
head(Education)

#### ------- Delete missing values ------ ####
Education = Education[!is.na(as.data.frame(Education)[,'School Leaver Participation In Higher Education 2016 Enrolled in higher education']),] 

###### ------ Simulation ------- #####
SimulationEducation = apply(Education, 1, function(x){data.frame('lga_name'=x[['lga_name']],'geometry'= st_sample(x[['geometry']], size=x[['School Leaver Participation In Higher Education 2016 Enrolled in higher education']]))})
SimulationEducation = do.call('rbind',SimulationEducation)
rownames(SimulationEducation) = NULL
#### ------ Dimensions ----- ###
dim(SimulationEducation)
#### ------ See how looks ----- ###
head(SimulationEducation)
ggplot() + geom_sf(aes(), data=SimulationEducation$geometry)
dev.off()
#### ----- Comparison ---- #####
table(SimulationEducation$lga_name)
Education[Education$lga_name=='Ballarat (C)',]


####### ------- SEIFA ------ ######
### ---- Read the data ---- ####
SEIFA = readRDS('data/seifa.rds')
##### ---------- transform ------ #######
SEIFA$geometry = st_transform(SEIFA$geometry, 4326)
#### -------- Filter: Victoria ----- ####
SEIFA = SEIFA[as.data.frame(SEIFA)$state=='VIC',]
#### --- Inspect the data --- ####
head(SEIFA)
SEIFA = SEIFA[,c('sa2_main16','irsd_score')]
#### ---- See how looks like the data ---- ####
ggplot() + geom_sf(aes(), data=st_simplify(SEIFA, preserveTopology = TRUE, dTolerance = 1000))
dev.off()

#### --------- Spatial Merge ------ #####
SEIFA = st_join(SEIFA,LGA)
head(SEIFA)
ggplot() + geom_sf(aes(), data=st_simplify(SEIFA, preserveTopology = TRUE, dTolerance = 1000))


################ ---------------- Part 3: Geospatial aggregation: -------------- ####################
SEIFA <- SEIFA %>% group_by(lga_code) %>% summarise(mean_irsd_score=mean(irsd_score))
#### ---- See how looks like the data ---- ####
ggplot() + geom_sf(aes(), data=SEIFA$geometry)
dev.off()

#### -------- Normalisation (Value - Min) / (Max - Min) -------- #####-
SEIFA$mean_irsd_score = round((SEIFA$mean_irsd_score - min(SEIFA$mean_irsd_score ))/(max(SEIFA$mean_irsd_score)-min(SEIFA$mean_irsd_score)),2)
SEIFA$mean_irsd_score = SEIFA$mean_irsd_score*100
#### ------- Simplification ------- #####
SEIFA = st_simplify(SEIFA, preserveTopology = TRUE, dTolerance = 1000)

################ ---------------- Part 4: Producing a map-------------- ####################
############### ----------- SEIFA ---------- ############### 
#### -------- Heatmap ------ ########
color_seifa = colorBin(
  palette= "viridis",
  domain=round(SEIFA$mean_irsd_score),
  bins = 7,
  pretty = TRUE,
  na.color = "#808080")

###### ------ Map ----- #####
SEIFA_MAP <- leaflet(SEIFA) %>% addTiles() %>% 
  addPolygons(fillColor = ~ color_seifa(mean_irsd_score), stroke = NA, fillOpacity = .7) %>%
  addLegend(pal = color_seifa, values = ~ mean_irsd_score)

SEIFA_MAP

############### ----------- Education ---------- ############### 
Education_score = as.data.frame(Education)[,c('School Leaver Participation In Higher Education 2016 % in higher education')]
Education$score = Education_score
#### ------- Simplification ------- #####

#### -------- Heatmap ------ ########
color_education = colorBin(
  palette= "viridis",
  domain=round(Education$score),
  bins = 7,
  pretty = TRUE,
  na.color = "#808080")

###### ------ Map ----- #####
Education_map <- leaflet(Education) %>% addTiles() %>% 
  addPolygons(fillColor = ~ color_education(score), stroke = NA, fillOpacity = .7) %>%
  addLegend(pal = color_education, values = ~ score)

Education_map


############### -----------  Descriptive Statistics  ---------- ############### 
####### -------- SEIFA Values --------- #######
SEIFA_values = as.data.frame(SEIFA)[,c('lga_code','mean_irsd_score')]
####### -------- Education Values --------- #######
Education_values = as.data.frame(Education)[,c('lga_code','score')]
###### ------ Merge by ID LGA ------ ######
SEIFA_education = merge(SEIFA_values,Education_values)

#### ------- Simple Correlation ------ #######
cor(SEIFA_education[,c("mean_irsd_score","score")])
