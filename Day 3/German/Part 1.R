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
setwd("~/HASS2024/Day 3/German")


################ ---------------- Part 1: Operations with geospatial data -------------- ####################
#### ------- Geospatial operations
AustraliaStates = read_sf("data/ste_2016_aust.gml")
###### ------ Set CRS: Identify the CRS ------ ######
AustraliaStates = AustraliaStates %>% st_set_crs(4283) 
variables = grep(colnames(AustraliaStates),pattern = "state_name_2016|geom")
AustraliaStates = AustraliaStates[,variables]
###### ------ Simplification ------ ######
AustraliaStates = st_simplify(AustraliaStates, preserveTopology = TRUE, dTolerance = 100)
### -------- Graphic: Image------- #### 
ggplot() + geom_sf(aes(), data=AustraliaStates) 
dev.off()

#### ------ Union: All States----- #### 
Australia = st_union(AustraliaStates[,2] )
Australia = st_transform(Australia, 4326)
###### ------ Simplification ------ ######
Australia = st_simplify(Australia, preserveTopology = TRUE, dTolerance = 1000)
### -------- Graphic ------- #### 
ggplot() + geom_sf(aes(), data=Australia) 
dev.off()

#### --------- Filter: Australian Capital Territory ACT ------ ##### 
ACT = AustraliaStates[AustraliaStates$state_name_2016=='Australian Capital Territory','state_name_2016']
ACT = st_transform(ACT[,2], 4326)
### -------- Graphic ------- #### 
ggplot() + geom_sf(aes(), data=ACT) 
dev.off()

##### ---------- Intersects ------- #########
##### -------  Sparse: logical; should a sparse index list be returned (TRUE) or a dense logical matrix --- ##
st_intersects(Australia,ACT,sparse = FALSE)

##### ---- ACT inside Australia --- ###
st_within(ACT,Australia,sparse = FALSE)

##### ---- Australia inside ACT --- ###
st_within(Australia,ACT,sparse = FALSE)

#### ------ Difference ---- ###
diff_act = st_difference(Australia,ACT)
ggplot() + geom_sf(aes(), data=diff_act) 
dev.off()

#### ------ Convex HULL  ---- ###
ACT_hull = st_convex_hull(ACT)
ggplot() + geom_sf(aes(), data=ACT_hull) +  geom_sf(aes(), data=ACT) 
dev.off()

####### -------- NSW Buffer ------- ######
ACT_buffer = st_buffer(ACT, dist = 10)
ggplot() + geom_sf(aes(), data=ACT_buffer) +  geom_sf(aes(), data=ACT) 
dev.off()

####### -------- Area  ------- ######
st_area(ACT)
### ----~ 2,358 km^ 2 ----- ##### 

## ----- Boundaries & Centroid --- #### 
boundaries_act = st_boundary(ACT)
centroid_act = st_centroid(ACT)
ggplot() + geom_sf(aes(), data=boundaries_act) + geom_sf(aes(), data=centroid_act) 
dev.off()


## ------ Task 1: 10 minutes ---- ####
#### --------- 1. Filter: the data only for Victoria ------ ##### 
#### --------- 2. Intersects the shape of Victoria with Australia ------ ##### 
#### --------- 3. Evaluate if Victoria is inside Australia ----- #####
#### --------- 4. Find the difference between Australia and Victoria

