# #############################################################################################
# ########################################    MAIN    #########################################
# #############################################################################################
rm(list=ls())

##### ------ Install GeoSocial ----- #####
# install.packages('geosocial_1.0.tar.gz',repos = NULL, type = 'source',dependencies=TRUE)
# remove.packages("geosocial")

##### ------ Load GeoSocial ----- #####
library(geosocial)
########## ------- Load parameters --------- ######
LoadParameters(file = "parameters.json")
##### ------ Create folders ------ #####
CreateFolders()
##### ------ Create log file ------ #####
# logNm <- 'test.log'
# GenerateLog(name = logNm)

######### ---------- 0. Read parameters ------- ###########
LSAY_cohort = ParsingParameter('LSAY_cohort')
LSAY_waves = as.numeric(ParsingParameter('LSAY_waves'))
LSAY_topics = ParsingParameter('LSAY_topics')
SurveyFiles = ParsingParameter('SurveyFiles')
TSP_year = ParsingParameter('TSP_year')
TSP_variables = ParsingParameter('TSP_variables')

####### -------------- 1. Loading data ----------- #########
###### -------- Load LSAY ------- ####
LSAY_2009 = LoadLSAY(wave = NULL,cohort =
                       LSAY_cohort,LSAY_topics = NULL)


##### ---- Extract survey responses --- #####
SurveyResponses = LSAY_2009[['SurveyResponses']]
##### ---- Exctract GeospatialResponses --- #####
GeospatialResponses = LSAY_2009[['GeospatialResponses']]

### -------- Load TSP 2021  -------- #######
TSP_2021 = LoadTSP2021(year=TSP_year,variables = TSP_variables)
### -------- Extract data ------ #####
TSP_2021_data = TSP_2021[['data']]
### -------- Extract metada ------ #####
TSP_2021_metadata = TSP_2021[['metadata']]


####### -------------- 2. Geospatial data linkage ----------- #########
####### ----------- 2.1 POAS TO SA3 ----------- ######
SurveyResponses_SA3 = LSAY_POA_SA3(data=GeospatialResponses,
                                   concordances=concordances)
###### --------- SA3 ------- ########
SurveyResponses_SA3_IN = SurveyResponses_SA3[['sa3']]
###### --------- ABS Metric ------- ########
SurveyResponses_SA3_metric = SurveyResponses_SA3[['metric']]
###### --------- ABS Concordances ------- ########
SurveyResponses_SA3_concordances = as.data.frame(SurveyResponses_SA3[['concordances']])
###### --------- ABS Ratio ------- ########
SurveyResponses_SA3_ratio = as.data.frame(SurveyResponses_SA3[['ratio']])

####### ----------- STAGE 2.2: SA3 TO SA3 2021 ----------- ######
SurveyResponses_SA3_TSP = LSAY_PSA3_SA3(data=SurveyResponses_SA3_IN,
                                        concordances=concordances,
                                        year_out = 2021)
###### --------- SA3 ------- ########
SurveyResponses_SA3_TSP_OUT = SurveyResponses_SA3_TSP[['sa3']]
###### --------- ABS Metric ------- ########
SurveyResponses_SA3_TSP_metric = SurveyResponses_SA3_TSP[['metric']]
###### --------- ABS Concordances ------- ########
SurveyResponses_SA3_TSP_concordances = as.data.frame(SurveyResponses_SA3_TSP[['concordances']])
###### --------- ABS Ratio ------- ########
SurveyResponses_SA3_TSP_ratio = as.data.frame(SurveyResponses_SA3_TSP['ratio'])

####### ------- GeoSpatial join ------ #####
DataJoined = GeoSpatialJoin(year=LSAY_waves,
                            GeospatialResponses=SurveyResponses_SA3_TSP_OUT,
                            SurveyResponses=SurveyResponses,
                            TSP_data = TSP_2021_data,
                            TSP_metadata = TSP_2021_metadata)

############ --------- Geospatial report --------- ############
summaryResults = SummaryReport(concordances_POA= SurveyResponses_SA3_concordances,
                               concordances_SA3 = SurveyResponses_SA3_TSP_concordances,
                               metrics_POA = SurveyResponses_SA3_metric,
                               metrics_SA3 = SurveyResponses_SA3_TSP_metric)

######## -------- Write the summary report --------- #######
openxlsx::write.xlsx(x=summaryResults,
                     file='outputs/Summary.xlsx')

####### ------- Write stata ------ #####
WriteStata(path = 'outputs/', waves = LSAY_waves,
           DataJoined = DataJoined,
           SurveyResponses = SurveyResponses)

####### -------- Read data ------ #######
new_data = haven::read_stata("outputs/LSAY09_TSP_2011.dta") 
View(new_data)

