# This is code to setup the tutorial - not for running by students

#################################################################################################
# Example code to download/import NHANES data files (SAS transport .XPT files) as a dataset     #
# For R                                                                                         #
#################################################################################################
  
## Note to tutorial users: you must update some lines of code (e.g. file paths) 
##  to run this code yourself. Search for comments labeled "TutorialUser"

# Include Foreign Package To Read SAS Transport Files
library(foreign)
library(dplyr)

###########################################################################
## Example 1: import SAS transport file that is saved on your hard drive ##
###########################################################################
  
# First, download the NHANES 2015-2016 Demographics file and save it to your hard drive #
# from: https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2015 #
# You may need to right-click the link to the data file and select "Save target as..." #
    
# Create data frame from saved XPT file
# TutorialUser: update the file path here
# for Windows users, be sure to change the slashes between directories to a forward slash / (as on Mac or Unix) 
#  or to double backslashes \\

#DEMO_I <- read.xport("C:\\NHANES\\DATA\\DEMO_I.xpt")
#DEMO_I2 <- read.xport("C:/NHANES/DATA/DEMO_I.xpt")

# this code with typical Windows single backslashes between directories will throw an error
#DEMO_I <- read.xport("C:\NHANES\DATA\DEMO_I.xpt")

# save as an R data frame
# TutorialUser: update the file path here to a directory where you want to save the data frame 
#saveRDS(DEMO_I, file="C:\\NHANES\\DATA\\DEMO_I.rds")


############################################################################
## Example 2: Download and import the transport file through R             #
############################################################################

# Download NHANES 2015-2016 to temporary file    
#download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DEMO_I.XPT", tf <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DEMO_I.xpt", tf <- tempfile(), mode="wb")

# Create Data Frame From Temporary File
DEMO_I3 <- foreign::read.xport(tf)
    
# save as an R data frame
# TutorialUser: update the file path here to a directory where you want to save the data frame 
saveRDS(DEMO_I3, file="data/DEMO_I.rds")    

# note, documentation on this dataset is here: 
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DEMO_I.htm

nhanes <- readRDS("data/DEMO_I.rds")
write.csv(nhanes, "data/nhanes.csv")

# clean up and then write out as Stata format to preserve labels

nhanes$sex <- factor(nhanes$RIAGENDR, levels = c(1,2), labels = c("Masculino", "Feminino"))
nhanes$marital <- factor(nhanes$DMDMARTL, 
                         levels = c(1:6, 77, 99),
                         labels = c("Married", "Widowed", "Divorced", "Separated",
                                    "Never Married", "Living with partner",
                                    "Refused", "Don't Know"))

nhanes$hhref_edu <- factor(nhanes$DMDHREDU, levels = c(1:5, 7, 9),
                           labels = c("<9th Grade", "9-11th Gr", "HS/GED", 
                                      "Some Col.", ">=Col.", 
                                      "Refused", "Don't Know"))

nhanes$hh_inc <- factor(nhanes$INDHHIN2,
                        levels = c(1:10,12:15, 77, 99),
                        labels = c("<5k",
                                   "5-<10k",
                                   "10-<15k",
                                   "15-<20k",
                                   "20-<25k",
                                   "25-<35k",
                                   "35-<45k",
                                   "45-<55k",
                                   "55-<65k",
                                   "65-<75k",
                                   "20k+",
                                   "<20k",
                                   "75-<99k",
                                   "100k+",
                                   "Refused", "Don't Know"))

nhanes$race <- factor(nhanes$RIDRETH1,
                      levels = c(1:5),
                      labels = c("Mexican American",
                                 "Other Hispanic",
                                 "Non-Hispanic White",
                                 "Non-Hispanic Black",
                                 "Other Race - Including Multi-Racial"))

nhanes$time_us <- factor(nhanes$DMDYRSUS,
                         levels = c(1:9, 77, 99),
                         labels = c("<1 yrs", "1-<5 yrs", "5-<10 yrs", "10-15 yrs", "15-<20 yrs", "20-<30 yrs", 
                                    "30-<40 yrs", "40-<50 yrs", "50+ yrs",
                                    "Refused", "Don't Know"))
attr(nhanes$sex, "label") <- "Gender"
attr(nhanes$marital, "label") <- "Marital status"
attr(nhanes$hhref_edu, "label") <- "HH Ref person's education"
attr(nhanes$hh_inc, "label") <- "HH income"
attr(nhanes$race, "label") <- "Race/Hispanic Origin"
attr(nhanes$time_us, "label") <- "Time in US"

attr(nhanes, "label") <- "Demo NHANES 2015-16 dataset"

write_dta(nhanes %>% select(sex, marital, hhref_edu, hh_inc, race, RIDAGEYR, WTINT2YR, SDMVPSU, SDMVSTRA), "data/nhanes.dta")

# check
nhanes2 <- read_dta("data/nhanes.dta")
nhanes2$sex <- as_factor(nhanes2$sex)
table(nhanes2$sex)
rm(nhanes2)
