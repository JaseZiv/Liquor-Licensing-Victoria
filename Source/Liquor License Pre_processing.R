# load libraries
library(tidyverse)
library(DataExplorer)
library(rvest)

#####################################################
#---------- Read in Liquor Licensing Data ----------#
#####################################################

# https://www.data.vic.gov.au/data/dataset/victorian-liquor-licences-by-location

# read in liquor licence data
raw_liquor <- readxl::read_xlsx("Data/Raw/current_victorian_licences_by_location_august_2018.xlsx", col_names = F)

# inspect the raw data
head(raw_liquor)

# columns X__13 and X__14 are breaks for formatting. Remove them
raw_liquor <- raw_liquor %>%
  select(-X__13, -X__14)

# the variable names are in the third row. Need to get them out of row 3 and into variable names
names(raw_liquor) <- raw_liquor[3,]

# now remove the first three lines of useless data
raw_liquor <- raw_liquor[-c(1:3),]

# look at the data again
head(raw_liquor)

# headers have spaces and we don't want that - extract a vector of variable names and remove spaces
clean_up_header <- names(raw_liquor)

# use stringr package to remove spaces in variable names
clean_up_header <- str_replace_all(string = clean_up_header, pattern = " ", replacement = "")
clean_up_header <- str_replace_all(string = clean_up_header, pattern = "/", replacement = "_")
# replace the variable names with the cleaned up version
colnames(raw_liquor) <- clean_up_header

# now have a look again
head(raw_liquor)

# how many NAs are in each variable?
print(colSums(is.na(raw_liquor)))

####################################
#---------- Analyse data ----------#
####################################

glimpse(raw_liquor)

unique_liquor_data <- sapply(raw_liquor, n_distinct)

# what type of licences are there?
raw_liquor %>%
  count(Category) %>%
  arrange(desc(n))

#see Notes_on_Data_Selection.md re the following exclusions
raw_liquor <- raw_liquor %>%
  filter(Category != "Pre-retail Licence",
         Category != "Wine and Beer Producer's Licence")

print(colSums(is.na(raw_liquor)))


##################################################################################################################################
# There appears to be duplicates in the data - at least at the address level. The same venue can have multiple licenses. 
# Need to remove these for our summarised data purposes later (otherwise double counting will occur for number of liquor licenses 
# within suburbs).
##################################################################################################################################

liquor_data_deduped <- raw_liquor %>%
  arrange(Licensee, TradingAs, Address, Postcode) %>%
  distinct(Licensee,  Address, Postcode, .keep_all = TRUE) %>%
  mutate(Longitude = as.numeric(Longitude),
         Latitude = as.numeric(Latitude))

print(colSums(is.na(liquor_data_deduped)))


#---------------------------------
# Get geocoordinate data 
#---------------------------------

################################
# Use the googleway package to 
# get missing geocordinate data
################################

# first, get a subset of all the missing lat/lon observations
missing_latlon <- liquor_data_deduped %>%
  filter(is.na(Longitude)) %>%
  mutate(full_address = paste(Address, Suburb, Postcode, sep = ", "))


#---------------------------
# googleway steps:
#---------------------------
source("Source/AddGeocoordinates.R")
#---------------------------

# also need to clean up council names to be able to join on to crime data
liquor_data_deduped$Council <- str_remove(liquor_data_deduped$Council, " CITY COUNCIL")
liquor_data_deduped$Council <- str_remove(liquor_data_deduped$Council, " SHIRE COUNCIL")
liquor_data_deduped$Council <- str_remove(liquor_data_deduped$Council, " RURAL")
liquor_data_deduped$Council <- str_remove(liquor_data_deduped$Council, " BOROUGH COUNCIL")

# remove variables not needed for analysis
liquor_data_deduped <- liquor_data_deduped %>%
  select(-starts_with("Postal"), -full_address) %>%
  select(-TradingHours, -After11pm, -MaximumCapacity, -Gaming)


# save file for later analysis
saveRDS(liquor_data_deduped, "Data/Analysis/liquor_data_deduped.rds")


#######################################
#---------- Population Data ----------#
#######################################

# read in population data
raw_population <- readxl::read_xls("Data/Raw/32350ds0011_lga_summary_statistics_2016.xls", sheet = "Table 1")

# because the data comes in formatted excel, remove all rows and columns not needed
raw_population <- raw_population[-c(1:7), c(2,4,7,9,13)]

# rename column headings
colnames(raw_population) <- c("STName",	"LGAName", "Population", "SexRatio", "MedianAge") 

# select only Victoria
raw_population <- raw_population %>%
  filter(STName == "Victoria")

# don't know what the parentheses are in the population data... will gsub them out an keep whatever is the LGA
raw_population$LGAName <- sub('\\(.*', '', raw_population$LGAName) %>% str_squish() #this removes the whitespace after the LGA

raw_population$LGAName <- toupper(raw_population$LGAName)

head(raw_population)

# where are the NAs?
print(colSums(is.na(raw_population)))

# remove the NAs in the LGA
raw_population <- raw_population %>%
  filter(!is.na(LGAName)) %>%
  select(-STName)

# coerce pop_total, 
raw_population[c(2:4)] <- sapply(raw_population[c(2:4)], as.numeric)


##########################################
#---------- LGA Square KM Data ----------#
##########################################
# define the url
url <- "https://en.wikipedia.org/wiki/Local_government_areas_of_Victoria"

# set url as a html object
webpage <- read_html(url)

# read in info
scraped_LGA_data <- html_table(webpage, fill = TRUE)

# separate out first table as it has an additional column
tab1 <- scraped_LGA_data[[1]]

tab1 <- tab1[,c(1,5)]
names(tab1) <- c("LocalGovernmentArea", "Area_KMSqr")
tab1 <- tab1[-1,]


# loop through the remaining 8 tables, extracting only the columns required
LGA_DF <- data.frame()

for(i in 2:9) {
  df <- scraped_LGA_data[[i]]
  df <- df[,c(1,5)]
  names(df) <- c("LocalGovernmentArea", "Area_KMSqr")
  df <- df[-1,]
  
  LGA_DF <- rbind(LGA_DF, df)
}

# bing the first table and the rest of the scraped tables together
LGA_DF <- rbind(tab1, LGA_DF)

# convert LGA to uppercase
LGA_DF$LocalGovernmentArea <- toupper(LGA_DF$LocalGovernmentArea)

# Remove prefixes at the beginning or end of LGA name
LGA_DF$LocalGovernmentArea <- str_remove(LGA_DF$LocalGovernmentArea, " CITY COUNCIL")
LGA_DF$LocalGovernmentArea <- str_remove(LGA_DF$LocalGovernmentArea, " SHIRE COUNCIL")
LGA_DF$LocalGovernmentArea <- str_remove(LGA_DF$LocalGovernmentArea, "SHIRE OF ")
LGA_DF$LocalGovernmentArea <- str_remove(LGA_DF$LocalGovernmentArea, "CITY OF ")
LGA_DF$LocalGovernmentArea <- str_remove(LGA_DF$LocalGovernmentArea, " SHIRE")
LGA_DF$LocalGovernmentArea <- str_remove(LGA_DF$LocalGovernmentArea, "RURAL")
LGA_DF$LocalGovernmentArea <- str_remove(LGA_DF$LocalGovernmentArea, "BOROUGH OF ")

# remove any unnecessary whitespace
LGA_DF$LocalGovernmentArea <- str_squish(LGA_DF$LocalGovernmentArea)

# need to add a "-" to Colac Otway: Need to do this to make joining possible later
LGA_DF$LocalGovernmentArea[LGA_DF$LocalGovernmentArea == "COLAC OTWAY"] <- "COLAC-OTWAY"

# remove commas and other special characters from numeric variables and convert to numeric
LGA_DF$Area_KMSqr <- str_remove(LGA_DF$Area_KMSqr, ",") %>% as.numeric()


# Join population and area dataframes together
LGA_data <- raw_population %>%
  left_join(LGA_DF, by = c("LGAName" = "LocalGovernmentArea")) %>%
  filter(LGAName != "UNINCORPORATED VIC")

# save LGA data to disk
saveRDS(LGA_data, "Data/Analysis/LGA_Data.rds")

























