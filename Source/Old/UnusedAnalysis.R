####################################
#---------- Housing data ----------#
####################################

# read in the housing data
raw_housing <- readxl::read_xls("Capstone/Data/Raw/house-Mar2018-final.xls")

# remove the first three rows and only select the suburb and median price
raw_housing <- raw_housing[-c(1:3),c(1,6)]

# rename column names
colnames(raw_housing) <- c("Suburb", "MedianPrice")

# write cleaned up data to use for later analysis
saveRDS(raw_housing, "Capstone/Data/Analysis/MedianHousePrices.rds")


#######################################
#---------- Population Data ----------#
#######################################

# read in population data
raw_population <- readxl::read_xls("Capstone/Data/Raw/32350ds0011_lga_summary_statistics_2016.xls", sheet = "Table 1")

# because the data comes in formatted excel, remove all rows and columns not needed
raw_population <- raw_population[-c(1:7), c(2,4,7,9,13)]

# rename column headings
colnames(raw_population) <- c("STName",	"LGAName", "PopTotal", "SexRatio", "MedianAge") 

# select only Victoria
raw_population <- raw_population %>%
  filter(STName == "Victoria")

# don't know what the parentheses are in the population data... will gsub them out an keep whatever is the LGA
raw_population$LGAName <- sub('\\(.*', '', raw_population$LGAName) %>% str_squish() #this removes the whitespace after the LGA

head(raw_population)

# where are the NAs?
print(colSums(is.na(raw_population)))

# remove the NAs in the LGA
raw_population <- raw_population %>%
  filter(!is.na(LGAName))

# coerce pop_total, 
raw_population[c(3:5)] <- sapply(raw_population[c(3:5)], as.numeric)

# write cleaned up data to use for later analysis
saveRDS(raw_population, "Capstone/Data/Analysis/PopulationLGA.rds")


