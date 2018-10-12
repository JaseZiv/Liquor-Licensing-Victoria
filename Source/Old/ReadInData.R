# Load Libraries
source("Capstone/Libraries/Load_InstallLibraries.r")

#############################################
#---------- Liquor Licensing Data ----------#
#############################################

# https://www.data.vic.gov.au/data/dataset/victorian-liquor-licences-by-location

# read in liquor licence data
raw_liquor <- readxl::read_xlsx("Capstone/Data/Raw/current_victorian_licences_by_location_august_2018.xlsx", col_names = F)

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

# write cleaned up data to use for later analysis
saveRDS(raw_liquor, "Capstone/Data/Analysis/LiquorLicenceData.rds")


########################################
#---------- Crime Stats Data ----------#
########################################

# https://www.crimestatistics.vic.gov.au/sites/default/files/embridge_cache/emshare/original/public/users/201808/47/f05431fd0/Data_tables_Criminal_Incidents_Visualisation_year_ending_March_2018.xlsx

# read in the crime data
raw_crime <- readxl::read_xlsx("Capstone/Data/Raw/Data_tables_Criminal_Incidents_Visualisation_year_ending_March_2018.xlsx", sheet = "Table 07")

# inspect the raw data
head(raw_crime)
dim(raw_crime) #277,753 observations, 7 variables
glimpse(raw_crime)

# headers have spaces and we don't want that - extract a vector of variable names and remove spaces
clean_up_header_crime <- names(raw_crime)

# use stringr package to remove spaces in variable names
clean_up_header_crime <- str_replace_all(string = clean_up_header_crime, pattern = " ", replacement = "")
clean_up_header_crime <- str_replace_all(string = clean_up_header_crime, pattern = "/", replacement = "_")
# replace the variable names with the cleaned up version
colnames(raw_crime) <- clean_up_header_crime

# now have a look again
head(raw_crime)

# write cleaned up data to use for later analysis
saveRDS(raw_crime, "Capstone/Data/Analysis/CrimeStatistics.rds")
