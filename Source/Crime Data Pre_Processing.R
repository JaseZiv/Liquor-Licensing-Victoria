##################################
#---------- Crime Data ----------#
##################################

# read in the crime data
raw_crime <- readxl::read_xlsx("Data/Raw/Data_tables_Criminal_Incidents_Visualisation_year_ending_June_2018.xlsx", sheet = "Table 05and06")


# headers have spaces and we don't want that - extract a vector of variable names and remove spaces
clean_up_header_crime <- names(raw_crime)

# use stringr package to remove spaces in variable names
clean_up_header_crime <- str_replace_all(string = clean_up_header_crime, pattern = " ", replacement = "")
clean_up_header_crime <- str_replace_all(string = clean_up_header_crime, pattern = "/", replacement = "_")
# replace the variable names with the cleaned up version
colnames(raw_crime) <- clean_up_header_crime

raw_crime <- rename(raw_crime,  RatePer100k = `Rateper100,000population`)


# # will not do this now: will use population data from the next bit of analysis (Population Data)
# calculate population: done using this link https://www.njsp.org/info/ucr2000/pdf/calc_ucr2000.pdf
# raw_crime <- raw_crime %>%
#   mutate(Population = round(IncidentsRecorded / (RatePer100k/100000),0))

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


#-----------------------------------------
# Join Crime and population data
#-----------------------------------------

crime_and_pop <- raw_crime %>%
  left_join(raw_population, by = c("LocalGovernmentArea" = "LGAName"))


print(colSums(is.na(crime_and_pop)))

# write file for later analysis
saveRDS(crime_and_pop, "Data/Analysis/crime_and_pop.rds")
