source("Libraries/Load_InstallLibraries.r")

###################################################
#---------- Read in Liquor License data ----------#
###################################################

# read in liquor licensing data
liquor_data <- readRDS("Data/Analysis/LiquorLicenceData.rds")

glimpse(liquor_data)

unique_liquor_data <- sapply(liquor_data, n_distinct)

# what type of licences are there?
liquor_data %>%
  count(Category) %>%
  arrange(desc(n))

#see Notes_on_Data_Selection.md re the following exclusions
liquor_data <- liquor_data %>%
  filter(Category != "Pre-retail Licence",
         Category != "Wine and Beer Producer's Licence")

print(colSums(is.na(liquor_data)))


##################################################################################################################################
# There appears to be duplicates in the data - at least at the address level. The same venue can have multiple licenses. 
# Need to remove these for our summarised data purposes later (otherwise double counting will occur for number of liquor licenses 
# within suburbs).
##################################################################################################################################

liquor_data_deduped <- liquor_data %>%
  arrange(Licensee, TradingAs, Address, Suburb) %>%
  distinct(Licensee,  Address, Suburb, .keep_all = TRUE) %>%
  mutate(Longitude = as.numeric(Longitude),
         Latitude = as.numeric(Latitude))

print(colSums(is.na(liquor_data_deduped)))


missing_latlon <- liquor_data_deduped %>%
  filter(is.na(Longitude)) %>%
  mutate(full_address = paste(Address, Suburb, Postcode, sep = ", "))

#---------------------------------
# Get geocoordinate data 
#---------------------------------

################################
# Use the googleway package to 
# get missing geocordinate data
################################

library(googleway)
key <- "AIzaSyCkvxRakBzHEimUYLlGswAD_RR9y9qwVvc"
set_key(key = key)

start_time <- Sys.time()

# uses function google_geocode() to get lat-lon coordinates for each element in the missing dataframe
res<-apply(missing_latlon,1,function (x){
  google_geocode(address=x[['full_address']], key= key)  
})

# now extract the lat and long elements of the list
geocodes<-lapply(seq_along(res),function(x) {
  coordinates<-res[[x]]$results$geometry$location[1,1:2] # need to subset as some of the results have more than one coordinates
})

# library(data.table)
# geocodes<-rbindlist(geocodes[1:755],fill=TRUE)

# using the rbindlist() function from the data.table package, extract the coordinates into a df

# do the first 755 observations
a <- rbindlist(geocodes[1:755],fill=TRUE)
# observation 756 is a NULL so create a 1x2 DF of NA
b <- data.frame(lat = NA, lng = NA)
# now get the rest of the coordinates
c <- rbindlist(geocodes[757:length(geocodes)],fill=TRUE)
# then combine them together into one DF
df <- bind_rows(a,b,c)

# create a vector of the license nums to join back on to the DF
LicenceNum <- missing_latlon$LicenceNum
df <- cbind(LicenceNum, df)

# join the results bacl to the DF of missing coordinates for the liuquor license data
d <- missing_latlon %>%
  select(-Latitude, -Longitude) %>%
  left_join(df, by = "LicenceNum") %>%
  rename(Latitude = lat, Longitude = lng)

# join the new data back to the liquor license dataset
liquor_data_deduped <- bind_rows(liquor_data_deduped, d)
# remove the old licenses that were missing coordinates
liquor_data_deduped <- filter(liquor_data_deduped, !is.na(Latitude))
# remove the concatenated address columns as not necessary any longer
liquor_data_deduped <- liquor_data %>% select(-Address)

(end_time <- Sys.time() - start_time)

# # save liquor data with lat-lon info in it from work done using googleway
# saveRDS(liquor_data_deduped, "Capstone/Data/Analysis/AssignmentData/liquor_data_deduped.rds")

##########################################
#---------- Read in Crime data ----------#
##########################################

# read in crime data
crime_data <- readRDS("Capstone/Data/Analysis/CrimeStatistics.rds")

head(crime_data)

# count the crime data by crime type
crime_counts <- crime_data %>%
  group_by(OffenceSubdivision) %>%
  summarise(total_incidents = sum(IncidentsRecorded))



#--------------------------------------------------------------------------
# Create dataframe of all suburbs between the two data sets
#--------------------------------------------------------------------------

# get a list of suburbs and postcodes from the liquor data
sub1 <- liquor_data_deduped %>%
  select(Suburb, Postcode) %>%
  distinct(., .keep_all = TRUE) %>%
  mutate_all(as.character)

# get a list of suburbs and postcodes from the crime data
sub2 <- crime_data %>%
  select(Suburb = Suburb_TownName, Postcode) %>%
  distinct(., .keep_all = TRUE) %>%
  mutate_all(as.character)

# join the two and remove any duplicates
suburbs_for_geo <- bind_rows(sub1, sub2) %>%
  distinct(., .keep_all = TRUE) %>%
  mutate(Address = paste(Suburb, Postcode, sep = ", "))

#------------------------------------------
# Get lat and lon for all suburbs
#------------------------------------------

hold <-apply(suburbs_for_geo,1,function (x){
  google_geocode(address=x[['Address']], key= key)  
})

hold_2 <- lapply(seq_along(hold),function(x) {
  coordinates<-hold[[x]]$results$geometry$location[1,1:2]
})

suburbs_no_geocode <- which(lapply(hold_2, is.null) == TRUE)

hold_2<-rbindlist(hold_2,fill=TRUE)

suburbs_for_geo_full <- suburbs_for_geo[-suburbs_no_geocode,] %>% cbind(hold_2)
suburbs_for_geo_missing <- suburbs_for_geo[suburbs_no_geocode,]

suburbs_for_geo <- bind_rows(suburbs_for_geo_full, suburbs_for_geo_missing)

saveRDS(suburbs_for_geo, "Data/Analysis/suburbs_geocord.rds")


suburbs_for_geo <- readRDS("Capstone/Data/Analysis/suburbs_geocord.rds")

# there are out of bounds geocodes
min_lon <- 140
max_lon <- 150
min_lat <- -39
max_lat <- -34

filtered_suburbs <- suburbs_for_geo %>%
  filter(lng < max_lon & lng > min_lon,
         lat < max_lat & lat > min_lat)

saveRDS(filtered_suburbs, "Capstone/Data/Analysis/AssignmentData/suburbs_geocord.rds")

filtered_suburbs <- readRDS("Data/Analysis/AssignmentData/suburbs_geocord.rds")



# Summarise geocordinates df, getting the average lat and lon for each postcode (because there are duplicates)
filtered_suburbs <- filtered_suburbs %>%
  mutate(Postcode = as.numeric(Postcode)) %>%
  group_by(Postcode) %>%
  summarise(Latitude = mean(lat),
            Longitude = mean(lng)) %>% ungroup()

# Join geocordinates to crime data
crime_data <- crime_data %>% 
  left_join(filtered_suburbs, by = "Postcode")


print(colSums(is.na(crime_data)))

saveRDS(crime_data, "Capstone/Data/Analysis/AssignmentData/crime_data.rds")






