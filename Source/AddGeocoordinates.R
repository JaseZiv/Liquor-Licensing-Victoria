
#---------------------------------
# Get geocoordinate data 
#---------------------------------

################################
# Use the googleway package to 
# get missing geocordinate data
################################

library(googleway)
key <- "KEY" #enter user's own API key
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

library(data.table)
geocodes<-rbindlist(geocodes[1:755],fill=TRUE)

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

