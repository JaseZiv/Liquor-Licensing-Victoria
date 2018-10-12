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


##########################################
#---------- Read in Crime data ----------#
##########################################

# read in crime data
crime_data <- readRDS("Data/Analysis/CrimeStatistics.rds")

head(crime_data)

# count the crime data by crime type
crime_counts <- crime_data %>%
  group_by(OffenceSubdivision) %>%
  summarise(total_incidents = sum(IncidentsRecorded))


########################################################
#---------- Summarised crime and liquor data ----------#
########################################################

# count the total incidents by suburb
crime_suburb_counts <- crime_data %>%
  group_by(Postcode, Suburb_TownName) %>%
  summarise(total_incidents = sum(IncidentsRecorded)) %>% ungroup()

# create a df of the number of liquor licences per suburb
liquor_by_suburb <- liquor_data %>%
  filter(Suburb != "[Not applicable] -") %>%
  group_by(Suburb) %>%
  summarise(liq_lic_count = n()) %>% ungroup()


# join the crime and liquor license counts
crime_and_liquor <- crime_suburb_counts %>%
  left_join(liquor_by_suburb, by = c("Suburb_TownName" = "Suburb"))

# write the data
saveRDS(crime_and_liquor, "Data/Analysis/CrimeAndLiquorJoined.rds")

print(colSums(is.na(crime_and_liquor)))

crime_and_liquor <- crime_and_liquor %>%
  filter(!is.na(liq_lic_count))
  

#---------- Plotting Visualisations ----------#

##### Histograms #####
ggplot(data = filter(crime_and_liquor, Suburb_TownName != "MELBOURNE"), aes(x= log(total_incidents))) +
  geom_histogram()


ggplot(data = filter(crime_and_liquor, Suburb_TownName != "MELBOURNE"), aes(x= log(liq_lic_count))) +
  geom_histogram()


ggplot(data = crime_and_liquor, aes(x=liq_lic_count, y= total_incidents)) + 
  geom_point() +
  labs(title = "Number of liquor licences and criminal incidents", 
       subtitle = "Is there a relationship between crime and liquor licenses?",
       x = "Liquor Licenses", y = "Criminal Incidents")

crime_liquor_cor <- cor(x=crime_and_liquor$liq_lic_count, y=crime_and_liquor$total_incidents, method = "pearson")

# remove the outlier, which is Melbourne
ggplot(data = filter(crime_and_liquor, Suburb_TownName != "MELBOURNE"), aes(x=liq_lic_count, y= total_incidents)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Number of liquor licences and criminal incidents", 
       subtitle = "Is there a relationship between crime and liquor licenses?",
       x = "Liquor Licenses", y = "Criminal Incidents") 




fit.lm <- lm(total_incidents ~ liq_lic_count, data = crime_and_liquor)
summary(fit.lm)

plot(fit.lm)

