rm(list = ls());gc()

library(tidyverse)
library(tidytext)
library(ggExtra)
library(grid)
library(ggmap)

###########################################################
#---------- Liquor Licencing analysis by Suburb ----------#
###########################################################

liquor_data_deduped <-  readRDS("Data/Analysis/liquor_data_deduped.rds")

liquor_data_deduped$After11pm <- as.numeric(liquor_data_deduped$After11pm)
liquor_data_deduped$MaximumCapacity <- as.numeric(liquor_data_deduped$MaximumCapacity)


head(liquor_data_deduped)

liquor_data_deduped %>%
  group_by(Category) %>%
  summarise(NumMissing = sum(is.na(MaximumCapacity)))


# which categories are the most awarded licenses?
liquor_data_deduped %>%
  count(Category) %>%
  ggplot(aes(x= reorder(Category, n), y= n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_label(aes(label = n), size = 8) +
  theme_minimal() +
  ggtitle("Types of licences are awarded") +
  theme(axis.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_text(size = 14), plot.title = element_text(size = 22)) +
  coord_flip() 

#######
# NOTE:
#######
# Late night (packaged liquor) Licence has only two licences. Will merge this with Packaged Liquor Licence
liquor_data_deduped$Category[liquor_data_deduped$Category == "Late night (packaged liquor) Licence"] <- "Packaged Liquor Licence"

# which categories are the most awarded licenses?
liquor_data_deduped %>%
  count(Category) %>%
  ggplot(aes(x= reorder(Category, n), y= n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_label(aes(label = n), size = 8) +
  theme_minimal() +
  ggtitle("Figure 1. Types of licences are awarded") +
  theme(axis.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_text(size = 14), plot.title = element_text(size = 24)) +
  coord_flip() 

# # which regions are the most awarded licenses?
# liquor_data_deduped %>%
#   count(Region) %>%
#   arrange(desc(n)) %>%
#   head(n=20) %>%
#   ggplot(aes(x= reorder(factor(Region), n), y= n)) +
#   geom_bar(stat = "identity", fill = "steelblue") +
#   geom_label(aes(label = n), size = 8) +
#   theme_minimal() +
#   ggtitle("Regions with the most Licences") +
#   theme(axis.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_text(size = 14), plot.title = element_text(size = 22)) +
#   coord_flip()


# top 10 suburbs by region

# first need to create a DF to be able to have top 5 suburbs ordered correctly
liquor_by_region_suburb <- 
  liquor_data_deduped %>%
  group_by(Region) %>%
  count(Suburb, Region, Metro_Regional, sort = TRUE) %>%
  slice(seq_len(5)) %>%
  ungroup() %>%
  arrange(Region, n) %>%
  mutate(row = row_number())

# now plot
liquor_by_region_suburb %>%
  ggplot(aes(x= row, y=n)) +
  geom_col(aes(fill = Metro_Regional)) +
  theme_bw() +
  scale_x_continuous(  # This handles replacement of row 
    breaks = liquor_by_region_suburb$row, # notice need to reuse data frame
    labels = liquor_by_region_suburb$Suburb) +
  scale_fill_manual(values = c("orange", "steelblue"), name = "") +
  coord_flip() +
  labs(title = "Figure 2. Suburbs with the most licenses", subtitle = "Top 5 suburbs in each region") +
  facet_wrap(~Region, scales = "free_y", drop = TRUE, ncol = 2) +
  theme_bw() +
  theme(axis.title = element_blank(), plot.title = element_text(size = 24), plot.subtitle = element_text(size = 17), 
        axis.text.y = element_text(size = 13), axis.text.x = element_text(size = 13), strip.text = element_text(size = 12))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dry Suburb Analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create a vector of dry suburbs
dry_suburbs <- toupper(c("Ashburton", "Glen Iris", "Camberwell", "Canterbury", "Balwyn", "Balwyn North", "Mont Albert", 
                         "Mont Albert North", "Box Hill", "Box Hill South", "Box Hill North"))


# create a variable to indicate whether the suburb is dry or not
liquor_data_deduped$DryArea <- liquor_data_deduped$Suburb %in% dry_suburbs

# how many licences per suburb
suburb_count <- liquor_data_deduped %>%
  group_by(Suburb, Metro_Regional, DryArea) %>%
  summarise(NumLicenses = n()) %>% ungroup()

by(suburb_count$NumLicenses, suburb_count$Metro_Regional, summary)

# plot
suburb_count %>%
  ggplot(aes(x= Metro_Regional, y= NumLicenses, fill = Metro_Regional)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,300)) +
  labs(title = "Figure 5. Distribution of Licenced Venues", subtitle = "How many licences do suburbs have?", y= "Number of licenses per suburb") +
  scale_fill_manual(values = c("orange", "steelblue")) +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_text(size = 15), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 24), plot.subtitle = element_text(size = 17))


# distribution of licences in dry suburbs VS rest of Metro
suburb_count %>%
  filter(Metro_Regional == "Metro") %>%
  ggplot(aes(x= NumLicenses, fill = DryArea)) +
  geom_density(alpha = 0.5, adjust = 1.5) +
  coord_cartesian(xlim = c(0,300)) +
  scale_fill_manual(values = c("grey", "palegreen4")) +
  theme_minimal() +
  labs(title = "Figure 3. Is a dry area really dry?", 
       subtitle = "Distribution of licences between the dry area and the rest of metro suburbs", x= "Licences in Suburbs") +
  theme(axis.title.y = element_blank(), plot.title = element_text(size = 24), plot.subtitle = element_text(size = 17), 
        axis.text.y = element_text(size = 13), axis.title.x = element_text(size = 15), axis.text.x = element_text(size = 13), 
        legend.text = element_text(size = 13), legend.title = element_text(size = 15))

by(suburb_count$NumLicenses, suburb_count$DryArea, summary)

sum_stats <- mosaic::favstats(suburb_count$NumLicenses ~ suburb_count$DryArea)

mosaic::favstats(suburb_count$NumLicenses ~ suburb_count$Metro_Regional)


liquor_data_deduped %>%
  filter(Metro_Regional == "Metro") %>%
  count(Category, DryArea) %>%
  ggplot(aes(x= DryArea, y= n, fill = Category)) +
  geom_bar(stat = "identity", position = "fill", colour = "black") +
  labs(title = "Types of Licences in the Dry Area", x= "Dry or Not") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette="Spectral") +
  theme_bw() +
  theme(axis.title.y = element_blank(), plot.title = element_text(size = 22), 
        axis.text.y = element_text(size = 13), axis.title.x = element_text(size = 15), axis.text.x = element_text(size = 13), 
        legend.text = element_text(size = 13), legend.title = element_text(size = 15))


# create a DF to create the following lollipop plot:
liquor_lollipop <- liquor_data_deduped %>%
  filter(Metro_Regional == "Metro") %>%
  group_by(DryArea, Category) %>%
  summarise(n = n()) %>%
  mutate(ProportionCategory = n / sum(n)) %>% ungroup() %>%
  select(-n) %>%
  spread(key = DryArea, value = ProportionCategory)

names(liquor_lollipop) <- c("Category", "NonDry", "Dry")

# https://www.r-graph-gallery.com/303-lollipop-plot-with-2-values/
ggplot(data = liquor_lollipop) +
  geom_segment(aes(x= Category, xend = Category, y = NonDry, yend = Dry), color = "grey", size = 1) +
  geom_point(aes(x = Category, y= NonDry), shape = 21, color = "black", fill = "grey", size = 6) +
  geom_point(aes(x = Category, y= Dry), shape = 21, color = "black", fill = "palegreen4", size = 6) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Figure 4. Types of Licences in the Dry Area", subtitle = "Green points indicate a greater proportion in the Dry Area") +
  coord_flip() +
  theme_minimal() +
  theme(axis.title = element_blank(), plot.title = element_text(size = 24), plot.subtitle = element_text(size = 17), 
        axis.text.y = element_text(size = 15), axis.text.x = element_text(size = 15))
  


# most licences per licensee
liquor_data_deduped %>%
  count(Licensee) %>%
  arrange(desc(n)) %>%
  head(n=20) %>%
  ggplot(aes(x= reorder(Licensee,n), y= n)) +
  geom_bar(stat = "identity") +
  coord_flip()


# venue_names <- liquor_data_deduped %>%
#   unnest_tokens(word, TradingAs, drop = F) %>%
#   anti_join(stop_words, by = "word")
# 
# # define a nice color palette
# pal <- RColorBrewer::brewer.pal(8,"Dark2")
# 
# venue_names %>%
#   count(word) %>%
#   with(wordcloud::wordcloud(words = word, freq = n, max.words = 200, colors = pal))
# 
# venue_names_grouped <- venue_names %>%
#   filter(!is.na(word)) %>%
#   group_by(Category) %>%
#   count(word, Category, sort = TRUE) %>%
#   slice(seq_len(10)) %>%
#   ungroup() %>%
#   arrange(Category, n) %>%
#   mutate(row = row_number())
# 
# # now plot
# venue_names_grouped %>%
#   ggplot(aes(x= row, y=n)) +
#   geom_col(fill = "midnightblue") +
#   theme_bw() +
#   scale_x_continuous(  # This handles replacement of row 
#     breaks = venue_names_grouped$row, # notice need to reuse data frame
#     labels = venue_names_grouped$word) +
#   coord_flip() +
#   labs(title = "Popular Words in Venue Names", subtitle = "Top 10 words in names for each category") +
#   facet_wrap(~Category, scales = "free_y", drop = TRUE, ncol = 2) +
#   theme_bw() +
#   theme(axis.title = element_blank(), plot.title = element_text(size = 22), plot.subtitle = element_text(size = 17), 
#         axis.text.y = element_text(size = 15), axis.text.x = element_text(size = 15), strip.text = element_text(size = 14))


#------------------------------
# LGA Data analysis
#------------------------------
# read in data
LGA_data <- readRDS("Data/Analysis/LGA_Data.rds")
 
# create a sumary liquor by LGA DF
liquor_by_LGA_summary <- liquor_data_deduped %>%
  group_by(Council) %>%
  summarise(TotalLicences = n()) %>% ungroup()


# create a joined DF

LGA_Analysis_data <- liquor_by_LGA_summary %>%
  left_join(LGA_data, by = c("Council" = "LGAName"))


LGA_Analysis_data <- LGA_Analysis_data %>%
  mutate(LicencePerSqKM = TotalLicences / Area_KMSqr) %>%
  mutate(LicencePer100kPop = (TotalLicences / Population) * 100000)

b <- LGA_Analysis_data %>%
  arrange(desc(TotalLicences)) %>%
  head(n=10) %>%
  mutate(both = ifelse(Council == "MELBOURNE" | Council == "YARRA", "Both", "Not")) %>%
  ggplot(aes(x= reorder(Council, TotalLicences), y= TotalLicences, fill = both)) +
  geom_bar(stat = "identity") +
  geom_text(x= 6, y= 1450, label = "Only Melbourne and\nYarra appear on\nboth top 10 lists", size = 9, color = "palegreen4") +
  scale_fill_manual(values = c("palegreen4", "grey")) +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(), legend.position = "none", axis.text.y = element_text(size = 15), 
        axis.title.x = element_text(size = 17),axis.text.x = element_text(size = 15))

c <- LGA_Analysis_data %>%
  arrange(desc(LicencePer100kPop)) %>%
  head(n=10) %>%
  mutate(both = ifelse(Council == "MELBOURNE" | Council == "YARRA", "Both", "Not")) %>%
  ggplot(aes(x= reorder(Council, LicencePer100kPop), y= LicencePer100kPop, fill = both)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("palegreen4", "grey")) +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(), legend.position = "none", axis.text.y = element_text(size = 15), 
        axis.title.x = element_text(size = 17),axis.text.x = element_text(size = 15))

gridExtra::grid.arrange(b, c, ncol = 2, top= textGrob("Figure 6. Top 10 LGAs by Total Licences and Licences per 100,000", gp = gpar(fontsize = 20)))


LGA_Analysis_data %>%
  mutate(MaleOrFemale = ifelse(SexRatio > 100, "Male", "Female")) %>%
  ggplot(aes(x= LicencePerSqKM)) +
  geom_density()

LGA_Analysis_data %>%
  mutate(MaleOrFemale = ifelse(SexRatio > 100, "Male", "Female")) %>%
  ggplot(aes(x= TotalLicences, fill = MaleOrFemale)) +
  geom_density(alpha = 0.5)


LGA_Analysis_data %>%
  mutate(MaleOrFemale = ifelse(SexRatio > 100, "Male", "Female")) %>%
  ggplot(aes(x= Population, y= TotalLicences, colour = MaleOrFemale)) +
  geom_point()

LGA_Analysis_data %>%
  mutate(MaleOrFemale = ifelse(SexRatio > 100, "Male", "Female")) %>%
  ggplot(aes(x= SexRatio, y= TotalLicences)) +
  geom_point()


LGA_Analysis_data %>%
  mutate(MaleOrFemale = ifelse(SexRatio > 100, "Male", "Female")) %>%
  ggplot(aes(x= MedianAge, y= TotalLicences, colour = MaleOrFemale)) +
  geom_point()

LGA_Analysis_data %>%
  select(2:6) %>%
  as.matrix() %>%
  cor(method = "spearman") %>%
  corrplot::corrplot(method = "number", type = "upper")


LGA_Analysis_data %>%
  select(2:6) %>%
  as.matrix() %>%
  cor(method = "spearman") %>%
  corrplot::corrplot(tl.srt = 45, title = "Figure 8. Spearman Correlation of numerical variables", outline = T, 
                   addgrid.col = "darkgray", mar = c(4,0,4,0), addCoef.col = "white",
                   cl.pos = "n", tl.col = "indianred4", tl.cex = 1.5, method = "color", number.digits = 3, number.cex=1.5)

# corr_data <- LGA_Analysis_data %>%
#   select(2:6) %>%
#   as.matrix() %>%
#   cor(method = "spearman")
# 
# DT::datatable(corr_data)


test_spread <- test %>%
  spread(key = Category, value = TotalLicences)

test_spread[is.na(test_spread)] <- 0


test_spread %>%
  select(-1) %>%
  as.matrix() %>%
  cor() %>%
  corrplot::corrplot()


#~~~~~~~~~~~~~~~~~~~~~~~~~
# Mapping by LGA
#~~~~~~~~~~~~~~~~~~~~~~~~~

# there are out of bounds geocodes
min_lon <- 140
max_lon <- 150
min_lat <- -39
max_lat <- -34

LGA_mapping <- liquor_data_deduped %>%
  filter(between(Latitude, min_lat, max_lat),
         between(Longitude, min_lon, max_lon)) %>%
  group_by(Council) %>%
  summarise(LGALatitude = mean(Latitude),
            LGALongitude = mean(Longitude)) %>%
  ungroup() %>%
  left_join(liquor_by_LGA_summary, by = "Council") %>%
  left_join(LGA_Analysis_data %>% select(Council, LicencePer100kPop), by = "Council")


map_toner <- get_map(location = c(lon = 144.964600, lat = -37.020100), zoom = 7, maptype = "toner")

map_roadmap <- get_map(location = c(lon = 144.964600, lat = -37.020100), zoom = 7, maptype = "roadmap")

ggmap(map_toner) +
  geom_jitter(data = LGA_mapping, aes(x=LGALongitude, y=LGALatitude, size = LicencePer100kPop), color = "black", fill = "orange", shape = 21) +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  ggtitle("Where are the Licences") +
  theme(plot.title = element_text(size = 30))


ggmap(map_roadmap) +
  geom_jitter(data = LGA_mapping, aes(x=LGALongitude, y=LGALatitude, size = LicencePer100kPop), color = "black", fill = "orange", shape = 21) +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  ggtitle("Figure 7. Where are the most Licences per capita") +
  theme(plot.title = element_text(size = 30))








# create a vector of terms that indicate violent crimes
violent_terms <- c("assault", "abduction", "homicide", "sexual", "aggravated", "threatening") 

# coerce the crime subgroups to lower case 
crimes_subgroup <- tolower(crime_and_pop$OffenceSubgroup)

# collapse the terms, put a bnoundry around to assist matching, then get the index of where the violent terms exist
violent_terms_new <- grep(paste0("\\b", paste(violent_terms, collapse = "|"), ")\\b"), crimes_subgroup)

# classify based on the index
crime_and_pop$ViolentCrime <- ifelse(seq_len(nrow(crime_and_pop)) %in% violent_terms_new, "Violent", "Non-Violent")


LGA_crime_and_pop_analysis <- crime_and_pop %>%
  filter(ViolentCrime == "Violent") %>%
  group_by(LocalGovernmentArea, ViolentCrime) %>%
  summarise(TotalViolentIncidents = sum(IncidentsRecorded),
            Population = max(Population),
            SexRatio = max(SexRatio),
            MedianAge = max(MedianAge)) %>%
  mutate(CrimePer100k = (TotalViolentIncidents / Population) * 100000) %>% ungroup()


LGA_liquor_analysis <- liquor_data_deduped %>%
  group_by(Council) %>%
  summarise(TotalLicences = n())

LGA_analysis <- LGA_liquor_analysis %>%
  left_join(LGA_crime_and_pop_analysis, by = c("Council" = "LocalGovernmentArea")) %>%
  mutate(LicencesPer100k = (TotalLicences / Population) * 100000)

LGA_analysis <- mutate(LGA_analysis, GenderLGA = ifelse(SexRatio > 100, "Male", "Female"))

options(scipen = 999)
p1 <- ggplot(data = LGA_analysis, aes(x= TotalLicences, y= CrimePer100k, size = Population, colour = GenderLGA)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  scale_colour_manual(values = c("orange", "steelblue"))

ggMarginal(p1, type = "histogram", fill = "midnightblue")

p2 <- ggplot(data = LGA_analysis, aes(x= TotalLicences, y= TotalViolentIncidents, size = Population, colour = GenderLGA)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, linetype = 2) +
  scale_colour_manual(values = c("orange", "steelblue")) +
  theme(legend.position = "none")

p3 <- ggplot(data = LGA_analysis, aes(x= LicencesPer100k, y= CrimePer100k, size = Population, colour = GenderLGA)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, linetype = 2) +
  scale_colour_manual(values = c("orange", "steelblue"))


gridExtra::grid.arrange(p2, p3, ncol = 2)

LGA_analysis %>%
  select(-c(1,3, 10)) %>%
  as.matrix() %>%
  cor() %>%
  corrplot::corrplot(method = "number")

ggplot(data = LGA_analysis, aes(x= SexRatio)) +
  geom_density()

ggplot(data = LGA_analysis, aes(x= MedianAge, y= TotalLicences, size = Population)) +
  geom_point()

hist(LGA_analysis$TotalLicences)

hist(log(LGA_analysis$TotalLicences))

plot(LGA_analysis$TotalLicences, LGA_analysis$CrimePer100k)
cor(LGA_analysis$TotalLicences, LGA_analysis$CrimePer100k)


LGA_lm_df <- LGA_analysis %>%
  select(-Council, -ViolentCrime, -TotalViolentIncidents, -LicencesPer100k, -GenderLGA)

fit.lm <- lm(CrimePer100k ~ ., data = LGA_lm_df)

summary(fit.lm)

hist(fit.lm$residuals)
plot(fit.lm)

ggplot(data = LGA_analysis, aes(x=MedianAge, y= CrimePer100k)) +
  geom_point() +
  geom_smooth(method = "lm")


LGA_liquor_analysis_Cateory <- liquor_data_deduped %>%
  group_by(Council, Category) %>%
  summarise(TotalLicences = n()) %>% ungroup()

LGA_liquor_analysis_Cateory_spread <- LGA_liquor_analysis_Cateory %>%
  spread(key = Category, value = TotalLicences)

LGA_liquor_analysis_Cateory_spread[is.na(LGA_liquor_analysis_Cateory_spread)] <- 0


test1 <- LGA_crime_and_pop_analysis %>%
  left_join(LGA_liquor_analysis_Cateory, by = c("LocalGovernmentArea" = "Council"))


test_cor <- test1 %>% 
  group_by(Category) %>%
  summarise(LicenceCrimeCorrelation = cor(CrimePer100k, TotalLicences))

test_cor[is.na(test_cor)] <- 0


ggplot(data = test1, aes(x= TotalLicences, y= CrimePer100k, size = Population)) +
  geom_point(shape = 21, col = "black", fill = "orange") +
  facet_wrap(~ Category, scales = "free")




test2 <- test %>%
  select(-c(1:3))

library(GGally)
ggpairs(test2)


test <- LGA_crime_and_pop_analysis %>%
  left_join(LGA_liquor_analysis_Cateory_spread, by = c("LocalGovernmentArea" = "Council"))


test %>%
  select(-c(1:2)) %>%
  as.matrix() %>%
  cor(method = "spearman") %>%
  corrplot::corrplot(method = "number", type = "upper")

df_for_lm <- test %>%
  select(-c(1:3))

fit.lm_spread <- lm(CrimePer100k ~ ., data = df_for_lm)
summary(fit.lm_spread)
