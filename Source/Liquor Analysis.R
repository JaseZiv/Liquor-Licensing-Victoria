library(tidyverse)
library(tidytext)
library(ggExtra)
library(grid)
library(ggmap)

###########################################################
#---------- Liquor Licencing analysis by Suburb ----------#
###########################################################

liquor_data_deduped <-  readRDS("Data/Analysis/liquor_data_deduped.rds")

liquor_data_deduped %>%
  group_by(Category) %>%
  summarise(NumMissing = sum(is.na(MaximumCapacity)))


#######
# NOTE:
#######
# Late night (packaged liquor) Licence has only two licences. Will merge this with Packaged Liquor Licence
liquor_data_deduped$Category[liquor_data_deduped$Category == "Late night (packaged liquor) Licence"] <- "Packaged Liquor Licence"

#~~~~~~~~~~~~~~~~~~~~
# Figure 1:
#~~~~~~~~~~~~~~~~~~~~

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


#~~~~~~~~~~~~~~~~~~~~
# Figure 2:
#~~~~~~~~~~~~~~~~~~~~
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

#~~~~~~~~~~~~~~~~~~~~
# Figure 3:
#~~~~~~~~~~~~~~~~~~~~
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

sum_stats <- mosaic::favstats(suburb_count$NumLicenses ~ suburb_count$DryArea)


#~~~~~~~~~~~~~~~~~~~~
# Figure 4:
#~~~~~~~~~~~~~~~~~~~~
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
  
#~~~~~~~~~~~~~~~~~~~~
# Figure 5:
#~~~~~~~~~~~~~~~~~~~~
suburb_count %>%
  ggplot(aes(x= Metro_Regional, y= NumLicenses, fill = Metro_Regional)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,300)) +
  labs(title = "Figure 5. Distribution of Licenced Venues", subtitle = "How many licences do suburbs have?", y= "Number of licenses per suburb") +
  scale_fill_manual(values = c("orange", "steelblue")) +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_text(size = 15), axis.text = element_text(size = 14), 
        plot.title = element_text(size = 24), plot.subtitle = element_text(size = 17))

mosaic::favstats(suburb_count$NumLicenses ~ suburb_count$Metro_Regional)


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

# add normalised licence measures per 100,000 population and by area
LGA_Analysis_data <- LGA_Analysis_data %>%
  mutate(LicencePerSqKM = TotalLicences / Area_KMSqr) %>%
  mutate(LicencePer100kPop = (TotalLicences / Population) * 100000)

#~~~~~~~~~~~~~~~~~~~~
# Figure 6:
#~~~~~~~~~~~~~~~~~~~~
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


#~~~~~~~~~~~~~~~~~~~~~~~~~
# Mapping by LGA
#~~~~~~~~~~~~~~~~~~~~~~~~~

# there are out of bounds geocodes
min_lon <- 140
max_lon <- 150
min_lat <- -39
max_lat <- -34

# filter out any coordinates out of bounds and find the average lat-lon for each LGA
LGA_mapping <- liquor_data_deduped %>%
  filter(between(Latitude, min_lat, max_lat),
         between(Longitude, min_lon, max_lon)) %>%
  group_by(Council) %>%
  summarise(LGALatitude = mean(Latitude),
            LGALongitude = mean(Longitude)) %>%
  ungroup() %>%
  left_join(liquor_by_LGA_summary, by = "Council") %>%
  left_join(LGA_Analysis_data %>% select(Council, LicencePer100kPop), by = "Council")


#~~~~~~~~~~~~~~~~~~~~
# Figure 7:
#~~~~~~~~~~~~~~~~~~~~
# set the mapping parameters
map_roadmap <- get_map(location = c(lon = 144.964600, lat = -37.020100), zoom = 7, maptype = "roadmap")

# plot the coordinates on the map
ggmap(map_roadmap) +
  geom_jitter(data = LGA_mapping, aes(x=LGALongitude, y=LGALatitude, size = LicencePer100kPop), color = "black", fill = "orange", shape = 21) +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  ggtitle("Figure 7. Where are the most Licences per capita") +
  theme(plot.title = element_text(size = 30))


#~~~~~~~~~~~~~~~~~~~~
# Figure 8:
#~~~~~~~~~~~~~~~~~~~~

LGA_Analysis_data %>%
  select(2:6) %>%
  as.matrix() %>%
  cor(method = "spearman") %>%
  corrplot::corrplot(tl.srt = 45, title = "Figure 8. Spearman Correlation of numerical variables", outline = T, 
                   addgrid.col = "darkgray", mar = c(4,0,4,0), addCoef.col = "white",
                   cl.pos = "n", tl.col = "indianred4", tl.cex = 1.5, method = "color", number.digits = 3, number.cex=1.5)

