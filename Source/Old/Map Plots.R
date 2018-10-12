#-------------------------------------------
# plot crime spots on map Victoria
#-------------------------------------------
ggmap(map_toner) + 
  geom_jitter(data = mapping_df, aes(x=Longitude, y=Latitude, size = NumIncidents), color = "black", fill = "orange", shape = 21) +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  ggtitle("Where are the crime hotspots") +
  theme(plot.title = element_text(size = 30))

ggsave(filename = "Capstone/Plots/CrimeHotspotMap.png")

#### or with colour gradient #####
ggmap(map_toner) + 
  geom_jitter(data = mapping_df, aes(x=Longitude, y=Latitude, size = NumIncidents, colour = NumIncidents)) +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  ggtitle("Where are the crime hotspots") +
  theme(plot.title = element_text(size = 30)) +
  scale_colour_gradient(low = "green", high = "red")

ggsave(filename = "Capstone/Plots/CrimeHotspotMap_Gradient.png")
