#PFAS Data Pull
#Following instructions outlined by Matt Dunn in 
#notebook entry from 2025-04-23

#Written by: Hannah Ferriby, hannah.ferriby@tetratech.com
#Date created: 2025-5-2
#Date updated: 2025-5-29

# if(!"remotes"%in%installed.packages()){
#   install.packages("remotes")
# }
# library(remotes)
# remotes::install_github("USEPA/EPATADA", ref = "develop", dependencies = TRUE, force = TRUE)

####Set Up####
library(EPATADA)
library(here)
library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
library(scatterpie)
library(fs)

#Check if the directories exists; create it if it doesn't
if (!dir_exists("output")) {
  dir_create("output")
}
if (!dir_exists("output/figures")) {
  dir_create("output")
}

####Download####
data <- TADA_DataRetrieval(characteristicName = c('PFOA ion'
                                                  , 'Perfluorooctanoic acid'
                                                  # , 'PERFLUOROOCTANOIC ACID'
                                                  ,'Perfluorooctanesulfonate'
                                                  , 'Perfluorooctane sulfonic acid'
                                                  # , 'Potassium perfluorooctanesulfonate'
                                                  , 'Perfluorooctanesulfonate (PFOS)'
                                                  # , 'POTASSIUM PERFLUOROOCTANESULFONATE'
                                                  ),
                           # sampleMedia = c('Water', 'Tissue'), 
                           applyautoclean = T,
                           ask = F)


####Export####
write_csv(data, here::here('output/data_pull.csv'))

####MAPS####
#read in state codes and formats STATE column entries so that this can be joined
state_num <- read.table(here::here('Data/state_codes.txt'), header = T, sep = "|", dec = ".") %>%
  mutate(STATE = ifelse(STATE < 10, as.character(paste0('0',STATE)),
                        as.character(STATE)))

#read in state shapefile - remove US territories, Alaska, Hawaii, and Puerto Rico
states <- st_read(here::here('Data/cb_2018_us_state_500k/cb_2018_us_state_500k.shp')) %>%
  filter(!STATEFP %in% c('60', '66', '69', '78',
                         '15', '02'))

#The scatterpie map does not take into consideration the type of PFAS compound
all_data_all_media <- data %>%
  left_join(state_num, by = c('StateCode' = 'STATE'))

#Group data by state to calculate total number of samples
states_w_data <- all_data_all_media %>%
  group_by(STATE_NAME) %>%
  mutate(n_samples_total = n()) %>%
  ungroup() %>%
  group_by(STATE_NAME, ActivityMediaName) %>%
  reframe(STATE_NAME = STATE_NAME, 
          ActivityMediaName = ActivityMediaName,
          n_samples_media_type = n(),
          n_samples_total = n_samples_total) %>%
  unique() %>%
  #join sample counts with state shapes
  left_join(states, by = c('STATE_NAME'= 'NAME')) %>%
  mutate(centroid = st_centroid(geometry)) %>%
  select(STATE_NAME, ActivityMediaName, n_samples_total, n_samples_media_type,
         centroid, geometry) 


#####Scatterpie#####
filt_pie <- states_w_data %>%
  select(STATE_NAME, ActivityMediaName, n_samples_media_type,
         n_samples_total, centroid, geometry) %>%
  pivot_wider(id_cols = c('STATE_NAME', 'n_samples_total', 'centroid', 'geometry'),
              names_from = 'ActivityMediaName',
              values_from = 'n_samples_media_type') %>%
  mutate(Tissue = ifelse(is.na(Tissue),0,Tissue),
         Sediment = ifelse(is.na(Sediment),0,Sediment),
         Soil = ifelse(is.na(Soil),0,Soil),
         Water = ifelse(is.na(Water),0,Water),
         Air = ifelse(is.na(Air),0,Air),
         Other = ifelse(is.na(Other),0,Other)) %>%
  st_drop_geometry()

filt_pie2 <- extract(filt_pie, centroid, into = c('Lat', 'Lon'), '\\((.*),(.*)\\)', conv = T) %>%
  filter(!is.na(Lat)) %>%
  rename(`All Water` = Water) %>%
  mutate(radius = sqrt(n_samples_total)/25)



ggplot() +
  geom_sf(data = states, color = 'black', fill = 'gray90') +
  geom_scatterpie(data = as.data.frame(filt_pie2), 
                  aes(x = Lat, y = Lon, group = STATE_NAME, r = radius), 
                  cols = c("Tissue", "All Water", "Sediment", "Air", "Soil", "Other"),
                  color = 'black', size = 0.1) +
  theme_bw() +
  scale_fill_manual(name = 'Media Type',
                    values = c(
                      "Tissue" = "#FF9999",   # Light red
                      "All Water" = "#99CCFF",    # Light blue
                      "Sediment" = "#FFCC99", # Light orange
                      "Air" = "#CCCC99",      # Light olive
                      "Soil" = "#99CC99",     # Light green
                      "Other" = "#CC99CC"     # Light purple
                    )) +
  xlab('')+
  ylab('')+
  theme(legend.position = 'top',
        legend.text = element_text(size = 8),    # Reduces legend text size
        legend.title = element_text(size = 8),   # Reduces legend title text size
        legend.key.size = unit(0.5, "lines"),
        axis.text = element_text(size = 8)) +
  guides(fill = guide_legend(override.aes = list(size = 0.5))) + 
  geom_scatterpie_legend(radius = filt_pie2$radius,
                         x=-120, y=20
                         , labeller=function(h) ((h*25)^2),
                         n = 5, 
                         breaks = c(0.4,
                                    0.8944271909999159,
                                    1.264911064067352,
                                    2.82842712474619,
                                    4),
                         size = 3)

ggsave(here::here('output/figures/scatterpie_map_all_media.jpg'), units = 'in', width = 6, height = 6, dpi = 500)
