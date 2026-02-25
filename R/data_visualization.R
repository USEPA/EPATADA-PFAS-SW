#Data visualization (not scatterpies) for phase 2

#Written by: Hannah Ferriby (hannah.ferriby@tetratech.com)
#Date created: 2025-5-6
#Date updated: 2025-5-29

# install.package('scales')

####Libraries####
library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
library(ggpmisc)
library(here)
library(fs)

#Check if the directories exists; create it if it doesn't
if (!dir_exists("output")) {
  dir_create("output")
}
if (!dir_exists("output/figures")) {
  dir_create("output")
}

theme_set(theme_classic())
options(scipen = 8)

####Data####
state_num <- read.table('Data/state_codes.txt', header = T, sep = "|", dec = ".") %>%
  mutate(STATE = ifelse(STATE < 10, as.character(paste0('0',STATE)),
                        as.character(STATE)))

all_data <- read_csv('output/EPATADA_Original_data_with_flags_tags.csv',
                     col_types = cols(FormationTypeText = col_character(),
                                      AquiferName = col_character(),
                                      ProjectFileUrl = col_character())) %>%
  left_join(state_num, by = c('StateCode' = 'STATE')) %>%
  mutate(TADA.ResultMeasure.MeasureUnitCode = ifelse(TADA.ResultMeasure.MeasureUnitCode == 'NG/ML',
                                                     'UG/L', TADA.ResultMeasure.MeasureUnitCode),
         Abbrev.Name = case_when(TADA.CharacteristicName == "PERFLUOROOCTANOIC ACID" ~ "PFOA",
                                 TADA.CharacteristicName == "PFOA ION" ~ "PFOA",
                                 TADA.CharacteristicName == "PERFLUOROOCTANESULFONATE (PFOS)" ~ "PFOS",
                                 TADA.CharacteristicName == "PERFLUOROOCTANESULFONATE" ~ "PFOS",
                                 TADA.CharacteristicName == "PERFLUOROOCTANE SULFONIC ACID" ~ "PFOS"))

summary(as.factor(all_data$ActivityMediaName))
summary(as.factor(all_data$ActivityMediaSubdivisionName))


all_data %>%
  group_by(Abbrev.Name) %>%
  reframe(Abbrev.Name = Abbrev.Name, n = n()) %>%
  unique() %>%
  arrange(-n)

all_data %>%
  filter(TADA.CensoredData.Flag == 'Non-Detect') %>%
  select(TADA.DetectionQuantitationLimitMeasure.MeasureValue) %>%
  mutate(Min = min(TADA.DetectionQuantitationLimitMeasure.MeasureValue, na.rm = TRUE),
         p25 = quantile(TADA.DetectionQuantitationLimitMeasure.MeasureValue, probs = 0.25, na.rm = TRUE),
         median = quantile(TADA.DetectionQuantitationLimitMeasure.MeasureValue, probs = 0.5, na.rm = TRUE),
         mean = mean(TADA.DetectionQuantitationLimitMeasure.MeasureValue, na.rm = TRUE),
         p75 = quantile(TADA.DetectionQuantitationLimitMeasure.MeasureValue, probs = 0.75, na.rm = TRUE),
         Max = max(TADA.DetectionQuantitationLimitMeasure.MeasureValue, na.rm = TRUE)) %>%
  select(!TADA.DetectionQuantitationLimitMeasure.MeasureValue) %>%
  unique()


summary(as.factor(all_data$Abbrev.Name))
summary(all_data$TADA.DetectionQuantitationLimitMeasure.MeasureValue)
summary(all_data$TADA.DetectionQuantitationLimitMeasure.MeasureValue[all_data$TADA.CensoredData.Flag == "Non-Detect"])
summary(as.factor(all_data$TADA.CensoredData.Flag))

filtered_data <- read_csv('output/EPATADA_priority_filtered_data.csv',
                          col_types = cols(FormationTypeText = col_character(),
                                           AquiferName = col_character(),
                                           ProjectFileUrl = col_character())) %>%
  mutate(StateCode = as.character(StateCode)) %>%
  left_join(state_num, by = c('StateCode' = 'STATE')) %>%
  mutate(Abbrev.Name = case_when(TADA.CharacteristicName == "PERFLUOROOCTANOIC ACID" ~ "PFOA",
                                 TADA.CharacteristicName == "PFOA ION" ~ "PFOA",
                                 TADA.CharacteristicName == "PERFLUOROOCTANESULFONATE (PFOS)" ~ "PFOS",
                                 TADA.CharacteristicName == "PERFLUOROOCTANESULFONATE" ~ "PFOS",
                                 TADA.CharacteristicName == "PERFLUOROOCTANE SULFONIC ACID" ~ "PFOS"))

states <- st_read('data/cb_2018_us_state_500k/cb_2018_us_state_500k.shp') %>%
  filter(!STATEFP %in% c('60', '66', '69', '78',
                         '15', '02'))

#Summary in ng/L
data_summary_surfacewater <- filtered_data %>%
  group_by(Abbrev.Name, sample_lower_than_detection_limit_flag) %>%
  summarise(count = n(),
            Min = min(TADA.ResultMeasureValue, na.rm = TRUE)*1000,
            p25 = quantile(TADA.ResultMeasureValue, probs = 0.25, na.rm = TRUE)*1000,
            median = quantile(TADA.ResultMeasureValue, probs = 0.5, na.rm = TRUE)*1000,
            mean = mean(TADA.ResultMeasureValue, na.rm = TRUE)*1000,
            p75 = quantile(TADA.ResultMeasureValue, probs = 0.75, na.rm = TRUE)*1000,
            Max = max(TADA.ResultMeasureValue, na.rm = TRUE)*1000,
            propabovemin = sum(TADA.ResultMeasureValue > Min)/count) %>%
  mutate(criteria_acute = case_when(Abbrev.Name == "PFOA" ~ 3100*1000, 
                                    Abbrev.Name == "PFOS" ~ 71*1000,
                                    Abbrev.Name == "PFHxS" ~ 210*1000, 
                                    Abbrev.Name == "PFDA" ~ 500*1000,
                                    Abbrev.Name == "PFNA" ~ 650*1000,
                                    Abbrev.Name == "PFBS" ~ 5000*1000),
         criteria_chronic = case_when(Abbrev.Name == "PFOA" ~ 100*1000, 
                                      Abbrev.Name == "PFOS" ~ 0.25*1000,
                                      TRUE ~ NA_real_),
         drinking_water = case_when(Abbrev.Name =="PFOA"~ 0.004*1000, 
                                    Abbrev.Name =="PFOS" ~ 0.004*1000,
                                    Abbrev.Name =="PFHxS"~ 0.010*1000,
                                    Abbrev.Name == "PFNA"~ 0.010*1000,
                                    Abbrev.Name == "GenX" ~ 0.010*1000,
                                    Abbrev.Name == "PFBS"~ 2.0*1000,
                                    TRUE~NA_real_))

sum(data_summary_surfacewater$count)

write_csv(data_summary_surfacewater, 'output/data_summary.csv')

####Summaries####
#####Total Samples by Compound#####
compound_totals <- all_data %>%
  group_by(Abbrev.Name) %>%
  reframe(Abbrev.Name = Abbrev.Name,
          count = n()) %>%
  unique()

sum(compound_totals$count)

#####Exceedance#####

#REMOVE NONDETECTS FOR CALCS
exceedance <- all_data %>%
  filter(TADA.CensoredData.Flag == "Uncensored") %>%
  filter(ActivityMediaSubdivisionName == "Surface Water") %>%
  filter(Abbrev.Name %in% c('PFOA', 'PFOS', 'PFHxS', 'PFDA', 'PFNA', 'PFBS')) %>%
  mutate(exceedance_flag_acute = case_when(Abbrev.Name == "PFOA" & TADA.ResultMeasureValue >= 3100 ~
                                             1, 
                                           Abbrev.Name == "PFOS" & TADA.ResultMeasureValue >= 71 ~
                                             1,
                                           TRUE ~ 0),
         exceedance_flag_chronic = case_when(Abbrev.Name == "PFOA" & TADA.ResultMeasureValue >= 100 ~
                                               1, 
                                             Abbrev.Name == "PFOS" & TADA.ResultMeasureValue >= 0.25 ~
                                               1,
                                             TRUE ~ 0)) %>%
  group_by(Abbrev.Name, TADA.CensoredData.Flag) %>%
  reframe(Abbrev.Name = Abbrev.Name,
          sample_lower_than_detection_limit_flag = TADA.CensoredData.Flag,
          count_uncensored = n(),
          exceedance_acute = sum(exceedance_flag_acute),
          exceedance_acute_perc = sum(exceedance_flag_acute)/count_uncensored * 100,
          exceedance_chronic = sum(exceedance_flag_chronic),
          exceedance_chronic_perc = sum(exceedance_flag_chronic)/count_uncensored * 100) %>%
  unique()



data_summary_surfacewater_exceed <- data_summary_surfacewater %>%
  left_join(exceedance, by = c('Abbrev.Name','sample_lower_than_detection_limit_flag'))

write_csv(data_summary_surfacewater_exceed, 'output/data_summary_surfacewater_exceed.csv')


#####Summarize by EPA region#####
samples_epa_region_flagged <- all_data %>%
  filter(TADA.ActivityMediaName == 'WATER') %>%
  select(Abbrev.Name, TADA.ResultMeasureValue,
         TADA.ResultMeasure.MeasureUnitCode, STATE_NAME) %>%
  filter(!is.na(STATE_NAME)) %>%
  #assign epa region
  mutate(EPA_Region = case_when(STATE_NAME %in% c('Maine', 'New Hampshire', 'Vermont',
                                                  'Massachusetts', 'Rhode Island',
                                                  'Connecticut') ~
                                  '1',
                                STATE_NAME %in% c('New York', 'New Jersey', 'Puerto Rico',
                                                  'Virgin Islands') ~
                                  '2',
                                STATE_NAME %in% c('Pennsylvania', 'Delaware','Maryland',
                                                  'West Virginia', 'Virginia') ~
                                  '3',
                                STATE_NAME %in% c('Kentucky', 'Tennessee', 
                                                  'North Carolina', 'South Carolina',
                                                  'Mississippi', 'Alabama', 'Georgia',
                                                  'Florida') ~
                                  '4',
                                STATE_NAME %in% c('Michigan', 'Ohio', 'Indiana',
                                                  'Illinois', 'Wisconsin', 'Minnesota') ~
                                  '5',
                                STATE_NAME %in% c('Louisiana', 'Arkansas', 'Oklahoma',
                                                  'Texas', 'New Mexico') ~
                                  '6',
                                STATE_NAME %in% c('Iowa', 'Missouri', 'Nebraska',
                                                  'Kansas') ~
                                  '7',
                                STATE_NAME %in% c('North Dakota', 'South Dakota',
                                                  'Montana', 'Wyoming', 'Colorado',
                                                  'Utah') ~
                                  '8',
                                STATE_NAME %in% c('Hawaii', 'Guam', 'California',
                                                  'Nevada', 'Arizona') ~
                                  '9',
                                STATE_NAME %in% c('Idaho', 'Oregon', 'Washington',
                                                  'Alaska') ~
                                  '10',
                                T ~ NA)) %>%
  #calculate summary stats by region
  group_by(EPA_Region, Abbrev.Name, TADA.ResultMeasure.MeasureUnitCode) %>%
  reframe(EPA_Region = EPA_Region,
          Abbrev.Name = Abbrev.Name,
          n_samples = n(),
          Quantile_25 = quantile(TADA.ResultMeasureValue, 0.25, na.rm = T),
          Mean = mean(TADA.ResultMeasureValue, na.rm = T),
          Median = median(TADA.ResultMeasureValue, na.rm = T),
          Quantile_75 = quantile(TADA.ResultMeasureValue, 0.75, na.rm = T)) %>%
  ungroup() %>%
  unique()

write_csv(samples_epa_region_flagged, 'output/samples_epa_region_flagged.csv')


#####Nondetects#####
# samples_nd_flagged <- all_data %>%
#   filter(TADA.ActivityMediaName == 'WATER') %>%
#   filter(TADA.ResultMeasureValueDataTypes.Flag == 'Result Value/Unit Estimated from Detection Limit')

ggplot(filtered_data,
       aes(x = TADA.DetectionQuantitationLimitMeasure.MeasureValue, y = TADA.ResultMeasureValue*1000,
           color = sample_lower_than_detection_limit_flag)) +
  geom_point(alpha = 0.5, size = 1) +
  facet_wrap(vars(Abbrev.Name), ncol = 7) +
  scale_x_log10(n.breaks = 6,
                labels = label_comma(drop0trailing = TRUE)) +
  scale_y_log10(n.breaks = 6,
                labels = label_comma(drop0trailing = TRUE)) +
  scale_color_manual(values = c("#fdae6b", "#377eb8", '#D2042D'), labels = c("Non-Detect", "Uncensored", "Ambiguous")) +
  geom_hline(data=subset(data_summary_surfacewater, sample_lower_than_detection_limit_flag =="Uncensored"), 
             aes(yintercept= Min), color="#377eb8")+
  geom_hline(data = data_summary_surfacewater,
             aes(yintercept = criteria_acute), lty = 2) +
  geom_hline(data = data_summary_surfacewater,
             aes(yintercept = criteria_chronic), lty = 2) +
  labs(y = expression("Concentration (ng/L)"), x = expression("MDL (ng/L)"),
       color = "Tag") +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) #+

ggsave('output/figures/nondetect_scatterplot.jpg', 
       height = 5, width = 8, dpi = 500)

#### nondetect_scatterplot FLIPPED Axis 
ggplot(filtered_data,
       aes(x = TADA.ResultMeasureValue, y = TADA.DetectionQuantitationLimitMeasure.MeasureValue,
           color = sample_lower_than_detection_limit_flag)) +
  geom_point(alpha = 0.5, size = 1) +
  facet_wrap(vars(Abbrev.Name), ncol = 7) +
  scale_x_log10(n.breaks = 6,
                labels = label_comma(drop0trailing = TRUE)) +
  scale_y_log10(n.breaks = 5,
                labels = label_comma(drop0trailing = TRUE)) +
  scale_color_manual(values = c("#fdae6b", "#377eb8", '#D2042D'), labels = c("Non-Detect", "Uncensored", "Ambiguous")) +
  geom_vline(data = data_summary_surfacewater,
             aes(xintercept = drinking_water), color = "#377eb8") +
  geom_vline(data = data_summary_surfacewater,
             aes(xintercept = criteria_acute), lty = 2) +
  geom_vline(data = data_summary_surfacewater,
             aes(xintercept = criteria_chronic), lty = 2) +
  labs(y = expression("Concentration ("*mu*"g/L)"), x = expression("MDL ("*mu*"g/L)"),
       color = "Tag") +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) #+

ggsave('output/figures/nondetect_scatterplot_flipped.jpg', 
       height = 5, width = 8, dpi = 500)

####Summary Tables####

#####Summarize by Media#####
table_by_media <- all_data %>%
  group_by(ActivityMediaName) %>%
  reframe(ActivityMediaName = ActivityMediaName,
          n_samples = n()) %>%
  unique()

write_csv(table_by_media, 'output/table_by_media.csv')

#####Summarize by Species#####
table_by_species <- all_data %>%
  filter(!is.na(SubjectTaxonomicName)) %>%
  group_by(SubjectTaxonomicName) %>%
  reframe(SubjectTaxonomicName = SubjectTaxonomicName, 
          'Number of Sample' = n()) %>%
  unique()

write_csv(table_by_species, 'output/table_by_species.csv')

#### Tables by state 
MN_data <- all_data %>%
  filter(STATE_NAME=='Minnesota')
WI_data <- all_data %>%
  filter(STATE_NAME=='Wisconsin')

table_by_species_MN <- MN_data %>%
  filter(!is.na(SubjectTaxonomicName)) %>%
  group_by(SubjectTaxonomicName) %>%
  reframe(SubjectTaxonomicName = SubjectTaxonomicName, 
          'Number of Sample' = n()) %>%
  unique()

write_csv(table_by_species_MN, 'output/table_by_species_MN.csv')

table_by_species_WI <- WI_data %>%
  filter(!is.na(SubjectTaxonomicName)) %>%
  group_by(SubjectTaxonomicName) %>%
  reframe(SubjectTaxonomicName = SubjectTaxonomicName, 
          'Number of Sample' = n()) %>%
  unique()

write_csv(table_by_species_WI, 'output/table_by_species_WI.csv')

#####Summarize by State#####
table_by_state <- all_data %>%
  filter(!is.na(STATE_NAME)) %>%
  group_by(STATE_NAME) %>%
  reframe(STATE_NAME = STATE_NAME,
          `Number of Samples` = n()) %>%
  unique()

table_by_state_flag <- filtered_data %>%
  filter(!is.na(STATE_NAME)) %>%
  group_by(STATE_NAME) %>%
  reframe(STATE_NAME = STATE_NAME,
          `Number of Samples After Critical Flagging` = n()) %>%
  unique()

table_by_state_combo <- table_by_state %>%
  left_join(table_by_state_flag, by = 'STATE_NAME')

write_csv(table_by_state_combo, 'output/table_by_state.csv')

#####Summarize by All#####
table_by_all <- all_data %>%
  group_by(STATE_NAME, ActivityMediaName) %>%
  reframe(STATE_NAME = STATE_NAME,
          ActivityMediaName = ActivityMediaName,
          n_samples = n()) %>%
  unique() %>%
  arrange(STATE_NAME, ActivityMediaName)

write_csv(table_by_all, 'output/table_by_all.csv')


#####Summarize by site by number of PFAS compounds reported & media#####
table_by_pfas_compounds <- all_data %>%
  select(ActivityMediaName, TADA.CharacteristicName, MonitoringLocationName,
         TADA.LatitudeMeasure, TADA.LongitudeMeasure) %>%
  unique() %>%
  group_by(MonitoringLocationName,TADA.LatitudeMeasure, TADA.LongitudeMeasure,
           ActivityMediaName) %>%
  reframe(MonitoringLocationName = MonitoringLocationName,
          TADA.LatitudeMeasure = TADA.LatitudeMeasure,
          TADA.LongitudeMeasure = TADA.LongitudeMeasure,
          ActivityMediaName = ActivityMediaName,
          n_pfas_compounds = n()) %>%
  unique()

####Boxplots####
all_data_4_plot <- filtered_data %>%
  mutate(TADA.CharacteristicName = case_when(TADA.CharacteristicName == 'PERFLUOROOCTANESULFONATE' ~
                                               'PERFLUOROOCTANE SULFONIC ACID', TADA.CharacteristicName == 'PFOA ION' ~ 'PERFLUOROOCTANOIC ACID',
                                             T ~ TADA.CharacteristicName)) %>%
  filter(TADA.ResultMeasure.MeasureUnitCode != 'NONE') %>%
  mutate(TADA.ActivityMediaName = ifelse(TADA.ActivityMediaName == 'WATER', 'SURFACE WATER', TADA.ActivityMediaName))
options(scipen=9999)

ggplot() + 
  geom_boxplot(data = all_data_4_plot, aes(x = Abbrev.Name, 
                                           y = TADA.ResultMeasureValue)) +
  theme_bw() +
  scale_y_log10() +
  facet_wrap(~TADA.ActivityMediaName,
             scales = 'free_y',
             nrow = 2) + 
  xlab('Characteristic Name') + 
  ylab('Sample Result Value') + 
  guides(x =guide_axis(angle = 45)) +
  theme(text = element_text(size = 10),
        strip.background = element_rect(fill="gray99"))

ggsave('output/figures/params_by_media_boxplot.jpg', units = 'in',
       height = 5, width = 6)


#####Jitter plot#####
ggplot() +
  geom_jitter(data = all_data, aes(x = Abbrev.Name,
                                   y = TADA.ResultMeasureValue),
              alpha = 0.25) +
  theme_bw() +
  scale_y_log10() +
  facet_wrap(~TADA.ActivityMediaName,
             scales = 'free_y',
             nrow = 2) +
  xlab('Characteristic Name') +
  ylab('Sample Result Value') +
  guides(x =guide_axis(angle = 45)) +
  theme(text = element_text(size = 10),
        strip.background = element_rect(fill="gray99"))

ggsave('output/figures/params_by_media_scatter.jpg', units = 'in',
       height = 5, width = 6)

# code to generate sample sizes 

y_breaks = c( 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000)
give.n <- function(x){
  return(c(y = (min(x) - 0.5), label = length(x)))
}

ggplot(all_data,
       aes(y = TADA.ResultMeasureValue*1000, x = Abbrev.Name, fill = sample_lower_than_detection_limit_flag,
           color = sample_lower_than_detection_limit_flag)) +
  geom_boxplot(data = filtered_data,
               alpha = 0.8, aes(fill = sample_lower_than_detection_limit_flag,
                                color = sample_lower_than_detection_limit_flag), position=position_dodge(preserve="single")) +
  #geom_point(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.2, size = 0.5) +
  stat_summary(fun.data = give.n, geom = "text", size = 2, position = position_dodge(width = 0.75)) +
  geom_segment(data = data_summary_surfacewater,
               aes(x = as.numeric(Abbrev.Name) - 0.3,
                   xend = as.numeric(Abbrev.Name) + 0.3,
                   y = criteria_acute, yend = criteria_acute),
               lty = 2, inherit.aes = FALSE) +
  geom_segment(data = data_summary_surfacewater,
               aes(x = as.numeric(Abbrev.Name) - 0.3, 
                   xend = as.numeric(Abbrev.Name) + 0.3, 
                   y = criteria_chronic, yend = criteria_chronic),
               lty = 2, inherit.aes = FALSE) +
  scale_y_log10(breaks=y_breaks,
                labels = scales::label_number(drop0trailing=TRUE)) +
  scale_fill_manual(values = c("#fdae6b", "#377eb8", '#D2042D'), labels = c("Non-Detect", "Uncensored", "Ambiguous")) +
  scale_color_manual(values = c("#ca8b55", "#265880", '#690216'), labels = c("Non-Detect", "Uncensored", "Ambiguous")) +
  labs(x = "Compound", y = "Concentration (ng/L)",
       fill = "Tag", color = 'Tag') + #, title = "PFAS Concentration in Surface Water") +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1)) 


ggsave('output/figures/params_surfacewater_boxplot.jpg',
       height = 4, width = 8, dpi = 500)

all_filt_data_4_plot <- filtered_data %>%
  mutate(TADA.CharacteristicName = case_when(TADA.CharacteristicName == '1-HEPTANESULFONIC ACID, 1,1,2,2,3,3,4,4,5,5,6,6,7,7,7-PENTADECAFLUORO-' ~
                                               '1-HEPTANESULFONIC ACID,1,1,2,2,3,3,\n4,4,5,5,6,6,7,7,7-PENTADECAFLUORO-',
                                             T ~ TADA.CharacteristicName)) %>%
  filter(TADA.ResultMeasure.MeasureUnitCode != 'NONE')


####Scatterpie but table form####
states_w_data <- all_data %>%
  group_by(STATE_NAME) %>%
  mutate(n_samples_total = n()) %>%
  ungroup() %>%
  group_by(STATE_NAME, ActivityMediaName) %>%
  reframe(STATE_NAME = STATE_NAME, 
          ActivityMediaName = ActivityMediaName,
          n_samples_media_type = n(),
          n_samples_total = n_samples_total) %>%
  unique() %>%
  left_join(states, by = c('STATE_NAME'= 'NAME')) %>%
  mutate(centroid = st_centroid(geometry)) %>%
  select(STATE_NAME, ActivityMediaName, n_samples_total, n_samples_media_type,
         centroid, geometry) 

filt_pie <- states_w_data %>%
  select(STATE_NAME, ActivityMediaName, n_samples_media_type,
         n_samples_total, centroid, geometry) %>%
  pivot_wider(id_cols = c('STATE_NAME', 'n_samples_total', 'centroid', 'geometry'),
              names_from = 'ActivityMediaName',
              values_from = 'n_samples_media_type') %>%
  mutate(Tissue = ifelse(is.na(Tissue),0,Tissue),
         Water = ifelse(is.na(Water),0,Water)) %>%
  st_drop_geometry()

pie_table <- filt_pie %>%
  select(!c(geometry,  centroid)) %>%
  rename(State = STATE_NAME,
         `Total Samples` = n_samples_total)

write_csv(pie_table, 'output/samples_by_media_by_state.csv')

