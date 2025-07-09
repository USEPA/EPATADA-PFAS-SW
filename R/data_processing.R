#PFAS Data Processing
#Following methods from phase 1

#Written by: Hannah Ferriby, hannah.ferriby@tetratech.com
#Date created: 2025-5-6
#Date updated: 2025-5-29


####Set Up####
library(EPATADA)
library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
library(scatterpie)

####Data####
data <- read_csv('output/data_pull.csv') %>%
  mutate(Abbrev.Name = case_when(TADA.CharacteristicName == "PERFLUOROOCTANOIC ACID" ~ "PFOA",
                                 TADA.CharacteristicName == "PFOA ION" ~ "PFOA",
                                 TADA.CharacteristicName == "PERFLUOROOCTANESULFONATE (PFOS)" ~ "PFOS",
                                 TADA.CharacteristicName == "PERFLUOROOCTANESULFONATE" ~ "PFOS",
                                 TADA.CharacteristicName == "PERFLUOROOCTANE SULFONIC ACID" ~ "PFOS"))

data_media_sums <- data %>%
  group_by(Abbrev.Name) %>%
  reframe(Abbrev.Name = Abbrev.Name,
          n = n()) %>%
  unique()

state_num <- read.table('data/state_codes.txt', header = T, sep = "|", dec = ".") %>%
  mutate(STATE = ifelse(STATE < 10, as.character(paste0('0',STATE)),
                        as.character(STATE)))

states <- st_read('data/cb_2018_us_state_500k/cb_2018_us_state_500k.shp') %>%
  filter(!STATEFP %in% c('60', '66', '69', '78',
                         '15', '02'))

####Filter####
unique(data$ActivityMediaName)
unique(data$ActivityMediaSubdivisionName)

data_filt <- data %>%
  filter(ActivityMediaName %in% c('Water', 'Tissue')) %>% 
  filter(ActivityMediaSubdivisionName == 'Surface Water')  %>%
  filter(!(TADA.ActivityMediaName == 'WATER' & TADA.ResultMeasure.MeasureUnitCode == 'UG/KG' &
             !is.na(TADA.ResultMeasureValue)))

data_media_filt_sums <- data_filt %>%
  group_by(Abbrev.Name) %>%
  reframe(Abbrev.Name = Abbrev.Name,
          n = n()) %>%
  unique()

####Initial Map####
all_data_2_media <- data_filt %>%
  left_join(state_num, by = c('StateCode' = 'STATE'))

states_w_data <- all_data_2_media %>%
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


#####Scatterpie#####
filt_pie <- states_w_data %>%
  select(STATE_NAME, ActivityMediaName, n_samples_media_type,
         n_samples_total, centroid, geometry) %>%
  pivot_wider(id_cols = c('STATE_NAME', 'n_samples_total', 'centroid', 'geometry'),
              names_from = 'ActivityMediaName',
              values_from = 'n_samples_media_type') %>%
  mutate(Tissue = ifelse(is.na(Tissue),0,Tissue),
         Water = ifelse(is.na(Water),0,Water)) %>%
  st_drop_geometry()

filt_pie2 <- extract(filt_pie, centroid, into = c('Lat', 'Lon'), '\\((.*),(.*)\\)', conv = T) %>%
  filter(!is.na(Lat)) %>%
  rename(`Surface Water` = Water) %>%
  mutate(radius = sqrt(n_samples_total)/25)



ggplot() +
  geom_sf(data = states, color = 'black', fill = 'gray90') +
  geom_scatterpie(data = as.data.frame(filt_pie2), 
                  aes(x = Lat, y = Lon, group = STATE_NAME, r = radius), 
                  cols = c("Tissue", "Surface Water"),
                  color = 'black', size = 0.1) +
  theme_bw() +
  scale_fill_manual(name = 'Media Type',
                    values = c(
                      "Tissue" = "#FF9999",   # Light red
                      "Surface Water" = "#99CCFF"    # Light blue
                    )) +
  xlab('')+
  ylab('')+
  theme(legend.position = 'top',
        legend.text = element_text(size = 8),    # Reduces legend text size
        legend.title = element_text(size = 8),   # Reduces legend title text size
        legend.key.size = unit(0.5, "lines"),
        axis.text = element_text(size = 5)) +
  guides(fill = guide_legend(override.aes = list(size = 0.5))) + 
  geom_scatterpie_legend(filt_pie2$radius, x=-120, y=20,
                         labeller=function(h) ((h*25)^2),
                         n = 5, 
                         breaks = c(0.4,
                                    0.8944271909999159,
                                    1.264911064067352,
                                    2.82842712474619,
                                    4),
                         size = 3)


ggsave('output/figures/scatterpie_map_water_tissue.jpg', units = 'in', width = 6, height = 6, dpi = 500)

####Data Processing####
#####1. Check Result Unit Validity#####
# This function adds the TADA.ResultUnit.Flag to the dataframe.
data_1 <- TADA_FlagResultUnit(data_filt, clean = 'none')
summary(as.factor(data_1$TADA.ResultUnit.Flag))
#####2. Check Sample Fraction Validity#####
# This function adds the TADA.SampleFraction.Flag to the dataframe.
data_2 <- TADA_FlagFraction(data_1, clean = F)
summary(as.factor(data_2$TADA.SampleFraction.Flag))

#####3. Check Method Speciation Validity#####
# This function adds the TADA.MethodSpeciation.Flag to the dataframe.
data_3 <- TADA_FlagSpeciation(data_2, clean = 'none')
summary(as.factor(data_3$TADA.MethodSpeciation.Flag))
# Cristina Mullin (USEPA/TADA) on October 30, 2023 (also in R Documentation): 
# "The “Not Reviewed” value means that the EPA WQX team has not yet reviewed the
## combinations (see https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV).
### The WQX team plans to review and update these new combinations quarterly.

#####4. Harmonize Characteristic Names#####
# This function adds the following columns to the dataframe:
# TADA.CharacteristicNameAssumptions
# TADA.SpeciationAssumptions     
# TADA.FractionAssumptions       
# TADA.Harmonized.Flag
data_4 <- TADA_HarmonizeSynonyms(data_3)

#####5. Flag unrealistic values#####
# This function adds the TADA.ResultValueAboveUpperThreshold.Flag to the dataframe.
data_5a <- TADA_FlagAboveThreshold(data_4, clean = F)
summary(as.factor(data_5a$TADA.ResultValueAboveUpperThreshold.Flag))

# See comment from Cristina Mullin above (Step 3)

# This function adds the TADA.ResultValueBelowLowerThreshold.Flag to the dataframe.
data_5b <- TADA_FlagBelowThreshold(data_5a, clean = F)
summary(as.factor(data_5b$TADA.ResultValueBelowLowerThreshold.Flag))
# See comment from Crstina Mullin above (Step 3)

#####6. Find continuous data#####
# This function adds the TADA.ContinuousData.Flag to the dataframe.
# data_6 <- TADA_FlagContinuousData(data_5b, clean = F, flaggedonly = FALSE)

#####7. Check method flags#####
# This function adds the TADA.AnalyticalMethod.Flag to the dataframe.
data_7 <- TADA_FlagMethod(data_5b, clean = F)
summary(as.factor(data_7$TADA.AnalyticalMethod.Flag))

#####8. Find potential duplicates#####
#Buffer distance set to 50 m, can change
# This function adds the following columns to the dataframe:
# TADA.NearbySiteGroups
# TADA.MultipleOrgDuplicate
# TADA.MultipleOrgDupGroupID
# TADA.ResultSelectedMultipleOrgs
data_8a <- TADA_FindPotentialDuplicatesMultipleOrgs(data_7, dist_buffer = 50) # Buffer distance can be changed.
summary(as.factor(data_8a$TADA.MultipleOrgDupGroupID))

# This function adds the following columns to the dataframe:
# TADA.SingleOrgDupGroupID
# TADA.SingleOrgDup.Flag
data_8b <- TADA_FindPotentialDuplicatesSingleOrg(data_8a)
summary(as.factor(data_8b$TADA.SingleOrgDup.Flag))


#####9. Find QC samples#####
# This function adds the TADA.ActivityType.Flag to the dataframe.
data_9 <- TADA_FindQCActivities(data_8b, clean = F)
summary(as.factor(data_9$TADA.ActivityType.Flag))

#####10. Flag invalid coordinates#####
# This function adds the TADA.InvalidCoordinates.Flag to the dataframe.
data_10 <- TADA_FlagCoordinates(data_9, clean_outsideUSA = 'no')
summary(as.factor(data_10$TADA.InvalidCoordinates.Flag))

#####11. Find any 'SUSPECT' samples#####
# This function adds the TADA.MeasureQualifierCode.Flag to the dataframe.
data_11a <- TADA_FlagMeasureQualifierCode(data_10, clean = F)
summary(as.factor(data_11a$TADA.MeasureQualifierCode.Flag))

# list uncategorized qualifiers in data
(uncategorized_qualifiers <- data_11a %>% 
    select(MeasureQualifierCode, TADA.MeasureQualifierCode.Flag) %>% 
    filter(TADA.MeasureQualifierCode.Flag == "uncategorized") %>% 
    distinct())

#####12. Replace non-detects #####
# This function adds the following columns to the dataframe:
# TADA.CensoredMethod
# TADA.CensoredData.Flag
# NOTE: This function uses the method detection limit

data_12 <- TADA_SimpleCensoredMethods(data_11a, 
                                      nd_method = 'multiplier',
                                      nd_multiplier = 1)
summary(as.factor(data_12$TADA.CensoredData.Flag))
#Use this total for TADA.MeasureQualifierCode!!! TADA_SimpleCensoredMethods
#updates this column
summary(as.factor(data_12$TADA.MeasureQualifierCode.Flag))

#####13. Identify columns with all NA values#####
# Check whether you expect data in any of the columns listed below.
(cols_NA <- data_12 %>% 
   keep(~all(is.na(.x))) %>% 
   names)

# Eliminate any columns with all NA values
data_13 <- data_12 %>% 
  select(where(~sum(!is.na(.x)) > 0)) 

#####Negative Values#####
data_14 <- data_13 %>%
  mutate(negative_value_flag = ifelse(TADA.ResultMeasureValue >= 0, 'PASS',
                                      'FAIL'))

summary(as.factor(data_14$negative_value_flag))

#####Result Detection Name####
detection_keynames <- data_14 %>%
  select(ResultDetectionConditionText) %>%
  unique()

data_15 <- data_14 %>%
  mutate(result_dection_name_flag = ifelse(TADA.CensoredData.Flag != 'Non-Detect',
                                           'PASS',
                                           'FAIL'))

summary(as.factor(data_15$result_dection_name_flag))

#####Detection Limit Units#####
units <- data_15 %>%
  select(TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode) %>%
  unique()

summary(as.factor(data_15$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode))


data_16 <- data_15 %>%
  mutate(detection_limit_numeric = case_when(TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode == 'UG/L'
                                             ~ TADA.DetectionQuantitationLimitMeasure.MeasureValue*1000,
                                             TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode == 'NG/ML'
                                             ~ TADA.DetectionQuantitationLimitMeasure.MeasureValue*1000,
                                             T ~ TADA.DetectionQuantitationLimitMeasure.MeasureValue),
         detection_limit_units = case_when(TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode == 'UG/L' ~
                                             'NG/L',
                                           TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode == 'NG/ML' ~
                                             'NG/L',
                                           T ~ TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode))



units_post <- data_16 %>%
  select(detection_limit_units) %>%
  unique()


#####Detection Limit Statistics#####
detect_summary <- data_16 %>%
  group_by(TADA.CharacteristicName, TADA.ActivityMediaName, detection_limit_units) %>%
  reframe(TADA.CharacteristicName = TADA.CharacteristicName,
          TADA.ActivityMediaName = TADA.ActivityMediaName,
          detection_limit_units = detection_limit_units,
          DL_avg = mean(detection_limit_numeric, na.rm=T),
          DL_median = median(detection_limit_numeric, na.rm=T),
          DL_std = sd(detection_limit_numeric, na.rm=T),
          DL_min = min(detection_limit_numeric, na.rm=T),
          DL_max = max(detection_limit_numeric, na.rm=T)) %>%
  unique() %>%
  filter(!is.na(detection_limit_units))

data_17 <- data_16 %>%
  left_join(detect_summary, by = c('TADA.CharacteristicName',
                                   'TADA.ActivityMediaName',
                                   'detection_limit_units')) %>%
  mutate(detection_limit_value_flag = case_when(detection_limit_numeric >= 2*DL_std + DL_avg &
                                                  (is.na(TADA.ResultMeasureValue) == T | TADA.CensoredData.Flag == 'Non-Detect') ~
                                                  'FAIL',
                                                detection_limit_numeric >= 2*DL_std + DL_median &
                                                  (is.na(TADA.ResultMeasureValue) == T | TADA.CensoredData.Flag == 'Non-Detect') ~
                                                  'FAIL',
                                                T ~ 'PASS'),
         sample_lower_than_detection_limit_flag = case_when(TADA.ResultMeasureValue < TADA.DetectionQuantitationLimitMeasure.MeasureValue &
                                                              TADA.CensoredData.Flag == 'Uncensored' ~
                                                              'Unknown',
                                                            TADA.CensoredData.Flag == 'Uncensored' ~
                                                              'Uncensored',
                                                            TADA.CensoredData.Flag == 'Non-Detect' ~
                                                              'Non-Detect',
                                                            T ~ NA))

summary(as.factor(data_17$detection_limit_value_flag))
summary(as.factor(data_17$sample_lower_than_detection_limit_flag))


#####EPA Methods#####
data_18 <- data_17 %>%
  mutate(EPA_method_flag = case_when(ResultAnalyticalMethod.MethodDescriptionText == 'EPA Method 537.1' &
                                       ResultAnalyticalMethod.MethodIdentifier == 'LM113' ~
                                       'PASS',
                                     ResultAnalyticalMethod.MethodDescriptionText == 'EPA 533' &
                                       ResultAnalyticalMethod.MethodIdentifier == 'LM119' ~
                                       'PASS',
                                     ResultAnalyticalMethod.MethodIdentifier == '533' ~
                                       'PASS',
                                     ResultAnalyticalMethod.MethodIdentifier == '1633' ~
                                       'PASS',
                                     ResultAnalyticalMethod.MethodIdentifier == '537.1' ~
                                       'PASS',
                                     ResultAnalyticalMethod.MethodIdentifier == '8327' ~
                                       'PASS',
                                     ResultAnalyticalMethod.MethodIdentifier == '537.1M-MDEQ-WQ' ~
                                       'PASS',
                                     ResultAnalyticalMethod.MethodIdentifier == 'EPA 537.1' ~
                                       'PASS',
                                     ResultAnalyticalMethod.MethodIdentifier == '537 Modified' ~
                                       'PASS',
                                     ResultAnalyticalMethod.MethodIdentifier == '537.1M-MDEQ-WQ' ~
                                       'PASS',
                                     T ~ 'FAIL')) 

summary(as.factor(data_18$EPA_method_flag))
summary(data_18$TADA.DetectionQuantitationLimitMeasure.MeasureValue)

####Export data with flags####
write_csv(data_18, 'output/EPATADA_Original_data_with_flags_tags.csv')

#Find non-detect detection limit range
summary(subset(data_18$TADA.DetectionQuantitationLimitMeasure.MeasureValue,
               data_18$TADA.CensoredData.Flag == 'Non-Detect'))

####Remove Flags####
# Flags not well explained. Vignettes don't match unique values.
# https://github.com/USEPA/TADA/blob/0eb2cb8e6abd29f214bc130382875e164e40310f/R/GenerateRefTables.R#L58C4-L58C4
# Assume the following:
# Not Reviewed <- "Not Reviewed" 
# Valid <- c("Accepted", "Y")
# Invalid <- c("Rejected", "Rejected ", "N")
# NonStandardized <- c("NonStandardized",
#                  "InvalidMediaUnit",
#                  "InvalidChar",
#                  "MethodNeeded")


samples_filtered <- data_18 %>%
  filter(TADA.ResultUnit.Flag != "Rejected"
         & TADA.ResultUnit.Flag != "Invalid") %>% # Step 1; critical. 0 samples.
  filter(TADA.SampleFraction.Flag != "Rejected"
         & TADA.SampleFraction.Flag != "Invalid") %>% # Step 2; critical. 0 samples.
  filter(TADA.MethodSpeciation.Flag != "Rejected"
         & TADA.MethodSpeciation.Flag != "Invalid") %>% # Step 3; critical. 0 samples.
  filter(TADA.AnalyticalMethod.Flag != "Rejected"
         & TADA.AnalyticalMethod.Flag != "Invalid") %>% # Step 7
  filter(TADA.SingleOrgDupGroupID == "Not a duplicate"
         | (TADA.SingleOrgDupGroupID != "Not a duplicate"
            & TADA.SingleOrgDup.Flag == "Unique")) %>% # Step 8; 57712 to 52942. SPOT CHECK
  filter(TADA.ActivityType.Flag == 'Non_QC') %>% # Step 9; critical. 52942 to 50102.
  filter(TADA.MeasureQualifierCode.Flag != 'Suspect' | is.na(TADA.MeasureQualifierCode.Flag))

#Export filtered data
write_csv(samples_filtered, 'output/EPATADA_priority_filtered_data.csv')


####Filtered Map####
filt_data_2_media <- samples_filtered %>%
  left_join(state_num, by = c('StateCode' = 'STATE'))

states_w_data <- filt_data_2_media %>%
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


#####Scatterpie#####
filt_pie <- states_w_data %>%
  select(STATE_NAME, ActivityMediaName, n_samples_media_type,
         n_samples_total, centroid, geometry) %>%
  pivot_wider(id_cols = c('STATE_NAME', 'n_samples_total', 'centroid', 'geometry'),
              names_from = 'ActivityMediaName',
              values_from = 'n_samples_media_type') %>%
  mutate(Tissue = ifelse(is.na(Tissue),0,Tissue),
         Water = ifelse(is.na(Water),0,Water)) %>%
  st_drop_geometry()

filt_pie2 <- extract(filt_pie, centroid, into = c('Lat', 'Lon'), '\\((.*),(.*)\\)', conv = T) %>%
  filter(!is.na(Lat)) %>%
  rename(`Surface Water` = Water) %>%
  mutate(radius = sqrt(n_samples_total)/25)


ggplot() +
  geom_sf(data = states, color = 'black', fill = 'gray90') +
  geom_scatterpie(data = as.data.frame(filt_pie2), 
                  aes(x = Lat, y = Lon, group = STATE_NAME, r = radius), 
                  cols = c("Tissue", "Surface Water"),
                  color = 'black', size = 0.1) +
  theme_bw() +
  scale_fill_manual(name = 'Media Type',
                    values = c(
                      "Tissue" = "#FF9999",   # Light red
                      "Surface Water" = "#99CCFF"    # Light blue
                    )) +
  xlab('')+
  ylab('')+
  theme(legend.position = 'top',
        legend.text = element_text(size = 8),    # Reduces legend text size
        legend.title = element_text(size = 8),   # Reduces legend title text size
        legend.key.size = unit(0.5, "lines"),
        axis.text = element_text(size = 5)) + 
  guides(fill = guide_legend(override.aes = list(size = 0.5))) + 
  geom_scatterpie_legend(filt_pie2$radius, x=-120, y=20,
                         labeller=function(h) ((h*25)^2),
                         n=5,
                         breaks = c(0.4,
                                    0.8944271909999159,
                                    1.264911064067352,
                                    2.82842712474619,
                                    4),
                         size = 3)


ggsave('output/figures/scatterpie_map_water_tissue_filtered.jpg', units = 'in', width = 6, height = 6, dpi = 500)
