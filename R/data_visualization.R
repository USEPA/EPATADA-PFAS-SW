#Data visualization (not scatterpies) for phase 2

#Written by: Hannah Ferriby (hannah.ferriby@tetratech.com)
#Date created: 2025-5-6
#Date updated: 2025-5-29

# Packages ---------------------------------------------------------------

{
  packages <- c(
  
	#IO
  'here',
	'janitor',
	'rio',
	
	#DB
	# 'duckdb',
	# 'arrow',
	# 'duckplyr',
	
	#Exploratory Data Analysis
	'skimr',
	
	#Plot
  'ragg',
  'esquisse',
	'geofacet',
	'patchwork',
	'marquee',
	'ggiraph',
	'geomtextpath',
	'ggpattern',
	'ggbump',
	'gghighlight',
	'ggdist',
  'ggforce',
  'ggdist',
  'gghalves'
  )

  # Loop through each package
  for (package in packages) {
    # Check if the package is installed
    if (!requireNamespace(package, quietly = TRUE)) {
      # If not installed, install the package
      install.packages(package)
    }
    # Load the package
    library(package, character.only = TRUE)
  }

packages <- c('EPATADA')

for(packages in packages) {
  if (!requireNamespace(packages, quietly = TRUE)) {
    # If not installed, install the package
    remotes::install_github(paste0("USEPA/", packages),
      ref = "develop",
    dependencies = TRUE,
    force = TRUE
)
  }
  # Load the package
  library(packages, character.only = TRUE)
}

  rm(packages, package)
}

skim_count <- skim_with(
  numeric = sfl(
    n = length,
    min = ~ min(.x, na.rm = T),
    median = ~ median(.x, na.rm = T),
    max = ~ max(.x, na.rm = T)
  )
)

# Raw --------------------------------------------------------------------

state_num <- read.table(
  here('data', 'state_codes.txt'),
  header = T,
  sep = "|",
  dec = ".",
  # Imports as character to avoid issues with leading zeros in state codes
  colClasses = "character"
)  %>% 
  select(STATE_NAME, STATE, STUSAB) %>% 
  # ! NOTE: Needed for geofacet to work properly
  mutate(
    STATE_NAME = case_when(
      STUSAB == 'MP' ~ "Northern Mariana Isl",
      STUSAB == 'VI' ~ "US Virgin Islands",
      .default = STATE_NAME
  )
) %>% 
  rename(
    state_name = STATE_NAME,
    state_code = STATE,
    state_abb = STUSAB
  )


all_data <- read_csv(here('output', 'EPATADA_priority_filtered_data.csv')) %>%
  left_join(state_num, by = c('StateCode' = 'state_code')) %>%
  mutate(
    TADA.ResultMeasure.MeasureUnitCode = ifelse(
      TADA.ResultMeasure.MeasureUnitCode == 'NG/ML',
      'UG/L',
      TADA.ResultMeasure.MeasureUnitCode
    ),
    Abbrev.Name = case_when(
      TADA.CharacteristicName == "PERFLUOROOCTANOIC ACID" ~ "PFOA",
      TADA.CharacteristicName == "PFOA ION" ~ "PFOA",
      TADA.CharacteristicName == "PERFLUOROOCTANESULFONATE (PFOS)" ~ "PFOS",
      TADA.CharacteristicName == "PERFLUOROOCTANESULFONATE" ~ "PFOS",
      TADA.CharacteristicName == "PERFLUOROOCTANE SULFONIC ACID" ~ "PFOS"
    )
  )

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
  # ! NOTE: Custom skim function to summarize data rather than using quantile
  skim(.)

summary(as.factor(all_data$Abbrev.Name))
summary(all_data$TADA.DetectionQuantitationLimitMeasure.MeasureValue)
summary(all_data$TADA.DetectionQuantitationLimitMeasure.MeasureValue[
  all_data$TADA.CensoredData.Flag == "Non-Detect"
])
summary(as.factor(all_data$TADA.CensoredData.Flag))

filtered_data <- read_csv(here('output', 'EPATADA_priority_filtered_data.csv')) %>%
  mutate(StateCode = as.character(StateCode)) %>%
  left_join(., state_num, by = c('StateCode' = 'state_code')) %>%
  mutate(
    Abbrev.Name = case_when(
      TADA.CharacteristicName == "PERFLUOROOCTANOIC ACID" ~ "PFOA",
      TADA.CharacteristicName == "PFOA ION" ~ "PFOA",
      TADA.CharacteristicName == "PERFLUOROOCTANESULFONATE (PFOS)" ~ "PFOS",
      TADA.CharacteristicName == "PERFLUOROOCTANESULFONATE" ~ "PFOS",
      TADA.CharacteristicName == "PERFLUOROOCTANE SULFONIC ACID" ~ "PFOS"
    )
  )

#Summary in ng/L
data_summary_surfacewater <- filtered_data %>%
  group_by(Abbrev.Name, sample_lower_than_detection_limit_flag) %>%
  summarise(
    count = n(),
    Min = min(TADA.ResultMeasureValue, na.rm = TRUE) * 1000,
    p25 = quantile(TADA.ResultMeasureValue, probs = 0.25, na.rm = TRUE) * 1000,
    median = quantile(TADA.ResultMeasureValue, probs = 0.5, na.rm = TRUE) *
      1000,
    mean = mean(TADA.ResultMeasureValue, na.rm = TRUE) * 1000,
    p75 = quantile(TADA.ResultMeasureValue, probs = 0.75, na.rm = TRUE) * 1000,
    Max = max(TADA.ResultMeasureValue, na.rm = TRUE) * 1000,
    propabovemin = sum(TADA.ResultMeasureValue > Min) / count
  ) %>%
  mutate(
    criteria_acute = case_when(
      Abbrev.Name == "PFOA" ~ 3100 * 1000,
      Abbrev.Name == "PFOS" ~ 71 * 1000,
      Abbrev.Name == "PFHxS" ~ 210 * 1000,
      Abbrev.Name == "PFDA" ~ 500 * 1000,
      Abbrev.Name == "PFNA" ~ 650 * 1000,
      Abbrev.Name == "PFBS" ~ 5000 * 1000
    ),
    criteria_chronic = case_when(
      Abbrev.Name == "PFOA" ~ 100 * 1000,
      Abbrev.Name == "PFOS" ~ 0.25 * 1000,
      TRUE ~ NA_real_
    ),
    drinking_water = case_when(
      Abbrev.Name == "PFOA" ~ 0.004 * 1000,
      Abbrev.Name == "PFOS" ~ 0.004 * 1000,
      Abbrev.Name == "PFHxS" ~ 0.010 * 1000,
      Abbrev.Name == "PFNA" ~ 0.010 * 1000,
      Abbrev.Name == "GenX" ~ 0.010 * 1000,
      Abbrev.Name == "PFBS" ~ 2.0 * 1000,
      TRUE ~ NA_real_
    )
  )

sum(data_summary_surfacewater$count)

write_csv(data_summary_surfacewater, 'output/figures/data_summary.csv')

####Summaries####
#####Total Samples by Compound#####
compound_totals <- all_data %>%
  group_by(Abbrev.Name) %>%
  reframe(Abbrev.Name = Abbrev.Name, count = n()) %>%
  unique()

sum(compound_totals$count)

#####Exceedance#####

#REMOVE NONDETECTS FOR CALCS
exceedance <- all_data %>%
  filter(TADA.CensoredData.Flag == "Uncensored") %>%
  filter(ActivityMediaSubdivisionName == "Surface Water") %>%
  filter(
    Abbrev.Name %in% c('PFOA', 'PFOS', 'PFHxS', 'PFDA', 'PFNA', 'PFBS')
  ) %>%
  mutate(
    exceedance_flag_acute = case_when(
      Abbrev.Name == "PFOA" & TADA.ResultMeasureValue >= 3100 ~ 1,
      Abbrev.Name == "PFOS" & TADA.ResultMeasureValue >= 71 ~ 1,
      TRUE ~ 0
    ),
    exceedance_flag_chronic = case_when(
      Abbrev.Name == "PFOA" & TADA.ResultMeasureValue >= 100 ~ 1,
      Abbrev.Name == "PFOS" & TADA.ResultMeasureValue >= 0.25 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  group_by(Abbrev.Name, TADA.CensoredData.Flag) %>%
  reframe(
    Abbrev.Name = Abbrev.Name,
    sample_lower_than_detection_limit_flag = TADA.CensoredData.Flag,
    count_uncensored = n(),
    exceedance_acute = sum(exceedance_flag_acute),
    exceedance_acute_perc = sum(exceedance_flag_acute) / count_uncensored * 100,
    exceedance_chronic = sum(exceedance_flag_chronic),
    exceedance_chronic_perc = sum(exceedance_flag_chronic) /
      count_uncensored *
      100
  ) %>%
  unique()

pfos_exceed <- all_data %>%
  filter(Abbrev.Name == 'PFOA' & TADA.ResultMeasureValue >= 100)


data_summary_surfacewater_exceed <- data_summary_surfacewater %>%
  left_join(
    exceedance,
    by = c('Abbrev.Name', 'sample_lower_than_detection_limit_flag')
  )

write_csv(
  data_summary_surfacewater_exceed,
  'output/data_summary_surfacewater_exceed.csv'
)


#####Summarize by EPA region#####
samples_epa_region_flagged <- all_data %>%
  filter(TADA.ActivityMediaName == 'WATER') %>%
  select(
    Abbrev.Name,
    TADA.ResultMeasureValue,
    TADA.ResultMeasure.MeasureUnitCode,
    STATE_NAME
  ) %>%
  filter(!is.na(STATE_NAME)) %>%
  mutate(
    EPA_Region = case_when(
      STATE_NAME %in%
        c(
          'Maine',
          'New Hampshire',
          'Vermont',
          'Massachusetts',
          'Rhode Island',
          'Connecticut'
        ) ~
        '1',
      STATE_NAME %in%
        c('New York', 'New Jersey', 'Puerto Rico', 'Virgin Islands') ~
        '2',
      STATE_NAME %in%
        c('Pennsylvania', 'Delaware', 'Maryland', 'West Virginia', 'Virginia') ~
        '3',
      STATE_NAME %in%
        c(
          'Kentucky',
          'Tennessee',
          'North Carolina',
          'South Carolina',
          'Mississippi',
          'Alabama',
          'Georgia',
          'Florida'
        ) ~
        '4',
      STATE_NAME %in%
        c('Michigan', 'Ohio', 'Indiana', 'Illinois', 'Wisconsin', 'Minnesota') ~
        '5',
      STATE_NAME %in%
        c('Louisiana', 'Arkansas', 'Oklahoma', 'Texas', 'New Mexico') ~
        '6',
      STATE_NAME %in% c('Iowa', 'Missouri', 'Nebraska', 'Kansas') ~ '7',
      STATE_NAME %in%
        c(
          'North Dakota',
          'South Dakota',
          'Montana',
          'Wyoming',
          'Colorado',
          'Utah'
        ) ~
        '8',
      STATE_NAME %in% c('Hawaii', 'Guam', 'California', 'Nevada', 'Arizona') ~
        '9',
      STATE_NAME %in% c('Idaho', 'Oregon', 'Washington', 'Alaska') ~ '10',
      T ~ NA
    )
  ) %>%
  group_by(EPA_Region, Abbrev.Name, TADA.ResultMeasure.MeasureUnitCode) %>%
  reframe(
    EPA_Region = EPA_Region,
    Abbrev.Name = Abbrev.Name,
    n_samples = n(),
    Quantile_25 = quantile(TADA.ResultMeasureValue, 0.25, na.rm = T),
    Mean = mean(TADA.ResultMeasureValue, na.rm = T),
    Median = median(TADA.ResultMeasureValue, na.rm = T),
    Quantile_75 = quantile(TADA.ResultMeasureValue, 0.75, na.rm = T)
  ) %>%
  ungroup() %>%
  unique()

write_csv(
  samples_epa_region_flagged,
  'output/figures/samples_epa_region_flagged.csv'
)


#####Nondetects#####
# samples_nd_flagged <- all_data %>%
#   filter(TADA.ActivityMediaName == 'WATER') %>%
#   filter(TADA.ResultMeasureValueDataTypes.Flag == 'Result Value/Unit Estimated from Detection Limit')

ggplot(
  filtered_data,
  aes(
    x = TADA.DetectionQuantitationLimitMeasure.MeasureValue,
    y = TADA.ResultMeasureValue * 1000,
    color = sample_lower_than_detection_limit_flag
  )
) +
  geom_point(alpha = 0.5, size = 1) +
  facet_wrap(vars(Abbrev.Name), ncol = 7) +
  scale_x_log10(n.breaks = 6, labels = label_comma(drop0trailing = TRUE)) +
  scale_y_log10(n.breaks = 6, labels = label_comma(drop0trailing = TRUE)) +
  scale_color_manual(
    values = c("#fdae6b", "#377eb8", '#D2042D'),
    labels = c("Non-Detect", "Uncensored", "Ambiguous")
  ) +
  #geom_hline(data = data_summary_surfacewater,
  #aes(yintercept = drinking_water), color = "#377eb8") +
  geom_hline(
    data = subset(
      data_summary_surfacewater,
      sample_lower_than_detection_limit_flag == "Uncensored"
    ),
    aes(yintercept = Min),
    color = "#377eb8"
  ) +
  geom_hline(
    data = data_summary_surfacewater,
    aes(yintercept = criteria_acute),
    lty = 2
  ) +
  geom_hline(
    data = data_summary_surfacewater,
    aes(yintercept = criteria_chronic),
    lty = 2
  ) +
  labs(
    y = expression("Concentration (ng/L)"),
    x = expression("MDL (ng/L)"),
    color = "Tag"
  ) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  ) #+
#guides(color = guide_legend(nrow = 2, byrow = TRUE))
ggsave(
  'output/figures/nondetect_scatterplot.jpg',
  height = 5,
  width = 8,
  dpi = 500
)

#### nondetect_scatterplot FLIPPED Axis
ggplot(
  filtered_data,
  aes(
    x = TADA.ResultMeasureValue,
    y = TADA.DetectionQuantitationLimitMeasure.MeasureValue,
    color = sample_lower_than_detection_limit_flag
  )
) +
  geom_point(alpha = 0.5, size = 1) +
  facet_wrap(vars(Abbrev.Name), ncol = 7) +
  scale_x_log10(n.breaks = 6, labels = label_comma(drop0trailing = TRUE)) +
  scale_y_log10(n.breaks = 5, labels = label_comma(drop0trailing = TRUE)) +
  scale_color_manual(
    values = c("#fdae6b", "#377eb8", '#D2042D'),
    labels = c("Non-Detect", "Uncensored", "Ambiguous")
  ) +
  geom_vline(
    data = data_summary_surfacewater,
    aes(xintercept = drinking_water),
    color = "#377eb8"
  ) +
  geom_vline(
    data = data_summary_surfacewater,
    aes(xintercept = criteria_acute),
    lty = 2
  ) +
  geom_vline(
    data = data_summary_surfacewater,
    aes(xintercept = criteria_chronic),
    lty = 2
  ) +
  labs(
    y = expression("Concentration (" * mu * "g/L)"),
    x = expression("MDL (" * mu * "g/L)"),
    color = "Tag"
  ) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  ) #+
#guides(color = guide_legend(nrow = 2, byrow = TRUE))
ggsave(
  'output/figures/nondetect_scatterplot_flipped.jpg',
  height = 5,
  width = 8,
  dpi = 500
)

####Summary Tables####

#####Summarize by Media#####
table_by_media <- all_data %>%
  group_by(ActivityMediaName) %>%
  reframe(ActivityMediaName = ActivityMediaName, n_samples = n()) %>%
  unique()

write_csv(table_by_media, 'output/figures/table_by_media.csv')

#####Summarize by Species#####
table_by_species <- all_data %>%
  filter(!is.na(SubjectTaxonomicName)) %>%
  group_by(SubjectTaxonomicName) %>%
  reframe(
    SubjectTaxonomicName = SubjectTaxonomicName,
    'Number of Sample' = n()
  ) %>%
  unique()

write_csv(table_by_species, 'output/figures/table_by_species.csv')

#### Tables by state

MN_data <- all_data %>%
  filter(STATE_NAME == 'Minnesota')
WI_data <- all_data %>%
  filter(STATE_NAME == 'Wisconsin')

table_by_species_MN <- MN_data %>%
  filter(!is.na(SubjectTaxonomicName)) %>%
  group_by(SubjectTaxonomicName) %>%
  reframe(
    SubjectTaxonomicName = SubjectTaxonomicName,
    'Number of Sample' = n()
  ) %>%
  unique()

write_csv(table_by_species_MN, 'output/figures/table_by_species_MN.csv')

table_by_species_WI <- WI_data %>%
  filter(!is.na(SubjectTaxonomicName)) %>%
  group_by(SubjectTaxonomicName) %>%
  reframe(
    SubjectTaxonomicName = SubjectTaxonomicName,
    'Number of Sample' = n()
  ) %>%
  unique()

write_csv(table_by_species_WI, 'output/figures/table_by_species_WI.csv')

#####Summarize by State#####
table_by_state <- all_data %>%
  filter(!is.na(STATE_NAME)) %>%
  group_by(STATE_NAME) %>%
  reframe(STATE_NAME = STATE_NAME, `Number of Samples` = n()) %>%
  unique()

table_by_state_flag <- filtered_data %>%
  filter(!is.na(STATE_NAME)) %>%
  group_by(STATE_NAME) %>%
  reframe(
    STATE_NAME = STATE_NAME,
    `Number of Samples After Critical Flagging` = n()
  ) %>%
  unique()

table_by_state_combo <- table_by_state %>%
  left_join(table_by_state_flag, by = 'STATE_NAME')

write_csv(table_by_state_combo, 'output/figures/table_by_state.csv')

#####Summarize by All#####
table_by_all <- all_data %>%
  group_by(STATE_NAME, ActivityMediaName) %>%
  reframe(
    STATE_NAME = STATE_NAME,
    ActivityMediaName = ActivityMediaName,
    n_samples = n()
  ) %>%
  unique() %>%
  arrange(STATE_NAME, ActivityMediaName)

write_csv(table_by_all, 'output/figures/table_by_all.csv')


#####Summarize by site by number of PFAS compounds reported & media#####
table_by_pfas_compounds <- all_data %>%
  select(
    ActivityMediaName,
    TADA.CharacteristicName,
    MonitoringLocationName,
    TADA.LatitudeMeasure,
    TADA.LongitudeMeasure
  ) %>%
  unique() %>%
  group_by(
    MonitoringLocationName,
    TADA.LatitudeMeasure,
    TADA.LongitudeMeasure,
    ActivityMediaName
  ) %>%
  reframe(
    MonitoringLocationName = MonitoringLocationName,
    TADA.LatitudeMeasure = TADA.LatitudeMeasure,
    TADA.LongitudeMeasure = TADA.LongitudeMeasure,
    ActivityMediaName = ActivityMediaName,
    n_pfas_compounds = n()
  ) %>%
  unique()

####Boxplots####

all_data_4_plot <- filtered_data %>%
  mutate(
    TADA.CharacteristicName = case_when(
      TADA.CharacteristicName == 'PERFLUOROOCTANESULFONATE' ~
        'PERFLUOROOCTANE SULFONIC ACID',
      TADA.CharacteristicName == 'PFOA ION' ~ 'PERFLUOROOCTANOIC ACID',
      T ~ TADA.CharacteristicName
    )
  ) %>%
  filter(TADA.ResultMeasure.MeasureUnitCode != 'NONE') %>%
  mutate(
    TADA.ActivityMediaName = ifelse(
      TADA.ActivityMediaName == 'WATER',
      'SURFACE WATER',
      TADA.ActivityMediaName
    )
  )
options(scipen = 10000)


# Combo plot -------------------------------------------------------------

all_data_4_plot %>% 
ggplot(
  aes(
    x = Abbrev.Name,
    y = TADA.ResultMeasureValue, 
    fill = TADA.ActivityMediaName, 
    color = TADA.ActivityMediaName
  )
) +

# ggdist::stat_halfeye(
#   adjust = .5, 
#   width = .6, 
#   .width = 0, 
#   justification = -.2, 
#   point_colour = NA
# ) +

# ggforce::geom_sina(
#   position = 'identity'
# ) +  

ggdist::stat_dots(
    ## orientation to the left
    side = "top", 
    # Moves the plot to the right
    justification = -0.12, 
  ) +   
  
gghalves::geom_half_point(
    position = 'identity',
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2
  ) +
  
  # geom_point(
  #   size = 1.5,
  #   alpha = .2,
  #   position = position_jitter(
  #     seed = 1, width = .1
    
  #     )
  # ) + 
  geom_boxplot(
    position = 'identity',
    width = .15, 
    outlier.shape = NA,
    fill = NA
  ) +
  
scale_fill_manual(
    values = c(
      "SURFACE WATER" = "#377eb8",
      "TISSUE" = "#e41a1c"
    ),
    name = 'Media Type'
  ) +    
scale_color_manual(
    values = c(
      "SURFACE WATER" = "#377eb8",
      "TISSUE" = "#e41a1c"
    ),
    name = 'Media Type'
  ) + 
scale_y_log10(
  limits = c(1e-4, 1e3),
  #labels = waiver()
  labels = label_log(digits = 2) 
  ) +
guides(x = guide_axis(angle = 45)) +
xlab('Compound') +
ylab('Concentration (ng/L)') +
theme(
  panel.background = element_rect(fill = 'white', color = 'white'),
  panel.border = element_rect(color = 'black', fill = NA),
  panel.spacing = unit(2, "lines"),
  panel.grid.major.x = element_line(colour = "grey"),
  panel.grid.minor.x = element_line(colour = "grey"),
  strip.background = element_blank(),
  axis.title.x = element_text(size = 10, face = "bold"),
  axis.title.y =  element_text(size = 10, face = "bold"),
  axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),  # Rotate x-axis text for better readability
  axis.text.y = element_text(face = "bold")
) + coord_flip()

ggsave(
  here('output', 'params_by_media_raincloud_p4.jpg'),
  units = 'in',
  height = 20,
  width = 24
)

# ! STOPPED HERE SXT

#####Jitter plot#####

ggplot() +
  geom_jitter(
    data = all_data,
    aes(x = Abbrev.Name, y = TADA.ResultMeasureValue),
    alpha = 0.25
  ) +
  theme_bw() +
  scale_y_log10() +
  facet_wrap(~TADA.ActivityMediaName, scales = 'free_y', nrow = 2) +
  xlab('Characteristic Name') +
  ylab('Sample Result Value') +
  guides(x = guide_axis(angle = 45)) +
  theme(
    text = element_text(size = 8),
    strip.background = element_rect(fill = "gray99")
  )

ggsave(
  'output/figures/params_by_media_scatter.jpg',
  units = 'in',
  height = 5,
  width = 6
)


#####tissue vs concentration #####
##make df for just MN data
MN_data <- all_data %>%
  filter(STATE_NAME == 'Minnesota')
WI_data <- all_data %>%
  filter(STATE_NAME == 'Wisconsin')

### ActivityStartDate ActivityStartDateTime

MN_wide <- MN_data %>%
  pivot_wider(
    id_cols = c(MonitoringLocationName, CharacteristicName),
    names_from = TADA.ActivityMediaName,
    values_from = TADA.ResultMeasureValue,
    #values_fn = list(ResultMeasureValue = mean),
    values_fn = mean,
    values_fill = NA
  )

WI_wide <- WI_data %>%
  pivot_wider(
    id_cols = c(MonitoringLocationName, CharacteristicName),
    names_from = TADA.ActivityMediaName,
    values_from = TADA.ResultMeasureValue,
    values_fn = mean,
    values_fill = NA
  )

### MN and WI line plot

MN_fig <- ggplot(MN_wide, aes(x = WATER, y = TISSUE)) +
  geom_point() +
  ggtitle("Water vs Tissue at co-sampled locations in Minnesota") +
  xlim(0, .1) +
  xlab("Water Concentration (ug/L)") +
  ylab("Tissue Concentration (ug/kg)") +
  geom_smooth(method = lm) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE
  )
MN_fig

ggsave(
  'output/figures/linear_regression_MN.jpg',
  units = 'in',
  height = 5,
  width = 6
)


WI_fig <- ggplot(WI_wide, aes(x = WATER, y = TISSUE)) + geom_point()
WI_fig

##### MN and WI state box plots combined

MN_WI_data <- all_data %>%
  filter(STATE_NAME == 'Minnesota' | STATE_NAME == 'Wisconsin')

MN_WI_box <- MN_WI_data %>%
  mutate(
    TADA.CharacteristicName = case_when(
      TADA.CharacteristicName == 'PERFLUOROOCTANESULFONATE' ~
        'PERFLUOROOCTANE SULFONIC ACID',
      TADA.CharacteristicName == 'PFOA ION' ~ 'PERFLUOROOCTANOIC ACID',
      T ~ TADA.CharacteristicName
    )
  ) %>%
  filter(TADA.ResultMeasure.MeasureUnitCode != 'NONE')
options(scipen = 10000)


media.labs <- c("TISSUE", "WATER")
names(media.labs) <- c("Tissue (ug/kg)", "Water (ug/L)")

state.labs <- c("Minnesota", "Wisconsin")
names(state.labs) <- c("Minn", "Wisc")


ggplot() +
  geom_boxplot(
    data = MN_WI_box,
    aes(x = Abbrev.Name, y = TADA.ResultMeasureValue)
  ) +
  # geom_jitter(data = all_data, aes(x = TADA.CharacteristicName,
  #                                  y = TADA.ResultMeasureValue)) +
  theme_bw() +
  scale_y_log10() +
  facet_grid(
    rows = vars(STATE_NAME),
    cols = vars(TADA.ActivityMediaName),
    scales = 'free_y',
    labeller = labeller(
      TADA.ActivityMediaName = c(
        "TISSUE" = "Tissue (ug/kg)",
        "WATER" = "Water (ug/L)"
      )
    )
  ) +
  xlab('Characteristic Name') +
  ylab('Sample Result Value') +
  guides(x = guide_axis(angle = 45)) +
  theme(
    text = element_text(size = 8),
    strip.background = element_rect(fill = "gray99")
  )

ggsave(
  'output/figures/params_by_media_boxplot_MN_WI.jpg',
  units = 'in',
  height = 5,
  width = 6
)


#### Box plots for each state

MN_box <- MN_data %>%
  mutate(
    TADA.CharacteristicName = case_when(
      TADA.CharacteristicName == 'PERFLUOROOCTANESULFONATE' ~
        'PERFLUOROOCTANE SULFONIC ACID',
      TADA.CharacteristicName == 'PFOA ION' ~ 'PERFLUOROOCTANOIC ACID',
      T ~ TADA.CharacteristicName
    )
  ) %>%
  filter(TADA.ResultMeasure.MeasureUnitCode != 'NONE')
options(scipen = 10000)

facet_names <- list(
  'TISSUE' = "Tissue (ug/kg)",
  'WATER' = "Water (ug/L)"
)

facet_labeller <- function(variable, value) {
  return(facet_names[value])
}


ggplot() +
  geom_boxplot(
    data = MN_box,
    aes(x = Abbrev.Name, y = TADA.ResultMeasureValue)
  ) +
  # geom_jitter(data = all_data, aes(x = TADA.CharacteristicName,
  #                                  y = TADA.ResultMeasureValue)) +
  theme_bw() +
  scale_y_log10() +
  facet_grid(
    #rows = vars(TADA.ResultMeasure.MeasureUnitCode),
    cols = vars(TADA.ActivityMediaName),
    scales = 'free_y',
    labeller = facet_labeller
  ) +
  xlab('Characteristic Name') +
  ylab('Sample Result Value') +
  guides(x = guide_axis(angle = 45)) +
  theme(text = element_text(size = 8)) +
  ggtitle('Minnesota')

ggsave(
  'output/figures/params_by_media_boxplot_MN.jpg',
  units = 'in',
  height = 5,
  width = 6
)

WI_box <- WI_data %>%
  mutate(
    TADA.CharacteristicName = case_when(
      TADA.CharacteristicName == 'PERFLUOROOCTANESULFONATE' ~
        'PERFLUOROOCTANE SULFONIC ACID',
      TADA.CharacteristicName == 'PFOA ION' ~ 'PERFLUOROOCTANOIC ACID',
      T ~ TADA.CharacteristicName
    )
  ) %>%
  filter(TADA.ResultMeasure.MeasureUnitCode != 'NONE')
options(scipen = 10000)


facet_names <- list(
  'TISSUE' = "Tissue (ug/kg)",
  'WATER' = "Water (ug/L)"
)

facet_labeller <- function(variable, value) {
  return(facet_names[value])
}

ggplot() +
  geom_boxplot(
    data = WI_box,
    aes(x = Abbrev.Name, y = TADA.ResultMeasureValue)
  ) +
  # geom_jitter(data = all_data, aes(x = TADA.CharacteristicName,
  #                                  y = TADA.ResultMeasureValue)) +
  theme_bw() +
  scale_y_log10() +
  facet_grid(
    #rows = vars(TADA.ResultMeasure.MeasureUnitCode),
    cols = vars(TADA.ActivityMediaName),
    scales = 'free_y',
    labeller = facet_labeller
  ) +
  xlab('Characteristic Name') +
  ylab('Sample Result Value') +
  guides(x = guide_axis(angle = 45)) +
  theme(text = element_text(size = 8)) +
  ggtitle('Wisconsin')

ggsave(
  'output/figures/params_by_media_boxplot_WI.jpg',
  units = 'in',
  height = 5,
  width = 6
)

# code to generate sample sizes

y_breaks = c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000)
give.n <- function(x) {
  return(c(y = (min(x) - 0.5), label = length(x)))
}

ggplot(
  all_data,
  aes(
    y = TADA.ResultMeasureValue * 1000,
    x = Abbrev.Name,
    fill = sample_lower_than_detection_limit_flag,
    color = sample_lower_than_detection_limit_flag
  )
) +
  geom_boxplot(
    data = filtered_data,
    alpha = 0.8,
    aes(
      fill = sample_lower_than_detection_limit_flag,
      color = sample_lower_than_detection_limit_flag
    ),
    position = position_dodge(preserve = "single")
  ) +
  #geom_point(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.2, size = 0.5) +
  stat_summary(
    fun.data = give.n,
    geom = "text",
    size = 2,
    position = position_dodge(width = 0.75)
  ) +
  geom_segment(
    data = data_summary_surfacewater,
    aes(
      x = as.numeric(Abbrev.Name) - 0.3,
      xend = as.numeric(Abbrev.Name) + 0.3,
      y = criteria_acute,
      yend = criteria_acute
    ),
    lty = 2,
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = data_summary_surfacewater,
    aes(
      x = as.numeric(Abbrev.Name) - 0.3,
      xend = as.numeric(Abbrev.Name) + 0.3,
      y = criteria_chronic,
      yend = criteria_chronic
    ),
    lty = 2,
    inherit.aes = FALSE
  ) +
  scale_y_log10(
    breaks = y_breaks,
    labels = scales::label_number(drop0trailing = TRUE)
  ) +
  scale_fill_manual(
    values = c("#fdae6b", "#377eb8", '#D2042D'),
    labels = c("Non-Detect", "Uncensored", "Ambiguous")
  ) +
  scale_color_manual(
    values = c("#ca8b55", "#265880", '#690216'),
    labels = c("Non-Detect", "Uncensored", "Ambiguous")
  ) +
  labs(
    x = "Compound",
    y = "Concentration (ng/L)",
    fill = "Tag",
    color = 'Tag'
  ) + #, title = "PFAS Concentration in Surface Water") +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


ggsave(
  'output/figures/params_surfacewater_boxplot.jpg',
  height = 4,
  width = 8,
  dpi = 500
)

all_filt_data_4_plot <- filtered_data %>%
  mutate(
    TADA.CharacteristicName = case_when(
      TADA.CharacteristicName ==
        '1-HEPTANESULFONIC ACID, 1,1,2,2,3,3,4,4,5,5,6,6,7,7,7-PENTADECAFLUORO-' ~
        '1-HEPTANESULFONIC ACID,1,1,2,2,3,3,\n4,4,5,5,6,6,7,7,7-PENTADECAFLUORO-',
      T ~ TADA.CharacteristicName
    )
  ) %>%
  filter(TADA.ResultMeasure.MeasureUnitCode != 'NONE')


####Scatterpie but table form####
states_w_data <- all_data %>%
  group_by(STATE_NAME) %>%
  mutate(n_samples_total = n()) %>%
  ungroup() %>%
  group_by(STATE_NAME, ActivityMediaName) %>%
  reframe(
    STATE_NAME = STATE_NAME,
    ActivityMediaName = ActivityMediaName,
    n_samples_media_type = n(),
    n_samples_total = n_samples_total
  ) %>%
  unique() %>%
  left_join(states, by = c('STATE_NAME' = 'NAME')) %>%
  mutate(centroid = st_centroid(geometry)) %>%
  select(
    STATE_NAME,
    ActivityMediaName,
    n_samples_total,
    n_samples_media_type,
    centroid,
    geometry
  )

filt_pie <- states_w_data %>%
  select(
    STATE_NAME,
    ActivityMediaName,
    n_samples_media_type,
    n_samples_total,
    centroid,
    geometry
  ) %>%
  pivot_wider(
    id_cols = c('STATE_NAME', 'n_samples_total', 'centroid', 'geometry'),
    names_from = 'ActivityMediaName',
    values_from = 'n_samples_media_type'
  ) %>%
  mutate(
    Tissue = ifelse(is.na(Tissue), 0, Tissue),
    Water = ifelse(is.na(Water), 0, Water)
  ) %>%
  st_drop_geometry()

pie_table <- filt_pie %>%
  select(!c(geometry, centroid)) %>%
  rename(State = STATE_NAME, `Total Samples` = n_samples_total)

write_csv(pie_table, 'output/figures/samples_by_media_by_state.csv')
