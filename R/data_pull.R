#PFAS Data Pull
#Following instructions outlined by Matt Dunn in
#notebook entry from 2025-04-23

#Written by: Hannah Ferriby, hannah.ferriby@tetratech.com
#Date created: 2025-5-2
#Date updated: 2025-5-29

# Packages ---------------------------------------------------------------

{
  packages <- c(
    'tidyverse',
    'sf',
    'scales',
    'scatterpie',
    # ! NOTE Suggests for better experience
    'here',
    'geofacet',
    #'tigris',
    'ggforce',
    'ggdist',
    'remotes'
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


# Download ---------------------------------------------------------------

data <- TADA_DataRetrieval(
  characteristicName = c(
    'PFOA ion',
    'Perfluorooctanoic acid',
    # , 'PERFLUOROOCTANOIC ACID'
    'Perfluorooctanesulfonate',
    'Perfluorooctane sulfonic acid',
    # , 'Potassium perfluorooctanesulfonate'
    'Perfluorooctanesulfonate (PFOS)'
    # , 'POTASSIUM PERFLUOROOCTANESULFONATE'
  ),
  # sampleMedia = c('Water', 'Tissue'),
  applyautoclean = T,
  ask = F
)

# Export of raw ----------------------------------------------------------

write_csv(data, here('output','data_pull.csv'))


# Spatial ----------------------------------------------------------------

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

# ? May not even be needed. 
# states <- tigris::states(year = 2018, resolution = "500k") %>%
#   # ! NOTE: Needed for geofacet to work properly
#   mutate(
#     NAME = case_when(
#       STUSPS == 'MP' ~ "Northern Mariana Isl",
#       STUSPS == 'VI' ~ "US Virgin Islands",
#       .default = NAME
#     )
#   )

# Cleaning ---------------------------------------------------------------

states_w_data <-  data %>%
  left_join(., state_num, join_by('StateCode' == 'state_code'))
  group_by(state_name) %>%
  mutate(n_samples_total = n()) %>%
  ungroup() %>%
  group_by(state_name, ActivityMediaName) %>%
  reframe(
    state_name = state_name,
    #StateCode = StateCode,
    ActivityMediaName = ActivityMediaName,
    n_samples_media_type = n(),
    n_samples_total = n_samples_total
  ) %>%
  unique() %>%
  select(
    state_name,
    ActivityMediaName,
    n_samples_total,
    n_samples_media_type
  )

# Plotting data ----------------------------------------------------------

plot_data <- states_w_data %>%
  # Pivot the data from long to wide format
  pivot_wider(
    id_cols = c('state_name', 'n_samples_total'), # Columns to keep as identifiers
    names_from = 'ActivityMediaName', # Column whose values will become new column names
    values_from = 'n_samples_media_type', # Column whose values will populate the new columns
    values_fill = NA # Fill missing values with 0
  ) %>% 
  pivot_longer(
    cols = -c(state_name, n_samples_total), # Columns to keep as identifiers
    names_to = 'media', # New column for the names of the previous columns
    values_to = 'n' # New column for the values of the previous columns
  ) %>%
  mutate(
    media = case_when(
      media == 'Biological Tissue' ~ 'Tissue',
      TRUE ~ media # Keep other values as they are
    ),
    media = factor(
      media,
      levels = c(
        "Water",
        "Sediment",
        "Soil",
        "Air",
        "Tissue",
        "Other"
      )
    )
  )

{
  p1 <- plot_data %>%
    # ! NOTE Removes state that doesn't have a name?
    # TODO Follow up why this is happening
    filter(!is.na(state_name) & !is.na(n)) %>%
    ggplot() +
    geom_bar(
      aes(x = media, y = n, fill = media),
      stat = 'identity'
    ) +
    scale_y_log10(limits = c(1, 10000), labels = waiver()) +
    scale_fill_viridis_d(
      name = 'Media',
      #option = 'D' #viridis
      option = 'H' #turbo
    ) +
    theme(
      panel.background = element_rect(fill = 'white', color = 'white'),
      panel.border = element_rect(color = 'black', fill = NA),
      panel.spacing = unit(2, "lines"),
      panel.grid.major.y = element_line(colour = "grey"),
      strip.background = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1), # Rotate x-axis text for better readability
    ) + #facet_wrap('state_name')
  
    facet_geo(
      ~state_name,
      grid = "us_states_territories_grid2",
      label = "name",
      move_axes = TRUE
    )

  p1

  ggsave(
    p1,
    filename = here('output', 'pfas_by_state_p1.png'),
    width = 20,
    height = 12.5,
    units = 'in',
    dpi = 500,
    bg = 'white',
    limitsize = FALSE
  )
}
