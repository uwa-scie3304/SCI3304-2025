###LAST YEARS


# Make cumulative MaxN - to visualise the most abundant species
top_species <- count %>%
  group_by(scientific) %>%
  summarise(sum_maxn = sum(count)) %>%
  arrange(sum_maxn) %>%
  glimpse()


# Check the data----

# Plot the most abundant species using ggplot
ggplot(data = top_species, aes(x = reorder(scientific, sum_maxn),
                               y = sum_maxn)) +
  geom_bar(stat = "identity", colour = "black", fill = "lightgrey",
           position = position_dodge()) +
  coord_flip() +
  labs(x = expression(Overall~abundance~(Sigma~MaxN)), y = "Species") +
  theme_classic()



# Format for plotting and modeling----
# Format the data to a format suitable to use in modelling scripts
# This needs to be long format data, with one line per deployment/opcode
# In this example we are just interested in the abundance of King George Whiting, so the data is filtered to this species, and a character column added (this will make sense when you run the modelling)

kgw <- count %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus") %>%
  dplyr::mutate(response = "Abundance of KGW") %>%
  glimpse()

# Save the count data
saveRDS(kgw, file = paste0("data/staging/", name, "_tidy-count.rds"))


# Bring in size of maturity----
# Load size of maturity cut off's data - this is loaded by the CheckEM package
maturity_mean <- maturity %>%
  dplyr::group_by(family, genus, species, sex) %>%
  dplyr::slice(which.min(l50_mm)) %>%
  ungroup() %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(Lm = mean(l50_mm)) %>%
  ungroup() %>%
  glimpse()


# Load length annotation data and join with size of maturity dataset
length <- read.csv(paste0("data/raw/fish/", name, "_length.csv")) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  left_join(maturity_mean) %>%
  glimpse()

# Create a 'metadata' file for lengths - this is a list of all the unique deployments/opcodes, so that the 0s can be added back in
metadata_length <- length %>%
  distinct(campaignid, opcode, latitude_dd, longitude_dd, depth_m, status)


# Create abundance by size class data----

# Create a dataframe for the abundance of greater than size of maturity King George Whiting
mature_kgw <- length %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus", length_mm > Lm) %>%
  group_by(campaignid, opcode) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "Mature KGW") %>%
  glimpse()

# Create a dataframe for the abundance of smaller than size of maturity King George Whiting
immature_kgw <- length %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus", length_mm < Lm) %>%
  group_by(campaignid, opcode) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "Immature KGW") %>%
  glimpse()

# Create a dataframe for the abundance of greater than legal size King George Whiting
legal_kgw <- length %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus", length_mm > 280) %>%
  group_by(campaignid, opcode) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "Legal KGW") %>%
  glimpse()

# Create a dataframe for the abundance of smaller than legal size King George Whiting
sublegal_kgw <- length %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus", length_mm < 280) %>%
  group_by(campaignid, opcode) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "Sublegal KGW") %>%
  glimpse()

# Join the datasets
tidy_length <- bind_rows(mature_kgw, immature_kgw, legal_kgw, sublegal_kgw) %>%
  glimpse()


# Save the length data----
saveRDS(tidy_length, file = paste0("data/staging/", name, "_tidy-length.rds"))


# Extra ways to check the data----
# Visualise the length distributions
# Manually added on minimum legal size and length of maturity (Lm)
library(png)

kgw <- as.raster(readPNG("data/images/Sillaginodes_punctatus_nb_TAYLOR.png"))

ggplot(filter(length, scientific %in% "Sillaginodes punctatus"), aes(length_mm)) +
  geom_histogram()+
  # geom_density(fill = "grey40", alpha = 0.3) +
  # Or think about another way to show this data? histogram?
  labs(title = "King George Whiting length distribution") +
  geom_vline(xintercept = 280, colour = "red", linetype = "dashed") +
  geom_vline(xintercept = 410, colour = "blue", linetype = "dashed") +
  annotation_raster(kgw, 100, 200, 0.0075, 0.001) +
  annotate(geom = "text", x = c(280, 410), y = 0.01, label = c("Minimum legal size", "Lm")) +
  theme_classic()

##########################################
#habitat LAST YEAR

###########################
## LAST YEAR

# Load and format habitat data
habitat <- read.delim("data/raw/Annotation_1_Dot Point Measurements.txt",
                      skip = 4, stringsAsFactors = F, colClasses = "character") %>%
  clean_names() %>%
  dplyr::select(campaignid, opcode, broad) %>%
  dplyr::filter(!broad %in% c("Unknown", NA, "")) %>%
  dplyr::mutate(number = 1) %>%
  group_by(campaignid, opcode, broad) %>%
  summarise(number = sum(number)) %>%
  pivot_wider(names_from = "broad", values_from = "number", values_fill = 0) %>%
  clean_names() %>%
  ungroup() %>%
  dplyr::mutate(total_points_annotated = rowSums(.[, 3:ncol(.)])) %>%
  glimpse()

#check habitat data & metadata to make sure we have the expected number of rows
nrow(metadata)

nrow(habitat)

#check if any mismatches between metadata and habitat
check_habitat <- anti_join(metadata, habitat) %>%
  glimpse()

#and the other way with habitat and metadata flipped
check_metadata <- anti_join(habitat, metadata) %>%
  glimpse()


# Load and format the relief data----
relief <- read.delim("data/raw/habitat/Relief_Dot Point Measurements.txt",
                     skip = 4, stringsAsFactors = F, colClasses = "character") %>%
  clean_names() %>%
  dplyr::select(campaignid, opcode, level_5) %>%
  dplyr::filter(!level_5 %in% c(NA, "")) %>%
  dplyr::mutate(level_5 = as.numeric(level_5)) %>%
  group_by(campaignid, opcode) %>%
  summarise(mean_relief = mean(level_5),
            sd_relief = sd(level_5)) %>%
  ungroup() %>%
  glimpse()

# Some basic checks
relief_missing_metadata <- relief %>%
  anti_join(metadata) %>%
  glimpse()

metadata_missing_relief <- metadata %>%
  anti_join(relief) %>%
  glimpse()

# Join the two datasets----
tidy_habitat <- habitat %>%
  left_join(relief) %>%
  glimpse()

# Save the final tidy dataset----
saveRDS(tidy_habitat, file = paste0("data/tidy/", name, "_tidy-habitat.rds"))




# Some extra plots to check Metadata to habitat points and habitat point to Metadata----
# Same as CheckEM?
#    - claude to add one or two bascis plots
#   bar and plot by deooth and scatter ies? and bubble?

# Transform the data back into long format - ggplot needs data this way
plot_data <- habitat %>%
  pivot_longer(cols = c(macroalgae, unconsolidated, seagrasses, sponges), names_to = "habitat",
               values_to = "count") %>%
  glimpse()

# Nicely jittered plot that only moves points along the y axis (eg it doesn't move the actual 'count' values)
# Keep in mind that this data doesn't factor the total number of points annotated - eg the open water points removed
ggplot() +
  geom_quasirandom(data = plot_data, aes(x = count, y = habitat), groupOnX = F, method = "quasirandom", alpha = 0.25, size = 1.8, width = 0.2) +
  labs(x = "Number of points", y = "") +
  theme_classic()

# Spatial bubble plot
# Join the data with the metadata to plot spatially
habitat_metadata <- habitat %>%
  left_join(metadata) %>%
  dplyr::filter(!is.na(longitude_dd)) %>% # 113 is missing metadata
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = F) %>%
  glimpse()

aus <- st_read("data/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")

# Another basic plot to make nice
# See package 'ggspatial' for scalebars and north arrows
ggplot() +
  geom_sf(data = aus) +
  geom_sf(data = habitat_metadata, aes(size = macroalgae/total_points_annotated),
          alpha = 0.5, colour = "darkblue") +
  coord_sf(xlim = c(117.839435, 117.949606),
           ylim = c(-35.007973, -35.091606),
           crs = 4326) +
  theme_minimal()

# To save plots, see
?ggplot2::ggsave

# Spatial pie charts - scatterpies

ggplot() +
  geom_sf(data = aus) +
  geom_scatterpie(data = as.data.frame(habitat_metadata), # Doesn't work with sf dataframes
                  aes(x = longitude_dd, y = latitude_dd),
                  cols = c("macroalgae", "unconsolidated", "seagrasses", "sponges"),
                  pie_scale = 1.5) +
  coord_sf(xlim = c(117.839435, 117.949606),
           ylim = c(-35.007973, -35.091606),
           crs = 4326) +
  theme_minimal()

# Add some nice colours, see
?ggplot2::scale_fill_manual
