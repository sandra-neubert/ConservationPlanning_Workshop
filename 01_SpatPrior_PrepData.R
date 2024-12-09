# Workshop on Conservation Planning (2h)
# Spatial Prioritization - Preparing input data
# Sandra Neubert (s.neubert@uq.edu.au) and Tin Buenafe (k.buenafe@uq.edu.au)

# Preliminaries -----------------------------------------------------------

# Load packages
pacman::p_load(tidyverse, sf, rnaturalearth, patchwork, prioritizr, viridis)

# Define CRS 
cCRS <- "ESRI:54009"

# Define file paths
inputDat <- file.path("Input")

## Create a boundary
GalapEcoregions <- c(20172:20174)

meowDat <- mregions::mr_shp(key = "Ecoregions:ecoregions") %>% 
  dplyr::filter(.data$eco_code %in% GalapEcoregions) %>% # dplyr::filter to make sure filter() function used is from the dplyr R package
  st_union() %>%
  st_as_sf() %>%
  st_transform(cCRS) %>%
  rename(geometry = x) %>%
  st_set_geometry(., "geometry") 

ggplot() +
  geom_sf(data = meowDat)

## Make grid
### Use boundary to create grid
PUs <- st_make_grid(meowDat, cellsize = 20000) %>% #cellsize: opposite edges
  st_sf() %>%
  mutate(cellID = row_number()) # Add a cell ID reference

### Plot grid
gg_PUs <- ggplot() +
  geom_sf(data = PUs, colour = "grey80", fill = NA, size = 0.1, show.legend = FALSE) +
  coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim) +
  labs(subtitle = "Planning Units")

### Using centroids and intersections: exclude land
#Get land info 
landmass <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  sf::st_transform(cCRS)

# Then get all the PUs partially/wholly within the planning region
logi_Reg <- st_centroid(PUs) %>%
  st_intersects(meowDat) %>%
  lengths() > 0 # Get logical vector instead of sparse geometry binary

PUs <- PUs[logi_Reg, ] # Get TRUE

# Second, get all the pu's with < 50 % area on land (approximated from the centroid)
logi_Ocean <- st_centroid(PUs) %>%
  st_intersects(landmass) %>%
  lengths() > 0 # Get logical vector instead of sparse geometry binary

dat_PUs <- PUs[!logi_Ocean, ] %>%
  mutate(cellID = row_number()) # Add a cell ID reference

### Plot grid
gg_PUsBasic <- ggplot() +
  geom_sf(data = dat_PUs, colour = "grey80", fill = NA, size = 0.1, show.legend = FALSE)

gg_PUsLand <- ggplot() +
  geom_sf(data = dat_PUs, colour = "grey80", fill = NA, size = 0.1, show.legend = FALSE)+
  geom_sf(data = landmass, colour = "black", fill = "black", show.legend = FALSE) + #plot landmass
  coord_sf(xlim = st_bbox(meowDat)$xlim, ylim = st_bbox(meowDat)$ylim) + #crop landmass
  labs(subtitle = "Planning Units") + 
  theme_bw()

ggsave(file.path("Figures", "gg_PUsLand.png"),  width = 6, height = 8, dpi = 200)

## Now go to data used in Galapagos work
# Load data ---------------------------------------------------------------

# Load Planning Units
PUs <- st_read(file.path(inputDat, "PUs", "Galapagos_Planning_Units.shp")) %>%
  st_transform(cCRS) %>%
  select(-"cost") %>%
  rename(cellID = puid)

ggplot() +
  geom_sf(data = PUs)

# Create an sf object for all features
features <- readRDS(file.path(inputDat, "Features", "Features_Combined.rds"))

ggplot() +
  geom_sf(data = features, aes(fill = tiger_shark))

ggplot() +
  geom_sf(data = features, aes(fill = seamount))

# Add cost
cost <- st_read(file.path(inputDat, "Cost", "cost_surface.shp")) %>%
  st_transform(cCRS) %>%
  rename(cellID = puid)

ggplot() +
  geom_sf(data = cost, aes(fill = .data$cost))

out_sf <- features %>%
  left_join(cost %>% sf::st_drop_geometry(), by = "cellID")
