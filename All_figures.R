###############LIBRARIES##################

library(RColorBrewer)
library(betapart)
library(cowplot)
library(dbscan)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggspatial)
library(iNEXT)
library(openxlsx)
library(patchwork)
library(purrr)
library(sf)
library(sp)
library(spatstat)
library(spatstat.explore)
library(spatstat.geom)
library(tidyverse)
library(vegan)
###############DATA#######################
### Load Shapefiles
city_polygon <- st_read("AreadeEstudio/quilla_mpio.shp") 
department_polygon <- st_read("AreadeEstudio/4326_atpio.shp") 
country_polygon <- st_read("AreadeEstudio/colombia_sinSA.shp") 
perimetro <- st_read("mapas/Perimetro del Barrio abajo.shp")
barrio_polygon <- st_transform(perimetro, 4326)
### Load Neighborhood Sectors
sectors <- st_read("AreadeEstudio/sectores.shp") %>% 
  st_transform(crs = 9377)
# Load spatial data
blocks <- st_read("mapas/Manzanas Barrio Abajo.shp")
neighborhood_boundary <- st_read("mapas/Perimetro del Barrio abajo.shp")
sectors <- st_read("mapas/sectores.shp")

# Load tree database
tree_data <- read.xlsx("Database_final.xlsx", sheet = 3)

#####FIG1###########################################

### Create Custom Scale Bar Function
custom_scale_bar <- function(sf_object,
                             round_to_nearest_km = 1,
                             width_percent = 0.25,
                             x_percent_from_left = 0.1,
                             y_percent_from_bottom = 0.04,
                             y_text_percent_from_bottom = 0.06,
                             color = "black",
                             text_size = 3) {
  mround <- function(x, base) base * round(x / base)
  bbox <- st_bbox(sf_object)
  x_range <- bbox$xmax - bbox$xmin
  y_range <- bbox$ymax - bbox$ymin
  crs_units <- st_crs(sf_object)$units_gdal
  is_meters <- crs_units %in% c("metre", "meter", "m")
  
  if (is_meters) {
    scale_length_m <- mround(x_range * width_percent, round_to_nearest_km * 1000)
    label_text <- paste0(scale_length_m / 1000, " km")
    scale_length_units <- scale_length_m
  } else {
    scale_length_km <- mround((x_range * width_percent) * 111, round_to_nearest_km)
    scale_length_units <- scale_length_km / 111
    label_text <- paste0(scale_length_km, " km")
  }
  
  x_start <- bbox$xmin + x_range * x_percent_from_left
  x_end <- x_start + scale_length_units
  y_pos <- bbox$ymin + y_range * y_percent_from_bottom
  y_text <- bbox$ymin + y_range * y_text_percent_from_bottom
  
  list(
    geom_segment(data = data.frame(x = x_start, xend = x_end, y = y_pos, yend = y_pos),
                 aes(x = x, xend = xend, y = y, yend = yend),
                 color = color, linewidth = 0.6),
    geom_text(data = data.frame(x = (x_start + x_end)/2, y = y_text, label = label_text),
              aes(x = x, y = y, label = label),
              size = text_size, color = color)
  )
}

### Latitude/Longitude Label Helpers
label_lon <- function(x) paste0(abs(x), "°W")
label_lat <- function(x) paste0(x, "°N")

map_city <- ggplot() +
  geom_sf(data = city_polygon, fill = "gray90", color = NA) +
  geom_sf(data = barrio_polygon, fill = "firebrick", color = "NA", linewidth = 0.5) +
  custom_scale_bar(
    sf_object = city_polygon,
    round_to_nearest_km = 1,
    width_percent = 0.2,
    x_percent_from_left = 0.1,
    y_percent_from_bottom = 0.03,
    y_text_percent_from_bottom = 0.1
  ) + 
  coord_sf(
    xlim = c(min(st_bbox(city_polygon)$xmin) - 0.001, max(st_bbox(city_polygon)$xmax) + 0.001),
    ylim = c(min(st_bbox(city_polygon)$ymin) - 0.001, max(st_bbox(city_polygon)$ymax) + 0.001)
  ) +
  scale_x_continuous(labels = label_lon, breaks = seq(-75, -74.8, 0.1)) +
  scale_y_continuous(labels = label_lat, breaks = seq(10.9, 11.1, 0.1)) +
  ggtitle("Barranquilla") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text = element_text(size = 8, color = "black"),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

map_department <- ggplot() +
  geom_sf(data = department_polygon, fill = "gray90", color = NA) +
  geom_sf(data = city_polygon, fill = "firebrick", color = NA) +
  custom_scale_bar(
    sf_object = department_polygon,
    round_to_nearest_km = 5,
    width_percent = 0.2,
    x_percent_from_left = 0.1,
    y_percent_from_bottom = 0.05,
    y_text_percent_from_bottom = 0.125
  ) + 
  coord_sf(
    xlim = c(min(st_bbox(department_polygon)$xmin) - 0.001, max(st_bbox(department_polygon)$xmax) + 0.001),
    ylim = c(min(st_bbox(department_polygon)$ymin) - 0.001, max(st_bbox(department_polygon)$ymax) + 0.001)
  ) +
  scale_x_continuous(labels = label_lon, breaks = seq(-75.2, -74.7, 0.5)) +
  scale_y_continuous(labels = label_lat, breaks = seq(10.4, 11, 0.5)) +
  ggtitle("Atlántico") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text = element_text(size = 8, color = "black"),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

map_country <- ggplot() +
  geom_sf(data = country_polygon, fill = "gray90", color = NA) +
  geom_sf(data = department_polygon, fill = "firebrick", color = "firebrick", linewidth = 0.4) +
  custom_scale_bar(
    sf_object = country_polygon,
    round_to_nearest_km = 200,
    width_percent = 0.3,
    x_percent_from_left = 0.1,
    y_percent_from_bottom = 0.025,
    y_text_percent_from_bottom = 0.1,
    color = "black",
    text_size = 3
  ) +
  coord_sf(
    xlim = c(min(st_bbox(country_polygon)$xmin) - 0.001, max(st_bbox(country_polygon)$xmax) + 0.001),
    ylim = c(min(st_bbox(country_polygon)$ymin) - 0.001, max(st_bbox(country_polygon)$ymax) + 0.001)
  ) +
  scale_x_continuous(labels = label_lon, breaks = seq(-78.9, -66, 10)) +
  scale_y_continuous(labels = label_lat, breaks = seq(0, 10, 5)) +
  ggtitle("Colombia") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text = element_text(size = 8, color = "black"),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

### Load Community Matrix for iNEXT
barrio_matrix <- read.xlsx("Database_final.xlsx", sheet = 11)[-c(82:88),]
abundances <- as.matrix(barrio_matrix[,-1])
rownames(abundances) <- barrio_matrix[,1]
colnames(abundances) <- "Barrio Abajo"

### Run iNEXT Diversity Estimation
inext_result <- iNEXT(abundances, q = c(0,1,2), datatype = "abundance")
div_summary <- as.data.frame(inext_result$AsyEst)

div_summary <- div_summary %>%
  mutate(Diversity = recode(Diversity,
                            "Species richness" = "q0: Species\nrichness",
                            "Shannon diversity" = "q1: Shannon\ndiversity",
                            "Simpson diversity" = "q2: Simpson\ndiversity"))

div_summary$Diversity <- sub(div_summary$Diversity,pat=": ",rep="\n")

diversity_plot <- ggplot(div_summary, aes(x = Diversity, y = Observed)) +
  geom_bar(stat = "identity", fill = "gray", color = "black") +
  labs(x = "Diversity Order", y = "Observed\nSpecies") +
  theme_minimal() +
  theme(panel.grid = element_blank(), legend.position = "none")

### Load Tree Data and Create Density Plot
tree_data <- read.xlsx("Database_final.xlsx", sheet = 3)
tree_points <- st_as_sf(tree_data, coords = c("X", "Y"), crs = 4326) %>%
  st_transform(crs = st_crs(sectors))


sf::sf_use_s2(FALSE)
bbox <- st_bbox(st_union(sectors))
win <- owin(xrange = c(bbox$xmin, bbox$xmax), yrange = c(bbox$ymin, bbox$ymax))
ppp_trees <- ppp(x = st_coordinates(tree_points)[,1], y = st_coordinates(tree_points)[,2], window = win)

density_est <- density(ppp_trees, sigma = bw.ppl(ppp_trees))
density_df <- as.data.frame(density_est)
colnames(density_df) <- c("x", "y", "z")
density_df$significant <- density_df$z > quantile(density_df$z, 0.95)




tree_points <- st_transform(tree_points, 9377)
tree_coords <- st_coordinates(tree_points)


db <- dbscan(tree_coords, eps = 20, minPts = 9)  # tweak eps and minPts
tree_points$cluster <- as.factor(db$cluster)

st_crs(tree_points)
st_crs(sectors)


#Grid size in m2
# 
# BA_grid <- manzanas %>% st_make_grid(n = c(20, 20))
# perimeter <- st_make_valid(perimetro)
# 
# BA_grid_map <- st_intersection(perimeter, BA_grid) %>%
#   st_as_sf() %>%
#   mutate(grid_id = 1:n())
# 
# BA_grid_map$area_m2 <- as.numeric(st_area(BA_grid_map))
# # Check summary
# hist(BA_grid_map$area_m2)


### Avenue and Cluster Labels
set.seed(123)
avenues <- data.frame(
  name = c("Carrera 50", "Av. Murillo", "Carrera 54", "Vía 40", "Calle 53", "Carrera 46"),
  lon =  c(-74.783,      -74.7843,       -74.7845,    -74.7795, -74.7896,    -74.784),
  lat = c( 10.98905,       10.9883,       10.9945,     10.9915,  10.990,      10.9867)
) %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(crs = st_crs(sectors))

# clusters <- data.frame(
#   name = c("R1", "A3", "A2", "T1", "A1", "A4", "T2"),
#   lon = c(-74.7871, -74.7836, -74.7869, -74.7786, -74.789, -74.7825, -74.778),
#   lat = c(10.9895, 10.9935, 10.9931, 10.989, 10.9909, 10.9897, 10.986)
# ) %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
#   st_transform(crs = st_crs(sectors))

# Load necessary packages
library(sf)
library(dplyr)
library(ggrepel)

# Add angles and coordinates
avenues_coords <- avenues %>%
  mutate(
    angle = c(-11, 75, 20, -80, 80, -11),
    label_color = c("black", "black", "black", "black", "black", "black"),
    label_fill = c("cornsilk", "lightblue", "cornsilk", "lightblue", "lightblue", "cornsilk")
  ) %>%
  cbind(st_coordinates(.))

# Plot using geom_text (rotates properly)
tree_cluster_map <- ggplot() + 
  geom_sf(data = sectors, fill= NA, color = "black") +
  geom_tile(data = filter(density_df, significant), aes(x = x, y = y), fill = "chartreuse2") +
  geom_sf(data = tree_points %>% filter(cluster != 0), color="gold", size = 3, alpha=0.2) +
  geom_sf(data = tree_points, color = adjustcolor("darkgreen", alpha.f = 0.7), size = 0.5) +
  geom_text(
    data = avenues_coords,
    aes(x = X, y = Y, label = name, angle = angle),
    size = 3,
    color = "black", fontface = "bold"
  ) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.05, "npc"), pad_y = unit(0.09, "npc"),
                         height = unit(2, "cm"), width = unit(2, "cm"),
                         style = north_arrow_fancy_orienteering()) +
  coord_sf() +
  scale_x_continuous(labels = label_lon) +
  scale_y_continuous(labels = label_lat) +
  ggtitle("Barrio Abajo") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.title = element_blank(),
        legend.position = "none", plot.title = element_text(hjust = 0.5))

### Composite Figure Assembly
top_row <- plot_grid(map_country, map_department, map_city, diversity_plot,
                     ncol = 4, rel_widths = c(0.15, 0.15, 0.15, 0.3),
                     labels = c("A", "B", "C", "D"), label_size = 12, label_fontface = "bold")

bottom_row <- plot_grid(tree_cluster_map, ncol = 1,
                        labels = "E", label_size = 12, label_fontface = "bold")


my.points<- tree_points %>% filter(cluster != 0)

cluster_palette <- c("1" = "gold", 
                     "2" = "gold", 
                     "3" = "gold", 
                     "4" = "gold", 
                     "5" = "gold",
                     "6" = "cornflowerblue",
                     "7" = "gold",
                     "8" = "cornflowerblue",
                     "9" = "cornflowerblue",
                     "10" = "cornflowerblue",
                     "11" = "orange",
                     "12" = "orange",
                     "13" = "gold",
                     "14" = "orange",
                     "15" = "orange",
                     "16" = "orange",
                     "17" = "orange",
                     "18" = "violet",
                     "19"="violet")

#cornflowerblue

inset_plot <- ggplot() + 
  geom_sf(data = sectors, fill = NA, color = "black", linewidth = 0.4) +
  geom_sf(data = my.points, aes(color = as.factor(cluster)), size = 2, alpha = 0.8) +
  scale_color_manual(values = cluster_palette)+
  theme_void() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

print(inset_plot)

fig1 <- plot_grid(top_row, bottom_row, ncol = 1, rel_heights = c(0.32, 0.7))

fig1_with_inset <- ggdraw() +
  draw_plot(fig1, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(inset_plot, x = 0.75, y = 0.40, width = 0.20, height = 0.20) +
  draw_plot_label(label = "F", x = 0.75, y = 0.60, size = 12, fontface = "bold")


### Save Figure
pdf("fig01.pdf", paper = "letter")
print(fig1_with_inset)
dev.off()



#########FIGS2#################
# FIGURE 2: Spatial clustering of the 10 most abundant species ###

tree_data$individuo <- 1  # Ensure individual count is set

# Convert to sf object and project
tree_points <- st_as_sf(tree_data, coords = c("X", "Y"), crs = 4326)
tree_points <- st_transform(tree_points, st_crs(blocks))

# Extract coordinates for use later
tree_points <- cbind(tree_points, st_coordinates(tree_points))

# Select top 10 most abundant species
top_species <- tree_points %>%
  count(Species_name, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(Species_name)

tree_top10 <- tree_points %>% filter(Species_name %in% top_species)

# Project sectors and create analysis window
sectors_proj <- st_transform(sectors, crs = 9377)
analysis_window <- st_union(sectors_proj)
st_crs(analysis_window) <- st_crs(sectors_proj)
window_spatstat <- as_Spatial(analysis_window)
bbox <- st_bbox(analysis_window)
window_ppp <- owin(xrange = c(bbox$xmin, bbox$xmax), yrange = c(bbox$ymin, bbox$ymax))

# Prepare projected points for analysis
tree_top10_proj <- st_transform(tree_top10, st_crs(sectors_proj))

# Add abundance labels to species
abundances <- table(tree_top10$Species_name)
tree_top10_proj$Species_label <- paste0(
  tree_top10_proj$Species_name,
  " (n=", abundances[tree_top10_proj$Species_name], ")"
)

# Order labels by abundance
label_order <- tree_top10_proj %>%
  st_drop_geometry() %>%
  count(Species_label) %>%
  arrange(desc(n)) %>%
  pull(Species_label)

tree_top10_proj$Species_label <- factor(tree_top10_proj$Species_label, levels = label_order)

# Compute kernel density and extract significant clusters per species
significant_density <- data.frame()
threshold_quantile <- 0.95

for (sp in unique(tree_top10$Species_name)) {
  sp_points <- tree_top10 %>% filter(Species_name == sp)
  sp_proj <- st_transform(sp_points, st_crs(sectors_proj))
  sp_coords <- st_coordinates(sp_proj)
  
  if (nrow(sp_coords) >= 5) {
    ppp_sp <- ppp(x = sp_coords[, 1], y = sp_coords[, 2], window = window_ppp)
    dens <- density(ppp_sp, sigma = bw.ppl(ppp_sp))
    dens_df <- as.data.frame(dens)
    names(dens_df) <- c("x", "y", "z")
    
    threshold <- quantile(dens_df$z, threshold_quantile)
    dens_df$significant <- dens_df$z > threshold
    
    label <- paste0(sp, " (n=", nrow(sp_points), ")")
    dens_df_filtered <- dens_df[dens_df$significant, ]
    dens_df_filtered$Species_label <- label
    
    significant_density <- rbind(significant_density, dens_df_filtered)
  }
}

# Apply the same label order to density data
significant_density$Species_label <- factor(significant_density$Species_label, levels = label_order)

# Improve label formatting by adding line breaks after genus
levels(significant_density$Species_label) <- gsub("^(\\S+)\\s+", "\\1\n", levels(significant_density$Species_label))
levels(tree_top10_proj$Species_label) <- gsub("^(\\S+)\\s+", "\\1\n", levels(tree_top10_proj$Species_label))

# Plot
top10_species_plot <- ggplot() +
  geom_sf(data = sectors_proj, fill = "gray98", color = "gray70", linewidth = 0.3) +
  geom_tile(data = significant_density, aes(x = x, y = y, fill = significant)) +
  scale_fill_manual(values = "chartreuse2") +
  geom_sf(data = tree_top10_proj, color = "green4", size = 0.5, alpha = 0.4, show.legend = FALSE) +
  facet_wrap(~Species_label, nrow = 2) +
  ggtitle("Top 10 most abundant species") +
  theme_void() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )


pdf("figS2.pdf", paper = "legal")
top10_species_plot
dev.off()






###################FIG2############

database <- tree_data
DB_points <- st_as_sf(database, coords = c("X", "Y"), crs = st_crs(sectors_proj))
DB_points <- cbind(DB_points, st_coordinates(DB_points))


# Summarize points in each grid cell
st_crs(DB_points) <- 4326  

BA_grid <- blocks %>% st_make_grid(n = c(20, 20))
perimeter <- st_make_valid(perimetro)

BA_grid_map <- st_intersection(perimeter, BA_grid) %>%
  st_as_sf() %>%
  mutate(grid_id = 1:n())


# Step 2: Now correctly transform it to EPSG:9377
DB_points <- st_transform(DB_points, crs = st_crs(BA_grid_map))
joined_data <- st_join(BA_grid_map, DB_points)
grid_summary <- joined_data %>%
  group_by(grid_id) %>%
  summarize(count = n()) %>%
  ungroup()

# Set CRS and spatial join
st_crs(DB_points) <- 4326
DB_points <- st_transform(DB_points, crs = st_crs(BA_grid_map))
joined_data <- st_join(BA_grid_map, DB_points)

# ---- A. Abundance by Grid Cell 
grid_summary <- joined_data %>%
  group_by(grid_id) %>%
  summarize(count = n()) %>%
  ungroup()

map_plot_n <-ggplot() +
  geom_sf(data = perimetro, fill = "white", color = NA, linewidth = 1) +
  geom_sf(data = grid_summary, aes(fill = count), color = NA) +
  scale_fill_gradient(name = "Abundance", low = "#ffffcc", high = "#C47F66",
                      breaks = c(1,20,40),
                      na.value = "white", labels = scales::label_number()) +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 7, title.position = "top")) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_void()

# ---- B. Species Richness by Grid Cell 
DB_points_proj <- st_transform(DB_points, st_crs(BA_grid_map))
joined_data <- st_join(BA_grid_map, DB_points_proj, left = FALSE)

grid_richness <- joined_data %>%
  group_by(grid_id) %>%
  summarize(richness = n_distinct(Species_name)) %>%
  ungroup()

grid_richness_values <- st_drop_geometry(grid_richness)
grid_richness_mapa <- BA_grid_map %>%
  left_join(grid_richness_values, by = "grid_id") %>%
  mutate(richness = replace_na(richness, 0))

map_plot_s <- ggplot() +
  geom_sf(data = perimetro, fill = "white", color = NA, linewidth = 1) +
  geom_sf(data = grid_richness_mapa, aes(fill = richness), color = NA) +
  scale_fill_gradient(name = "Richness", low = "#ffffcc", high = "#7FB166",
                      breaks = c(1, 8, 16), na.value = "white") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 7, title.position = "top")) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_void()


# ---- C. Sector-level iNEXT Diversity
matriz_sectores <- read.xlsx("Database_final.xlsx", sheet = "Diversidad_Sectores")
matriz_sectores <- matriz_sectores[-c(82:88), -19]
tabla_sectores <- matriz_sectores[,-1]
m_sectores <- as.matrix(tabla_sectores)
row.names(m_sectores) <- matriz_sectores[,1]

inext_sectores <- iNEXT(m_sectores, q=c(0,1,2), datatype="abundance", endpoint = 196, knots=100, se=TRUE, conf=0.95, nboot=99)
info_sectores <- as.data.frame(inext_sectores$DataInfo)

manzanas_ba <- read.csv("Escalas/manzanas_ba2.csv")
crs_blocks <- st_crs(blocks)
indices_man <- st_as_sf(x = manzanas_ba, wkt = 1, crs = crs_blocks)
indices_man$Abundancia.s <- info_sectores$n[match(manzanas_ba$Sector, info_sectores$Assemblage)]
indices_man$q0.s <- info_sectores$S.obs[match(manzanas_ba$Sector, info_sectores$Assemblage)]

S.sector <- ggplot() +
  geom_sf(data = indices_man, aes(fill = q0.s), linewidth = 0.05, color = NA) +
  #geom_sf(data = sectors, fill = NA, linewidth = 0.5) +
  #geom_sf(data = blocks, fill = NA, linewidth = 0.25) +
  scale_fill_gradient(
    name = "Richness",
    low = "#BFD899",
    high = "darkgreen",
    limits = c(1, 33),na.value = "white",
    breaks = c(1, 10, 20, 30, 33),
    labels = scales::label_number()
  ) +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 7, title.position = "top")) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  coord_sf(xlim = range(st_bbox(sectors)[c("xmin", "xmax")]) + c(-0.001, 0.001),
           ylim = range(st_bbox(sectors)[c("ymin", "ymax")]) + c(-0.001, 0.001)) +
  theme_void()

# ---- D. Abundance by Sector
N.sector <- ggplot() +
  geom_sf(data = indices_man, aes(fill = Abundancia.s), linewidth = 0.05, color = NA) +
  #geom_sf(data = sectors, fill = NA, linewidth = 0.5) +
  #geom_sf(data = blocks, fill = NA, linewidth = 0.25) +
  scale_fill_gradient(
    name = "Abundance",
    low = "#E2BF99",
    high = "darkred",
    limits = c(1, 187),
    breaks = c(1, 50, 100, 150, 187),na.value = "white",
    labels = scales::label_number()
  ) +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 7, title.position = "top")) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  coord_sf(xlim = range(st_bbox(sectors)[c("xmin", "xmax")]) + c(-0.001, 0.001),
           ylim = range(st_bbox(sectors)[c("ymin", "ymax")]) + c(-0.001, 0.001)) +
  theme_void()


# ---- E. iNEXT Richness by Block
matriz_manzanas <- read.xlsx("Database_final.xlsx", sheet = "D_Manzanas")
tabla_manzanas <- matriz_manzanas[,-1]
m_manzanas <- as.matrix(tabla_manzanas)
row.names(m_manzanas) <- matriz_manzanas[,1]

inext_manzanas <- iNEXT(m_manzanas, q=c(0,1,2), datatype="abundance", endpoint = 196, knots=100, se=TRUE, conf=0.95, nboot=99)
info_manzanas <- as.data.frame(inext_manzanas$DataInfo)

indices_man$q0.m <- info_manzanas$S.obs[match(manzanas_ba$Manzana, info_manzanas$Assemblage)]
indices_man$Abundancia.M <- info_manzanas$n[match(manzanas_ba$Manzana, info_manzanas$Assemblage)]

S.man <- ggplot() +
  geom_sf(data = indices_man, aes(fill = q0.m), linewidth = 0.05, color = NA) +
  #geom_sf(data = sectors, fill = NA, linewidth = 0.3) +
  #geom_sf(data = blocks, fill = NA, linewidth = 0.25) +
  scale_fill_gradient(name = "Richness", low = "#ffffcc", high = "#3F8A32",na.value = "white",
                      breaks = c(1, 10, 20, 22),
                      labels = scales::label_number()) +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 7, title.position = "top")) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  coord_sf(xlim = range(st_bbox(sectors)[c("xmin", "xmax")]) + c(-0.001, 0.001),
           ylim = range(st_bbox(sectors)[c("ymin", "ymax")]) + c(-0.001, 0.001)) +
  theme_void()

# ---- F. Abundance by Block
N.man <- ggplot() +
  geom_sf(data = indices_man, aes(fill = Abundancia.M), linewidth = 0.05, color = NA) +
  #geom_sf(data = sectors, fill = NA, linewidth = 0.3) +
  #geom_sf(data = blocks, fill = NA, linewidth = 0.25) +
  scale_fill_gradient(name = "Abundance", low = "#ffffcc", high = "#A83F32",
                      breaks = c(1, 50, 100, 108),na.value = "white",
                      labels = scales::label_number()) +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 7, title.position = "top")) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  coord_sf(xlim = range(st_bbox(sectors)[c("xmin", "xmax")]) + c(-0.001, 0.001),
           ylim = range(st_bbox(sectors)[c("ymin", "ymax")]) + c(-0.001, 0.001)) +
  theme_void()

# ---- Final Assembly: Figure 3
N_col <- plot_grid(N.sector, N.man, map_plot_n, 
                   ncol = 1, 
                   labels = c("A", "B", "C"),
                   label_size = 12, 
                   label_fontface = "bold")

# Right column: Richness (D, E, F)
S_col <- plot_grid(S.sector, S.man, map_plot_s, 
                   ncol = 1, 
                   labels = c("D", "E", "F"),
                   label_size = 12, 
                   label_fontface = "bold")

# Combine both columns side by side
diversity_final_columns <- plot_grid(N_col, S_col, 
                                     ncol = 2, 
                                     rel_widths = c(1, 1))


# Save outputs
pdf("fig02.pdf", paper = "letter")
diversity_final_columns
dev.off()




#######Fig S3#############


# Convert abundance matrices to presence-absence


manzana_pa <- m_manzanas
manzana_pa[manzana_pa > 0] <- 1
head(manzana_pa)

sector_pa <- m_sectores
sector_pa[sector_pa > 0] <- 1

# Grid matrix from joined data
grid_pa <- t(joined_data %>%
  st_drop_geometry() %>%
  group_by(grid_id, Species_name) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = Species_name, values_from = count, values_fill = 0) %>%
  column_to_rownames("grid_id"))



grid_pa[grid_pa > 0] <- 1


# Load package
library(betapart)

# Transpose to correct orientation: rows = sites (manzanas), columns = species


# Optional summary function to extract average distances
summary_distances <- function(beta_obj) {
  c(
    mean_jaccard    = mean(beta_obj$beta.jac[lower.tri(beta_obj$beta.jac)], na.rm = TRUE),
    mean_turnover   = mean(beta_obj$beta.jtu[lower.tri(beta_obj$beta.jtu)], na.rm = TRUE),
    mean_nestedness = mean(beta_obj$beta.jne[lower.tri(beta_obj$beta.jne)], na.rm = TRUE)
  )
}
# Run beta diversity decomposition (Jaccard)
beta_manzanas <- beta.pair(manzana_pa, index.family = "jaccard")

# Get average beta diversity across manzanas
summary_distances(beta_manzanas)

beta_grids   <- beta.pair(grid_pa, index.family = "jaccard")
beta_sectors <- beta.pair(sector_pa, index.family = "jaccard")

# Final summary table
rbind(
  Sectors  = summary_distances(beta_sectors),
  Block = summary_distances(beta_manzanas),
  Grid    = summary_distances(beta_grids)
)



# Prepare data
beta_summary <- rbind(
  Sectors = summary_distances(beta_sectors),
  Block = summary_distances(beta_manzanas),
  Grid = summary_distances(beta_grids)
)

# Create stacked data: keep only turnover and nestedness
stacked_beta <- as.data.frame(beta_summary) %>%
  rownames_to_column("Scale") %>%
  select(Scale, Turnover = mean_turnover, Nestedness = mean_nestedness) %>%
  pivot_longer(cols = c("Turnover", "Nestedness"), names_to = "Component", values_to = "Value")

# Reorder for plotting
stacked_beta$Scale <- factor(stacked_beta$Scale, levels = c("Sectors", "Block", "Grid"))
stacked_beta$Component <- factor(stacked_beta$Component, levels = c("Nestedness", "Turnover"))

# Plot
library(ggplot2)

betaplot <- ggplot(stacked_beta, aes(x = Scale, y = Value, fill = Component)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("Nestedness" = "#fdae61", "Turnover" = "#2b83ba")) +
  labs(
    title = "Partitioned Jaccard Dissimilarity by Spatial Scale",
    x = "Spatial Scale",
    y = "Mean Jaccard Dissimilarity",
    fill = "Component"
  ) +
  theme_minimal(base_size = 12)

pdf("figS3.pdf", paper = "letter")
betaplot
dev.off()


#####FIG3###########

### 1. Prepare datasets ###
top_10_species <- DB_points %>%
  count(Species_name, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(Species_name)

DB_points_top_10 <- DB_points %>% filter(Species_name %in% top_10_species)
DB_points_other  <- DB_points %>% filter(!Species_name %in% top_10_species)

DB_points_top_10 <- st_transform(DB_points_top_10, st_crs(sectors_proj))
DB_points_other  <- st_transform(DB_points_other,  st_crs(sectors_proj))

### 2. KDE Overlap ###
window <- as.owin(st_union(sectors_proj))


get_significant_kde <- function(points_sf, label, threshold_quantile = 0.90) {
  coords <- st_coordinates(points_sf)
  ppp_obj <- ppp(x = coords[,1], y = coords[,2], window = window)
  
  # Increase resolution (optional)
  dens <- density(ppp_obj, sigma = bw.ppl(ppp_obj), dimyx = 256)
  
  dens_df <- as.data.frame(dens)
  names(dens_df) <- c("x", "y", "z")
  dens_df$z <- as.numeric(dens_df$z)
  
  # Threshold correctly
  threshold <- quantile(dens_df$z[dens_df$z > 0], probs = threshold_quantile, na.rm = TRUE)
  dens_df$significant <- dens_df$z > threshold
  
  dens_df <- dens_df[dens_df$significant, ]
  dens_df$type <- label
  
  return(dens_df)
}


df_top10 <- get_significant_kde(DB_points_top_10, "Top 10", threshold_quantile = 0.90)
df_rest  <- get_significant_kde(DB_points_other, "Rest", threshold_quantile = 0.90)

combined_df <- bind_rows(df_top10, df_rest) %>%
  mutate(cell_id = paste(round(x, 3), round(y, 3), sep = "_"))

cell_classification <- combined_df %>%
  group_by(cell_id) %>%
  summarize(
    x = first(x),
    y = first(y),
    type = case_when(
      all(type == "Top 10") ~ "Top 10",
      all(type == "Rest") ~ "Rest",
      TRUE ~ "Overlap"
    ),
    .groups = 'drop'
  )

# Set desired order and labels for the legend
cell_classification$type <- factor(
  cell_classification$type,
  levels = c("Top 10", "Rest", "Overlap"),
  labels = c("Top 10 Abundant Species", "Rest of Species", "Overlap")
)


library(dplyr)
library(sf)
library(ggplot2)

# Step 1: Filter only overlap cells
overlap_cells <- cell_classification %>%
  filter(type == "Overlap")

# Step 2: Convert to sf
overlap_sf <- st_as_sf(overlap_cells, coords = c("x", "y"), crs = st_crs(sectors_proj))
overlap_sf <- st_buffer(overlap_sf, dist = 5)  # Tiny buffer to connect close cells

# Step 3: Group spatially connected blobs
overlap_union <- st_union(overlap_sf)
blobs <- st_cast(overlap_union, "POLYGON")

# Step 4: Create dataframe with individual blobs
blobs_sf <- st_sf(geometry = blobs)
blobs_sf$blob_id <- as.factor(seq_len(nrow(blobs_sf)))  # Unique color per blob


# Example color assignment (adjust as needed)
custom_colors <- rep("blue",length(blobs_sf$blob_id))

blobs_sf$cluster_type <- case_when(
  blobs_sf$blob_id %in% c("1") ~ "Residential",
  blobs_sf$blob_id %in% c(5,8,9:14) ~ "Main Avenues",
  blobs_sf$blob_id %in% c(15:18) ~ "Touristic",
  blobs_sf$blob_id %in% c(2,3,4) ~ "Commercial",
  TRUE ~ "Other"
)

plot_overlap <- ggplot() +
  geom_sf(data = sectors_proj, fill = NA, color = "gray80", linewidth = 0.4) +
  geom_sf(data = blobs_sf, aes(fill = cluster_type), color = NA) +
  scale_fill_manual(
    name = "Cluster Type",
    values = c("Commercial" = "cornflowerblue",
               "Residential" = "violet", 
               "Main Avenues" = "gold", 
               "Touristic" = "orange"),
    guide = guide_legend(nrow = 4)
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  ) +
  ggtitle("E")


### 3. Grid Abundances ###
BA_grid <- blocks %>% st_make_grid(n = c(20, 20))
perimeter <- st_make_valid(perimetro)

BA_grid_map <- st_intersection(perimeter, BA_grid) %>%
  st_as_sf() %>%
  mutate(grid_id = 1:n())

DB_points_top_10 <- st_transform(DB_points_top_10, st_crs(BA_grid_map))
DB_points_other  <- st_transform(DB_points_other,  st_crs(BA_grid_map))

# 1. Count top10 vs rest
bar_data <- bind_rows(
  DB_points_top_10 %>% mutate(Category = "Top 10"),
  DB_points_other  %>% mutate(Category = "Other")
) %>%
  count(Category) %>%
  mutate(Category = factor(Category, levels = c("Top 10", "Other")))


BA_grid_map$top10 <- lengths(st_intersects(BA_grid_map, DB_points_top_10))
BA_grid_map$rest  <- lengths(st_intersects(BA_grid_map, DB_points_other))

#range(BA_grid_map$rest)


plot_top10_grid <- ggplot() +
  geom_sf(data = perimeter, fill = "white", color = "black", linewidth = 1) +
  geom_sf(data = BA_grid_map, aes(fill = top10), color=NA) +
  scale_fill_gradient(name = "Abundance", low = "#ffffcc", high = "gray30") +
  theme_void() +
  ggtitle("B")

plot_rest_grid <- ggplot() +
  geom_sf(data = perimeter, fill = "white", color = "black", linewidth = 1) +
  geom_sf(data = BA_grid_map, aes(fill = rest), color=NA) +
  scale_fill_gradient(name = "Abundance", low = "#ffffcc", high = colorRampPalette(colors = c("#ffffcc", high = "gray30"))(34)[15]) +
  theme_void() +
  ggtitle("D")

# transparent_spacer <- ggplot() + 
#   theme_void() + 
#   theme(panel.background = element_rect(fill = "white", color = NA))
# 
#4. point plots
plot_top10_points <- ggplot() +
  geom_sf(data = sectors_proj, fill = NA, linewidth = 0.5) +
  geom_tile(
    data = filter(cell_classification, type %in% c("Top 10 Abundant Species", "Overlap")),
    aes(x = x, y = y, fill = type),
    alpha = 0.6
  ) +
  scale_fill_manual(
    values = c("Top 10 Abundant Species" = "chartreuse2", "Overlap" = "chartreuse2"),
    guide = "none"  # No legend here
  ) +
  geom_sf(data = DB_points_top_10, color = "black", size = 0.5, alpha = 0.5) +
  theme_void() +
  ggtitle("A")

plot_rest_points <- ggplot() +
  geom_sf(data = sectors_proj, fill = NA, linewidth = 0.5) +
  geom_tile(
    data = filter(cell_classification, type %in% c("Rest of Species", "Overlap")),
    aes(x = x, y = y, fill = type),
    alpha = 0.6
  ) +
  scale_fill_manual(
    values = c("Rest of Species" = "chartreuse2", "Overlap" = "chartreuse2"),
    guide = "none"  # No legend here
  ) +
  geom_sf(data = DB_points_other, color = "black", size = 0.5, alpha = 0.5) +
  theme_void() +
  ggtitle("C")


# 2. Barplot
bar_top10_rest <- ggplot(bar_data, aes(x = Category, y = n, fill = Category)) +
  geom_col(width = 0.6, color = "black") +
  scale_fill_manual(values = c("Top 10" = "gray30", "Other" = colorRampPalette(colors = c("#ffffcc", high = "gray30"))(34)[15])) +
  labs(x = NULL, y = "Number of Trees") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    panel.grid.major.x = element_blank()
  ) +
  ggtitle("F")




#5.plot composite
# 1. Create labels
label_top10 <- ggdraw() + draw_label("Top 10 species", fontface = "bold", angle = 90, size = 11)
label_rest  <- ggdraw() + draw_label("Other species", fontface = "bold", angle = 90, size = 11)

# 2. Create rows with labels in between plots
row_top10 <- plot_grid(
  label_top10,
  plot_top10_points,
  plot_top10_grid,
  ncol = 3,
  rel_widths = c(0.07, 1, 1)
)

row_rest <- plot_grid(
  label_rest,
  plot_rest_points,
  plot_rest_grid,
  ncol = 3,
  rel_widths = c(0.07, 1, 1)
)

# 3. Combine top rows
top_row_abundant <- plot_grid(
  row_top10,
  row_rest,
  ncol = 1
)

bottom_row_abundant <- plot_grid(plot_overlap, bar_top10_rest)

# 4. Combine with bottom row
layout_abundant <- plot_grid(
  top_row_abundant,
  bottom_row_abundant,
  ncol = 1,
  rel_heights = c(0.7, 0.4)
)

# 5. Export to PDF
pdf("fig03.pdf", paper = "letter")
layout_abundant
dev.off()






######################FIG4###########
# 1. Load both sheets
individuals <- tree_data
species_info <- read.xlsx("Database_final.xlsx", sheet = 2)  # characteristics


# 2. Merge species origin info into individuals dataset
species_info <- species_info %>%
  rename(Species_name = Species_name)  # Match column names for joining

individuals <- left_join(individuals, species_info[, c("Species_name", "Origin")], by = "Species_name")

# 3. Convert to sf object
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
DB_points <- st_as_sf(individuals, coords = c("X", "Y"), crs = projcrs)
DB_points <- st_transform(DB_points, crs = st_crs(sectors_proj))  # Use your project CRS

# 4. Split into native and exotic
DB_points_native <- DB_points %>% filter(Origin == "Native")
DB_points_exotic <- DB_points %>% filter(Origin == "Exotic")

print(paste0(collapse = "",c(
  "Exotic trees: ",
  as.character(dim(DB_points_exotic)[1]),
  "; ",
  "Exotic species: ",
  as.character(length(table(DB_points_exotic$Species_name))))))

print(paste0(collapse = "",c(
  "Native trees: ",
  as.character(dim(DB_points_native)[1]),
  "; ",
  "Native species: ",
  as.character(length(table(DB_points_native$Species_name))))))

# 5. Continue as in Fig 4:
# Use `get_significant_kde()` for native and exotic
df_native <- get_significant_kde(DB_points_native, "Native", threshold_quantile = 0.90)
df_exotic <- get_significant_kde(DB_points_exotic, "Exotic", threshold_quantile = 0.90)

# Combine
combined_df <- bind_rows(df_native, df_exotic) %>%
  mutate(cell_id = paste(round(x, 3), round(y, 3), sep = "_"))

# 2. KDE + Overlap
window <- as.owin(st_union(sectors_proj))



df_native <- get_significant_kde(DB_points_native, "Native")
df_exotic <- get_significant_kde(DB_points_exotic, "Exotic")

combined_df <- bind_rows(df_native, df_exotic) %>%
  mutate(cell_id = paste(round(x, 3), round(y, 3), sep = "_"))

cell_classification <- combined_df %>%
  group_by(cell_id) %>%
  summarize(
    x = first(x),
    y = first(y),
    type = case_when(
      all(type == "Native") ~ "Native",
      all(type == "Exotic") ~ "Exotic",
      TRUE ~ "Overlap"
    ),
    .groups = 'drop'
  )

cell_classification$type <- factor(
  cell_classification$type,
  levels = c("Native", "Exotic", "Overlap"),
  labels = c("Native Species", "Exotic Species", "Overlap")
)

# 3. Overlap blobs
overlap_cells <- cell_classification %>% filter(type == "Overlap")
overlap_sf <- st_as_sf(overlap_cells, coords = c("x", "y"), crs = st_crs(sectors_proj)) %>% st_buffer(dist = 5)
blobs <- st_cast(st_union(overlap_sf), "POLYGON")
blobs_sf <- st_sf(geometry = blobs)
blobs_sf$blob_id <- as.factor(seq_len(nrow(blobs_sf)))

blobs_sf$cluster_type <- case_when(
  blobs_sf$blob_id %in% c(3,5) ~ "Residential",
  blobs_sf$blob_id %in% c(4,6,7,8,9,10,11) ~ "Main Avenues",
  blobs_sf$blob_id %in% c(12,13,14) ~ "Touristic",
  blobs_sf$blob_id %in% c(1,2) ~ "Commercial",
  
  TRUE ~ "Other"
)

plot_overlap_origin <-   ggplot() +
  geom_sf(data = sectors_proj, fill = NA, color = "gray80", linewidth = 0.4) +
  geom_sf(data = blobs_sf, aes(fill = cluster_type), color = NA) +
  scale_fill_manual(
    name = "Cluster Type",
    values = c("Commercial" = "cornflowerblue",
      "Residential" = "violet", 
      "Main Avenues" = "gold", 
      "Touristic" = "orange"),
    guide = guide_legend(nrow = 4)
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  ) +
  ggtitle("E")

plot_overlap_origin

# 4. Grid Abundances
BA_grid <- blocks %>% st_make_grid(n = c(20, 20))
perimeter <- st_make_valid(perimetro)

BA_grid_map <- st_intersection(perimeter, BA_grid) %>%
  st_as_sf() %>%
  mutate(grid_id = 1:n())

DB_points_native <- st_transform(DB_points_native, st_crs(BA_grid_map))
DB_points_exotic <- st_transform(DB_points_exotic, st_crs(BA_grid_map))


BA_grid_map$native <- lengths(st_intersects(BA_grid_map, DB_points_native))
BA_grid_map$exotic <- lengths(st_intersects(BA_grid_map, DB_points_exotic))

# range(BA_grid_map$native)
# range(BA_grid_map$exotic)
#
colorRampPalette(colors = c("#ffffcc", high = "gray30"))(36)[16]

plot_native_grid <- ggplot() +
  geom_sf(data = perimeter, fill = "white", color = "black", linewidth = 1) +
  geom_sf(data = BA_grid_map, aes(fill = native), color = NA) +
  scale_fill_gradient(name = "Abundance", low = "#ffffcc", high = colorRampPalette(colors = c("#ffffcc", high = "gray30"))(36)[16]) +
  theme_void() +
  ggtitle("D")

plot_exotic_grid <- ggplot() +
  geom_sf(data = perimeter, fill = "white", color = "black", linewidth = 1) +
  geom_sf(data = BA_grid_map, aes(fill = exotic), color = NA) +
  scale_fill_gradient(name = "Abundance", low = "#ffffcc", high = "gray30") +
  theme_void() +
  ggtitle("B")

# 5. Point plots
plot_DB_points_native <- ggplot() +
  geom_sf(data = sectors_proj, fill = NA, linewidth = 0.5) +
  geom_tile(data = filter(cell_classification, type %in% c("Native Species", "Overlap")),
            aes(x = x, y = y, fill = type), alpha = 0.6) +
  scale_fill_manual(values = c("Native Species" = "chartreuse2", "Overlap" = "chartreuse2"),
                    guide = "none") +
  geom_sf(data = DB_points_native, color = "black", size = 0.5, alpha = 0.5) +
  theme_void() +
  ggtitle("C")

plot_DB_points_exotic <- ggplot() +
  geom_sf(data = sectors_proj, fill = NA, linewidth = 0.5) +
  geom_tile(data = filter(cell_classification, type %in% c("Exotic Species", "Overlap")),
            aes(x = x, y = y, fill = type), alpha = 0.6) +
  scale_fill_manual(values = c("Exotic Species" = "chartreuse2", "Overlap" = "chartreuse2"),
                    guide = "none") +
  geom_sf(data = DB_points_exotic, color = "black", size = 0.5, alpha = 0.5) +
  theme_void() +
  ggtitle("A")


# 1. Barplot data
origin_count <- data.frame(
  Origin = c("Native", "Exotic"),
  n = c(nrow(DB_points_native), nrow(DB_points_exotic))
)

# 2. Barplot
bar_origin <- ggplot(origin_count, aes(x = Origin, y = n, fill = Origin)) +
  geom_col(width = 0.6, color = "black") +
  scale_fill_manual(values = c(
    "Exotic" = "gray30",
    "Native" = colorRampPalette(colors = c("#ffffcc", "gray30"))(36)[16]
  )) +
  labs(x = NULL, y = "Number of Trees") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    panel.grid.major.x = element_blank()
  ) +
  ggtitle("F")


### Composite Figure Assembly
# Create labels as ggdraw objects
label_native <- ggdraw() + draw_label("Native", fontface = "bold", angle = 90, size = 11)
label_exotic <- ggdraw() + draw_label("Exotic", fontface = "bold", angle = 90, size = 11)

# Create 3-part rows: Point plot | Label | Grid plot
row_native <- plot_grid(
  label_native,
  plot_DB_points_native,
  plot_native_grid,
  ncol = 3,
  rel_widths = c(0.07, 1, 1)
)

row_exotic <- plot_grid(
  label_exotic,
  plot_DB_points_exotic,
  plot_exotic_grid,
  ncol = 3,
  rel_widths = c(0.07, 1, 1)
)

# Combine top rows
top_row_origin <- plot_grid(
  row_exotic,
  row_native,
  ncol = 1
)

bottom_row_origin <- plot_grid(
  plot_overlap_origin,bar_origin
)

layout_origin <- plot_grid(
  top_row_origin,
  bottom_row_origin,
  ncol = 1,
  rel_heights = c(0.7, 0.4)
)




# 4. Export
pdf("fig04.pdf", paper = "letter")
layout_origin
dev.off()


###FIG 5##################################################

tree_data_clean <- individuals %>%
  filter(!is.na(ESTH), !is.na(AVGDBH), ESTH > 0, AVGDBH > 0) %>%
  mutate(
    size_index = ESTH * AVGDBH,
    size_class = cut(
      size_index,
      breaks = quantile(size_index, probs = seq(0, 1, 0.25), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Small", "Medium", "Large", "Very Large")
    ),
    size_class = factor(size_class, levels = c("Small", "Medium", "Large", "Very Large"))  # enforce order
  )


tree_sf <- st_as_sf(tree_data_clean, coords = c("X", "Y"), crs = 4326)  # or use your specific CRS
tree_sf <- st_transform(tree_sf, crs = st_crs(sectors_proj))
#tree_sf <- st_transform(tree_sf, st_crs(perimetro))

  library(spatstat)
library("terra")

get_kde_tiles <- function(points_sf, class_name, threshold_quantile = 0.95) {
  
  # Convert sf to spatstat ppp
  coords <- st_coordinates(points_sf)
  win <- as.owin(st_union(sectors_proj))  # Use your area boundary
  ppp_obj <- ppp(x = coords[,1], y = coords[,2], window = win)
  
  # Estimate density
  kde <- density(ppp_obj, sigma = bw.diggle(ppp_obj), edge = TRUE)
  
  # Convert raster to terra format and extract values
  r <- rast(kde)
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colnames(df) <- c("x", "y", "density")
  
  # Filter by quantile
  threshold <- quantile(df$density, threshold_quantile, na.rm = TRUE)
  df <- df %>% filter(density >= threshold)
  df$size_class <- class_name
  return(df)
}




sizeclass_kde <- tree_sf %>%
  split(.$size_class) %>%
  map2_df(names(.), ~ get_kde_tiles(.x, .y))

sizeclass_kde$size_class <- factor(
  sizeclass_kde$size_class,
  levels = c("Small", "Medium", "Large", "Very Large")
)

size_col <- colorRampPalette(c("chartreuse","darkgreen"))(4)
#size_col <-rainbow(7)[c(1:4)]
map_tree_size_uniform <- ggplot() +
  geom_sf(data = perimetro, fill = "white", color = "black") +
  geom_tile(data = sizeclass_kde, aes(x = x, y = y, fill = size_class), alpha = 0.7) +
#  geom_sf(data = tree_sf, alpha = 0.6) +
  coord_sf(crs = st_crs(sectors_proj)) +
  scale_fill_manual(
    name = "Size Class",
    values = c(
      "Small" = size_col[1],        # very light gray
      "Medium" = size_col[2],       # medium gray
      "Large" = size_col[3],        # dark gray
      "Very Large" = size_col[4]    # very dark gray
    )
  ) +
 # scale_color_manual(values = c("Native" = "darkgreen", "Exotic" = "orange")) +
  theme_void() +
  ggtitle("Quartile-based Size Class Clusters")
map_tree_size_uniform



tree_sf <- tree_sf %>%
  filter(!is.na(ESTH), !is.na(AVGDBH), ESTH > 0, AVGDBH > 0) %>%
  mutate(
    size_index = ESTH * AVGDBH,
    dbh_class = cut(
      AVGDBH,
      breaks = c(0, 20, 40, 60, Inf),
      labels = c("0–20 cm", "20–40 cm", "40–60 cm", "60+ cm"),
      include.lowest = TRUE,
      right = FALSE
    ),
    dbh_class = factor(dbh_class, levels = c("0–20 cm", "20–40 cm", "40–60 cm", "60+ cm"))
  )


hist_tree_size_richards <- ggplot(tree_sf, aes(x = dbh_class, fill = dbh_class)) +
  geom_bar(width = 0.6, color = NA) +
  scale_fill_manual(
    values = c(
      "0–20 cm" = "#d0d1e6",
      "20–40 cm" = "#a6bddb",
      "40–60 cm" = "#74a9cf",
      "60+ cm" = "#0570b0"
    ),
    name = "DBH Class"
  ) +
  labs(
    x = "DBH Class (cm)",
    y = "Number of Trees",
    title = "Distribution of Trees by DBH Class"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )



sizeclass_kde2 <- tree_sf %>%
  split(.$dbh_class) %>%
  map2_df(names(.), ~ get_kde_tiles(.x, .y))


size_col <- colorRampPalette(c("lightskyblue","darkblue"))(4)


map_tree_size_richards <- ggplot() +
  geom_sf(data = perimetro, fill = "white", color = "black") +
  geom_tile(data = sizeclass_kde2, aes(x = x, y = y, fill = size_class), alpha = 0.7) +
  #  geom_sf(data = tree_sf, alpha = 0.6) +
  coord_sf(crs = st_crs(sectors_proj)) +
  scale_fill_manual(
    name = "Size Class",
    values = c(
      "0–20 cm" = "#d0d1e6",
      "20–40 cm" = "#a6bddb",
      "40–60 cm" = "#74a9cf",
      "60+ cm" = "#0570b0"    )
  ) +
  # scale_color_manual(values = c("Native" = "darkgreen", "Exotic" = "orange")) +
  theme_void() +
  ggtitle("Fixed Size Class Clusters")




library(vegan)
tree_sf$dbh_class

library(dplyr)
library(tidyr)
library(tibble)
library(vegan)

# Summarize and pivot
diversity_matrix <- tree_sf %>%
  st_drop_geometry() %>%  # remove geometry for analysis
  group_by(dbh_class, Species_name) %>%
  summarize(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = Species_name,
    values_from = count,
    values_fill = 0
  )

# Save DBH class separately and use as rownames
dbh_classes <- diversity_matrix$dbh_class
diversity_matrix <- diversity_matrix %>% select(-dbh_class)
rownames(diversity_matrix) <- dbh_classes

# Diversity indices
shannon <- diversity(diversity_matrix, index = "shannon")
richness <- specnumber(diversity_matrix)

# Combine into dataframe
diversity_summary <- data.frame(
  dbh_class = rownames(diversity_matrix),
  Richness = richness,
  Shannon = shannon
)



# Define color palette for DBH classes
dbh_colors <- c(
  "0–20 cm"  = "#d0d1e6",
  "20–40 cm" = "#a6bddb",
  "40–60 cm" = "#74a9cf",
  "60+ cm"   = "#0570b0"
)

# Plot: Diversity by DBH Class with custom colors


hist_diversity_richards <- ggplot(diversity_summary, aes(x = dbh_class)) +
  geom_col(aes(y = Richness, fill = dbh_class), alpha = 0.8) +
  geom_point(aes(y = Shannon * 10), color = "black", size = 3) +
  geom_line(aes(y = Shannon * 10, group = 1), color = "black", linetype = "dashed") +
  scale_y_continuous(
    name = "Species Richness",
    sec.axis = sec_axis(~./10, name = "Shannon Diversity Index")
  ) +
  scale_fill_manual(
    values = dbh_colors,
    name = "DBH Class"
  ) +
  labs(
    title = "Diversity by DBH Class",
    x = "DBH Class"
  ) +
  theme_minimal() +
  theme(legend.position = "none")




dbh_plots <- plot_grid(
  map_tree_size_uniform, map_tree_size_richards,
  hist_tree_size_richards,
  hist_diversity_richards,
  ncol = 2,
  labels = c("A", "B", "C", "D"),
  label_size = 14,        # Optional: adjust label size
  label_fontface = "bold" # Optional: make labels bold
)


pdf("fig05.pdf",paper = "letter")
dbh_plots
dev.off()
