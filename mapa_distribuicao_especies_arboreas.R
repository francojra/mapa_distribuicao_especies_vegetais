
# Mapa de distribuição de espécies ---------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 21/05/24 ---------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(rgbif)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(cols4all)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

# Buscar dados de ocorrência no GBIF para uma espécie específica
# Exemplo: Panthera onca (onça-pintada)

species_name <- c("Paubrasilia echinata")
occ_data <- occ_search(scientificName = species_name, limit = 500)

species_name1 <- c("Araucaria angustifolia")
occ_data1 <- occ_search(scientificName = species_name1, limit = 500)

species_name2 <- c("Euterpe edulis")
occ_data2 <- occ_search(scientificName = species_name2, limit = 500)

species_name3 <- c("Cariniana legalis")
occ_data3 <- occ_search(scientificName = species_name3, limit = 500)

species_name4 <- c("Anadenanthera colubrina")
occ_data4 <- occ_search(scientificName = species_name4, limit = 500)

# Extrair coordenadas

coords <- occ_data$data %>%
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>%
  select(decimalLongitude, decimalLatitude)

coords1 <- occ_data1$data %>%
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>%
  select(decimalLongitude, decimalLatitude)

coords2 <- occ_data2$data %>%
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>%
  select(decimalLongitude, decimalLatitude)

coords3 <- occ_data3$data %>%
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>%
  select(decimalLongitude, decimalLatitude)

coords4 <- occ_data4$data %>%
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>%
  select(decimalLongitude, decimalLatitude)

# Converter para objeto sf

coords_sf <- st_as_sf(coords, coords = c("decimalLongitude", 
                                         "decimalLatitude"), 
                      crs = 4326)

coords_sf1 <- st_as_sf(coords1, coords = c("decimalLongitude", 
                                         "decimalLatitude"), 
                      crs = 4326)

coords_sf2 <- st_as_sf(coords2, coords = c("decimalLongitude", 
                                         "decimalLatitude"), 
                      crs = 4326)

coords_sf3 <- st_as_sf(coords3, coords = c("decimalLongitude", 
                                         "decimalLatitude"), 
                      crs = 4326)

coords_sf4 <- st_as_sf(coords4, coords = c("decimalLongitude", 
                                         "decimalLatitude"), 
                      crs = 4326)

# Obter dados das fronteiras dos países

world <- ne_countries(scale = "medium", returnclass = "sf")

# Filtrar para um país específico (por exemplo, Brasil)

america <- world %>%
  filter(continent %in% c("South America")) 

# Ajustar os limites do mapa para focar na América do Norte
xlim <- c(-82, -34)
ylim <- c(-39, 9)   

# Visualizar mapa --------------------------------------------------------------------------------------------------------------------------

# Definir cores

cols4all::c4a_table(type = "cat", n = 5)

# Criar mapa básico com ggplot2

ggplot() +
  geom_sf(data = america, fill = "gray80", 
          color = "white") +  # Fronteiras dos países
  geom_sf(data = coords_sf, aes(color = "Mustela nigripes"),
          size = 2, alpha = 0.8) + 
  geom_sf(data = coords_sf1, aes(color = "Bison bison"), 
          size = 2, alpha = 0.8) + 
  geom_sf(data = coords_sf2, aes(color = "Gymnogyps californianus"),
          size = 2, alpha = 0.8) + 
  geom_sf(data = coords_sf3, aes(color = "Oncorhynchus nerka"),
          size = 2, alpha = 0.8) +
  geom_sf(data = coords_sf4, aes(color = "Canis rufus"),
          size = 2, alpha = 0.8) +
  scale_color_manual(labels = c(expression(italic("Mustela nigripes")),
                                expression(italic("Bison bison")),
                                expression(italic("Gymnogyps californianus")),
                                expression(italic("Oncorhynchus nerka")),
                                expression(italic("Canis rufus"))),
      values = c(c("#DF9ED4", "#C93F55", 
                                  "#EACC62", "#469D76", 
                                  "#3C4B99"))) +
  coord_sf(xlim = xlim, ylim = ylim) +
  labs(title = "Distribuição de Espécies Arbóreas Nativas\n Ameaçadas de Extinção na América do Sul",
       x = "Longitude",
       y = "Latitude",
       colour = "") +
  theme_gray() +
  theme(#legend.position = "bottom", # c(0.3, 0.38)
        axis.text = element_text(color = "black", 
                                 family = "serif", size = 12),
        axis.title = element_text(family = "serif", 
                                  size = 10, hjust = 1, lineheight = 5),
        legend.text = element_text(family = "serif", size = 12),
        text = element_text(family = "serif", size = 12),
        legend.text.align = 0) 

# Salvar mapa ------------------------------------------------------------------------------------------------------------------------------

ggsave("m.jpg", dpi = 300,
       width = 35, height = 15, 
       units = "cm", m)

ggsave("m.pdf", dpi = 300,
       width = 35, height = 15, 
       units = "cm", m) 
