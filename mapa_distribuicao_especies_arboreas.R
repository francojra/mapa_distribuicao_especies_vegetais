
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

species_name1 <- c("Dimorphandra wilsonii Rizzini") # Cerrado
occ_data1 <- occ_search(scientificName = species_name1, limit = 500)

species_name2 <- c("Achyrocline satureioides") # Pampa
occ_data2 <- occ_search(scientificName = species_name2, limit = 500)

species_name3 <- c("Bertholletia excelsa") # Amazonia
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

brazil <- world %>% filter(name == "Brazil") 

# Filtrar ocorrências para aquelas dentro do Brasil

coords_sf_brazil <- coords_sf[brazil, ]
coords_sf_brazil1 <- coords_sf1[brazil, ]
coords_sf_brazil2 <- coords_sf2[brazil, ]
coords_sf_brazil3 <- coords_sf3[brazil, ]
coords_sf_brazil4 <- coords_sf4[brazil, ]

# Ajustar os limites do mapa para focar na América do Norte

xlim <- c(-81, -36)
ylim <- c(-37, 6.7)   

# Visualizar mapa --------------------------------------------------------------------------------------------------------------------------

# Definir cores

cols4all::c4a_table(type = "cat", n = 5)
c4a_gui()

# Criar mapa básico com ggplot2

ggplot() +
 geom_sf(data = brazil, fill = "#000000") +  
  geom_sf(data = coords_sf_brazil, aes(color = "Paubrasilia echinata"),
          size = 2.3, alpha = 0.8, shape = 18) + 
 geom_sf(data = coords_sf_brazil1, aes(color = "Dimorphandra wilsonii Rizzini"), 
          size = 2.3, alpha = 0.8, shape = 18) + 
  geom_sf(data = coords_sf_brazil2, aes(color = "Bertholletia excelsa"),
          size = 2.3, alpha = 0.8, shape = 18) + 
 geom_sf(data = coords_sf_brazil3, aes(color = "Cariniana legalis"),
          size = 2.3, alpha = 0.8, shape = 18) +
  geom_sf(data = coords_sf_brazil4, aes(color = "Anadenanthera colubrina"),
        size = 2.3, alpha = 0.8, shape = 18) +
  scale_color_manual(labels = c(expression(italic("Paubrasilia echinata")),
                                expression(italic("Araucaria angustifolia")),
                                expression(italic("Euterpe edulis")),
                                expression(italic("Cariniana legalis")),
                                expression(italic("Anadenanthera colubrina"))),
      values = c(c(	
c("#CC6677", "#332288", "#DDCC77", "#117733", "#88CCEE")))) + 
  coord_sf(xlim = xlim, ylim = ylim) +
  labs(title = "Distribuição de Espécies Arbóreas Nativas Ameaçadas\n de Extinção na América do Sul",
       x = "Longitude",
       y = "Latitude",
       colour = "") +
  theme_minimal() +
  theme(legend.position = c(0.28, 0.28),
        axis.text = element_text(color = "black",size = 8),
        axis.title = element_text(size = 10, hjust = 1),
        legend.text = element_text(size = 10),
        text = element_text(size = 10),
        legend.text.align = 0) 

# Salvar mapa ------------------------------------------------------------------------------------------------------------------------------

ggsave("m.jpg", dpi = 300,
       width = 35, height = 15, 
       units = "cm", m)

ggsave("m.pdf", dpi = 300,
       width = 35, height = 15, 
       units = "cm", m) 
