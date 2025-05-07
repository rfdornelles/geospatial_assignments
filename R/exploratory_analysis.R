#exploratory analysis 

## Packages 

library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(leaflet)
library(ggthemes)
library(viridis)
library(readr) # in case you need to read other file types



df_clean <- readRDS("C:/Users/luisf/Desktop/Semester6/Geospatial_analysis/assignment2/geospatial_assignments/data/mortes_policia_clean.rds")
summary(df_clean)
df_clean |> 
  summary()


## start cleaning and specifying the variables 

df_clean <- df_clean |> 
  mutate(
    data_fato = as.Date(data_fato),
    ano_estatistica = as.integer(ano_estatistica),
    mes_estatistica = as.integer(mes_estatistica),
    idade_pessoa = as.numeric(idade_pessoa),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )

unique(df_clean$idade_pessoa[is.na(as.numeric(df_clean$idade_pessoa))])
# TO UNDERSTAND THE WARNING, BUT ONLY FEW NAs 

df_clean  |> 
  filter(!is.na(idade_pessoa)) |> 
  ggplot(aes(x = idade_pessoa)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Age Distribution of People Killed by Police", x = "Age", y = "Count") +
  theme_minimal()

# now temporal view of deaths per years 

df_clean |> 
  count(ano_estatistica) |> 
  ggplot(aes(x = ano_estatistica, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Police-caused Deaths per Year",
       x = "Year", y = "Number of Deaths") +
  theme_minimal()

df_clean |> 
  count(mes_estatistica) |> 
  ggplot(aes(x = mes_estatistica, y = n)) +
  geom_line(group = 1, color = "tomato") +
  labs(title = "Police-caused Deaths by Month",
       x = "Month", y = "Number of Deaths") +
  scale_x_continuous(breaks = 1:12) +
  theme_minimal()

#grid graphs


library(dplyr)
library(ggplot2)
library(lubridate)

# First, make sure date variables are correct
df_clean <- df_clean %>%
  mutate(
    data_fato = as.Date(data_fato),  # ensure proper Date type
    ano = year(data_fato),
    mes = month(data_fato, label = TRUE, abbr = TRUE)  # extract month as factor (Jan, Feb...)
  )

# Count deaths per month and year
monthly_counts <- df_clean %>%
  count(ano, mes)

# Plot: One plot per year, x = month, y = number of deaths
ggplot(monthly_counts, aes(x = mes, y = n)) +
  geom_col(fill = "darkred") +
  facet_wrap(~ ano, ncol = 3) +
  labs(title = "Monthly Deaths by Police Intervention in São Paulo",
       x = "Month", y = "Number of Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## demographic overview 

df_clean %>%
  count(sexo_pessoa) %>%
  ggplot(aes(x = reorder(sexo_pessoa, n), y = n, fill = sexo_pessoa)) +
  geom_col() +
  labs(title = "Deaths by Sex", x = "Sex", y = "Count") +
  theme_minimal() +
  scale_fill_viridis_d()


df_clean %>%
  count(cor_pele) %>%
  ggplot(aes(x = reorder(cor_pele, n), y = n, fill = cor_pele)) +
  geom_col() +
  labs(title = "Deaths by Race/Skin Color", x = "Skin Color", y = "Count") +
  theme_minimal() +
  scale_fill_viridis_d()
##

ggplot(df_clean, aes(x = longitude, y = latitude)) +
  geom_point(alpha = 0.4, color = "red", size = 1) +
  coord_equal() +
  labs(title = "Map of Police-caused Deaths in São Paulo",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

df_clean %>%
  count(situacao) %>%
  ggplot(aes(x = reorder(situacao, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Cases by Police Situation", x = "Situation", y = "Count") +
  theme_minimal()


df_clean %>%
  filter(profissao != "" & !is.na(profissao)) %>%
  count(profissao, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(profissao, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 Victim Professions", x = "Profession", y = "Count") +
  theme_minimal()

df_clean %>%
  filter(logradouro != "" & !is.na(logradouro)) %>%
  count(logradouro, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(logradouro, n), y = n)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(title = "Top 10 Locations by Number of Incidents", x = "Street", y = "Count") +
  theme_minimal()
df_clean %>%
  filter(coorporacao != "" & !is.na(coorporacao)) %>%
  count(coorporacao, sort = TRUE) %>%
  ggplot(aes(x = reorder(coorporacao, n), y = n)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(title = "Cases by Police Force", x = "Corporation", y = "Number of Cases") +
  theme_minimal()

df_clean %>%
  filter(desc_tipolocal != "" & !is.na(desc_tipolocal)) %>%
  count(desc_tipolocal, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(desc_tipolocal, n), y = n)) +
  geom_col(fill = "mediumpurple") +
  coord_flip() +
  labs(title = "Top 10 Types of Locations in Fatal Interventions", x = "Type of Location", y = "Count") +
  theme_minimal()
#tablas para entender las variables categóricas 
table(df_clean$sexo_pessoa)
table(df_clean$situacao)
table(df_clean$coorporacao)
table(df_clean$desc_tipolocal)
#some cross-tabulations 

table(df_clean$sexo_pessoa, df_clean$situacao)

### analysis suggested by GPT 
library(ggmap)
library(ggplot2)
library(MASS)

summary(df_clean$latitude)
names(df_clean)

library(dplyr)

# Extract coordinates explicitly using dplyr
coords <- df_clean %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  dplyr::select(longitude, latitude)


# 1.2 Compute 2D KDE
kde2d_out <- with(coords, MASS::kde2d(longitude, latitude, n = 200))

# 1.3 Convert to data.frame for plotting
kde_df <- data.frame(
  expand.grid(lon = kde2d_out$x, lat = kde2d_out$y),
  density = as.vector(kde2d_out$z)
)

# plot a map
# 
# 
distritos <- st_read("C:/Users/luisf/Desktop/Semester6/Geospatial_analysis/assignment2/geospatial_assignments/data-raw/shapefile_distrito/SIRGAS_SHP_distrito.shp")

# Step 1: Convert to sf object using WGS84 (standard GPS system)


df_sf <- df_clean %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Step 2: Reproject to SIRGAS2000 / UTM zone 23S (EPSG:31983)
df_sf <- st_transform(df_sf, crs = 31983)

# Step 3 (Optional): Preview coordinate range or bounding box
print(st_bbox(df_sf))

# Step 4: Plot spatial points
ggplot() +
  geom_sf(data = df_sf, aes(color = situacao), alpha = 0.6, size = 1) +
  labs(title = "Police-Related Deaths in São Paulo",
       subtitle = "Projected in UTM / SIRGAS2000 Zone 23S (EPSG:31983)",
       color = "Victim Status") +
  theme_minimal()



# 2. Reproject to ensure EPSG:31983 (usually already correct, but just in case)
# 2. Manually set original CRS to SIRGAS2000 / UTM zone 23S (EPSG:31983)
st_crs(distritos) <- 31983
distritos <- st_transform(distritos, crs = 31983)

# 3. Clean and convert your data to sf object
df_sf <- df_clean %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 31983)

# 4. Spatial join: tag each incident with district info
df_joined <- st_join(df_sf, distritos, join = st_within)

# 5. Plot points over district map
ggplot() +
  geom_sf(data = distritos, fill = "white", color = "gray70") +
  geom_sf(data = df_sf, aes(color = situacao), alpha = 0.7, size = 1) +
  labs(
    title = "Police‑Related Deaths in São Paulo",
    subtitle = "Mapped over District Boundaries (SIRGAS2000)",
    color = "Victim Status"
  ) +
  theme_minimal()