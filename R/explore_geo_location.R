## objective: open the Sao Paulo districts and identify where
# each point belongs to

library(ggplot2)
library(tmap)

## load deaths dataset
df_mortes <- readr::read_rds("data/mortes_policia_clean.rds")

## load Sao Paulo districts
# important: EPSG:SIRGAS2000 / UTM zone 23S (EPSG:31983)
sp_distritos <- "data-raw/shapefile_distrito/SIRGAS_SHP_distrito.shp" |> 
  sf::read_sf()  |> 
  sf::st_set_crs(31983)


sp_distritos
# check the projection
sp_distritos |> 
  sf::st_crs()

#plot
sp_distritos |> 
  ggplot() +
  geom_sf() +
  theme_minimal() +
  labs(title = "Sao Paulo districts") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# test quick tematic map
tmap::tmap_mode("view")
tmap::qtm(sp_distritos)

### identify the districts of each point
# check the projection
summary(df_mortes$latitude)

df_mortes_sf <- df_mortes |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4674) |>  
  sf::st_transform(31983)      


## join points in deaths in each district
df_spatial_join <- df_mortes_sf |> 
  sf::st_join(sp_distritos) 


df_spatial_join |> 
  dplyr::group_by(ds_nome) |> 
  dplyr::summarise(n = dplyr::n()) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(-n) 

## plot the deaths across the map
tmap::tm_shape(sp_distritos) +
  tmap::tm_borders() +
  tmap::tm_shape(df_mortes_sf) +
  tmap::tm_dots(col = "ano_bo", size = 0.2, fill_alpha = 0.5) +
  tmap::tm_title("Deaths in Sao Paulo")
  
  


## plot the deaths across the map
df_mortes_sf |> 
  ggplot() +
  geom_sf(data = sp_distritos, fill = NA, color = "black") +
  geom_sf(size = 0.5) +
  theme_minimal() +
  labs(title = "Deaths in Sao Paulo",
       subtitle = "by year",
       color = "Year") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

