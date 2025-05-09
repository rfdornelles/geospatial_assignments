## objective: open the Sao Paulo districts and identify where
# each point belongs to

library(ggplot2)
library(tmap)

## load deaths dataset
df_mortes <- readr::read_rds("data/mortes_policia_clean.rds")

## load Sao Paulo districts
# important: EPSG:SIRGAS2000 / UTM zone 23S (EPSG:31983)
# sp_distritos <- "data-raw/shapefile_distrito/SIRGAS_SHP_distrito.shp" |> 
#   sf::read_sf()  |> 
#   sf::st_set_crs(31983)
sp_distritos <- "data/distritos_pop_2022.geojson" |> 
  sf::read_sf()  


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


df_deaths_by_district <- df_spatial_join |> 
  dplyr::group_by(ds_nome) |> 
  dplyr::summarise(n = dplyr::n()) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(-n) 

# more deaths 
df_deaths_by_district |> 
  # dplyr::slice_max(n, n = ) |> 
  dplyr::mutate(ds_nome = forcats::fct_reorder(ds_nome, n)) |> 
  ggplot(aes(x = ds_nome, y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Deaths by district",
       x = "District",
       y = "Number of deaths") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

df_deaths_by_district |> 
  head(10)


# less deaths
df_deaths_by_district |> 
  tail(10)

# districts without deaths
df_deaths_by_district |> 
  dplyr::filter(n == 0) |> 
  dplyr::pull(ds_nome) |> 
  sort()

# maybe need to reverse the join
df_spatial_join_reverse <- sp_distritos |> 
  sf::st_join(df_mortes_sf) 

# districts without deaths
df_spatial_join_reverse |> 
  dplyr::count(ds_nome, sort = TRUE)

## plot the deaths across the map
tmap::tm_shape(sp_distritos) +
  tmap::tm_borders() +
  tmap::tm_shape(df_mortes_sf) +
  tmap::tm_dots(col = "ano_bo", size = 0.2, fill_alpha = 0.5) +
  tmap::tm_title("Deaths in Sao Paulo")
  
# helper to identify bounding boxes
city      <- sf::st_union(sp_distritos)           # polígono único da cidade
envelope  <- sf::st_as_sfc(sf::st_bbox(city))         # retângulo máximo
mask_out  <- sf::st_difference(envelope, city)    # área fora da cidade p/ pintar branco

df_spatial_join |> 
  ## ── 1. gera colunas X e Y em metros ───────────────────────────
  dplyr::mutate(
    X = sf::st_coordinates(geometry)[, 1],
    Y = sf::st_coordinates(geometry)[, 2]
  ) |> 
  ggplot() +
  # 3a ─ densidade contínua
  stat_density_2d(
    # data    = plot_data,
    aes(x = X, y = Y, fill = after_stat(density)),
    geom    = "raster",
    contour = FALSE,
    n       = 600,        # resolução (reduza se ficar pesado)
    adjust  = 0.3        # ↓ bandwidth → “ninhos” menores
  ) +
  # 3b ─ tampa tudo que está fora do município
  geom_sf(data = mask_out, fill = "white", colour = NA) +
  # 3c ─ contorno dos distritos
  geom_sf(data = sp_distritos,
          fill = NA, colour = "white", linewidth = .35) +
  
  # 4 ─ escala de cor contínua
  scale_fill_viridis_c(option = "mako",
                       direction = -1,
                       name = "Densidade") +
  
  # 5 ─ enquadra exatamente no limite da cidade
  coord_sf(crs = sf::st_crs(sp_distritos),
           xlim = sf::st_bbox(city)[c("xmin", "xmax")],
           ylim = sf::st_bbox(city)[c("ymin", "ymax")],
           expand = FALSE) +
  
  # 6 ─ título, subtítulo etc.
  labs(
    title    = "Deaths in São Paulo",
    subtitle = "Kernel-density heat-map",
    caption  = "Fonte: df_spatial_join"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(hjust = .5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = .5)
  )

## identify the deaths/area in each district
deaths_tbl <- df_spatial_join |>
  sf::st_drop_geometry() |>
  dplyr::count(ds_nome, name = "n")          # n = nº de óbitos

# 2.  JUNTAR a contagem ao shape dos distritos
deaths_map <- sp_distritos |>
  dplyr::left_join(deaths_tbl, by = "ds_nome") |>
  dplyr::mutate(
    ds_areakm        = as.numeric(ds_areakm),       # garante numérico
    deaths_per_area  = n / ds_areakm,
    deaths_per_area  = tidyr::replace_na(deaths_per_area, 0),   # distritos sem caso
    deaths_per_hab = n / pop_total,
    deaths_per_hab = tidyr::replace_na(deaths_per_hab, 0)        # distritos sem caso
  )

# 3.  MAPA choropleth
ggplot(deaths_map) +
  geom_sf(aes(fill = deaths_per_area),
          colour = "white", linewidth = .25) +
  scale_fill_viridis_c(
    option    = "mako",
    direction = -1,
    name      = "Mortes / km²",
    trans     = "sqrt"          # p/ suavizar extremos
  ) +
  coord_sf(crs = sf::st_crs(sp_distritos)) +
  ## add the name of districts
  # geom_sf_text(aes(label = ds_nome),
  #              size = 2.5,
  #              colour = "white",
  #              check_overlap = TRUE) +
  labs(
    title    = "Mortes por km² nos distritos de São Paulo",
    subtitle = "Período: df_spatial_join",
    caption  = "Fonte: SSP - elaboração própria"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(hjust = .5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = .5)
  )


######## trying to nornalize
# 3.  MAPA choropleth
ggplot(deaths_map) +
  geom_sf(aes(fill = deaths_per_hab),
          colour = "white", linewidth = .25) +
  scale_fill_viridis_c(
    option    = "mako",
    direction = -1,
    name      = "Mortes / km²",
    trans     = "sqrt"          # p/ suavizar extremos
  ) +
  coord_sf(crs = sf::st_crs(sp_distritos)) +
  ## add the name of districts
  # geom_sf_text(aes(label = ds_nome),
  #              size = 2.5,
  #              colour = "white",
  #              check_overlap = TRUE) +
  labs(
    title    = "Mortes por hab nos distritos de São Paulo",
    subtitle = "Período: df_spatial_join",
    caption  = "Fonte: SSP - elaboração própria"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(hjust = .5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = .5)
  )


##### try to plot buffer zones 
buffer_m   <- 500
pal_option <- "inferno"

# 1 ─ buffers individuais
buffers <- df_spatial_join |>
  sf::st_buffer(dist = buffer_m)

# 2 ─ contar óbitos dentro de cada buffer
buffers$deaths_in_buf <- lengths(sf::st_intersects(buffers, df_spatial_join))

# 3 ─ classificar *incluindo zero*
buffers <- buffers |>
  dplyr::mutate(
    faixa = cut(
      deaths_in_buf,
      breaks  = c(-Inf, 0, 1, 3, 5, 10, Inf),   # agora tem 0
      right   = FALSE,
      labels  = c("0", "1", "2–3", "4–5", "6–10", "11+")
    )
  )

# 4 ─ dissolver por faixa *e* cortar pelo município
city_union <- sf::st_union(sp_distritos)

buffers_diss <- buffers |>
  dplyr::group_by(faixa) |>
  dplyr::summarise(do_union = TRUE) |>
  sf::st_intersection(city_union)        # evita spill para fora

# 5 ─ mapa -------------------------------------------------------------
ggplot() +
  # fundo do município (cinza claro) para referência
  geom_sf(data = city_union, fill = "grey95", colour = NA) +
  
  # buffers
  geom_sf(data = buffers_diss,
          aes(fill = faixa),
          colour = NA, alpha = 0.85) +
  
  # contorno distritos
  geom_sf(data = sp_distritos,
          fill = NA, colour = "white", linewidth = .35) +
  
  scale_fill_viridis_d(
    option    = pal_option,
    direction = -1,
    name      = glue::glue("Mortes\na ≤{buffer_m/1000} km")
  ) +
  
  coord_sf(crs = sf::st_crs(sp_distritos), expand = FALSE) +
  labs(
    title    = glue::glue("Mapa de calor com buffer-zones de {buffer_m} m"),
    subtitle = "Cores indicam quantas mortes existem dentro do raio",
    caption  = "Fonte: df_spatial_join"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(hjust = .5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = .5),
    legend.key.width = unit(0.5, "cm")
  )

