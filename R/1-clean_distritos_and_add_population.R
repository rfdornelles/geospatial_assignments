### objective: bring the population data from the census 2022 to the setors
# so we can calculate the deaths by population in each sector

## read the files
## check censo 2022
df_censo2022 <- sf::read_sf("data-raw/SIRGAS_SHP_setorcensitario2022/SIRGAS_SHP_setorcensitario2022_polygon.shp")

sf::st_geometry_type(df_censo2022)
sf::st_crs(df_censo2022) 

# set the correct crs
df_censo2022 <- df_censo2022 |> 
  sf::st_set_crs(31983)

## open distritos
df_distritos <- sf::read_sf("data-raw/shapefile_distrito/SIRGAS_SHP_distrito.shp")
sf::st_geometry_type(df_distritos)
sf::st_crs(df_distritos)

df_distritos <- df_distritos |> 
  sf::st_set_crs(31983)


sf::st_crs(df_distritos) == sf::st_crs(df_censo2022)

## check if any sector is in more than one district

idx  <- sf::st_intersects(df_censo2022, df_distritos)
df_censo2022$ndist <- lengths(idx)                                  # 1 = OK

## quick report
message(sum(df_censo2022$ndist > 1), " sectors intersect > 1 district")


## distribute the problematic sectors
# split
sect_split <- sf::st_intersection(
  df_censo2022 |> dplyr::select(sc_setor, sc_populac),   # keep id + pop
  df_distritos |> dplyr::select(ds_nome)                 # keep district name
)

## 3 ─ attach original area + compute area share + pop share -------
sect_split <- sect_split |>
  ## join the lookup table (now a pure tibble)  → no sf warning
  dplyr::left_join(area_tbl, by = "sc_setor") |>
  
  ## calculate area of this slice (m²)
  dplyr::mutate(
    area_slice = sf::st_area(geometry) |> as.numeric(),
    prop_area  = area_slice / area_orig                       
  ) |>
  
  ## rescale so that every sector sums EXACTLY to 1
  dplyr::group_by(sc_setor) |>
  dplyr::mutate(
    prop_area = prop_area / sum(prop_area),                   # fix rounding
    pop_share = sc_populac * prop_area                        # final people
  ) |>
  dplyr::ungroup()


## check again
# only if necessary -- take soooo long!
# sect_split |>
#   dplyr::group_by(sc_setor) |>
#   dplyr::summarise(check = sum(prop_area)) |>
#   dplyr::filter(abs(check - 1) > 1e-12) 

# distribute the pop into districts
pop_distritos <- sect_split |>
  sf::st_drop_geometry() |>
  dplyr::group_by(ds_nome) |>
  dplyr::summarise(pop_total = sum(pop_share), .groups = "drop")


df_distritos_pop <- df_distritos |>
  dplyr::left_join(pop_distritos, by = "ds_nome") |>
  dplyr::mutate(pop_total = tidyr::replace_na(pop_total, 0))


## plot the population by district 
df_distritos_pop |>
  dplyr::mutate(pop_total = pop_total / 1000) |> # in thousands
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = pop_total)) +
  ggplot2::scale_fill_viridis_c(option = "plasma") +
  ggplot2::labs(title = "Population by district") +
  ggplot2::theme_minimal() 
  # ggplot2::theme(legend.position = "bottom")


## save the new district data
sf::write_sf(
  obj   = df_distritos_pop,
  dsn   = "data/distritos_pop_2022.geojson",
  driver = "GeoJSON",
  delete_dsn = TRUE
)
