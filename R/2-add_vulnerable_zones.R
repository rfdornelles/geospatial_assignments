## objective: join data from vulnerable zones

df_zonas_vulneraveis <- fs::dir_info(path = "data-raw", 
             regexp = "SIRGAS_SHP_(cortico|favela)*\\.shp", 
             type = "file", 
             recurse = TRUE)  |> 
  dplyr::pull(path) |>
  purrr::map_df(
    .f = sf::read_sf,
    .id = "file",
  ) |> 
  dplyr::mutate(
    kind = dplyr::if_else(file == "1", "cortico", "favela"),
  ) |> 
  dplyr::relocate(kind) |> 
  dplyr::select(-file) |> 
  sf::st_set_crs(31983)

# library(tmap)
# tmap::tmap_mode("view")
# 
# df_zonas_vulneraveis |> 
#   tmap::qtm() 

## save data
sf::write_sf(
  obj = df_zonas_vulneraveis,
  dsn = "data/zonas_vulneraveis.shp",
  delete_dsn = TRUE
)
