### objective: clean data from police-caused deaths


df_name <- "data-raw/MortesDecorrentesIntervencaoPolicial_2025.xlsx"

## load data
df_mortes_policia <- readxl::read_excel(df_name) |> 
  # clean column names for convenience
  janitor::clean_names()
# 
# df_mortes_policia |> 
#   summary()
# 
# df_mortes_policia |> 
#   dplyr::count(municipio_circunscricao, sort = TRUE)

## filter to only São Paulo city
# municipio_circunscricao = municipality responsible for a giving criminal act
df_mortes_policia <- df_mortes_policia |> 
  dplyr::filter(municipio_circunscricao == "S.PAULO")

# transform the dates from excel fromat to real dates
df_mortes_policia_clean <- df_mortes_policia |> 
  dplyr::mutate(
    dplyr::across(
      .cols = c("data_fato", "data_nascimento_pessoa"),
      # transform into numeric than use janitor
      .fns = ~ janitor::excel_numeric_to_date(as.numeric(.x))
    ),
    # # transform hours to time 
    # hora_fato = as.numeric(hora_fato)*24,
  ) 

# remove useless columns
df_mortes_policia_clean <- df_mortes_policia_clean |> 
  dplyr::select(-id_delegacia,
                -departamento_circunscricao,
                -municipio_circunscricao,
                -dp_circunscricao,
                -datahora_registro_bo,
                -municipio_elaboracao:-dep_elaboracao,
                -data_nascimento_pessoa,
                -natureza_apurada,
                )


# remove cases without lat long
df_mortes_policia_clean <- df_mortes_policia_clean |>
  dplyr::filter(!is.na(latitude) & !is.na(longitude)) |> 
  dplyr::filter(latitude != 0 & longitude != 0)


# there's about 100 dupes, remove them
df_mortes_policia_clean <- df_mortes_policia_clean |> 
  dplyr::distinct() |> 
  dplyr::arrange(data_fato)


## save
readr::write_rds(
  df_mortes_policia_clean,
  file = "data/mortes_policia_clean.rds",
)
