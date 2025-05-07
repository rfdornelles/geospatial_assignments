## https://www.ssp.sp.gov.br/estatistica/consultas


# https://www.ssp.sp.gov.br/assets/estatistica/transparencia/spDados/MDIP_2025.xlsx

# https://www.ssp.sp.gov.br/assets/estatistica/transparencia/spDados/SPDadosCriminais_2022.xlsx

SPDadosCriminais_2025 |> 
  dplyr::filter(
    !LATITUDE %in% c(0, "0", ""),
    !LONGITUDE %in% c(0, "0", ""))

MortesDecorrentesIntervencaoPolicial_2025 |> 
  dplyr::filter(
    !LATITUDE %in% c(0, "0", ""),
    !LONGITUDE %in% c(0, "0", ""))

## https://geosampa.prefeitura.sp.gov.br/PaginasPublicas/_SBC.aspx#