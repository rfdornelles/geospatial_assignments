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

