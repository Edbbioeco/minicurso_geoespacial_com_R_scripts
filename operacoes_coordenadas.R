# Pacotes ----

library(readxl)

library(tidyverse)

library(sp)

library(parzer)

library(fields)

library(geosphere)

# Dados ----

## Dataframe de coordenadas ----

### Importando ----

df_xy <- readxl::read_xlsx("coordenadas.xlsx")

### Visualizando ----

df_xy |> dplyr::glimpse()

df_xy |> as.data.frame()

## Azimute -----

set.seed(123); azimute <- sample(seq(0, 360, by = 0.1), size = 1)

azimute

## Distância -----

distancia <- seq(10, 100, 5)

distancia

# operações ----

## Decimal para graus-minutos-segundos ----

df_xy_gms <- df_xy |>
  dplyr::mutate(Long = Long |>
                  sp::dd2dms(NS = FALSE) |>
                  as.character(),
                Lat = Lat |>
                  sp::dd2dms(NS = TRUE) |>
                  as.character())

df_xy_gms

## Graus-minutos-segundos para decimal ----

df_xy_gms |>
  dplyr::mutate(Long = Long |>
                  parzer::parse_lon(),
                Lat = Lat |>
                  parzer::parse_lat()) |>
  as.data.frame()

## Distância geodésica ----

df_xy_matriz <- df_xy |>
  as.data.frame(row.names = paste0("ponto_", 1:13)) |>
  fields::rdist.earth(miles = FALSE) |>
  as.dist()

df_xy_matriz

## Calcular distância entre dois pontos individuais ----

pontos_dist <- geosphere::distGeo(df_xy[1, ],
                                  df_xy[2, ])

pontos_dist

## Calcular coordenadas baseado em graus de azimute ----

### Para apenas uma distância ----

geosphere::destPoint(p = df_xy,
                     b = azimute,
                     d = distancia[1])

df_xy |> as.data.frame()

### Para diversas distâncias ----

calcular_coordenadas_azimute_fun <- function(x){

  message(paste0("calculo para ", distancia[x], " m"))

  novas_coords <- geosphere::destPoint(p = df_xy,
                                       b = azimute,
                                       d = distancia[x])

  novas_coords |>
    print()

  message("")

}

purrr::walk(1:length(distancia),
            calcular_coordenadas_azimute_fun)
