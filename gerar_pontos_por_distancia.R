# Pacotes ----

library(geobr)

library(tidyverse)

library(magrittr)

library(sf)

library(spatstat)

# Dados ----

## Importando ----

rec <- geobr::read_municipality(year = 2019) |> 
  dplyr::filter(name_muni == "Recife")  

## Visualizando ----

rec

ggplot() +
  geom_sf(data = rec, color = "black")

## Tratando ----

rec %<>%
  sf::st_transform(crs = 32725)

rec

# Pontos aleatórios ----

## Gerando os pontos aleatórios equidistantes ----

pontos <- rec |> 
  spatstat.geom::as.owin() |> 
  spatstat.random::rSSI(r = 2000,
                        n = 5000)

pontos

## Convertendo para shapefile ----

pontos_shp <- pontos |> 
  as.data.frame() |> 
  sf::st_as_sf(coords = c("x", "y"),
               crs = 32725)

pontos_shp

## Visualizando ----

pontos_shp

ggplot() +
  geom_sf(data = rec, color = "black") +
  geom_sf(data = pontos_shp, color = "black")

# Pontos regulares equidistantes ----

## Gerando os pontos ----

pontos_reg <- rec |> 
  sf::st_make_grid(what = "centers",
                   cellsize = 2000) |> 
  sf::st_as_sf() |> 
  sf::st_intersection(rec)

## Visualizando ----

pontos_reg

ggplot() +
  geom_sf(data = rec, color = "black") +
  geom_sf(data = pontos_reg, color = "black")
