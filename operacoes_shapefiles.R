# Pacotes ----

library(geobr)

library(sf)

library(tidyverse)

library(lwgeom)

# Dados ----

## Estados ----

### Importando ----

estados <- geobr::read_state(year = 2019)

estados <- sf::st_read("estados.shp")

### Visualizando ----

ggplot() +
  geom_sf(data = estados, color = "black")

## Biomas ----

### Importando ----

biomas <- geobr::read_biomes(year = 2019)

biomas <- sf::st_read("biomas.shp")

### Visualizando ----

biomas

ggplot() +
  geom_sf(data = biomas, color = "black")

## Brasil ----

### Importando ----

br <- geobr::read_country(year = 2019)

br <- sf::st_read("pais.shp")

### Visualizando ----

br

ggplot() +
  geom_sf(data = br, color = "black")

## Pontos aleatórios ----

set.seed(123); pontos_aleatorios <- br |>
  sf::st_sample(size = 50, type = "random") |>
  sf::st_as_sf()

pontos_aleatorios

ggplot() +
  geom_sf(data = estados, color = "black") +
  geom_sf(data = pontos_aleatorios)

## Trajetória de movimento ----

### Importando ----

mov <- read_csv("Short-term movements on the Dark-headed cat snake Boiga nigriceps in Kubah National Park.csv")

### Tratando ----

mov_sf <- mov |>
  sf::st_as_sf(coords = c("location-long", "location-lat"),
               crs = 4674) |>
  dplyr::group_by(`individual-local-identifier`) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING") |>
  dplyr::mutate(`individual-local-identifier` = `individual-local-identifier` |>
                  as.character())

### Visualizando ----

mov_sf

ggplot() +
  geom_sf(data = mov_sf, aes(color = `individual-local-identifier`), linewidth = 1)

# Operações ----

## Área geográfica ----

estados |>
  sf::st_area() / 1000000

estados |>
  dplyr::mutate(Área = (estados |>
                          sf::st_area() / 1000000) |>
                  as.numeric()) |>
  ggplot() +
  geom_sf(aes(fill = Área), color = "black") +
  scale_fill_viridis_c()

## Intersecção ----

interseccao <- sf::st_intersection(estados,
                                   biomas |>
                                     dplyr::filter(name_biome == "Mata Atlântica"))

interseccao

ggplot() +
  geom_sf(data = interseccao, color = "black")

## Diferença ----

diferenca <- sf::st_difference(estados,
                               biomas |>
                                 dplyr::filter(name_biome == "Mata Atlântica"))

diferenca

ggplot() +
  geom_sf(data = diferenca, color = "black")

## Mínimo polígono convexo ----

minimo_poligono_convexo <- br |>
  sf::st_convex_hull()

ggplot() +
  geom_sf(data = minimo_poligono_convexo, color = "black", fill = "green4") +
  geom_sf(data = estados, color = "black")

## Intersecção de atributos ----

interseccao_atributos <- pontos_aleatorios |>
  sf::st_join(biomas)

interseccao_atributos

ggplot() +
  geom_sf(data = estados, color = "black") +
  geom_sf(data = interseccao_atributos,
          aes(fill = name_biome),
          size = 3,
          shape = 21,
          stroke = 1) +
  scale_fill_manual(values = c("darkgreen",
                               "goldenrod",
                               "orangered",
                               "limegreen",
                               "royalblue",
                               "orange4"))

## Tortuosidade de deslocamento ----

### Comprimento total -----

deslocamento <- mov_sf |>
  sf::st_length()

deslocamento

### Compri,emto da menor distância ----

menor_deslocamento <- mov_sf |>
  dplyr::mutate(ponto_ini = lwgeom::st_startpoint(geometry),
                ponto_fim = lwgeom::st_endpoint(geometry),
                dist_metros = sf::st_distance(ponto_ini, ponto_fim, by_element = TRUE)) |>
  dplyr::pull(dist_metros)

menor_deslocamento

### Calculando ----

1 - as.numeric(menor_deslocamento/deslocamento)

