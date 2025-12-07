# Pacotes ----

library(geobr)

library(sf)

library(tidyverse)

library(adehabitatHR)

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

mov_sf

ggplot() +
  geom_sf(data = mov_sf)

# Pontos dentro de um shapefile -----

## Pontos aleatórios ----

set.seed(123); pontos_aleatorios <- br |>
  sf::st_sample(size = 50, type = "random") |>
  sf::st_as_sf(crs = 4674)

pontos_aleatorios

ggplot() +
  geom_sf(data = estados, color = "black") +
  geom_sf(data = pontos_aleatorios)

## Pontos regulares baseado em quantidade ----

pontos_reg_quant <- br |>
  sf::st_sample(size = 50, type = "regular") |>
  sf::st_as_sf(crs = 4674)

pontos_reg_quant

ggplot() +
  geom_sf(data = estados, color = "black") +
  geom_sf(data = pontos_reg_quant)

## Pontos regulares baseado em distâncias entre os pontos ----

pontos_reg_dist <- br |>
  sf::st_transform(crs = 32725) |>
  sf::st_make_grid(what = "centers",
                   cellsize = 200000) |>
  sf::st_intersection(br |>
                        sf::st_transform(crs = 32725)) |>
  sf::st_transform(crs = br |> sf::st_crs())

pontos_reg_dist |> plot()

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = pontos_reg_dist, color = "black")

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

### Para sahepfiles ----

minimo_poligono_convexo <- br |>
  sf::st_convex_hull()

ggplot() +
  geom_sf(data = minimo_poligono_convexo, color = "black", fill = "green4") +
  geom_sf(data = estados, color = "black")

### Para pontos ----

#### Para 100 % ----

minimo_poligono_convexo_pontos100 <- pontos_aleatorios |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 100) |>
  sf::st_as_sf() |>
  dplyr::mutate(`% de ocorrências` = "100%")

minimo_poligono_convexo_pontos100

#### Para 90 % ----

minimo_poligono_convexo_pontos90 <- pontos_aleatorios |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 90) |>
  sf::st_as_sf() |>
  dplyr::mutate(`% de ocorrências` = "090%")

minimo_poligono_convexo_pontos90

#### Para 50 % ----

minimo_poligono_convexo_pontos50 <- pontos_aleatorios |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 50) |>
  sf::st_as_sf() |>
  dplyr::mutate(`% de ocorrências` = "050%")

minimo_poligono_convexo_pontos50

#### Unindo e Visualizando os dados ----

pontos_mcp <- ls(pattern = "minimo_poligono_convexo_pontos") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

pontos_mcp

ggplot() +
  geom_sf(data = pontos_mcp, aes(color = `% de ocorrências`),
          fill = NULL,
          linewidth = 1) +
  geom_sf(data = pontos_aleatorios)

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
  sf::st_transform(crs = 32725) |>
  sf::st_length()

deslocamento

### Compriemto da menor distância ----

menor_deslocamento <- mov_sf |>
  sf::st_transform(crs = 32725) |>
  dplyr::mutate(ponto_ini = lwgeom::st_startpoint(geometry),
                ponto_fim = lwgeom::st_endpoint(geometry),
                dist_metros = sf::st_distance(ponto_ini,
                                              ponto_fim,
                                              by_element = TRUE)) |>
  dplyr::pull(dist_metros)

menor_deslocamento

### Calculando ----

1 - as.numeric(menor_deslocamento/deslocamento)

