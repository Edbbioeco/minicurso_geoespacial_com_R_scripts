# Pacotes ----

library(geodata)

library(here)

library(tidyverse)

library(terra)

library(tidyterra)

library(geobr)

library(sf)

library(gstat)

library(spatstat)

library(spatstat.geom)

library(spatstat.explore)

# Dados ----

## Variáveis bioclimáticas ----

### Importando ----

bio <- geodata::worldclim_country(country = "BRA",
                                  var = "bio",
                                  res = 0.5,
                                  path = here::here())

bio <- terra::rast("bio_worldclim.tif")

### Visualizando ----

ggplot() +
  tidyterra::geom_spatraster(data = bio) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c()

## Cidade de Amaraji ----

### Importando ----

amaraji <- geobr::read_municipality(year = 2019) |>
  dplyr::filter(name_muni == "Amaraji")

amaraji <- sf::st_read("amaraji.shp")

### Visualizando ----

ggplot() +
  geom_sf(data = amaraji) +
  scale_fill_viridis_c()

## Biomas ----

### Importando ----

biomas <- geobr::read_biomes(year = 2019)

biomas <- sf::st_read("biomas.shp")

### Visualizando -----

ggplot() +
  geom_sf(data = biomas, color = "black")

ggsave(filename = "bioma.png", height = 10, width = 12)

# Setar temas ----

theme_set(theme_bw() +
            theme(axis.text = element_text(size = 15, color = "black"),
                   axis.title = element_text(size = 15, color = "black"),
                   strip.text = element_text(size = 15, color = "black"),
                   strip.background = element_rect(linewidth = 1, color = "black"),
                   legend.text = element_text(size = 15, color = "black"),
                   legend.title = element_text(size = 15, color = "black"),
                   panel.border = element_rect(linewidth = 1, color = "black")))

# Opeações ----

## Matemática de rasters ----

bio$soma <- bio[[1]] + bio[[2]]

bio$subtração <- bio[[1]] - bio[[2]]

bio$divisão <- bio[[1]] / bio[[2]]

ggplot() +
  tidyterra::geom_spatraster(data = bio[[c(1, 2, 20, 21)]]) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_viridis_c(na.value = "transparent") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

ggsave(filename = "operacoes_matematicas_rasters.png", height = 10, width = 12)

## Shapefile para raster ----

shapefile_p_raster <- biomas |>
  terra::vect() |>
  terra::ext() |>
  terra::rast(resolution = bio |> terra::res())

shapefile_p_raster <- biomas |>
  terra::vect() |>
  terra::rasterize(shapefile_p_raster, field = "name_biome")

shapefile_p_raster

ggplot() +
  tidyterra::geom_spatraster(data = shapefile_p_raster) +
  scale_fill_viridis_d(na.value = "transparent")

ggsave(filename = "shapefile_p_raster.png", height = 10, width = 12)

## Raster para shapefile ----

raster_p_shapefile <- shapefile_p_raster |>
  terra::as.polygons() |>
  sf::st_as_sf()

ggplot() +
  geom_sf(data = raster_p_shapefile, color = "black")

ggsave(filename = "raster_p_shapefile.png", height = 10, width = 12)

## Filtragem de rasters ----

bio_5 <- bio$wc2.1_30s_bio_5

`25_c `<- bio$wc2.1_30s_bio_5 |>
    tidyterra::filter(wc2.1_30s_bio_5 >= 25)

`27.5_c `<- bio$wc2.1_30s_bio_5 |>
    tidyterra::filter(wc2.1_30s_bio_5 >= 27.5)

`30_c `<- bio$wc2.1_30s_bio_5 |>
    tidyterra::filter(wc2.1_30s_bio_5 >= 30)

bio5 <- c(bio_5, `25_c `, `27.5_c `, `30_c `)

names(bio5) <- c("Bio 5", ">=25°C", ">=27.5°C", ">=30°C")

bio5

ggplot() +
  tidyterra::geom_spatraster(data = bio5) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(na.value = "transparent") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

ggsave(filename = "bio_5_filtrado.png", height = 10, width = 12)

## Binarizando rasters ----

b5 <- bio_5 >= 37

b25 <- bio_5 >= 25

b27.5 <- bio_5 >= 27.5

b30 <- bio_5 >= 30

b_binario <- c(b5, b25, b27.5, b30)

names(b_binario) <- c("Bio5", ">=25°C", ">=27.5°C", ">=30°C")

b_binario

ggplot() +
  tidyterra::geom_spatraster(data = b_binario) +
  facet_wrap(~lyr) +
  scale_fill_viridis_d() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

ggsave(filename = "bio_5_binario.png", height = 10, width = 12)

## Recortando rasters ----

ggplot() +
  tidyterra::geom_spatraster(data = bio_5) +
  geom_sf(data = biomas |>
    dplyr::filter(name_biome == "Mata Atlântica"),
         color = "black",
         linewidth = 1,
         fill = "transparent") +
  scale_fill_viridis_c(na.value = "transparent") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

ggsave(filename = "bio_5_ma.png", height = 10, width = 12)

bio_5_ma <- bio_5 |>
  terra::mask(biomas |>
                dplyr::filter(name_biome == "Mata Atlântica")) |>
  terra::crop(biomas |>
                dplyr::filter(name_biome == "Mata Atlântica"))

bio_5_ma

ggplot() +
  tidyterra::geom_spatraster(data = bio_5_ma) +
  scale_fill_viridis_c(na.value = "transparent") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

ggsave(filename = "bio_5_ma_crop.png", height = 10, width = 12)

## Extraindo valores de rasters ----

### Pontos ----

set.seed(123); bio_5_pontos <- bio_5 |>
  terra::as.polygons() |>
  sf::st_as_sf() |>
  sf::st_set_crs(bio_5 |> terra::crs()) |>
  sf::st_sample(size = 15,
                type = "regular")

bio_5_pontos

### Extraindo ----

valores_extraidos <- bio_5 |>
  terra::extract(bio_5_pontos |>
                   sf::st_as_sf())

valores_extraidos

### Plotando ----

bio_5_pontos <- bio_5_pontos |>
  sf::st_as_sf() |>
  sf::st_set_crs(bio_5 |> terra::crs()) |>
  dplyr::mutate(id = 1:15)

bio_5_pontos

ggplot() +
  tidyterra::geom_spatraster(data = bio_5) +
  geom_sf_text(data = bio_5_pontos, aes(label = id),
               size = 7,
               fontface = "bold") +
  scale_fill_viridis_c(na.value = "transparent") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

ggsave(filename = "bio_5_pontos.png", height = 10, width = 12)

## Interpolação espacial ----

### Recortando o raster ----

bio_5_amaraji <- bio_5 |>
  terra::mask(amaraji) |>
  terra::crop(amaraji)

ggplot() +
  tidyterra::geom_spatraster(data = bio_5_amaraji) +
  scale_fill_viridis_c(na.value = "transparent") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

### Pontos para extrair os valores ----

pontos_amaraji <- amaraji |>
  sf::st_sample(size = 50, type = "regular") |>
  sf::st_as_sf(crs = sf::st_crs(bio_5_amaraji))

ggplot() +
  tidyterra::geom_spatraster(data = bio_5_amaraji) +
  geom_sf(data = pontos_amaraji) +
  scale_fill_viridis_c(na.value = "transparent") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

### Extraindo os valores ----

pontos_amaraji$valor <- terra::extract(bio_5_amaraji,
                                       pontos_amaraji)[[2]]

sf::st_crs(pontos_amaraji) <- sf::st_crs(amaraji)

ggplot() +
  geom_sf(data = amaraji, color = "black") +
  geom_sf(data = pontos_amaraji, aes(fill = valor),
          shape = 21,
          color = "black",
          size = 2.5) +
  scale_fill_viridis_c(na.value = "transparent") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

### Shapefile do raster ----

amaraji_sf <- bio_5_amaraji |>
  terra::classify(matrix(c(-Inf, Inf, 1), ncol = 3, byrow = TRUE)) |>
  terra::as.polygons(dissolve = TRUE) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = amaraji |> sf::st_crs())

ggplot() +
  geom_sf(data = amaraji_sf, color = "black") +
  geom_sf(data = pontos_amaraji, aes(fill = valor),
          shape = 21,
          color = "black",
          size = 2.5) +
  scale_fill_viridis_c(na.value = "transparent") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

### Criando a grade ----

res <- bio_5_amaraji |>
  terra::res()

grid_amaraji <- amaraji |>
  sf::st_make_grid(cellsize = res[1])

ggplot() +
  geom_sf(data = amaraji, color = "black") +
  geom_sf(data = grid_amaraji, color = "black", fill = "transparent") +
  geom_sf(data = pontos_amaraji, aes(fill = valor),
          shape = 21,
          color = "black",
          size = 5,
          stroke = 1) +
  scale_fill_viridis_c(na.value = "transparent") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

ggsave(filename = "grade_interpolação_espacial.png", height = 10, width = 12)

### Interpolação espacial ----

calcular_interpolação <- function(idp){

  resultado_idw <- gstat::idw(formula = valor ~ 1,
                              locations = pontos_amaraji,
                              newdata = grid_amaraji,
                              idp = idp)

  assign(paste0("idw_", idp),
         resultado_idw,
         envir = globalenv())

}

purrr::walk(seq(0, 3.5, 0.5), calcular_interpolação)

ls(pattern = "idw_") |>
  mget(envir = globalenv())

### Criando o raster ----

rasterizar_interpolação <- function(interpolado, idp){

  raster_inter <- interpolado |>
    sf::st_intersection(amaraji_sf) |>
    terra::vect() |>
    terra::rasterize(y = bio_5_amaraji,
                     field = "var1.pred")

  assign(paste0("raster_", idp),
         raster_inter,
         envir = globalenv())

}

lista_inter <- paste0("idw_", seq(0, 3.5, 0.5)) |>
  mget(envir = globalenv())

lista_inter

purrr::walk2(lista_inter, seq(0, 3.5, 0.5), rasterizar_interpolação)

paste0("raster_", seq(0, 3.5, 0.5))|>
  mget(envir = globalenv()) |> c()

rasters_interpolacao <- c(bio_5_amaraji,
                          raster_0,
                          raster_0.5,
                          raster_1,
                          raster_1.5,
                          raster_2,
                          raster_2.5,
                          raster_3,
                          raster_3.5)

names(rasters_interpolacao) <- c("Bio 5",
                                 paste0("idp = ", seq(0, 3.5, 0.5)))

rasters_interpolacao

ggplot() +
  tidyterra::geom_spatraster(data = rasters_interpolacao) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(na.value = "transparent") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        strip.text = element_text(size = 15, color = "black"),
        strip.background = element_rect(linewidth = 1, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        panel.border = element_rect(linewidth = 1, color = "black"))

ggsave(filename = "interpolacoes.png", height = 10, width = 12)

### Variograma ----

gstat::variogram(valor ~ 1,
          pontos_amaraji |>
            sf::as_Spatial()) |>
  ggplot(aes(dist, gamma, label = np)) +
  geom_text(fontface = "bold",
            size = 7.5) +
  labs(x = "Distance (km)",
       y = "y(h)") +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        panel.border = element_rect(linewidth = 1, color = "black"))

ggsave(filename = "variograma.png", height = 10, width = 12)

## Kernels ----

### Pontos aleatórios ----

br <- geobr::read_country(year = 2019)

br <- sf::st_read("pais.shp")

pontos <- dismo::gbif(genus = "Scinax", species = "x-signatus") |>
  dplyr::select(lon, lat) |>
  tidyr::drop_na() |>
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = 4674) |>
  sf::st_intersection(br)

pontos <- sf::st_read("pontos.shp")

pontos

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = pontos)

ggsave(filename = "pontos_kernel.png", height = 10, width = 12)

### Calcular KDE -----

#### Calculando a janela ----

w <- spatstat.geom::as.owin(br |>
                              sf::st_transform(crs = 31984) |>
                              sf::st_bbox() |>
                              sf::st_as_sfc())

w

#### Coordenadas dos pontos -----

coords <- pontos |>
  sf::st_transform(crs = 31984) |>
  sf::st_coordinates()

coords


#### Objeto ppp ----

ppp_obj <- spatstat.geom::ppp(x = coords[,1],
                              y = coords[,2],
                              window = w)

ppp_obj

#### Criando o Kernel ----

dens <- density(ppp_obj, kernel = "gaussian")

dens

plot(dens)

### Rasterizando o kernel ----

raster_kernel <- dens |>
  terra::rast()

terra::crs(raster_kernel) <- br |> terra::crs()

terra::ext(raster_kernel) <- br |> terra::ext()

raster_kernel <- raster_kernel |>
  terra::crop(br) |>
  terra::mask(br)

ggplot() +
  tidyterra::geom_spatraster(data = raster_kernel) +
  geom_sf(data = br, color = "black", fill = "transparent") +
  geom_sf(data = pontos) +
  scale_fill_viridis_c(na.value = "transparent")

ggsave(filename = "pontos_kernel_raster.png", height = 10, width = 12)

### Perfil de verossimelhaça ----

h_ppl <- spatstat.explore::bw.ppl(ppp_obj)

h_ppl

dens_ppl <- density(ppp_obj, kernel = "gaussian", sigma = h_ppl)

plot(dens_ppl)

raster_kernel_ppl <- dens_ppl |>
  terra::rast()

terra::crs(raster_kernel_ppl) <- br |> terra::crs()

terra::ext(raster_kernel_ppl) <- br |> terra::ext()

raster_kernel_ppl <- raster_kernel_ppl |>
  terra::crop(br) |>
  terra::mask(br)

ggplot() +
  tidyterra::geom_spatraster(data = raster_kernel_ppl) +
  geom_sf(data = br, color = "black", fill = "transparent") +
  geom_sf(data = pontos) +
  scale_fill_viridis_c(na.value = "transparent")

ggsave(filename = "pontos_kernel_raster_ppl.png", height = 10, width = 12)

### Regra prática de Diggle ----

h_dgg <- spatstat.explore::bw.diggle(ppp_obj)

h_dgg

dens_dgg <- density(ppp_obj, kernel = "gaussian", sigma = h_dgg)

plot(dens_dgg)

raster_kernel_dgg <- dens_dgg |>
  terra::rast()

terra::crs(raster_kernel_dgg) <- br |> terra::crs()

terra::ext(raster_kernel_dgg) <- br |> terra::ext()

raster_kernel_dgg <- raster_kernel_dgg |>
  terra::crop(br) |>
  terra::mask(br)

ggplot() +
  tidyterra::geom_spatraster(data = raster_kernel_dgg) +
  geom_sf(data = br, color = "black", fill = "transparent") +
  geom_sf(data = pontos) +
  scale_fill_viridis_c(na.value = "transparent")

ggsave(filename = "pontos_kernel_raster_dgg.png", height = 10, width = 12)

### Validação cruzada dos mínimos quadrados ----

h_cvl <- spatstat.explore::bw.CvL(ppp_obj)

h_cvl

dens_cvl <- density(ppp_obj, kernel = "gaussian", sigma = h_cvl)

plot(dens_cvl)

raster_kernel_cvl <- dens_cvl |>
  terra::rast()

terra::crs(raster_kernel_cvl) <- br |> terra::crs()

terra::ext(raster_kernel_cvl) <- br |> terra::ext()

raster_kernel_cvl <- raster_kernel_cvl |>
  terra::crop(br) |>
  terra::mask(br)

ggplot() +
  tidyterra::geom_spatraster(data = raster_kernel_cvl) +
  geom_sf(data = br, color = "black", fill = "transparent") +
  geom_sf(data = pontos) +
  scale_fill_viridis_c(na.value = "transparent")

ggsave(filename = "pontos_kernel_raster_cvl.png", height = 10, width = 12)

### Regra empírica de Scott ----

h_sct <- spatstat.explore::bw.scott(ppp_obj)

h_sct

dens_sct <- density(ppp_obj, kernel = "gaussian", sigma = h_sct)

plot(dens_sct)

raster_kernel_sct <- dens_sct |>
  terra::rast()

terra::crs(raster_kernel_sct) <- br |> terra::crs()

terra::ext(raster_kernel_sct) <- br |> terra::ext()

raster_kernel_sct <- raster_kernel_sct |>
  terra::crop(br) |>
  terra::mask(br)

ggplot() +
  tidyterra::geom_spatraster(data = raster_kernel_sct) +
  geom_sf(data = br, color = "black", fill = "transparent") +
  geom_sf(data = pontos) +
  scale_fill_viridis_c(na.value = "transparent")

ggsave(filename = "pontos_kernel_raster_sct.png", height = 10, width = 12)
