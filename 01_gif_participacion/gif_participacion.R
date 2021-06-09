library(tidyverse)
library(viridis)
library(sf)
library(ggtext)
library(magick)

theme_map <- function(...) {
  theme_minimal() +
    theme(
      #text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      ...
    )
}

# Necesitas descargar alguna versión válida del PREP para correr esto.
bd <- read_csv("PREP_INE/20210607_2000_PREP_Diputaciones/Diputaciones_2021.csv", skip = 4)

bd2 <- bd %>%
  select(ID_ESTADO,
         ESTADO,DISTRITO_FEDERAL,
         SECCION, LISTA_NOMINAL,
         TOTAL_VOTOS_CALCULADOS,
         TOTAL_VOTOS_ASENTADO) %>%
  mutate(pp = 100*(as.numeric(TOTAL_VOTOS_ASENTADO)/LISTA_NOMINAL)) %>%
  mutate(pp = ifelse(is.na(pp),
                     yes = 100*(as.numeric(TOTAL_VOTOS_CALCULADOS)/LISTA_NOMINAL),
                     no = pp
                     )) %>%
  mutate(pp = ifelse(pp > 100 | pp <= 0,
         yes = NA,
         no = pp))

# Mergeamos con el shape
# Estos shapes los saqué de un tweet de @moaimx: https://twitter.com/moaimx/status/1392108901382885377
shape <- st_read("geojsons/secciones.geojson")
munis <- st_read("geojsons/municipio.geojson")
# Checamos los datos ----
# plot(shape, max.plot = 1)

# Caso n = 1. Mapa de Morelos

munis_1 <- munis %>%
  filter(entidad == 17)

shape_1 <- shape %>%
  filter(entidad == 17)

bd2_1 <- bd2 %>%
  rename(entidad = ID_ESTADO,
         seccion = SECCION) %>%
  filter(entidad == 17)

map <- left_join(shape_1, bd2_1)


map %>%
  ggplot() +
  geom_sf(aes(fill = pp), color = "white", size = 0.05) +
  geom_sf(data = munis_1, size = 0.8,
          fill = NA, color = "white") +
  scale_fill_gradientn(colors = magma(begin = 0,
                                        end = 1,
                                        n = 10),
                       breaks = seq(10,100,10),
                       limits = c(0,100),
                       label = scales::comma_format(suffix = "%")) +
  theme_minimal() +
  theme_map() +
  labs(title = str_c("<b>Porcentaje de Participación</b><br>Estado de ",
                     str_to_sentence(map$ESTADO[1]),
                     ""),
       caption = "<b>Fuente: </b> INE. Datos del PREP a las 20:00 del 7 de Junio del 2021<br>Cálculo de la participación: 100*(TOTAL_VOTOS_ASENTADO/LISTA NOMINAL)<br>@JuvenalCamposF") +
  theme(legend.position = "bottom",
        plot.caption = element_markdown(hjust = 1),
        plot.title = element_markdown(hjust = 0.5,
                                      family = "Poppins")) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 15,
                               barheight = 0.5
                               ))

ggsave(str_c("imgs_gif/", map$ESTADO[1], ".png"),
       device = "png",
       width = 11.1,
       height = 8.93)


# Armamos el loop ----

for(i in 1:32){

  munis_1 <- munis %>%
    filter(entidad == i)

  shape_1 <- shape %>%
    filter(entidad == i)

  bd2_1 <- bd2 %>%
    rename(entidad = ID_ESTADO,
           seccion = SECCION) %>%
    filter(entidad == i)

  map <- left_join(shape_1, bd2_1)


  map %>%
    ggplot() +
    geom_sf(aes(fill = pp), color = "white", size = 0.05) +
    geom_sf(data = munis_1, size = 0.8,
            fill = NA, color = "white") +
    scale_fill_gradientn(colors = magma(begin = 0,
                                        end = 1,
                                        n = 10),
                         breaks = seq(10,100,10),
                         limits = c(0,100),
                         label = scales::comma_format(suffix = "%")) +
    theme_minimal() +
    theme_map() +
    labs(fill = "Porcentaje de Participación (%)",
      title = str_c("<b>Porcentaje de Participación</b><br>Estado de ",
                       str_to_sentence(map$ESTADO[1]),
                       ""),
         caption = "<b>Fuente: </b> INE. Datos del PREP a las 20:00 del 7 de Junio del 2021<br>Cálculo de la participación: 100*(TOTAL_VOTOS_ASENTADO/LISTA NOMINAL)<br>@JuvenalCamposF") +
    theme(legend.position = "bottom",
          plot.caption = element_markdown(hjust = 1),
          plot.title = element_markdown(hjust = 0.5,
                                        family = "Poppins")) +
    guides(fill = guide_colorbar(title.position = "top",
                                 title.hjust = 0.5,
                                 barwidth = 15,
                                 barheight = 0.5
    ))

  ggsave(str_c("imgs_gif/", map$ESTADO[1], ".png"),
         device = "png",
         width = 11.1,
         height = 8.93)

  print(i)

}

# CREACIÓN DEL GIF.
str_c("imgs_gif/", list.files("imgs_gif/")) %>%
  map(image_read) %>% # Lee rutas de los archivos.
  image_join() %>% # Junta imágenes
  image_animate(fps=1) %>% # Anima las imagenes, con 1 segundo entre imágenes.
  image_write("pp.gif") # Escribe el gif en el directorio.




