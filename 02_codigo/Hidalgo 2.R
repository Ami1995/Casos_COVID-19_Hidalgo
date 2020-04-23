#############################
### COVID-19 14 abril 2020 ##
#############################

# Conteo municipal de casos con Resultado Positivo SARS-CoV-2

# Codigo elaborado por: Ami Gabriela Sosa Vera

# Paquetes ----
library(pacman)
p_load(janitor, tidyverse, readr, readxl, dplyr, ggplot2, scales, sf, leaflet)

# Setup ----

Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen=999)

# Base de datos ----

# Datos Abiertos (SSA Federal)

covid_14_04 <- read_csv("01_datos/14 abril/200414COVID19MEXICO.csv")
#View(covid_14_04)

# Municipios de Hgo 

catalogo_mun <- read_excel("01_datos/13abril2020/diccionario_datos_covid19/Catalogos_0412.xlsx", 
                             sheet = "Catálogo MUNICIPIOS", range = "A466:C550", 
                           col_names = c("clave_municipio", "nombre_municipio", "clave_entidad"))

View(catalogo_mun)

# Archivo geojson municipios Hgo

mapa_hgo <- st_read("http://datamx.io/dataset/f9b34c5a-21bd-4cdd-90b1-12c9656d6435/resource/714b2f5a-c39b-4582-936b-3e1283f8c38d/download/hgomunicipal.geojson")%>% 
  st_transform(crs = 4326)

# Juguemos ----

covid_14_04 %>% 
  group_by(ENTIDAD_RES) %>% 
  filter(RESULTADO == 1) %>% 
  count(RESULTADO) %>% 
  ungroup() %>% 
  summarise(total_casos = sum(n))

# 5,399 casos en total en el país

hgo_dia2 <- covid_14_04 %>% 
  filter(ENTIDAD_RES == 13 & RESULTADO == 1)

hgo_dia2 %>% 
  group_by(MUNICIPIO_RES) %>% 
  count(RESULTADO) %>% 
  ungroup() %>% 
  summarise(total_casos = sum(n))

# 64 casos confirmados COVID-19 en total en Hidalgo

# Numero de casos confirmados por municipio ----

casos_munihgo <- merge(hgo_dia2, catalogo_mun, 
                       by.x="MUNICIPIO_RES", 
                       by.y = "clave_municipio")


casos_munhgo2 <- casos_munihgo %>% 
  select(MUNICIPIO_RES, nombre_municipio, RESULTADO) %>% 
  group_by(nombre_municipio) %>% 
  count(RESULTADO) %>% 
  arrange(-n) 

casos_munhgo2 

# Paletas

RColorBrewer::display.brewer.all()

# Óptimos para variables numéricas (las primeras)
# Óptimos para categóricas (las segundas y terceras)

# Grafica de casos pos. por municipio ----

casos_munhgo2 %>% 
  ggplot(aes(x = fct_reorder(nombre_municipio, 
                             -n),
             y = n, fill = n)) +
  geom_col() +
  labs(title = "Casos confirmados de COVID-19 en Hidalgo",
       subtitle = "CONTEO POR MUNICIPIO", 
       x = NULL,
       y = "Número de casos positivos",
       caption = "Fuente: Datos Abiertos Secretaria de Salud\nElaboración propia. 14 Abril 2020") +
  scale_x_discrete(labels=c("Pachuca de Soto", "Mineral de la Reforma", "Tulancingo de Bravo", "Tizayuca", "Emiliano Zapata", "Atotonilco de Tula", "San Agustin Tlaxiaca", "Actopan", "Chilcuautla", "Epazoyucan", "Francisco I. Madero", "Huasca de Ocampo", "Huejutla de Reyes", "Mineral del Monte", "Santiago de Anaya", "Tepeji del Rio de Ocampo", "Tetepango", "Tezontepec de Aldama")) +
  scale_y_continuous(breaks = seq(0, 25, 5),
                     limits = c(0, 25)) +
  coord_flip() +
  geom_text(aes(label = n), position=position_dodge(width=0.9), hjust = - 0.5, color = "black") +
  theme_minimal()+
  theme(legend.title = element_blank()) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(linetype = "solid"),
        title = element_text(size = 12, hjust = 1, color = "black", face = "bold"),
        plot.caption = element_text(size = 11, hjust = 1, color = "grey4", face = "italic"),
        plot.subtitle = element_text(size = 11, color = "grey4", face = "plain"),
        axis.text.y = element_text(size = 10, color = "grey4", face = "plain"), 
        axis.title.x = element_text(size = 11, color = "grey4", face = "plain", hjust = 0.5)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1) 


# Mapa municipios con casos confirmados ----

# Seleccion de municipios en el mapa

mapa_muni_hgo2 <- mapa_hgo %>% 
  mutate(corona= ifelse(NOMBRE %in% c("Pachuca de Soto", "Mineral de la Reforma", "Tulancingo de Bravo", "Tizayuca", "Emiliano Zapata", "Atotonilco de Tula", "San Agustin Tlaxiaca", "Actopan", "Chilcuautla", "Epazoyucan", "Francisco I. Madero", "Huasca de Ocampo", "Huejutla de Reyes", "Mineral del Monte", "Santiago de Anaya", "Tepeji del Rio de Ocampo", "Tetepango", "Tezontepec de Aldama"),
                        1, 0 ))

# Graficamos

plot(mapa_muni_hgo2, max.plot = 1)


# Colores

palMuni <- colorFactor(palette = c("#18dede", "#de6118"), 
                       domain = mapa_muni_hgo2$corona)


leaflet(mapa_muni_hgo2, options = leafletOptions(zoomControl = F)) %>% 
  addTiles() %>% 
  addPolygons(color = "black", 
              weight = 1, 
              fillColor = palMuni(mapa_muni_hgo2$corona), 
              fillOpacity = 0.7) %>% 
  addLegend(position = "bottomleft", 
            colors = c("#de6118", "#18dede"), 
            labels = c("Casos positivos COVID-19", 
                       "Sin casos"), 
            opacity = 1) 

# Añadido 22 abril ---- 
# defunciones totales 

covid_14_04 %>% 
  filter(!is.na(FECHA_DEF)) %>% 
  filter(ENTIDAD_RES == 13 & RESULTADO == 1) %>% 
  count(FECHA_DEF) %>% 
  summarise(tot_def = sum(n))

# 8 def. tot. 

