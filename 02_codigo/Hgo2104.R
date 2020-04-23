#############################
### COVID-19 21 abril 2020 ##
#############################

# Conteo municipal y total estatal de casos con Resultado Positivo SARS-CoV-2

# Codigo elaborado por: Ami Gabriela Sosa Vera

# Paquetes ----
library(pacman)
p_load(janitor, tidyverse, readr, readxl, dplyr, ggplot2, scales, sf, leaflet)

# Setup ----

Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen=999)

# Base de datos ----

# Datos Abiertos (SSA Federal)

covid_21_04 <- read_csv("01_datos/21 abril/200421COVID19MEXICO.csv")
#View(covid_21_04)

# Municipios de Hgo 

catalogo_mun <- read_excel("01_datos/diccionario_datos_covid19/Catalogos_0412.xlsx", 
                           sheet = "Catálogo MUNICIPIOS", range = "A466:C550", col_names = c("clave_municipio", "nombre_municipio", "clave_entidad"))
#View(catalogo_mun)

# Archivo geojson municipios Hgo

mapa_hgo <- st_read("http://datamx.io/dataset/f9b34c5a-21bd-4cdd-90b1-12c9656d6435/resource/714b2f5a-c39b-4582-936b-3e1283f8c38d/download/hgomunicipal.geojson")%>% 
  st_transform(crs = 4326)

# Juguemos ----

covid_21_04 %>% 
  group_by(ENTIDAD_RES) %>% 
  filter(RESULTADO == 1) %>% 
  count(RESULTADO)

hgo_21 <- covid_21_04 %>% 
  filter(ENTIDAD_RES == 13 & RESULTADO == 1)

hgo_21 %>% 
  group_by(MUNICIPIO_RES) %>% 
  count(RESULTADO) %>% 
  arrange(-n)


casos_munihgo <- merge(hgo_21, catalogo_mun, 
                 by.x="MUNICIPIO_RES", 
                 by.y = "clave_municipio")


casos_munhgo21 <- casos_munihgo %>% 
  select(MUNICIPIO_RES, nombre_municipio, RESULTADO) %>% 
  group_by(nombre_municipio) %>% 
  count(RESULTADO) %>% 
  arrange(-n)

# Numero de casos confirmados por municipio 

municipios_21 <- hgo_21 %>% 
  group_by(MUNICIPIO_RES) %>% 
  count(RESULTADO) %>% 
  arrange(-n) %>% 
  print(n = Inf)

municipios_21 %>% 
  select(MUNICIPIO_RES) %>% 
  print(n = Inf)

# Paletas

RColorBrewer::display.brewer.all()

# Óptimos para variables numéricas (las primeras)
# Óptimos para categóricas (las segundas y terceras)

# Grafica de casos positivos por municipio ----


# Gráfica para comparación de labels

# casos_munhgo21 %>% 
#   ggplot(aes(x = fct_reorder(nombre_municipio, 
#                              -n),
#              y = n, fill = n)) +
#   geom_col() +
#   labs(title = "Casos confirmados de COVID-19 en Hidalgo",
#        subtitle = "CONTEO POR MUNICIPIO DE RESIDENCIA", 
#        x = NULL,
#        y = "Número de casos positivos",
#        caption = "Fuente: Datos Abiertos Secretaria de Salud\nElaboración propia. 21 Abril 2020") +
#   scale_y_continuous(breaks = seq(0, 40, 5),
#                      limits = c(0, 40)) +
#   coord_flip() +
#   geom_text(aes(label = n), position=position_dodge(width=0.9), hjust = - 0.5, color = "black") +
#   theme_minimal()+
#   theme(legend.title = element_blank()) +
#   theme(panel.grid.minor = element_blank(), 
#         panel.grid.major = element_line(linetype = "solid"),
#         title = element_text(size = 12, hjust = 1, color = "black", face = "bold"),
#         plot.caption = element_text(size = 11, hjust = 1, color = "grey4", face = "italic"),
#         plot.subtitle = element_text(size = 11, color = "grey4", face = "plain"),
#         axis.text.y = element_text(size = 10, color = "grey4", face = "plain"), 
#         axis.title.x = element_text(size = 11, color = "grey4", face = "plain", hjust = 0.5)) +
#   scale_fill_distiller(palette = "YlGnBu", direction = 1) 

casos_munhgo21

municipios_21 %>% 
  ggplot(aes(x = fct_reorder(MUNICIPIO_RES, 
                             -n),
             y = n, fill = n)) +
  geom_col() +
  labs(title = "Casos confirmados de COVID-19 en Hidalgo",
       subtitle = "CONTEO POR MUNICIPIO DE RESIDENCIA", 
       x = NULL,
       y = "Número de casos positivos",
       caption = "Fuente: Datos Abiertos Secretaria de Salud\nElaboración propia. 21 Abril 2020") +
  scale_x_discrete(labels = c("Pachuca de Soto", "Mineral de la Reforma", "Tizayuca", "Tulancingo de Bravo", "Emiliano Zapata", "Atotonilco de Tula", "Metztitlán","San Agustín Tlaxiaca", "Tezontepec de Aldama", "Mixquiahuala de Juárez","Tepeapulco","Tlaxcoapan","Actopan", "Chilcuautla", "Epazoyucan", "Francisco I. Madero", "Huasca de Ocampo", "Huejutla de Reyes", "Mineral del Monte", "San Salvador", "Santiago de Anaya", "Singuilucan", "Tecozautla", "Tepeji del Río de Ocampo", "Tetepango", "Villa de Tezontepec", "Tlahuelilpan","Tula de Allende", "Zempoala","Zimapán")) +
  scale_y_continuous(breaks = seq(0, 40, 5),
                     limits = c(0, 40)) +
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

casos_munhgo21 %>% 
  print(n = Inf)

nombres21 <- orden21[,4]


mapa_muni_hgo21 <- mapa_hgo %>%
  mutate(corona= ifelse(NOMBRE %in% c("Pachuca de Soto", "Mineral de la Reforma", "Tizayuca", "Tulancingo de Bravo", "Emiliano Zapata", "Atotonilco de Tula", "Metztitlán","San Agustín Tlaxiaca", "Tezontepec de Aldama", "Mixquiahuala de Juárez","Tepeapulco","Tlaxcoapan","Actopan", "Chilcuautla", "Epazoyucan", "Francisco I. Madero", "Huasca de Ocampo", "Huejutla de Reyes", "Mineral del Monte", "San Salvador", "Santiago de Anaya", "Singuilucan", "Tecozautla", "Tepeji del Río de Ocampo", "Tetepango", "Villa de Tezontepec", "Tlahuelilpan","Tula de Allende", "Zempoala","Zimapán"), 1, 0 ))

# Graficamos

plot(mapa_muni_hgo21, max.plot = 1)


# Colores

palMuni <- colorFactor(palette = c("#18dede", "#de6118"), 
                       domain = mapa_muni_hgo21$corona)


leaflet(mapa_muni_hgo21, options = leafletOptions(zoomControl = T)) %>% 
  addTiles() %>% 
  addPolygons(color = "black", 
              weight = 1, 
              fillColor = palMuni(mapa_muni_hgo21$corona), 
              fillOpacity = 0.7) %>% 
  addLegend(position = "bottomleft", 
            colors = c("#de6118", "#18dede"), 
            labels = c("Casos positivos COVID-19", 
                       "Sin casos"), 
            opacity = 1) 

# Añadido 22 abril ---- 
#defunciones por municipio 

covid_21_04 %>% 
  filter(!is.na(FECHA_DEF)) %>% 
  filter(ENTIDAD_RES == 13 & RESULTADO == 1) %>% 
  count(FECHA_DEF) %>% 
  summarise(tot_def = sum(n))
