#############################
### COVID-19 17 abril 2020 ##
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

covid_17_04 <- read_csv("01_datos/17 abril/200417COVID19MEXICO.csv")
#View(covid_17_04)

# Municipios de Hgo 

catalogo_mun <- read_excel("01_datos/13abril2020/diccionario_datos_covid19/Catalogos_0412.xlsx", 
                             sheet = "Catálogo MUNICIPIOS", range = "A466:C550", 
                           col_names = c("clave_municipio", "nombre_municipio", "clave_entidad"))

View(catalogo_mun)

# Archivo geojson municipios Hgo

mapa_hgo <- st_read("http://datamx.io/dataset/f9b34c5a-21bd-4cdd-90b1-12c9656d6435/resource/714b2f5a-c39b-4582-936b-3e1283f8c38d/download/hgomunicipal.geojson")%>% 
  st_transform(crs = 4326)

# Juguemos ----

covid_17_04 %>% 
  group_by(ENTIDAD_RES) %>% 
  filter(RESULTADO == 1) %>% 
  count(RESULTADO)

hgo_dia4 <- covid_17_04 %>% 
  filter(ENTIDAD_RES == 13 & RESULTADO == 1)

hgo_dia4 %>% 
  count(RESULTADO) 

entidad_nac <- covid_17_04 %>% 
  select(ENTIDAD_NAC) %>% 
  count(ENTIDAD_NAC)



estado <- c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche","Coahuila","Colima","Chiapas","Chihuahua","Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","Mexico","Michoacán de Ocampo","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas")
clave_estado <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32")

mx <- tibble(estado, clave_estado)

país <- merge(entidad_nac, mx, 
                by.x="ENTIDAD_NAC", 
                by.y = "clave_estado") 


país %>% 
  count(ENTIDAD_NAC) %>% 
  arrange(-n) %>% 
  filter(ENTIDAD_NAC == "09")

país %>% 
  filter(ENTIDAD_NAC == "09")

# Entidad (ENTIDAD_NAC)

covid_17_04 %>% 
  filter(RESULTADO == 1) %>% 
  count(ENTIDAD_NAC) %>% 
  arrange(-n) %>% 
  ggplot(aes(x = fct_reorder(ENTIDAD_NAC, -n), y = n, fill = n)) +
  geom_col() +
  geom_text(aes(label = n), position=position_dodge(width=0.9), vjust = - 0.5, color = "black", size = 03) +
  labs(title = "Casos confirmados de COVID-19 en México",
       subtitle = "CONTEO POR ENTIDAD DE RESIDENCIA", 
       x = NULL,
       y = "Número de casos positivos",
       caption = "Fuente: Datos Abiertos Secretaria de Salud\nElaboración propia. 17 Abril 2020") +
  scale_fill_distiller(palette = "RdYlGn", direction = -1)

# Entidad (ENTIDAD_RES)

covid_17_04 %>% 
  filter(RESULTADO == 1) %>% 
  count(ENTIDAD_RES) %>% 
  arrange(-n) %>% 
  ggplot(aes(x = fct_reorder(ENTIDAD_RES, -n), y = n, fill = n)) +
  geom_col() +
  geom_text(aes(label = n), position=position_dodge(width=0.9), vjust = - 0.5, color = "black") +
  labs(title = "Casos confirmados de COVID-19 en México",
       subtitle = "CONTEO POR ENTIDAD DE RESIDENCIA", 
       x = NULL,
       y = "Número de casos positivos",
       caption = "Fuente: Datos Abiertos Secretaria de Salud\nElaboración propia. 17 Abril 2020") +
  scale_fill_distiller(palette = "RdYlGn", direction = -1)


# Numero de casos confirmados por municipio 

municipios_dia4 <- hgo_dia4 %>% 
  group_by(MUNICIPIO_RES) %>% 
  count(RESULTADO) %>% 
  arrange(-n) %>% 
  print(n = Inf)

municipios_dia4[,1]

cat4 <- catalogo_mun %>%
  filter(clave_municipio %in% c("048", "051","077","069", "021","013","052","067", "003","019","022","023","024","028","039","055","061", "063","065","076", "084")) %>%
  select(-c(clave_entidad))

# Nombres de municipios con casos confirmados dia 3

orden4 <- merge(municipios_dia4, cat4, 
                by.x="MUNICIPIO_RES", 
                by.y = "clave_municipio") %>% 
  arrange(-n)

# Paletas

RColorBrewer::display.brewer.all()

# Óptimos para variables numéricas (las primeras)
# Óptimos para categóricas (las segundas y terceras)

# Grafica de casos positivos por municipio ----

municipios_dia4 %>% 
  ggplot(aes(x = fct_reorder(MUNICIPIO_RES, 
                             -n),
             y = n, fill = n)) +
  geom_col() +
  labs(title = "Casos confirmados de COVID-19 en Hidalgo",
       subtitle = "CONTEO POR MUNICIPIO DE RESIDENCIA", 
       x = NULL,
       y = "Número de casos positivos",
       caption = "Fuente: Datos Abiertos Secretaria de Salud\nElaboración propia. 17 Abril 2020") +
  scale_x_discrete(labels = c("Pachuca de Soto", "Mineral de la Reforma", "Tulancingo de Bravo", "Tizayuca", "Emiliano Zapata", "Atotonilco de Tula", "San Agustin Tlaxiaca", "Tezontepec de Aldama", "Actopan", "Chilcuautla", "Epazoyucan", "Francisco I. Madero", "Huasca de Ocampo", "Huejutla de Reyes", "Mineral del Monte", "Santiago de Anaya", "Tepeapulco", "Tepeji del Río de Ocampo", "Tetepango", "Tula de Allende", "Zimapán")) +
  scale_y_continuous(breaks = seq(0, 30, 5),
                     limits = c(0, 30)) +
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

nombres4 <- orden4[,4]


mapa_muni_hgo4 <- mapa_hgo %>%
  mutate(corona= ifelse(NOMBRE %in% c("Pachuca de Soto", "Mineral de la Reforma", "Tulancingo de Bravo", "Tizayuca", "Emiliano Zapata", "Atotonilco de Tula", "San Agustín Tlaxiaca", "Tezontepec de Aldama", "Actopan", "Chilcuautla", "Epazoyucan", "Francisco I. Madero", "Huasca de Ocampo", "Huejutla de Reyes", "Mineral del Monte", "Santiago de Anaya", "Tepeapulco", "Tepeji del Río de Ocampo", "Tetepango", "Tula de Allende", "Zimapán"), 1, 0 ))

# Graficamos

plot(mapa_muni_hgo4, max.plot = 1)


# Colores

palMuni <- colorFactor(palette = c("#18dede", "#de6118"), 
                       domain = mapa_muni_hgo4$corona)


leaflet(mapa_muni_hgo4, options = leafletOptions(zoomControl = T)) %>% 
  addTiles() %>% 
  addPolygons(color = "black", 
              weight = 1, 
              fillColor = palMuni(mapa_muni_hgo4$corona), 
              fillOpacity = 0.7) %>% 
  addLegend(position = "bottomleft", 
            colors = c("#de6118", "#18dede"), 
            labels = c("Casos positivos COVID-19", 
                       "Sin casos"), 
            opacity = 1) 

