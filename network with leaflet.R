# Carregar bibliotecas
library(geobr)
library(leaflet)
library(sf)
library(tidyverse)

rm(list=ls()) # limpar as variáveis carregadas

# Dados da rede de cidades
network <- data.frame(cod_muni_origem = c(1200203,1721000,5107909,1400472,1302504,5107909,1304062),
                      cod_muni_destino = c(1200401,1200401,2105302,1702109,2101202,5103403,1506807))

# Baixar e armazenar shape da amazônia legal 
shape.amazon <- read_amazon() 

# Baixar e organizar coordenadas das cidades brasileiras
coordMunicipal <- read_municipal_seat(year = 2010,showProgress = T) 
coordMunicipal$lat <- as.numeric(st_coordinates(coordMunicipal$geom)[,2])
coordMunicipal$lng <- as.numeric(st_coordinates(coordMunicipal$geom)[,1])

# TRANSFORMAR O VETOR COM AS COORDENADAS EM CAMPOS SEPARADOS

# Filtrar as cidades que serão analisadas
mun.inter<- coordMunicipal %>% 
  dplyr::filter(code_muni %in% c('1100023','1100122','1100205','1200401','1200203','1302405','1302504','1301902','1304203',
                                 '1304062','1303403','1400472','1400100','1502400','1500602','1501808','1504208','1506138',
                                 '1506807','1600303','1600501','1702109','1709500','1721000','2101202','2109908','2109106',
                                 '2105302','2103000','5102504','5101803','5107602','5103403','5107909'))

# Organizar dataframe e remover colunas que não serão utilizadas e renomear aquelas que serão usadas.
coordMunicipal$code_muni <- as.character(coordMunicipal$code_muni)
mun.inter$code_muni <- as.character(mun.inter$code_muni)
network$cod_muni_destino <- as.character(network$cod_muni_destino)
network$cod_muni_origem <- as.character(network$cod_muni_origem)


network <- merge(network,coordMunicipal,by.x ='cod_muni_origem',by.y = 'code_muni')
colnames(network)[3] <- 'name_muni_origem'
colnames(network)[9] <- 'geom_origem'
colnames(network)[10] <- 'lat_origem'
colnames(network)[11] <- 'lng_origem'

network <- merge(network,coordMunicipal,by.x ='cod_muni_destino',by.y = 'code_muni')
colnames(network)[12] <- 'name_muni_destino'
colnames(network)[18] <- 'geom_destino'
colnames(network)[19] <- 'lat_destino'
colnames(network)[20] <- 'lng_destino'

network <- network[c(1:4,10:13,19:21)]

# Criar o mapa
mapa <- leaflet(shape.amazon) %>% 
  addTiles() %>%  
  addMapPane('polygons',zIndex=410) %>%
  addMapPane('dots',zIndex=420) #

#paleta de cores para categorias de cidades (ex: centroA, centroB)
pal <- colorFactor(c("red", "navy"), domain = c("centroA", "centroB"))

mapa <- mapa %>% 
  addPolygons(
    opacity = 0.1,
    labelOptions = labelOptions(
      options = pathOptions(pane = 'polygons')
    )
  ) %>% 
  addCircleMarkers(
    data = mun.inter,
    lat = ~lat,
    lng = ~lng,
    radius = 5,
    #radius = ~ifelse(tipo == "centroA", 5, 4), ########
    #color = ~pal(tipo), # Aqui ele busca na variável pal o domain da linha e retorna a cor do color factor
    stroke = FALSE, 
    fillOpacity = 0.3,
    label = mun.inter$name_muni,
    labelOptions = labelOptions(
      options = pathOptions(pane = 'dots')
    )) 

mapa

# Loop para gerar as ligações entre as cidades.
i <- 1
repeat{
  k <- network$lat_origem [i]
  j <- network$lat_destino[i]
  l <- network$lng_origem [i]
  m <- network$lng_destino[i]
  
  mapa <- mapa %>% 
            addPolylines(data = network, #aqui as polylines estão seguindo a ordem das linhas no arquivo. Criar um arquivo novo?
                         lng = c(l,m), #~lng, 
                         lat = c(k,j), #~lat,
                         weight = .7,
                         labelOptions = labelOptions(
                           options = pathOptions(pane = 'dots')
                         ))
  
  if (i == nrow(network)) break
  i <- i + 1
}

mapa 
