"Datos/Indicador_pobreza_localidad_2020/Indicador_pobreza_localidad_2020.xlsx" |> readxl::excel_sheets()
pobreza_urbana=readxl::read_excel("Datos/Indicador_pobreza_localidad_2020/Indicador_pobreza_localidad_2020.xlsx",sheet = "Hidalgo",skip = 3)
pobreza_urbana=pobreza_urbana |> 
  dplyr::filter(!is.na(Localidad))


##Trae un rango de pobreza. Propongo que tomemos el punto medio para estimar la pobreza.
#Trae un campo de población del ITER**

# En 2020, en estas localidades urbanas residía el 79.6% de la población del país, es decir, 8 de cada 10 personas
# habitaban en alguna de las 5,234 localidades urbanas; además, 39 de los 56 millones de personas en situación
# de pobreza7
# se ubicaron en estos espacios

localidades_urbanas=sf::read_sf("../../Reutilizables/Cartografia/conjunto_de_datos/13l.shp")
localidades_puntuales=sf::read_sf("../../Reutilizables/Cartografia/conjunto_de_datos/13lpr.shp")
municipios=sf::read_sf("../../Reutilizables/Cartografia/municipiosjair.shp")

colnames(pobreza_urbana)[c(2,5)]=c("Entidad federativa","Clave de Localidad")
localidades_urbanas_c_pobreza=localidades_urbanas |> merge(pobreza_urbana,
                                                           by.x="CVEGEO",by.y='Clave de Localidad',all.x=T)
localidades_urbanas_c_pobreza=localidades_urbanas_c_pobreza |> 
  dplyr::select(CVEGEO,Municipio,NOMGEO,AMBITO,Localidad:geometry)


source("../../ASUS Gamer Jair/codigos/puras_librerias.R")
##Propongo que se coloree con respecto al absoluto de la población en pobreza. 
## Estimamos la población absoluta en pobreza por localidad. 
mean(c(as.numeric(stri_extract_first(localidades_urbanas_c_pobreza$`Rango de pobreza (%)`[1],regex = "\\d+"))
,as.numeric(stri_extract_last(localidades_urbanas_c_pobreza$`Rango de pobreza (%)`[1],regex = "\\d+"))))



##Datos dummy para las localidades rurales de tipo poligono: 
sample(x = 1000:100000,size = (2505),replace = T)

localidades_urbanas_c_pobreza=localidades_urbanas_c_pobreza |> 
  dplyr::mutate(pob_abs=floor(`Población del ITER**`*
                                mean(c(as.numeric( stri_extract_first(`Rango de pobreza (%)`,regex = "\\d+"),
                                                   stri_extract_last(`Rango de pobreza (%)`,regex = "\\d+"))))/100))
localidades_urbanas_c_pobreza$pob_abs=sample(x = 1000:100000,size = (2505),replace = T)
colorear_rojos_factor=colorFactor(palette = c("yellow","yellow","orange","red","red"),domain = c("[0-20)", "[20,40)", "[40,60)", "[60,80)", "[80-100]"))     




colorear_rojos=colorNumeric(palette = c("yellow","red","red"),domain = sqrt(localidades_urbanas_c_pobreza$pob_abs))
table(pobreza_urbana |> dplyr::select(Municipio)) |> sort()
mapa_web=leaflet() |> 
  addTiles(options = leaflet::tileOptions(opacity =0.6))|>
  addPolygons(data=municipios |> as("Spatial"),
              label = municipios$NOM_MUN,fillColor = "gray",fillOpacity = 0.1,color = "white",weight = 3) |> 
  addPolygons(data=localidades_urbanas_c_pobreza |> st_transform(st_crs("EPSG:4326")),
              fillColor = colorear_rojos(sqrt(localidades_urbanas_c_pobreza$pob_abs)),color = "black",weight = 0.1,opacity = 1,fillOpacity = 1) |> 
  addLegend(title = "Porcentaje de Pobreza",,position = "bottomright",pal = colorear_rojos_factor,values =c("[0-20)", "[20,40)", "[40,60)", "[60,80)", "[80-100]"), opacity = 1)
mapa_web













saveWidget(mapa_web,file = "Entregables/Mapa web/pobreza_a_nivel_localidad.html",title = "Pobreza X Localidad")
