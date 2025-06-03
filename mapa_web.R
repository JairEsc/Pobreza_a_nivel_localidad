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



colorear_rojos=colorNumeric(palette = c("yellow","#ffc200","#ff9300", "#ff5100","red"),domain = c(0,max(c((localidades_rurales_poligonos_c_pobreza$`Pobr%` |> as.numeric() |> max())
                                                                                                          ,(localidades_urbanas_c_pobreza$valor_pobreza |> max())))))
#table(pobreza_urbana |> dplyr::select(Municipio)) |> sort()
mapa_web=leaflet() |> 
  addTiles(options = leaflet::tileOptions(opacity =0.6))|>
  addPolygons(data=municipios |> as("Spatial"),
              label = municipios$NOM_MUN,fillColor = "gray",fillOpacity = 0.1,color = "white",weight = 3,group = "Municipios") |> 
  addPolygons(data=localidades_urbanas_c_pobreza |> st_transform(st_crs("EPSG:4326")),
              fillColor = colorear_rojos((localidades_urbanas_c_pobreza$valor_pobreza)),color = "black",weight = 0.1,opacity = 1,fillOpacity = 1,
              
              popup = paste0("Municipio: ",localidades_urbanas_c_pobreza$Municipio," <br>",
                             "Localidad: ",localidades_urbanas_c_pobreza$NOMGEO,"<br>",
                             "pobtot: ",localidades_urbanas_c_pobreza$`Población del ITER**`,"<br>",
                             "intervalo_pobreza: ",localidades_urbanas_c_pobreza$`Rango de pobreza (%)`,"<br>",
                             "porcentaje municipal: ",localidades_urbanas_c_pobreza$`Pobr%`,"---",localidades_urbanas_c_pobreza$`Pobr_mode%`,"---",localidades_urbanas_c_pobreza$`Pobr_ext%`,"<br>"
                             )
              ,group = "Localidades urbanas"
              ) |> 
  addPolygons(data=localidades_rurales_poligonos_c_pobreza |> st_transform(st_crs("EPSG:4326")),
              fillColor = colorear_rojos(as.numeric((localidades_rurales_poligonos_c_pobreza$`Pobr%`))),color = "black",weight = 0.1,opacity = 1,fillOpacity = 1,
              
              popup = paste0("Municipio: ",localidades_rurales_poligonos_c_pobreza$NOM_MUN," <br>",
                             "Localidad: ",localidades_rurales_poligonos_c_pobreza$NOMGEO,"<br>",
                             "pobtot: ",localidades_rurales_poligonos_c_pobreza$POBTOT,"<br>",
                             "porcentaje municipal: ",localidades_urbanas_c_pobreza$`Pobr%`,"---",localidades_urbanas_c_pobreza$`Pobr_mode%`,"---",localidades_urbanas_c_pobreza$`Pobr_ext%`,"<br>",
                             "absoluto municipal: ",as.numeric(localidades_rurales_poligonos_c_pobreza$POBTOT)*as.numeric(localidades_rurales_poligonos_c_pobreza$`Pobr%`)/100,"<br>",
                             "absoluto municipal moderada: ",as.numeric(localidades_rurales_poligonos_c_pobreza$POBTOT)*as.numeric(localidades_rurales_poligonos_c_pobreza$`Pobr_mode%`)/100,"<br>",
                             "absoluto municipal extrema: ",as.numeric(localidades_rurales_poligonos_c_pobreza$POBTOT)*as.numeric(localidades_rurales_poligonos_c_pobreza$`Pobr_ext%`)/100,"<br>"
              )
              ,group = "Localidades Rurales"
              ) |> 
  addCircleMarkers(data=localidades_puntuales_c_demo |> st_transform(st_crs("EPSG:4326")) ,radius = 1,
              fillColor = colorear_rojos(as.numeric((localidades_puntuales_c_demo$`Pobr%`))),color = "black",weight = 0.1,opacity = 1,fillOpacity = 1,
              
              popup = paste0("Municipio: ",localidades_puntuales_c_demo$NOM_MUN," <br>",
                             "Localidad: ",localidades_puntuales_c_demo$NOMGEO,"<br>",
                             "pobtot: ",localidades_puntuales_c_demo$POB1,"<br>",
                             "porcentaje municipal: ",localidades_puntuales_c_demo$`Pobr%`,"---",localidades_puntuales_c_demo$`Pobr_mode%`,"---",localidades_puntuales_c_demo$`Pobr_ext%`,"<br>",
                             "absoluto municipal: ",as.numeric(localidades_puntuales_c_demo$POB1)*as.numeric(localidades_puntuales_c_demo$`Pobr%`)/100,"<br>",
                             "absoluto municipal moderada: ",as.numeric(localidades_puntuales_c_demo$POB1)*as.numeric(localidades_puntuales_c_demo$`Pobr_mode%`)/100,"<br>",
                             "absoluto municipal extrema: ",as.numeric(localidades_puntuales_c_demo$POB1)*as.numeric(localidades_puntuales_c_demo$`Pobr_ext%`)/100,"<br>"
              )
              ,group = "Localidades Rurales punto"
              ) |> 
  addLayersControl(overlayGroups = c("Localidades urbanas","Localidades Rurales", "Localidades Rurales punto"),options = layersControlOptions(collapsed = F)) |> 
  addLegend(title = "Porcentaje de Pobreza",,position = "bottomright",pal = colorear_rojos_factor,values =c("[0-20)", "[20,40)", "[40,60)", "[60,80)", "[80-100]"), opacity = 1) |> 
  addSearchFeatures(targetGroups = "Municipios",
                    options = searchFeaturesOptions(
                      zoom = 12, 
                      openPopup = F,
                      firstTipSubmit =F,initial = F,
                      hideMarkerOnCollapse =T))|>setView(lng = -98.7591, lat = 20.0511, zoom = 9) |> 
  addEasyButton(
    easyButton(
      icon = "fa-info-circle",
      title = "Información",
      onClick = JS("function(btn, map){ 
        var modal = document.getElementById('infoModal');
        if (modal) modal.style.display = 'block';
      }")
    )
  ) |> 
  prependContent(
    tags$div(
      id = "infoModal",
      class = "modal",
      style = "display:none; position:fixed; top:20%; left:20%; width:60%; background:white; padding:20px; border:2px solid black; z-index:1000;",
      tags$h3("Información del Mapa"),
      tags$p("Se utiliza la base de pobreza multidimensional de INEGI 2020 para nivel de municipio y la base de pobreza a nivel de localidad urbana"),
      tags$button("Cerrar", onclick = "document.getElementById('infoModal').style.display='none'")
    )
  )
mapa_web





###Minimal_Example_modal
# leaflet() |> 
#   addTiles() |> 
#   addEasyButton(
#     easyButton(
#       icon = "fa-info-circle",
#       title = "Información",
#       onClick = JS("function(btn, map){ 
#         var modal = document.getElementById('infoModal');
#         if (modal) modal.style.display = 'block';
#       }")
#     )
#   ) |> 
#   prependContent(
#     tags$div(
#       id = "infoModal",
#       class = "modal",
#       style = "display:none; position:fixed; top:20%; left:20%; width:60%; background:white; padding:20px; border:2px solid black; z-index:1000;",
#       tags$h3("Información del Mapa"),
#       tags$p("blaaa blaaa blaaa"),
#       tags$button("Cerrar", onclick = "document.getElementById('infoModal').style.display='none'")
#     )
#   )




saveWidget(mapa_web,file = "Entregables/Mapa web/pobreza_a_nivel_localidad.html",title = "Pobreza X Localidad")
