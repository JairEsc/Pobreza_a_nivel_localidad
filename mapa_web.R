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
lim_mun=read_sf("../../Reutilizables/Cartografia/conjunto_de_datos/13ent.shp")
municipios=merge(municipios,pobreza_concentrado,by.x='CVEGEO',by.y='Clave de municipio',all.x=T)
colnames(pobreza_urbana)[c(2,5)]=c("Entidad federativa","Clave de Localidad")
localidades_urbanas_c_pobreza=localidades_urbanas |> merge(pobreza_urbana,
                                                           by.x="CVEGEO",by.y='Clave de Localidad',all.x=T)
localidades_urbanas_c_pobreza=localidades_urbanas_c_pobreza |> 
  dplyr::select(CVEGEO,Municipio,NOMGEO,AMBITO,Localidad:geometry)
localidades_urbanas_c_pobreza$valor_pobreza=10+localidades_urbanas_c_pobreza$valor_pobreza

source("../../ASUS Gamer Jair/codigos/puras_librerias.R")
##Propongo que se coloree con respecto al absoluto de la población en pobreza. 
## Estimamos la población absoluta en pobreza por localidad. 
mean(c(as.numeric(stri_extract_first(localidades_urbanas_c_pobreza$`Rango de pobreza (%)`[1],regex = "\\d+"))
,as.numeric(stri_extract_last(localidades_urbanas_c_pobreza$`Rango de pobreza (%)`[1],regex = "\\d+"))))




localidades_urbanas_c_pobreza=localidades_urbanas_c_pobreza |> 
  dplyr::mutate(pob_abs=floor(`Población del ITER**`*
                                mean(c(as.numeric( stri_extract_first(`Rango de pobreza (%)`,regex = "\\d+"),
                                                   stri_extract_last(`Rango de pobreza (%)`,regex = "\\d+"))))/100))
localidades_urbanas_c_pobreza$pob_abs=sample(x = 1000:100000,size = (2505),replace = T)
municipios  |> st_union()|> st_cast("MULTIPOLYGON") |> st_cast("POLYGON")|> plot()
#table(pobreza_urbana |> dplyr::select(Municipio)) |> sort()
mapa_web=leaflet()  |> 
  addTiles(options = leaflet::tileOptions(opacity =0.6))|>
  setView(lng =-98.88704 ,lat =20.47901,zoom=9) |> 
  addPolygons(data=municipios |> as("Spatial"),
              label = municipios$NOM_MUN,fillColor = "gray",fillOpacity = 0.1,color = "white",weight = 3,group = "Municipios",
              popup = generarPopupMunicipal()) |> 
  addPolygons(data=localidades_urbanas_c_pobreza |> st_transform(st_crs("EPSG:4326")),
              fillColor = colorear_rojos((localidades_urbanas_c_pobreza$valor_pobreza)),color = "black",weight = 0.1,opacity = 1,fillOpacity = 1,
              label = paste0(localidades_urbanas_c_pobreza$NOMGEO,"-",localidades_urbanas_c_pobreza$Municipio),
              
              popup = generarPopup()
                # paste0("Municipio: ",localidades_urbanas_c_pobreza$Municipio," <br>",
                #              "Localidad: ",localidades_urbanas_c_pobreza$NOMGEO,"<br>",
                #              "pobtot: ",localidades_urbanas_c_pobreza$`Población del ITER**`,"<br>",
                #              "intervalo_pobreza: ",localidades_urbanas_c_pobreza$`Rango de pobreza (%)`,"<br>",
                #              "porcentaje municipal: ",localidades_urbanas_c_pobreza$`Pobr%`,"---",localidades_urbanas_c_pobreza$`Pobr_mode%`,"---",localidades_urbanas_c_pobreza$`Pobr_ext%`,"<br>"
                #              )
              ,group = "Localidades urbanas"
              ) |> 
  addPolygons(data=localidades_rurales_poligonos_c_pobreza |> st_transform(st_crs("EPSG:4326")),
              fillColor = colorear_rojos(as.numeric((localidades_rurales_poligonos_c_pobreza$`Pobr%`))),color = "black",weight = 0.1,opacity = 1,fillOpacity = 1,
              label = paste0(localidades_rurales_poligonos_c_pobreza$NOMGEO,"-",localidades_rurales_poligonos_c_pobreza$NOM_MUN),
              popup = generarPopupRural()
              #   paste0("Municipio: ",localidades_rurales_poligonos_c_pobreza$NOM_MUN," <br>",
              #                "Localidad: ",localidades_rurales_poligonos_c_pobreza$NOMGEO,"<br>",
              #                "pobtot: ",localidades_rurales_poligonos_c_pobreza$POBTOT,"<br>",
              #                "porcentaje municipal: ",localidades_urbanas_c_pobreza$`Pobr%`,"---",localidades_urbanas_c_pobreza$`Pobr_mode%`,"---",localidades_urbanas_c_pobreza$`Pobr_ext%`,"<br>",
              #                "absoluto municipal: ",as.numeric(localidades_rurales_poligonos_c_pobreza$POBTOT)*as.numeric(localidades_rurales_poligonos_c_pobreza$`Pobr%`)/100,"<br>",
              #                "absoluto municipal moderada: ",as.numeric(localidades_rurales_poligonos_c_pobreza$POBTOT)*as.numeric(localidades_rurales_poligonos_c_pobreza$`Pobr_mode%`)/100,"<br>",
              #                "absoluto municipal extrema: ",as.numeric(localidades_rurales_poligonos_c_pobreza$POBTOT)*as.numeric(localidades_rurales_poligonos_c_pobreza$`Pobr_ext%`)/100,"<br>"
              # )
              ,group = "Localidades Rurales"
              ) |> 
  addCircleMarkers(data=localidades_puntuales_c_demo |> st_transform(st_crs("EPSG:4326")) ,radius = 1,
              fillColor = colorear_rojos(as.numeric((localidades_puntuales_c_demo$`Pobr%`))),color = "black",weight = 0.1,opacity = 1,fillOpacity = 1,
              
              popup = generarPopupRural(df = localidades_puntuales_c_demo |> dplyr::rename('POBTOT'=POB1))
              #   paste0("Municipio: ",localidades_puntuales_c_demo$NOM_MUN," <br>",
              #                "Localidad: ",localidades_puntuales_c_demo$NOMGEO,"<br>",
              #                "pobtot: ",localidades_puntuales_c_demo$POB1,"<br>",
              #                "porcentaje municipal: ",localidades_puntuales_c_demo$`Pobr%`,"---",localidades_puntuales_c_demo$`Pobr_mode%`,"---",localidades_puntuales_c_demo$`Pobr_ext%`,"<br>",
              #                "absoluto municipal: ",as.numeric(localidades_puntuales_c_demo$POB1)*as.numeric(localidades_puntuales_c_demo$`Pobr%`)/100,"<br>",
              #                "absoluto municipal moderada: ",as.numeric(localidades_puntuales_c_demo$POB1)*as.numeric(localidades_puntuales_c_demo$`Pobr_mode%`)/100,"<br>",
              #                "absoluto municipal extrema: ",as.numeric(localidades_puntuales_c_demo$POB1)*as.numeric(localidades_puntuales_c_demo$`Pobr_ext%`)/100,"<br>"
              # )
              ,group = "Localidades Rurales punto"
              ) |> 
  addLayersControl(overlayGroups = c("Localidades urbanas","Localidades Rurales", "Localidades Rurales punto"),options = layersControlOptions(collapsed = F)) |> 
  addLegend(title = "Porcentaje de Pobreza",,position = "bottomright",pal = colorear_rojos_factor,values =c("[0-20)", "[20,40)", "[40,60)", "[60,80)", "[80-100]"), opacity = 1) |> 
  addSearchFeatures(targetGroups = c("Localidades urbanas","Localidades Rurales"),
                    options = searchFeaturesOptions(
                      zoom = 12,
                      openPopup = F,
                      firstTipSubmit =F,initial = F,
                      hideMarkerOnCollapse =T)) |> 
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
      style = "display:none; position:fixed; top:20%; left:20%; width:60%; background:white; padding:20px; border:2px solid black; z-index:1000; overflow-y: auto;", # Added overflow-y: auto for scroll if content is long

      tags$h4("Metodología"),
      tags$p(
        "La elaboración de este mapa se basó en dos fuentes principales de datos y abordó la estimación de la pobreza a dos niveles geográficos distintos:"
      ),
      tags$ul(
        tags$li(
          tags$strong("Localidades Urbanas (268 polígonos): "),
          "Por primera vez,",tags$a(href = "https://www.coneval.org.mx/Medicion/Paginas/pobreza_localidad_urbana.aspx", "CONEVAL") ," publicó datos de pobreza a nivel de localidad urbana en 2020. Para estas localidades, filtramos la información para el estado de Hidalgo. Dado que CONEVAL presenta la información como un intervalo del porcentaje de pobreza, tomamos el punto medio de dicho intervalo. Combinamos este valor con los datos de población a nivel de localidad del",tags$a(href = "https://www.inegi.org.mx/programas/ccpv/2020/#tabulados", "INEGI 2020") ," para estimar la ",
          tags$strong("población en pobreza"),
          " en cada una de estas 268 localidades urbanas."
        ),
        tags$li(
          tags$strong("Otras Localidades (rurales y urbanas no cubiertas por CONEVAL a nivel localidad): "),
          "Para el resto de las más de 5,000 localidades (entre urbanas y rurales) de Hidalgo que están georreferenciadas, se utilizó el ",
          tags$strong("porcentaje de pobreza del municipio"),
          " al que pertenecen. Con los datos de población de estas localidades reportados por INEGI, se estimó la población en pobreza utilizando los indicadores de pobreza a nivel municipal (pobreza, pobreza moderada y pobreza extrema)."
        )
      ),
      tags$p(
        "Adicionalmente, el mapa también presenta los datos de ",
        tags$strong("población en pobreza"),
        ", ",
        tags$strong("pobreza extrema"),
        " y ",
        tags$strong("pobreza moderada"),
        ", así como sus respectivos porcentajes, a ",
        tags$strong("nivel municipal"),
        "."
      ),
      
      tags$button("Cerrar", onclick = "document.getElementById('infoModal').style.display='none'")
    )
  ) |> 
  onRender("function() {
    var map = this; 
    function resizeMarkers() {
      var zoom = map.getZoom();
      if(zoom>=10){
        map.eachLayer(function(layer) {
          if (layer.options) {
            var newSize;
            if (layer.options.group === 'Localidades Rurales punto' ) {
              newSize = zoom-10+2
              layer.setRadius(newSize);
            }
          }
        });
      }
      else{
      map.eachLayer(function(layer) {
          if (layer.options) {
            var newSize;
            if (layer.options.group === 'Localidades Rurales punto' ) {
              newSize = 1
              layer.setRadius(newSize);
            }
          }
        });
      }
    }

    // Llamar a resizeMarkers en eventos de zoom y al cargar el mapa
    map.on('zoomend', resizeMarkers);
    map.whenReady(resizeMarkers);
    }")|> addLogo(img = "https://raw.githubusercontent.com/JairEsc/Gob/main/Otros_archivos/imagenes/Planeacion_sigeh.png",src = "remote",width = "400px",height='71px',position = "bottomleft") 
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

