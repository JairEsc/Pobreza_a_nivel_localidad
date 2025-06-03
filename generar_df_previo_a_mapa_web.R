#pobreza_municipal_2020

pobreza_concentrado=read_excel("Datos/Concentrado_indicadores_de_pobreza_2020.xlsx",skip = 3)
pobreza_concentrado=pobreza_concentrado |> 
  dplyr::filter(`Clave de entidad`=='13' &!is.na(`Clave de entidad`))
colnames(pobreza_concentrado)
pobreza_concentrado=pobreza_concentrado |> 
  dplyr::select(`Clave de municipio`,`Clave de municipio`,`Población 2020*\r\n(leer nota al final del cuadro)`,...10,...13,...19,...22,...28,...31) 
colnames(pobreza_concentrado)[2:8]=c("Pob_tot_2020"
                                     ,"Pobr%","pobr_pob",
                                     "Pobr_ext%","pobr_extr_pob",
                                     "Pobr_mode%","pobr_mode_pob")
pobreza_concentrado
####Dividimos los poligonos en rurales y urbanos 
localidades_urbanas_c_pobreza=localidades_urbanas |> merge(pobreza_urbana,
                                                           by.x="CVEGEO",by.y='Clave de Localidad',all.y=T)
localidades_urbanas_c_pobreza=localidades_urbanas_c_pobreza |> 
  dplyr::select(CVEGEO,Municipio,NOMGEO,AMBITO,Localidad:geometry)
localidades_urbanas_c_pobreza=localidades_urbanas_c_pobreza |> 
  rowwise() |> 
  dplyr::mutate(valor_pobreza=mean(c(as.numeric( stri_extract_first(`Rango de pobreza (%)`,regex = "\\d+"),
                                             stri_extract_last(`Rango de pobreza (%)`,regex = "\\d+")))))

localidades_urbanas_c_pobreza=merge(localidades_urbanas_c_pobreza |> dplyr::mutate(CVEGEO=substr(CVEGEO,1,5)),
                                    pobreza_concentrado |> dplyr::select(`Clave de municipio`,Pob_tot_2020:pobr_mode_pob),
                                    by.x='CVEGEO',by.y='Clave de municipio',all.x=T)










localidades_rurales=localidades_urbanas[!localidades_urbanas$CVEGEO %in%
                                          localidades_urbanas_c_pobreza$CVEGEO,]

#Le agregamos a las localidades tipo poligono el porcentaje medio o heredado del municipio
localidades_rurales_poligonos_c_pobreza=localidades_rurales|> dplyr::mutate(CVEGEO2=substr(CVEGEO,1,5)) |> merge(pobreza_concentrado,
                                                           by.x="CVEGEO2",by.y='Clave de municipio',all.x=T)
localidades_rurales_poligonos_c_pobreza=merge(localidades_rurales_poligonos_c_pobreza,municipios |> dplyr::select(CVE_MUN,NOM_MUN) |> st_drop_geometry(),by='CVE_MUN',all.x=T)
localidades_rurales_poligonos_c_pobreza=localidades_rurales_poligonos_c_pobreza |> 
  dplyr::select(CVEGEO,CVEGEO2,NOM_MUN,NOMGEO,AMBITO,Pob_tot_2020:pobr_mode_pob)

demografia_localidad=read.csv("../../Reutilizables/Demograficos/iter_13_cpv2020/conjunto_de_datos/conjunto_de_datos_iter_13CSV20.csv")
demografia_localidad=demografia_localidad |> 
  dplyr::mutate(CVEGEO=paste0("13",sprintf("%03d",MUN),sprintf("%04d",LOC))) |> 
  dplyr::select(CVEGEO,POBTOT)
localidades_rurales_poligonos_c_pobreza=merge(localidades_rurales_poligonos_c_pobreza,
                                              demografia_localidad,by='CVEGEO',all.x=T
                                              )
localidades_rurales_poligonos_c_pobreza$POBTOT[localidades_rurales_poligonos_c_pobreza$POBTOT |> is.na()]=c(10,36,46,10,55,7,7,10)




##############
#Y ya final finalisimo ponemos los puntuales con los heredados 
localidades_puntuales_c_demo=read_sf("../../Reutilizables/Demograficos/scince/loc_rur.shp")
localidades_puntuales_c_demo=localidades_puntuales_c_demo |> 
  dplyr::select(CVEGEO:NOMGEO,POB1)
localidades_puntuales_c_demo=merge(localidades_puntuales_c_demo |> dplyr::mutate(CVEGEO2=substr(CVEGEO,1,5)),
                                   pobreza_concentrado,by.x='CVEGEO2',by.y='Clave de municipio')
