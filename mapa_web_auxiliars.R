colorear_rojos_factor=colorFactor(palette = c("yellow","#ffc200","#ff9300", "#ff5100","red"),domain = c("[0-20)", "[20,40)", "[40,60)", "[60,80)", "[80-100]"))     



colorear_rojos=colorNumeric(palette = c("yellow","#ffc200","#ff9300", "#ff5100","red"),domain = c(0,max(c((localidades_rurales_poligonos_c_pobreza$`Pobr%` |> as.numeric() |> max())
                                                                                                          ,(localidades_urbanas_c_pobreza$valor_pobreza |> max())))))
css_popup = {"<style>
    .leaflet-popup-content{
      margin:0px
    }
    .card {
            width: 100%; /* Ancho fijo para la card */
            background-color: #fff;
            border-radius: 8px;
            text-align: center;
            padding: 10px;
            box-sizing: border-box; /* Incluir padding en el ancho */
        }

        .card h2 {
            margin-top: 0;
            color: #333;
            font-size: 1.8em;
            margin-bottom: 5px;
        }

        .card h3 {
            color: #555;
            font-size: 1.2em;
            margin-top: 0;
            margin-bottom: 15px;
        }

        .coords {
            font-size: 1.1em;
            font-weight: bold;
            color: #777;
            margin-bottom: 0.5em;
        }

        .poverty-description {
            font-size: 0.9em;
            color: #666;
            line-height: 1.4;
            margin-bottom: 20px;
        }

        .poverty-number {
            font-size: 1.1em;
            font-weight: bold;
            color: #333;
            margin-bottom: 20px;
        }

        .divider {
            border-bottom: 1px solid #eee;
            margin: 20px 0;
        }

        .indicators-title {
            font-size: 1.3em;
            color: #333;
            margin-bottom: 15px;
        }

        .indicators-grid {
            display: grid;
            grid-template-columns: repeat(3, 1fr);
            gap: 15px;
            text-align: center;
        }

        .indicator-item {
            display: flex;
            flex-direction: column;
            align-items: center;
        }

        .indicator-item p {
            margin: 0;
            font-size: 0.9em;
            color: #444;
        }

        .indicator-item .percentage {
            font-weight: bold;
            font-size: 1.1em;
            color: #e44d26; /* Color para el porcentaje */
            margin-top: 5px;
        }

        .indicator-bar {
            width: 80%;
            height: 10px;
            background-color: #ccc; /* Placeholder para la barra */
            margin-top: 8px;
            border-radius: 5px;
        }

        .indicator-value {
            font-size: 0.85em;
            color: #777;
            margin-top: 5px;
        }
  </style>"}
generarPopup=function(ambito='urbana',df=localidades_urbanas_c_pobreza |> st_drop_geometry()
                      #nomgeo_loc,municipio, intervalo,pob_localidad,ind_mun_pob,ind_mun_pob_extr,ind_mun_pob_mod,pob_mun_tot
){
  pob_abs_pobreza=df$`Rango de pobreza (%)` |> lapply(\(x) {
    mean(c(as.numeric(stri_extract_first(x,regex = "\\d+"))
           ,as.numeric(stri_extract_last(x,regex = "\\d+"))))
  }) |> unlist()
  pob_abs_pobreza=pob_abs_pobreza*(df$`Población del ITER**`)/100
  ##HTML##
  ############NOMGEO############
  #######Municipio_nombre#######
  ### (A, B) ###
  #Porcentaje de población
  #en pobreza en la 
  #localidad urbana
  #*Aproximadamente* valor_num personas en condición de pobreza
  #----------------------------#
  ##Indicadores municipales##
  #Pobreza#  #Pobreza \n Moderada# #Pobreza \n Extrema#
  # p_1 %          # p_1 %              # p_1 %
  #\bar            #\bar                #\bar
  # val_1          #val_2               #val_3
  
  return(paste0(css_popup,
                
                '<div class="card">',
                '
        <h2 id="nomgeo-val">',df$NOMGEO,'</h2>',
                '<h3 id="municipio-nombre-val" style="margin-bottom:0.5em">',df$Municipio,'</h3>',
                '<div class="divider" style="margin:0.5em"></div><h3 class="indicators-title">Indicadores a nivel localidad</h3>',
                '<p class="poverty-description" style="margin-top:1em">',
                ifelse(df$`Rango de pobreza (%)`=="[ 0, 20)",'Menos del 20 porciento de la población de la localidad urbana se encuentra en pobreza',
                       ifelse(df$`Rango de pobreza (%)`=="[20, 40)",'Entre 20 y 40 porciento de la población de la localidad urbana se encuentra en pobreza',
                              ifelse(df$`Rango de pobreza (%)`=="[40, 60)","Entre 40 y 60 porciento de la población de la localidad urbana se encuentra en pobreza",
                                     ifelse(df$`Rango de pobreza (%)`=="[60, 80)","Entre 60 y 80 porciento de la población de la localidad urbana se encuentra en pobreza",
                                            "Más del 80 porciento de la población de la localidad urbana se encuentra en pobreza" ))
                              )) ,
                '</p>'
        ,
                '<p class="poverty-number">',
                'Aproximadamente <span id="valor-num-val">',formatC(big.mark = ",",x =  round(pob_abs_pobreza,0),format = "d"),'</span> personas en condición de pobreza
        </p>','
        <div class="divider"></div>

        <h3 class="indicators-title">Indicadores municipales</h3>

        <div class="indicators-grid">
            <div class="indicator-item">
                <p style=\'line-height: 3\'>Pobreza</p>
                <p class="percentage" id="p1-val">',paste0(round(as.numeric(df$`Pobr%`),1),"%"),'</p>
                <div class="indicator-bar" style="background-color:',colorear_rojos(round(as.numeric(df$`Pobr%`),1)),'"></div>',
                #'<p class="indicator-value" id="val2-val">',df$pobr_pob,'</p>',
                '</div>
            <div class="indicator-item">
                <p>Pobreza<br>Moderada</p>
                <p class="percentage" id="p2-val">',paste0(round(as.numeric(df$`Pobr_mode%`),1),"%"),'</p>
                <div class="indicator-bar" style="background-color:',colorear_rojos(round(as.numeric(df$`Pobr_mode%`),1)),'"></div>',
                #'<p class="indicator-value" id="val2-val">',df$pobr_mode_pob,'</p>',
                '</div>
            <div class="indicator-item">
                <p>Pobreza<br>Extrema</p>
                <p class="percentage" id="p3-val">',paste0(round(as.numeric(df$`Pobr_ext%`),1),"%"),'</p>
                <div class="indicator-bar" style="background-color:',colorear_rojos(round(as.numeric(df$`Pobr_ext%`),1)),'"></div>',
                #'<p class="indicator-value" id="val3-val">',df$pobr_extr_pob,'</p>',
                '</div>
        </div>
    </div>'))
}
localidades_rurales_poligonos_c_pobreza=localidades_rurales_poligonos_c_pobreza |> 
  dplyr::mutate(
    pobr_pob=POBTOT*as.numeric(`Pobr%`),
    pobr_mode_pob=POBTOT*as.numeric(`Pobr_mode%`),
    pobr_extr_pob=POBTOT*as.numeric(`Pobr_ext%`),
                )
generarPopupRural=function(ambito='urbana',df=localidades_rurales_poligonos_c_pobreza |> st_drop_geometry()
                      #nomgeo_loc,municipio, intervalo,pob_localidad,ind_mun_pob,ind_mun_pob_extr,ind_mun_pob_mod,pob_mun_tot
){
  df=df |> 
    dplyr::mutate(
      pobr_pob=round(POBTOT*as.numeric(`Pobr%`)/100,0),
      pobr_mode_pob=round(POBTOT*as.numeric(`Pobr_mode%`)/100,0),
      pobr_extr_pob=round(POBTOT*as.numeric(`Pobr_ext%`)/100,0),
    )
  ##HTML##
  ############NOMGEO############
  #######Municipio_nombre#######
  ### (A, B) ###
  #Porcentaje de población
  #en pobreza en la 
  #localidad urbana
  #*Aproximadamente* valor_num personas en condición de pobreza
  #----------------------------#
  ##Indicadores municipales##
  #Pobreza#  #Pobreza \n Moderada# #Pobreza \n Extrema#
  # p_1 %          # p_1 %              # p_1 %
  #\bar            #\bar                #\bar
  # val_1          #val_2               #val_3
  
  return(paste0(css_popup,
                
                '<div class="card">',
                '
        <h2 id="nomgeo-val">',df$NOMGEO,'</h2>',
                '<h3 id="municipio-nombre-val">',df$NOM_MUN,'</h3>',
                '<div class="divider"></div>',
                '<h3 class="indicators-title">De acuerdo a indicadores municipales</h3>',
        '<div class="indicators-grid">
            <div class="indicator-item">
                <p style=\'line-height: 3\'>Pobreza</p>
                <p class="percentage" id="p1-val">',paste0(round(as.numeric(df$`Pobr%`),1),"%"),'*</p>
                <div class="indicator-bar" style="background-color:',colorear_rojos(round(as.numeric(df$`Pobr%`),1)),'"></div>',
        '<p class="indicator-value" id="val2-val">',formatC(big.mark = ",",x = df$pobr_pob,format = "d"),'<br>personas*','</p>',
                '</div>
            <div class="indicator-item">
                <p>Pobreza<br>Moderada</p>
                <p class="percentage" id="p2-val">',paste0(round(as.numeric(df$`Pobr_mode%`),1),"%"),'*</p>
                <div class="indicator-bar" style="background-color:',colorear_rojos(round(as.numeric(df$`Pobr_mode%`),1)),'"></div>',
        '<p class="indicator-value" id="val2-val">',formatC(big.mark = ",",x = df$pobr_mode_pob,format = "d"),'<br>personas*','</p>',
                '</div>
            <div class="indicator-item">
                <p>Pobreza<br>Extrema</p>
                <p class="percentage" id="p3-val">',paste0(round(as.numeric(df$`Pobr_ext%`),1),"%"),'*</p>
                <div class="indicator-bar" style="background-color:',colorear_rojos(round(as.numeric(df$`Pobr_ext%`),1)),'"></div>',
        '<p class="indicator-value" id="val3-val">',formatC(big.mark = ",",x = df$pobr_extr_pob,format = "d"),'<br>personas*','</p>',
                '</div>','
        </div>',
        
        '<div style="text-align: left; position: relative;margin-top:2em;line-height:0.5; bottom: 0; left: 0;">
  <p style="color:#e44d26; margin:0px;line-height:1">*Porcentaje del municipio</p>
  <br>
  <p style="margin:0px;line-height:1">*Población en la localidad calculado con el porcentaje del municipio</p>
</div>',
        '
    </div>'))
}
generarPopupMunicipal=function(ambito='urbana',df=municipios |> st_drop_geometry()
                      #nomgeo_loc,municipio, intervalo,pob_localidad,ind_mun_pob,ind_mun_pob_extr,ind_mun_pob_mod,pob_mun_tot
){
  df=df |> 
    dplyr::mutate(
      pobr_pob=round(as.numeric(Pob_tot_2020) *as.numeric(`Pobr%`)/100,0),
      pobr_mode_pob=round(as.numeric(Pob_tot_2020) *as.numeric(`Pobr_mode%`)/100,0),
      pobr_extr_pob=round(as.numeric(Pob_tot_2020) *as.numeric(`Pobr_ext%`)/100,0),
    )
  ##HTML##
  ############NOMGEO############
  #######Municipio_nombre#######
  ### (A, B) ###
  #Porcentaje de población
  #en pobreza en la 
  #localidad urbana
  #*Aproximadamente* valor_num personas en condición de pobreza
  #----------------------------#
  ##Indicadores municipales##
  #Pobreza#  #Pobreza \n Moderada# #Pobreza \n Extrema#
  # p_1 %          # p_1 %              # p_1 %
  #\bar            #\bar                #\bar
  # val_1          #val_2               #val_3
  
  return(paste0(css_popup,'<div class="card" style="background-color:rgba(128,128,128,0.1)">',
                '<h2 id="nomgeo-val">',df$NOM_MUN,'</h2>',
                '<h3 class="indicators-title">Indicadores municipales</h3>',
                '<div class="indicators-grid">
            <div class="indicator-item">
                <p style=\'line-height: 3\'>Pobreza</p>
                <p class="percentage" id="p1-val">',paste0(round(as.numeric(df$`Pobr%`),1),"%"),'</p>
                <div class="indicator-bar" style="background-color:',colorear_rojos(round(as.numeric(df$`Pobr%`),1)),'"></div>',
                '<p class="indicator-value" id="val2-val">',formatC(big.mark = ",",x = df$pobr_pob,format = "d"),'<br>personas','</p>',
                '</div>
            <div class="indicator-item">
                <p>Pobreza<br>Moderada</p>
                <p class="percentage" id="p2-val">',paste0(round(as.numeric(df$`Pobr_mode%`),1),"%"),'</p>
                <div class="indicator-bar" style="background-color:',colorear_rojos(round(as.numeric(df$`Pobr_mode%`),1)),'"></div>',
                '<p class="indicator-value" id="val2-val">',formatC(big.mark = ",",x = df$pobr_mode_pob,format = "d"),'<br>personas','</p>',
                '</div>
            <div class="indicator-item">
                <p>Pobreza<br>Extrema</p>
                <p class="percentage" id="p3-val">',paste0(round(as.numeric(df$`Pobr_ext%`),1),"%"),'</p>
                <div class="indicator-bar" style="background-color:',colorear_rojos(round(as.numeric(df$`Pobr_ext%`),1)),'"></div>',
                '<p class="indicator-value" id="val3-val">',formatC(big.mark = ",",x = df$pobr_extr_pob,format = "d"),'<br>personas','</p>',
                '</div>
        </div>
    </div>'))
}


colorearBarritas=function(vector){
  return(quantile(x = vector,c(0.25,0.75)))
}
colorearBarritas(localidades_urbanas_c_pobreza$valor_pobreza)[1]
colorearBarritas(localidades_urbanas_c_pobreza$valor_pobreza)[2]
hist(localidades_urbanas_c_pobreza$valor_pobreza,breaks=c(1:20)*5)
