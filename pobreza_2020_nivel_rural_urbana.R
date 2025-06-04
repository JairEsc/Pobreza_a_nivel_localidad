####Resumen
##Notamos que no hay información de todos los municipios. 

##Pendiente decidir si se incluye como dato. 


pobreza_mun_2020=read.csv("Datos/R_MMP_2020/Base final/pobreza_20.csv")
pobreza_mun_2020=pobreza_mun_2020 |> 
  dplyr::select(ubica_geo,rururb,tam_loc,pobreza:pobreza_m,factor) |> 
  dplyr::filter(substr(ubica_geo,1,2)=='13')

###
##Supongamos filtro 
###pobreza
pobreza_mun_2020$factor[pobreza_mun_2020$pobreza==1] |> sum()

pobreza_mun_2020_=pobreza_mun_2020 |> dplyr::filter(pobreza==1)|> 
  dplyr::mutate(CVE_MUN=substr(ubica_geo,3,5)) |> 
  dplyr::group_by(CVE_MUN,rururb) |> 
  dplyr::summarise(pob_pobreza=sum(factor,na.rm = T))
pobreza_mun_2020_e=pobreza_mun_2020 |> dplyr::filter(pobreza_e==1) |> 
  dplyr::mutate(CVE_MUN=substr(ubica_geo,3,5)) |> 
  dplyr::group_by(CVE_MUN,rururb) |> 
  dplyr::summarise(pob_pobreza_extr=sum(factor,na.rm = T))
pobreza_mun_2020_m=pobreza_mun_2020 |> dplyr::filter(pobreza_m==1) |> 
  dplyr::mutate(CVE_MUN=substr(ubica_geo,3,5)) |> 
  dplyr::group_by(CVE_MUN,rururb) |> 
  dplyr::summarise(pob_pobreza_mode=sum(factor,na.rm = T))

pobreza_mun_2020__=merge(pobreza_mun_2020_ |> tidyr::pivot_wider(names_from = rururb,names_prefix = "rururb_p",values_from =  pob_pobreza),
                         merge(pobreza_mun_2020_e|> tidyr::pivot_wider(names_from = rururb,names_prefix = "rururb_p_e",values_from =  pob_pobreza_extr),
                               pobreza_mun_2020_m|> tidyr::pivot_wider(names_from = rururb,names_prefix = "rururb_p_m",values_from =  pob_pobreza_mode),by='CVE_MUN',all=T)
                         ,by='CVE_MUN',all=T)

pobreza_mun_2020__=pobreza_mun_2020__ |> dplyr::rename(
  "pob_rur_pob"=rururb_p1,
  "pob_urb_pob"=rururb_p0,
  'pob_rur_pob_extr'=rururb_p_e1,
  'pob_urb_pob_extr'=rururb_p_e0,
  'pob_rur_pob_mode'=rururb_p_m1,
  'pob_urb_pob_mode'=rururb_p_m0
)

