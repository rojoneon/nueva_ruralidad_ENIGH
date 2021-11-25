######################################################################
# Acceso nominal a salud
# 
#Elaborado por: Máximo Ernesto Jaramillo-Molina 
#               Twitter: @rojo_neon
#               Github: @rojoneon
######################################################################



##############
#Configuración----
rm(list = ls())

library(pacman)
# Abrimos las paqueterías con un sólo comando:
p_load(haven, tidyverse, knitr, foreign,data.table, kableExtra, srvyr)


###Carpeta de trabajo y carga de datos
#setwd ("D:/Encuestas/ENIGH Vieja Construccion")
setwd ("/Users/macsernst/Documents/Encuestas/ENIGH Vieja Construccion")

# Prestaciones laborales
trab <- read_dta("2020/bases/trabajos.dta") %>% rename_all(tolower)

# Tipo de trabajador: identifica la población subordinada e independiente
trab<- mutate(trab, 
              tipo_trab=case_when(
                # Subordinados   
                subor==1 ~ 1,
                # Independientes que reciben un pago
                subor==2 & indep==1 & tiene_suel==1 ~ 2,
                subor==2 & indep==2 & pago==1 ~ 2,
                # Independientes que no reciben un pago
                subor==2 & indep==1 & tiene_suel==2 ~ 3,
                subor==2 & indep==2 & (pago==2 | pago==3) ~ 3) ,
              # Ahorro para el retiro o pensión para la vejez (SAR, Afore)
              aforlab=case_when(is.na(pres_8) ~ 0,
                                pres_8==8 ~ 1),
              # Ocupación principal o secundaria
              id_trabajo= as.numeric(id_trabajo),
              ocupa=case_when(id_trabajo==1 ~ 1,
                              id_trabajo==2 ~ 1))

# Distinción de prestaciones en trabajo principal y secundario
trab <- dplyr::select(trab, folioviv, foliohog, numren, id_trabajo, tipo_trab, aforlab, ocupa) %>% 
  as.data.table() %>% 
  dcast(folioviv + foliohog + numren ~ id_trabajo, 
        value.var = c("tipo_trab", "aforlab", "ocupa"), 
        sep="", fill=0) %>%
  # Identificación de la población trabajadora toda 
  # la que reporta al menos un empleo en la base de trabajos.csv)
  mutate(trab=1) %>%
  dplyr::select(folioviv, foliohog, numren, trab, tipo_trab1, tipo_trab2, 
                aforlab1, aforlab2, ocupa1, ocupa2)

fwrite(trab, "2020/bases/mod/prestaciones20.csv", row.names=F)








poblacion <- read_dta("2020/bases/poblacion.dta")   
glimpse(poblacion)



poblacion_mod <- poblacion %>% 
  mutate(
    # Instituciones de salud
    inst_insabi    = case_when(pop_insabi==1 ~ 1, T ~ 0),
    inst_medica    = case_when(atemed==1 ~ 1, T ~ 0),
    inst_IMSS      = case_when(inst_1==1 ~ 1, T ~ 0),
    inst_ISSSTE_N  = case_when(inst_2==2 ~ 1, T ~ 0),
    inst_ISSSTE_E  = case_when(inst_3==3 ~ 1, T ~ 0),
    inst_ISSSTE    = case_when(inst_ISSSTE_N==1 | inst_ISSSTE_E==1 ~ 1, T ~ 0),
    inst_PEMEX_FAM = case_when(inst_4==4 ~ 1, T ~ 0),
    inst_PROSPERA  = case_when(inst_5==5 ~ 1, T ~ 0),
    inst_OTRA      = case_when(inst_6==6 ~ 1, T ~ 0),
    inst_Total     = case_when(inst_insabi==1 | inst_IMSS==1 | inst_ISSSTE==1 |
                               inst_PEMEX_FAM==1 | inst_PROSPERA==1 | inst_OTRA==1 ~ 1, 
                               T ~ 0))
    
# Construcción del indicador
segsoc <- poblacion_mod
segsoc <- left_join(segsoc, trab, by = c("folioviv", "foliohog", "numren"))


segsoc_m <- segsoc %>% 
  mutate(
    # PEA (personas de 16 años o más)
    pea=case_when(trab==1 & (edad>=16 & !is.na(edad)) ~ 1, # PEA: ocupada
                  (act_pnea1==1 | act_pnea2==1) & 
                    (edad>=16 & !is.na(edad)) ~ 2, # PEA: desocupada
                  (edad>=16 & !is.na(edad)) & 
                    ((act_pnea1!=1 | is.na(act_pnea1)) & 
                       (act_pnea2!=1 | is.na(act_pnea2))) & 
                    ((act_pnea1>=2 & act_pnea1<=6) | 
                       (act_pnea2>=2 & act_pnea2<=6))  ~ 0), # PNEA
    # Tipo de trabajo
    # Ocupación principal
    tipo_trab1=ifelse(pea==1, tipo_trab1, tipo_trab1),  # Depende de un patrón, jefe o superior
    tipo_trab1=ifelse((pea==0 | pea==2), NA_real_, tipo_trab1), # No depende de un jefe y recibe o tiene asignado un sueldo
    tipo_trab1=ifelse(is.na(pea), NA_real_, tipo_trab1), # No depende de un jefe y no recibe o no tiene asignado un sueldo
    
    # Ocupación secundaria
    tipo_trab2=ifelse(pea==1, tipo_trab2, tipo_trab2), # Depende de un patrón, jefe o superior
    tipo_trab2=ifelse((pea==0 | pea==2), NA, tipo_trab2), # No depende de un jefe y recibe o tiene asignado un sueldo
    tipo_trab2=ifelse(is.na(pea), NA, tipo_trab2), # No depende de un jefe y no recibe o no tiene asignado un sueldo
    
    # Jubilados o pensionados
    jub=case_when(trabajo_mp==2 & act_pnea1==2 | act_pnea2==2 ~ 1, # Población pensionada o jubilada
                 # ing_pens>0 &  !is.na(ing_pens) ~ 1, # Población pensionada o jubilada
                  inscr_2==2 ~ 1, # Población pensionada o jubilada
                  TRUE ~0), # Población no pensionada o jubilada
    # Prestaciones básicas
    
    # Prestaciones laborales (Servicios médicos)
    
    # Ocupación principal
    smlab1=case_when(ocupa1==1 & atemed==1 & 
                       (inst_1==1 | inst_2==2 |  inst_3==3 |inst_4==4) & 
                       (inscr_1==1) ~ 1, # Sin servicios médicos
                     ocupa1==1 ~ 0), # Con servicios médicos
    # Ocupación secundaria
    smlab2=case_when(ocupa2==1 & atemed==1 & 
                       (inst_1==1 | inst_2==2 |inst_3==3 | inst_4==4) & 
                       (inscr_1==1) ~  1, # Sin servicios médicos
                     ocupa2==1 ~ 0), # Con servicios médicos
    
    # Contratación voluntaria: servicios médicos y ahorro para el retiro o pensión para 
    # la vejez (SAR, Afore, Haber de retiro)
    
    # Servicios médicos
    smcv=case_when(atemed==1 & (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4) &
                     inscr_6==6 & (edad>=12 & !is.na(edad)) ~ 1, # Sí cuenta
                   (edad>=12 & !is.na(edad))  ~ 0), # No cuenta
    # SAR o Afore
    aforecv=case_when(segvol_1==1 & (edad>=12 & !is.na(edad)) ~ 1, # Sí cuenta
                      is.na(segvol_1) & (edad>=12 & !is.na(edad)) ~ 0)) # No cuenta


segsoc_m <- segsoc_m %>% 
  mutate(
    # Núcleos familiares
    par=case_when(parentesco>=100 & parentesco<200 ~ 1, # Jefe o jefa del hogar
                  parentesco>=200 & parentesco<300 ~ 2, # Cónyuge del  jefe/a
                  parentesco>=300 & parentesco<400 ~ 3, # Hijo del jefe/a
                  parentesco==601 ~ 4, # Padre o Madre del jefe/a
                  parentesco==615 ~ 5, # Suegro del jefe/a
                  TRUE ~6), # Sin parentesco directo
    # Asimismo, se utilizará la información relativa a 
    # la asistencia a la escuela
    inas_esc=case_when(asis_esc==1 ~ 0,   # Sí asiste
                       asis_esc==2 ~ 1 ), # No asiste
    # Acceso directo a la seguridad social
    ss_dir=case_when(  
      # Ocupación principal
      tipo_trab1==1 & smlab1==1 ~ 1, # Con acceso
      tipo_trab1==2 & ((smlab1==1 | smcv==1) & (aforlab1==1 | aforecv==1)) ~ 1, # Con acceso
      tipo_trab1==3 & ((smlab1==1 | smcv==1) & aforecv==1) ~ 1, # Con acceso
      # Ocupación secundaria
      tipo_trab2==1 & smlab2==1 ~ 1, # Con acceso
      tipo_trab2==2 & ((smlab2==1 | smcv==1) & (aforlab2==1 | aforecv==1)) ~ 1, # Con acceso  
      tipo_trab2==3 & ((smlab2==1 | smcv==1) & aforecv==1) ~ 1, # Con acceso
      # Jubilados y pensionados
      jub==1 ~ 1, # Con acceso
      TRUE ~0), # Sin acceso
  )


segsoc_m <- mutate(segsoc_m,                 
                 # En primer lugar se identifican los principales parentescos respecto a la jefatura 
                 # del hogar y si ese miembro cuenta con acceso directo
                 jef=case_when(par==1 & ss_dir==1 & 
                                 (((inst_2=="2" | inst_3=="3") & inscr_6=="6") & 
                                    (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) &
                                    (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                       is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7))) ~ NA_real_,
                               par==1 & ss_dir==1 ~ 1),
                 
                 cony=case_when(par==2 & ss_dir==1 & 
                                  (((inst_2=="2" | inst_3=="3") & inscr_6=="6") &
                                     (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) &
                                     (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                        is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7) )) ~ NA_real_,
                                par==2 & ss_dir==1 ~ 1),
                 
                 hijo=case_when(par==3 & ss_dir==1 & 
                                  (((inst_2=="2" | inst_3=="3") & inscr_6=="6") & 
                                     (is.na(inst_1)  & is.na(inst_4) & is.na(inst_6)) & 
                                     (is.na(inscr_1)  & is.na(inscr_2)  & is.na(inscr_3)  & 
                                        is.na(inscr_4)  & is.na(inscr_5)  & is.na(inscr_7) )) ~ NA_real_,
                                par==3 & ss_dir==1 & jub==1 & (edad>25 & !is.na(edad)) ~ 1,
                                par==3 & ss_dir==1 & jub==0 ~ 1))

segsoc_m <- as.data.table(segsoc_m)[, c("jef_ss", "cony_ss", "hijo_ss") :=
                                  .(sum(jef, na.rm=TRUE),
                                    sum(cony, na.rm=TRUE),
                                    sum(hijo, na.rm=TRUE)), by=.(folioviv, foliohog)] %>% 
  mutate(jef_ss=if_else(jef_ss>0, 1,jef_ss), # Acceso directo a la seguridad social de la jefatura del hogar
         cony_ss=if_else(cony_ss>0, 1,cony_ss), # Acceso directo a la seguridad social de cónyuge de la jefatura del hogar
         hijo_ss=if_else(hijo_ss>0, 1,hijo_ss)) # Acceso directo a la seguridad social de hijos(as) de la jefatura del hogar

# Otros núcleos familiares: se identifica a la población con acceso a la seguridad
# social mediante otros núcleos familiares a través de la afiliación
# o inscripción a servicios de salud por algún familiar dentro o 
# fuera del hogar, muerte del asegurado o por contratación propia

segsoc_m <-  mutate(segsoc_m,
                  s_salud=case_when(atemed==1 & 
                                      (inst_1==1 | inst_2==2 | inst_3==3 |inst_4==4) & 
                                      (inscr_3==3 | inscr_4==4 | inscr_6==6 | inscr_7==7) ~ 1, # Con acceso
                                    !is.na(pop_insabi) & !is.na(atemed) ~ 0)) # Sin acceso

################################################################################
# Indicador de carencia por acceso a la seguridad social
#
# Se encuentra en situación de carencia por acceso a la seguridad social
# a la población que:
#  1. No disponga de acceso directo a la seguridad social.
#  2. No cuente con parentesco directo con alguna persona dentro del hogar
#     que tenga acceso directo.
#  3. No recibe servicios médicos por parte de algún familiar dentro o
#     fuera del hogar, por muerte del asegurado o por contratación propia.
################################################################################

#Indicador de carencia por acceso a la seguridad social
segsoc <-mutate(segsoc_m, 
                ic_segsoc=NA,
                ic_segsoc=case_when(
                  # Acceso directo
                  ss_dir==1 ~ 0, # No presenta carencia
                  # Parentesco directo: jefatura
                  par==1 & cony_ss==1 ~ 0, # No presenta carencia
                  par==1 & pea==0 & hijo_ss==1 ~ 0, # No presenta carencia
                  # Parentesco directo: cónyuge
                  par==2 & jef_ss==1 ~ 0, # No presenta carencia 
                  par==2 & pea==0 & hijo_ss==1 ~ 0, # No presenta carencia
                  # Parentesco directo: descendientes
                  par==3 & edad<16 & jef_ss==1 ~ 0, # No presenta carencia
                  par==3 & edad<16 & cony_ss==1 ~ 0, # No presenta carencia
                  par==3 & (edad>=16 & edad<=25) & inas_esc==0 & jef_ss==1 ~ 0, # No presenta carencia
                  par==3 & (edad>=16 & edad<=25) & inas_esc==0 & cony_ss==1 ~ 0, # No presenta carencia
                  # Parentesco directo: ascendientes
                  par==4 & pea==0 & jef_ss==1 ~ 0, # No presenta carencia
                  par==5 & pea==0 & cony_ss==1 ~ 0, # No presenta carencia
                  # Otros núcleos familiares
                  s_salud==1 ~ 0, # No presenta carencia
                  
                  TRUE ~1)) # Presenta carencia


segsoc <- select(segsoc, folioviv, foliohog, numren, starts_with("tipo_trab"), 
                 starts_with("aforlab"), starts_with("smlab"), smcv, aforecv, 
                 pea, jub, ss_dir, par, ends_with("_ss"), s_salud,
                 ic_segsoc,
                 starts_with("inst_") )

fwrite(segsoc, "2020/bases/mod/ic_segsoc20.csv", row.names=F)
gdata::keep(segsoc, poblacion, poblacion_mod, sure=T)



#####tablas

# Concentrado
concentradohogar <- read_dta("2020/bases/concentradohogar.dta") %>% rename_all(tolower)

datitos20 <- left_join(segsoc, concentradohogar, by = c("folioviv", "foliohog"))
datitos20 <- left_join(datitos20, poblacion, by = c("folioviv", "foliohog","numren"))
datitos20 = as.data.frame(datitos20)
datitos20m <- datitos20 %>%
  mutate(
    nueva_rur=    case_when(tam_loc==3 | tam_loc==4 ~ "Menos de 15k",
                            tam_loc==1 | tam_loc==2 ~ "15k o más"),
    edad_recoded= case_when(  edad<=18  ~ "Menos de 18",
                              edad>=19 & edad<=29 ~ "Entre 19 y 29",
                              edad>=30 & edad<=44 ~ "Entre 30 y 44",
                              edad>=45 & edad<=59 ~ "Entre 45 y 59",
                              edad>=60            ~ "Mayor que 60"),
    habla_ind= case_when(  hablaind==1  ~ 1,
                           hablaind==2  ~ 0),
    ing_cor_pc=ing_cor/tot_integ,
    decil  = dineq::ntiles.wtd(ing_cor_pc, 10, weights = factor),
    fuente_predo = case_when(  trabajo>negocio  ~ "Más remuneraciones",
                               trabajo<negocio  ~ "Más negocios"),
    tam_loc=      factor(tam_loc,
                            levels = c(1,2,3,4),
                            labels = c("100k y mas", "15k a 100k", "2.5k a 15k", "Menos de 2.5k")))
glimpse(datitos20m)         
table(datitos20m$edad_recoded)
#Distribución de acceso a salud por tamaño de localidad
datitos20m %>% 
  group_by(tam_loc) %>% 
  summarise( 
    count  = n(),
    suma   = sum(inst_Total),
    mean   = mean(inst_Total),
    median = median(inst_Total),
    q25    = quantile(inst_Total, 0.25),
    q75    = quantile(inst_Total, 0.75),
  )%>% 
  kbl() %>% 
  kable_styling() %>% 
  kable_classic() 

#SRVYR
enigh20_design <- datitos20m %>% 
  as_survey_design(ids = upm, weights = factor, strata = est_dis)
enigh20_boot <- enigh20_design %>% 
  as_survey_rep(type = "bootstrap", replicates = 50)

#1. Distribución de acceso a salud por tamaño de localidad completo
  enigh20_boot %>%
  #filter (parentesco==101) %>% 
  group_by(tam_loc) %>% 
  srvyr::summarise(promedio = survey_mean (inst_Total, na.rm = TRUE)
  )%>% 
    kbl() %>% 
    kable_styling() %>% 
    kable_classic() 

  enigh20_boot %>%
    #filter (parentesco==101) %>% 
    group_by(nueva_rur) %>% 
    srvyr::summarise(promedio = survey_mean (inst_Total, na.rm = TRUE)
    )%>% 
    kbl() %>% 
    kable_styling() %>% 
    kable_classic() 

  glimpse(datitos20m)
  
  #1. Distribución de acceso a salud por tamaño de localidad completo
  enigh20_boot %>%
    #filter (parentesco==101) %>% 
    group_by(tam_loc) %>% 
    srvyr::summarise(
      Total         = survey_mean (inst_Total, na.rm = TRUE),
      INSABI        = survey_mean (inst_insabi, na.rm = TRUE),
      IMSS          = survey_mean (inst_IMSS, na.rm = TRUE),
      ISSSTE        = survey_mean (inst_ISSSTE, na.rm = TRUE),
      PEMEX_FAM     = survey_mean (inst_PEMEX_FAM, na.rm = TRUE),
      IMSS_PROSPERA = survey_mean (inst_PROSPERA, na.rm = TRUE),
      OTRA          = survey_mean (inst_OTRA, na.rm = TRUE),
    )%>% 
    kbl() %>% 
    kable_styling() %>% 
    kable_classic() 
  
  enigh20_boot %>%
    #filter (parentesco==101) %>% 
    group_by(nueva_rur) %>% 
    srvyr::summarise(
      Total         = survey_mean (inst_Total, na.rm = TRUE),
      INSABI        = survey_mean (inst_insabi, na.rm = TRUE),
      IMSS          = survey_mean (inst_IMSS, na.rm = TRUE),
      ISSSTE        = survey_mean (inst_ISSSTE, na.rm = TRUE),
      PEMEX_FAM     = survey_mean (inst_PEMEX_FAM, na.rm = TRUE),
      IMSS_PROSPERA = survey_mean (inst_PROSPERA, na.rm = TRUE),
      OTRA          = survey_mean (inst_OTRA, na.rm = TRUE), 
    )%>% 
    kbl() %>% 
    kable_styling() %>% 
    kable_classic() 

#Sexo
  enigh20_boot %>%
    #filter (nueva_rur=="Menos de 15k") %>% 
    group_by(nueva_rur,sexo) %>% 
    srvyr::summarise(
      Total         = survey_mean (inst_Total, na.rm = TRUE),
      INSABI        = survey_mean (inst_insabi, na.rm = TRUE),
      IMSS          = survey_mean (inst_IMSS, na.rm = TRUE),
      ISSSTE        = survey_mean (inst_ISSSTE, na.rm = TRUE),
      PEMEX_FAM     = survey_mean (inst_PEMEX_FAM, na.rm = TRUE),
      IMSS_PROSPERA = survey_mean (inst_PROSPERA, na.rm = TRUE),
      OTRA          = survey_mean (inst_OTRA, na.rm = TRUE), 
    )%>% 
    kbl() %>% 
    kable_styling() %>% 
    kable_classic() 
  
#Edad
  enigh20_boot %>%
    mutate(
      edad_recoded= case_when(  edad<=18  ~ "Menos de 18",
                                edad>=19 & edad<=29 ~ "Entre 19 y 29",
                                edad>=30 & edad<=44 ~ "Entre 30 y 44",
                                edad>=45 & edad<=59 ~ "Entre 45 y 59",
                                edad>=60            ~ "Mayor que 60",
      )
    ) %>% 
    #filter (nueva_rur=="Menos de 15k") %>% 
    group_by(nueva_rur,edad_recoded) %>% 
    srvyr::summarise(
      Total         = survey_mean (inst_Total, na.rm = TRUE),
      INSABI        = survey_mean (inst_insabi, na.rm = TRUE),
      IMSS          = survey_mean (inst_IMSS, na.rm = TRUE),
      ISSSTE        = survey_mean (inst_ISSSTE, na.rm = TRUE),
      PEMEX_FAM     = survey_mean (inst_PEMEX_FAM, na.rm = TRUE),
      IMSS_PROSPERA = survey_mean (inst_PROSPERA, na.rm = TRUE),
      OTRA          = survey_mean (inst_OTRA, na.rm = TRUE), 
    )%>% 
    kbl() %>% 
    kable_styling() %>% 
    kable_classic() 
  
#Habla lengua indígena
  enigh20_boot %>%
    mutate(
      habla_ind= case_when(  hablaind==1  ~ 1,
                             hablaind==2  ~ 0,
      )
    ) %>% 
    #filter (nueva_rur=="Menos de 15k") %>% 
    group_by(nueva_rur,habla_ind) %>% 
    srvyr::summarise(
      Total         = survey_mean (inst_Total, na.rm = TRUE),
      INSABI        = survey_mean (inst_insabi, na.rm = TRUE),
      IMSS          = survey_mean (inst_IMSS, na.rm = TRUE),
      ISSSTE        = survey_mean (inst_ISSSTE, na.rm = TRUE),
      PEMEX_FAM     = survey_mean (inst_PEMEX_FAM, na.rm = TRUE),
      IMSS_PROSPERA = survey_mean (inst_PROSPERA, na.rm = TRUE),
      OTRA          = survey_mean (inst_OTRA, na.rm = TRUE), 
    )%>% 
    kbl() %>% 
    kable_styling() %>% 
    kable_classic()    
     
    
  #Deciles de ingreso
  enigh20_boot %>%
    mutate(
      ing_cor_pc=ing_cor/tot_integ,
      decil  = dineq::ntiles.wtd(ing_cor_pc, 10, weights = factor)
      ) %>% 
    #filter (nueva_rur=="Menos de 15k") %>% 
    group_by(nueva_rur,decil) %>% 
    srvyr::summarise(
      Total         = survey_mean (inst_Total, na.rm = TRUE),
      INSABI        = survey_mean (inst_insabi, na.rm = TRUE),
      IMSS          = survey_mean (inst_IMSS, na.rm = TRUE),
      ISSSTE        = survey_mean (inst_ISSSTE, na.rm = TRUE),
      PEMEX_FAM     = survey_mean (inst_PEMEX_FAM, na.rm = TRUE),
      IMSS_PROSPERA = survey_mean (inst_PROSPERA, na.rm = TRUE),
      OTRA          = survey_mean (inst_OTRA, na.rm = TRUE), 
    )%>% 
    kbl() %>% 
    kable_styling() %>% 
    kable_classic()      
  
  
  
  #Fuente de ingreso predominante
  enigh20_boot %>%
    mutate(
      fuente_predo = case_when(  trabajo>negocio  ~ "Más remuneraciones",
                                 trabajo<negocio  ~ "Más negocios",
      )
    ) %>%  
    filter (parentesco==101)  %>% 
    group_by(nueva_rur,fuente_predo) %>% 
    srvyr::summarise(
      Total         = survey_mean (inst_Total, na.rm = TRUE),
      INSABI        = survey_mean (inst_insabi, na.rm = TRUE),
      IMSS          = survey_mean (inst_IMSS, na.rm = TRUE),
      ISSSTE        = survey_mean (inst_ISSSTE, na.rm = TRUE),
      PEMEX_FAM     = survey_mean (inst_PEMEX_FAM, na.rm = TRUE),
      IMSS_PROSPERA = survey_mean (inst_PROSPERA, na.rm = TRUE),
      OTRA          = survey_mean (inst_OTRA, na.rm = TRUE), 
    )%>% 
    kbl() %>% 
    kable_styling() %>% 
    kable_classic()      
   
  
  
  #Fuente de ingreso predominante
  enigh20_boot %>%
    mutate(
      fuente_predo = case_when(  trabajo>negocio  ~ "Más remuneraciones",
                                 trabajo<negocio  ~ "Más negocios",
      )
    ) %>%  
    filter (parentesco==101)  %>% 
    group_by(nueva_rur,fuente_predo) %>% 
    srvyr::summarise(
      Total         = survey_mean (inst_Total, na.rm = TRUE),
      INSABI        = survey_mean (inst_insabi, na.rm = TRUE),
      IMSS          = survey_mean (inst_IMSS, na.rm = TRUE),
      ISSSTE        = survey_mean (inst_ISSSTE, na.rm = TRUE),
      PEMEX_FAM     = survey_mean (inst_PEMEX_FAM, na.rm = TRUE),
      IMSS_PROSPERA = survey_mean (inst_PROSPERA, na.rm = TRUE),
      OTRA          = survey_mean (inst_OTRA, na.rm = TRUE), 
    )%>% 
    kbl() %>% 
    kable_styling() %>% 
    kable_classic()      
  