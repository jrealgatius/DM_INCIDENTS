

# # Descarregar funcions github -
# link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
# devtools::source_url(link_source)


# donat un vector exclusions, generar les cohorts d'exclusions sequencials
# dt<-dades


# exclusions<-c("exclusio1", "exclusio2","exclusio3", "exclusio4", "exclusio5")
# exclusions<-c("exclusio1", "exclusio2","exclusio3")
# exclusions<-c("exclusio1","exclusio3")


# devtools::install_github("tgerke/ggconsort")

library(ggconsort)
# library(dplyr)
# library(purrr)
# library(ggplot2)


# Funció que: 
# 1. Generi variables dummis segons criteris d'exclusio determinats a partir de conductor
# 2. Cridi la funció que generi flow chart

# names(iris) %>% write.csv2("vars.csv")


## Generar variables dummies
generar_dummies_exclusions<-function(dt=iris, criteris=c("Sepal.Length < 5","Species=='setosa'")) {
  
  # dt=dt
  # criteris=exclusions
  # dt=dt, criteris=exclusions
  
  # Generar dummies segons criteris d'inclusió
  num_excl<-criteris %>% length()
  cols_dummies<-paste0("exclusio",c(1:num_excl))
  
  dt_dumies<-
    purrr::map2_dfc(criteris,cols_dummies,
       ~dplyr::transmute(dt,!!sym(.y):=eval(parse(text=.x)))
    ) %>% dplyr::mutate_all(as.numeric)

  # Juntar-ho tot
  # Truc per eliminar dumiis existents en dt
  vars_dt<-names(dt)[!names(dt)%in% c(names(dt_dumies))]
  dt %>% select(vars_dt) %>% dplyr::bind_cols(dt_dumies) 
  
  }


Generar_flow_chart_consort<-function(dt=iris, 
                                     taulavariables="conductor_cars.xls",
                                     camp="camp",criteris="exclusio",
                                     grups=NA,
                                     missings=F,
                                     sequencial=T,
                                     labels=NULL,
                                     lab_start="Assessed for eligibility",
                                     lab_random="Randomized",
                                     ...) 
  {
  
    # # dt=dt_temp
    # # taulavariables=conductor_variables
    #  sheet="criteris_exclusio"
    #  criteris="exc_ggconsort"
    #  missings=F
    #  labels="descripcio"
    #  sequencial=F
    #  grups=NA
    #  camp="camp"
    #  lab_start="Assessed for eligibility"
    #  lab_random="Randomized"
  
    # dt=iris
    # taulavariables="conductor_cars.xls"
    # camp="camp"
    # criteris="exclusio"
    # missings=F
    # labels="descripcio"
  
  # taulavariables = here::here("Conductor.xls")
  # sheet="criteris_exclusio"
  # criteris="exclusio_nou"
  
  
  # 1. Obrir conductor d'exclusions del conductor

  ##  Llegeixo criteris de variables i selecciono variables amb filtres 
    variables <- read_conductor(taulavariables,col_types = "text",...)  %>% 
    # variables <- read_conductor(taulavariables,col_types = "text",sheet="criteris_exclusio")  %>%
      dplyr::select(camp=!!camp,criteris=!!criteris, labels=!!labels) %>% 
      # Filtrar valors
      dplyr::filter(!is.na(criteris))
    
    # Parar si no hi ha criteris d'exclusió
    num_excl<-variables$criteris %>% length()
    if (num_excl==0) {
      print("No hi ha criteris jejejj")
      return("Error") }
    
  # 2. Generar dummies 
    dt_nova<-generar_dummies_exclusions(dt=dt,criteris=variables$criteris)
  
    # Noves variables generades 
    exclusions=paste0("exclusio",c(1:num_excl))
  
  # labels si no s'han passat
    if (is.null(labels)) variables<-variables %>% mutate(labels=criteris)
    
  # 3. Generar flow_chart
    Flow_chart_Consort(dt=dt_nova, 
                       exclusions=exclusions,
                       lab_exclusions=variables$labels,
                       grups = grups,
                       sequencial=sequencial,
                       lab_start=lab_start,
                       lab_random=lab_random)
  
  }

# dt<-trial_data %>% select(idp=id,exclusio1=declined ,exclusio2=prior_chemo ,exclusio3=bone_mets,grup=treatment)

Flow_chart_Consort<-function(dt=trial_data,
                             exclusions=c("declined","prior_chemo","bone_mets"),sequencial=T,grups=NA,
                                         lab_start="Assessed for eligibility",
                                         lab_random="Randomized",
                                         lab_exclusions=NULL
                                         ) 
  {

  ## Testing 
  # dt=dt_nova
  # exclusions= exclusions
  # lab_exclusions=variables$labels
  # grups = grups
  # sequencial=sequencial
  # lab_start="Assessed for eligibility"
  # lab_random="Randomized"
  # lab_exclusions=NULL

  # dt=trial_data
  # exclusions=c("declined","prior_chemo","bone_mets")
    # # # Parametres
  # lab_start="Assessed for eligibility"
  # lab_random="Randomized"
  # lab_exclusions=c("A","B","C")
  # dt=trial_data
  # exclusions<-c("exclusio1", "exclusio2","exclusio3")
  # exclusions=c("declined","prior_chemo","bone_mets")
  # exclusions=c("prior_chemo","bone_mets")
  # # exclusions=c("bone_mets")
  # grups="treatment"
  # sequencial=T
  # grups=NA
  # grups="treatment"
  # # # 
  # # # 
  # dt=iris 
  # exclusions=c("Sepal.Length < 5","Species=='setosa'")

  # Proces per dicotomitzar criteris d'inclusió
  # vector_crit_excl<-c("DG.ANGOR","edat<30 | edat>=90","ANT.FA","ANT.event","sexe=='H'","DG.AIT") 
  # subvector_a_canviar<-vector_crit_excl[vector_crit_excl %in% (dt_temp %>% names())] %>% paste0("==1")
  # vector_crit_excl[vector_crit_excl %in% (dt_temp %>% names())]<-subvector_a_canviar
  
  # Dicotomitzar....
  subvector_a_canviar<-exclusions[exclusions %in% (dt %>% names())] %>% paste0("==1")
  exclusions[exclusions %in% (dt %>% names())]<-subvector_a_canviar
  dt<-generar_dummies_exclusions(dt=dt, criteris=exclusions)
  
  # Num d'exclusions
  N_exc<-length(exclusions)
  noms_exclusions<-exclusions
  exclusions<-paste0("exclusio",c(1:N_exc))

  # Selecciono camps necessaris de dt (dades)
  if (is.na(grups)) 
    {dt<-dt %>% 
      dplyr::select(exclusions) %>% dplyr::mutate(grup="Overall",idp = row_number())
      grup<-"Overall"} else 
    {dt<-dt %>% dplyr::select(exclusions,grup=grups) %>% dplyr::mutate(idp = dplyr::row_number())}
  
  # capturar etiquetes
  if (is.null(lab_exclusions)) labels_exclusions<-noms_exclusions else labels_exclusions<-lab_exclusions
  
  label_grup<-grups
  levels_grup<-dt %>% dplyr::select(grup) %>% na.omit() %>% dplyr::distinct() %>% dplyr::pull(grup)
  # Canvi de categories a grup numerats en character
  dt<-dt %>% dplyr::mutate(grup=as.factor(grup) %>% as.numeric(),
                grup=dplyr::if_else(!is.na(grup),as.character(paste0("grup",grup)),NA_character_))

  # Generar dataframes de filtre

  # Inclusions sequencials
  # Genero fitxers inclusions sequencials 
  dtlist_incl<-seq(1:N_exc) %>% 
    purrr::map(~paste0("!",exclusions[1:.x],collapse = " & ")) %>% 
    purrr::map(~dt %>% filter(eval(parse(text = .x)))) %>% 
    purrr::set_names(paste0("included",1:N_exc))
  
  # Inclusio final
  dt_inclusio_final<-dtlist_incl[N_exc] %>% purrr::set_names("Included_final")
  
  # Generar fitxers d'exclusions sequencials
  dt_excluded_totals = anti_join(dt, dt_inclusio_final[[1]], by = "idp")
  dt_exc1 = anti_join(dt, dtlist_incl[[1]], by = "idp")
  # si hi ha una unica exclusio dtlist_exclusions<-NULL

  if (N_exc>1) {
    if (sequencial) {
      dtlist_exclusions<-
        purrr::map2(dtlist_incl[1:(N_exc-1)],dtlist_incl[-1],~anti_join(.x,.y,by = "idp")) %>% 
        purrr::set_names(paste0("Excluded",c(2:N_exc)))} else {
      
      dtlist_exclusions<-c(2:N_exc) %>%
        purrr::map(~paste0(exclusions[.x],"==1")) %>%
        purrr::map(~dt %>% filter(eval(parse(text = .x)))) %>%
        purrr::set_names(paste0("Excluded",2:N_exc))
      }} else {dtlist_exclusions<-NULL} 

  dtlist_exclusions<-append(list(Excluded1=dt_exc1),dtlist_exclusions)
  # Inclusions finals per grups
  # grup="FF.HTA"
  dt_inclusions_grups<-
    dtlist_incl[[N_exc]] %>% base::split(.[["grup"]]) 
  
  # # Elimino els NA's
  # dt_inclusions_grups$grupNA<-NULL
  
  # Fusionar llistats de de inclosos + exclosos
  llistat_arguments<-
    c(dt_inclusio_final,dtlist_incl,dt_inclusions_grups,
      list(Excluded_total=dt_excluded_totals),
      dtlist_exclusions)
  
  arglist = append(list(cohort_start(dt,lab_start)),
                   llistat_arguments) 
  
  # Generar les cohorts via funció
  dades_cohorts<-
    do.call(cohort_define, 
          arglist)
  
  # Provide text labels for cohorts ---------------------------
  llistat_noms<-dades_cohorts$data %>% names()
  
  llistat_labels<- c(lab_start,lab_random,
                     paste0("Included",c(1:N_exc)),
                     levels_grup,"Excluded",labels_exclusions) 
  
  # Labels 
  # llistat_noms<-dades_cohorts$data %>% names()
  # llistat_noms<-c(".full","Included_final", paste0("Included",c(1:N_exc)),
  #                 paste0("grup",1:length(levels_grup)), 
  #                 "Excluded_total",  paste0("Excluded",c(1:N_exc)))

  for (i in 1:length(llistat_noms)) {
    dades_cohorts$labels[llistat_noms[i]]<-llistat_labels[i]}
  
  study_cohorts<-dades_cohorts
  
  # 
  # study_cohorts<-
  #   dades_cohorts %>% 
  #   cohort_label(
  #     Included_final = lab_random,
  #     grup1= "Allocated to arm A",
  #     grup2 = "Allocated to arm B",
  #     Excluded_total = "Excluded",
  #     Excluded1 = "Declined to participate",
  #     Excluded2 = "Prior chemotherapy",
  #     Excluded3 = "Bone metastasis")
  
  # Generar caixa d'exclusions
  noms_exc<-paste0("Excluded",1:N_exc)
  
  caixa_exc<-noms_exc %>% 
    map_chr(~paste0('- {cohort_count_adorn(study_cohorts, ', .x,')}<br>')) %>% 
    glue::glue_collapse()
  caixa_exclusions<-paste0(
    "{cohort_count_adorn(study_cohorts, Excluded_total)}<br>",
    caixa_exc)
  
  study_consort <- study_cohorts %>%
    consort_box_add(
      "full", 0, 50, cohort_count_adorn(study_cohorts, .full)
    ) %>%
    consort_box_add(
      lab_random, 0, 30, cohort_count_adorn(study_cohorts,Included_final)
    ) %>% 
    
    # consort_box_add(
    #   "exclusions", 20, 40, glue::glue(caixa_exclusions)
    # ) %>% 


    consort_box_add(
      "exclusions", 10, 40, glue::glue(caixa_exclusions)
    ) %>% 
    
        
    consort_arrow_add(
      end = "exclusions", end_side = "left", start_x = 0, start_y = 40
    ) %>%
    consort_arrow_add(
      "full", "bottom", lab_random, "top"
    ) 
  
  # En cas de By grups
  if (!is.na(grups)) {
    
    # By grups 
    study_consort <- study_consort %>%
      
      consort_box_add(
        "arm_a", -30, 10, cohort_count_adorn(study_cohorts, grup1)
      ) %>%  
      
      consort_box_add(
        "arm_b", 30, 10, cohort_count_adorn(study_cohorts, grup2)
      ) 
    
    study_consort<- study_consort %>%
      consort_arrow_add(
        start_x = 0, start_y = 30, end_x = 0, end_y = 20,
      ) %>% 
      consort_line_add(
        start_x = -30, start_y = 20, end_x = 30, end_y = 20,
      )  %>% 
      consort_arrow_add(
        end = "arm_a", end_side = "top", start_x = -30, start_y = 20
      ) %>%
      consort_arrow_add(
        end = "arm_b", end_side = "top", start_x = 30, start_y = 20)
    
    }
  
  ## Fer-ho maco
 
  if (!is.na(grups)) {
    
    study_consort %>%
      ggplot() + 
      geom_consort() +
      theme_consort(margin_h = 8, margin_v = 1) +
    # you can include other ggplot geoms, as needed -------------
      ggtext::geom_richtext(
      aes(x = 0, y = 10, label = "Grup"),
      fill = "#9bc0fc") } else 
        {
          study_consort %>%
          ggplot() + 
          geom_consort() + xlim(-10,20) + 
          theme_consort(margin_h = 10, margin_v = 1) 
          }
   }


# #############       Exemples        ################
# 
# Flow_chart_Consort(dt=trial_data,exclusions=c("declined","prior_chemo","bone_mets"),sequencial = F,grups = "treatment",
#                    lab_start = "Pob inicial", lab_random = "Aleatoritzats")
# 
# # pp<-Flow_chart_Consort(dt=trial_data,exclusions=c("declined","prior_chemo","bone_mets"),sequencial = T)
# 
# # ggsave("figura.jpg",plot = pp,dpi = 300,width=7,height = 5)
# 
# 
# 
# #
# Flow_chart_Consort(dt=trial_data,exclusions=c("prior_chemo","bone_mets"))
# 
# Flow_chart_Consort(dt=trial_data,exclusions=c("declined","prior_chemo","bone_mets"),sequencial = F,grups="treatment")
# Flow_chart_Consort(dt=trial_data,exclusions=c("declined","prior_chemo","bone_mets"),sequencial = T,grups="treatment")
# 
# Flow_chart_Consort(dt=trial_data,exclusions=c("declined","prior_chemo","bone_mets"),sequencial = T,grups=NA)
# 
# Flow_chart_Consort(dt=trial_data,exclusions=c("prior_chemo","bone_mets"),sequencial = F,grups="treatment")
# 
# Flow_chart_Consort(dt=trial_data,exclusions=c("prior_chemo","bone_mets"),sequencial = F,grups="treatment")
# 
# Flow_chart_Consort(dt=trial_data,exclusions=c("bone_mets"))
# 
# 
# table(trial_data$prior_chemo)
# 
# Flow_chart_Consort(exclusions=c("declined","prior_chemo","bone_mets"),sequencial = T)
# Flow_chart_Consort(exclusions=c("declined","prior_chemo","bone_mets"),sequencial = F)
# 
# Flow_chart_Consort(exclusions=c("prior_chemo","bone_mets"),sequencial = T)
# Flow_chart_Consort(exclusions=c("prior_chemo","bone_mets"),sequencial = F)
# 
# Flow_chart_Consort(exclusions=c("bone_mets","prior_chemo"),
#                    grups="treatment",sequencial=T)
# 
# 
# Flow_chart_Consort(exclusions=c("bone_mets","prior_chemo"),sequencial=T)
# 
# 
# Flow_chart_Consort(exclusions=c("prior_chemo"),
#                    grup="treatment",sequencial=T)
# 
# 
# 




