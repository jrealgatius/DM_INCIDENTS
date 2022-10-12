
# Retorna taula plana a partir d'un fitxer amb la data index, el fitxer del cataleg i el fitxer de parametres
# Els parametres contenen els seguens camps: 
#     fitxer (nom) ,
#     domini (refereix al tipus d'agregació i codis del cataleg
#     Finestra1 Finestra2 (Periode)
#     camp (Quin camp s'utilitza com agregació)
#     prefix (o suffix de les variables generades)
#     funcio (Funció d'agregacio en variables )

Generar_taula_plana<-function(dt=dt_index,
                              cataleg=dt_cataleg,
                              parametres=dt_parametres,...) 
{
  
  # dt=dt_index
  # cataleg=here::here(fitxer_cataleg)
  # parametres=dt_parametres
  
  cataleg<-read_conductor(cataleg)
  parametres<-read_conductor(dt_parametres,...)
  
  # Llegeixo parametres
  # Agrego problemes de salut
  # Capturar: Dades, finestra, camp_agregador i prefix, filtrar cataleg
  
  # Depurar parametres ---------------------------------
  # en funció de la existencia dels fitxers si no existeixen -----
  exist_file<-parametres$fitxer %>% set_names(parametres$fitxer) %>% 
    map(~exists(.x)) %>% map(~as.integer(.x)) %>% 
    unlist() %>% as_tibble()
  parametres<-parametres %>% bind_cols(exist_file) %>% rename("exist"=value)
  
  arxius_inexistens<-parametres %>% filter(exist==0) %>% pull(fitxer) %>% unique()
  # Warning
  if (nrow(parametres %>% filter(exist==0))>0) { warning(paste0("No existeixen alguns fitxers:", paste0(arxius_inexistens,collapse = ", ")))}
  # filtro parametres logics
  parametres<-parametres %>% filter(exist==1)

  
  # PROBLEMES  ---------------------------------
  # Seleccionar parametres i cataleg
  par_problemes<-
    parametres %>% filter(domini=="diagnostics") 
  
  if (nrow(par_problemes)>0) {
    
    cat_problemes <-cataleg %>% filter(domini%in% c("diagnostics","cmbdh_diag","cmbd_procediments") )
    
    # Generar dades historic en funció del nom fitxer
    nom_fitxer<-  par_problemes %>% distinct(fitxer) %>% pull() 
    nom_fitxer<-set_names(nom_fitxer,nom_fitxer)
    dt_historic<-nom_fitxer %>% map_df(~eval(sym(.x)),.id="nom_fitxer") %>% semi_join(dt,by="idp") %>% select(nom_fitxer,idp,cod,dat)
    
    # Generar agregacions problemes segons llista de parametres
    DTAGR_PROBLEMES<-
      pmap(transmute(par_problemes,fitxer,as.numeric(Finestra1),as.numeric(Finestra2),prefix,camp),    
           
           ~agregar_problemes(
             dt=dt_historic %>% filter(nom_fitxer==..1),
             bd.dindex=dt,
             dt.agregadors=cat_problemes,
             finestra.dies=c(..2,..3),
             prefix=..4,
             camp_agregador=..5,
             cataleg_mana=T)
      ) %>% 
      reduce(full_join,by=c("idp","dtindex"))   # Juntar-ho tot
    
  } else DTAGR_PROBLEMES<-dt
  
  # FARMACS -------------------------------
  # Seleccionar parametres i cataleg
  par_farmacs<-
    parametres %>% filter(domini=="farmacs") 
  
  # Només passar si en parametres existeix
  if (nrow(par_farmacs)>0) {
    
    # Cataleg
    cat_farmacs <-cataleg %>% filter(domini%in% c("farmacs_facturats","farmacs","farmacs_prescrits"))
    
    # Generar dades historic en funció del nom fitxer
    nom_fitxer<-  par_farmacs %>% distinct(fitxer) %>% pull() 
    nom_fitxer<-set_names(nom_fitxer,nom_fitxer)
 
  
    dt_historic<-nom_fitxer %>% map_df(~eval(sym(.x)),.id="nom_fitxer") %>% semi_join(dt,by="idp")
      
    # 
    DTAGR_FARMACS<-
      pmap(transmute(par_farmacs,fitxer,as.numeric(Finestra1),as.numeric(Finestra2),prefix,camp),
             
             ~agregar_facturacio(
               dt_historic %>% select(idp,cod,dat,env) %>% filter(nom_fitxer==..1),
               bd.dindex=dt_index,
               finestra.dies=c(..2,..3),
               dt.agregadors=cat_farmacs,
               prefix=..4,
               camp_agregador=..5,
               agregar_data=F,
               cataleg_mana = T) 
      ) %>% 
      reduce(full_join,by=c("idp","dtindex")) %>%  # Juntar-ho tot
      mutate(dtindex=data.to.string(dtindex) %>% as.integer)

  } else DTAGR_FARMACS<-dt
  
  # ANALITIQUES  -------
  par_analit<-
    parametres %>% filter(domini=="analitiques") 
  
  if (nrow(par_analit)>0) {
    
    # Generar dades historic en funció del nom fitxer
    nom_fitxer<-  par_analit %>% distinct(fitxer) %>% pull() 
    
    nom_fitxer<-set_names(nom_fitxer,nom_fitxer)
    dt_historic<-nom_fitxer %>% map_df(~eval(sym(.x)),.id="nom_fitxer") %>% semi_join(dt,by="idp") 
    
    DTAGR_ANALITIQUES<-
      pmap(
        
        transmute(par_analit,
                  fitxer,as.numeric(Finestra1),as.numeric(Finestra2),prefix,funcio,camp),
        
        ~ agregar_analitiques(dt_historic %>% filter(nom_fitxer==..1),
                              bd.dindex = dt,
                              finestra.dies = c(..2,..3),
                              sufix = c(..4,".dies"),
                              fun=..5,
                              camp=..6)
        
      ) %>% 
      
      reduce(full_join,by=c("idp","dtindex")) %>%  # Juntar-ho tot
      mutate(dtindex=lubridate::as_date(dtindex) %>% data.to.string %>% as.integer)
    
  } else DTAGR_ANALITIQUES<-dt 
  
  # Finalment: juntar-ho tot
  #
  
  dt %>% 
    left_join(DTAGR_PROBLEMES,by=c("idp","dtindex")) %>% 
    left_join(DTAGR_FARMACS,by=c("idp","dtindex")) %>% 
    left_join(DTAGR_ANALITIQUES,by=c("idp","dtindex"))
  
}