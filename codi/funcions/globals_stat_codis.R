
criteris_exclusio<-function(dt=dades,taulavariables="VARIABLES_R3b.xls",criteris="exclusio1",missings=T,...) {
  
  # dt=dt_matching
  # taulavariables=conductor
  # criteris="exc_pre"
  # missings=T
  
  ##  2. Eliminar els espais en blanc de les variables factors del data.frame
  dt<-dt %>% 
    dplyr::mutate_if(is.factor,funs(stringr::str_trim(.))) %>% 
    dplyr::mutate_if(is.character,funs(stringr::str_trim(.)))
  
  ##  Llegeix criteris de variables 
  variables <- read_conductor(taulavariables,col_types = "text",...) %>% tidyr::as_tibble() %>% dplyr::select(camp,!!criteris)
  
  # Filtrar valors
  criteris_sym<-sym(criteris)
  variables<-variables %>% dplyr::filter(!is.na(!!criteris_sym))
  # variables[is.na(variables)]<- 0
  
  # llista de caracters logics del filtre
  char_logics<-c(">",">=","<","<=","==","!=","is.na") %>% paste0(collapse = '|')
  
  ##  0. Filtro taula variables només variables implicades en el filtre i el genero 
  maco<-variables %>% 
    dplyr::filter_(paste0(criteris,"!=0")) %>% dplyr::select_("camp",criteris) %>% 
    transmute_("camp","crit_temp"=criteris) %>% 
    # if criteri missing is.na()
    mutate(crit_temp=if_else(stringr::str_detect(crit_temp,"is.na"),paste0("is.na(",camp,")"),crit_temp)) %>% 
    mutate(camp=if_else(stringr::str_detect(crit_temp,"is.na"),"",camp)) %>% 
    # Si es texte sense igualtat --> la poso 
    mutate(crit_temp=if_else(stringr::str_detect(crit_temp,char_logics),crit_temp,paste0("=='",crit_temp,"'"))) 
  
  # Genero la llista de filtres     
  maco<-maco %>% tidyr::unite(filtres, c("camp", "crit_temp"),sep="", remove=F) %>% 
    mutate(filtres=paste0("(",filtres,")"))  
  
  # Afegir valors valids per aplicar criteri (Si missings==F)
  if (missings==F) maco<-maco %>% mutate(filtres=stringr::str_c("(", filtres, " & !is.na(",camp, "))"))
  
  # Concateno condicions amb un OR
  maco<-stringr::str_c(maco$filtres,collapse=" | ")
  
  ## 1. Genera filtre en base a columna exclusio1   popes
  popes<-stringr::str_c("!(",maco,")")
  
  ##  3. Aplicar filtre: popes a dt
  dt %>% dplyr::filter(eval(parse(text=popes)))
  
}

