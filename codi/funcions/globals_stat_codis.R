
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




fores.plot.v4<-function(dadesmodel=dt_outHR,label="etiqueta",mean="estimate",lower="Linf",upper="Lsup",label_X="OR (95% CI)",
                        intercept=1,
                        nivell="outcome", 
                        factor="Event",
                        label_Xvertical="Cardiovascular event",
                        title = "Forest plot of hazard hatios and confidence interval (95%CI)",
                        subtitle = "Sensitivity analysis",
                        caption="",
                        label_Favors="Favors SGLT-2        Favors oGLD-2") {
  
  # dadesmodel=pepe
  # label="Y"
  # mean="OR"
  # lower="CI_low"
  # upper="CI_high"
  # 
  # nivell="Antecedent_CV"
  # # nivell=""
  # factor="Antecedent_CV"
  # # factor<-""
  # 
  # intercept=1
  # color=T
  # label_X="HR (95% CI)"
  # title = "Hazard ratios for Parkinson disease according retinophaty status"
  # subtitle = "Sensitivity analysis"
  # caption=""
  # label_Favors="Favors without DR               Favors Retinophaty"
  # 
  
  # Selecciono camps de dades necessaris
  dt<-dadesmodel %>% 
    select(label=!!sym(label),mean=!!sym(mean),lower=!!sym(lower),upper=!!sym(upper),nivell=!!sym(nivell),factor=!!sym(factor))
  
  # Indexo ordre
  dt_temp<-dt %>% mutate (id=n():1)
  
  # Si existeix nivell preparar dades per nivell
  # Afegir nivell amb etiqueta si cal
  if (nivell!="") {
    dt_temp<- dt_temp %>% 
      left_join(dt_temp %>% distinct(nivell) %>% mutate(id_nivell=1:n()),by = "nivell") %>% 
      mutate(label=paste0("           ",label)) %>% 
      split(.$nivell) %>% 
      map_dfr(~add_row(.x,.before = 0),.id = "nivell" ) %>% 
      mutate(label=if_else(is.na(label),nivell,label)) %>% 
      group_by(nivell) %>% 
      mutate(id_nivell=max(id_nivell,na.rm = T)) %>% ungroup() %>% 
      arrange(id_nivell) %>% 
      mutate(id=n():1)
    # reemplenar categoria missing de factor (si existeix)
    dt_temp<-dt_temp %>% group_by(id_nivell) %>% mutate(factor=if_else(is.na(factor),max(factor,na.rm = T),factor))
  }
  
  # Plot 
  ## Forest Plot 
  pplot<-
    dt_temp %>% 
    ggplot(aes(x=id, y=mean, ymin=lower, ymax=upper)) +
    geom_errorbar(size=0.2, width=0.5)+
    geom_hline(yintercept=intercept, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab("") + ylab(label_X) +
    scale_x_continuous(breaks=dt_temp %>% pull(id),labels=dt_temp %>% pull(label)) +
    # ylim(-0.5,10) +
    theme_minimal()
  
  if (nivell!="") {pplot <- pplot + geom_point(aes(color=nivell),size=4,shape=15) + labs(color=nivell)}
  
  if (factor!="") { pplot <- pplot + geom_point(aes(shape=factor)) + labs(shape=factor) }
  
  # tituls + labs
  pplot + 
    labs(title = title, subtitle = subtitle,caption = caption) +
    annotate("text", x=-0.8,y=1,label=label_Favors, colour = "black",size=4)
  
  
}


