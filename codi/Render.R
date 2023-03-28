#--------------------------------------------------------------------------------------------------#
# -------------------------   PARAMETRES  --------------------------------------------------------
gc()
rm(list=ls())
mostra=F

#       Lectura       # 
rmarkdown::render(input=here::here("codi","1_lectura_DM2_GENDER.Rmd"),
                  params = list(fitxers_test= mostra))


gc()
rm(list=ls())
mostra=F

#       Preparacio    # 
rmarkdown::render(input=here::here("codi","2_preparacio_DM2_GENDER.Rmd"),
                  params = list(fitxers_test= mostra),
                  output_file=paste0(here::here("resultats/Informe_exploratori_"),Sys.Date()))



#.
