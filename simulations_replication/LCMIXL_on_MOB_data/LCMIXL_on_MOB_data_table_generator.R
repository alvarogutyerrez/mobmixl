library(MobMixlogit)
library(xtable)
source(here::here('utils/utils_using_MobMixlogit.R'))


route <- "simulations_replication/LCMIXL_on_MOB_data/"

LC_MIXL_3C_model_delta_05 <- readRDS(file = paste0(route,  "LCMIXL_on_MOB_data_delta_05.rds"  )) %>% 
  create_LaTeX_table_LC_MIXL_on_MOB_data(file_name = "res_LCMIXL_on_MOB_data_delta_05",
                                         model_name = "LC-MIXL $(\\delta=0.5)$")

  
LC_MIXL_3C_model_delta_1 <- readRDS(file = paste0(route,  "LCMIXL_on_MOB_data_delta_1.rds"  )) %>% 
  create_LaTeX_table_LC_MIXL_on_MOB_data(file_name = "res_LCMIXL_on_MOB_data_delta_1",
                                         model_name = "LC-MIXL $(\\delta=1)$")



tab <- merge(x = LC_MIXL_3C_model_delta_05,
            y = LC_MIXL_3C_model_delta_1,
            by = "Variables") %>% 
  dplyr::slice(match(LC_MIXL_3C_model_delta_1$Variables, Variables )) %>% 
  dplyr::select(Variables,`LC-MIXL $(\\delta=0.5)$1`, 
                `LC-MIXL $(\\delta=1)$1`) %>% 
  dplyr::rename(`LC-MIXL $(\\delta=0.5)$` = `LC-MIXL $(\\delta=0.5)$1`,
                `LC-MIXL $(\\delta=1)$`   = `LC-MIXL $(\\delta=1)$1`) 



tab_to_latex<- print.xtable(xtable(tab),
                            include.rownames=F,
                            sanitize.text.function = function(x){x},
                            floating = F)

fileConn <-file(paste0("simulations_replication/LCMIXL_on_MOB_data/res_LCMIXL_on_MOB_data.tex"))
writeLines(tab_to_latex, fileConn)
close(fileConn)

