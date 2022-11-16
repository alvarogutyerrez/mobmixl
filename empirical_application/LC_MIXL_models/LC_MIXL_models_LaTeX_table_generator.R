library(dplyr)
library(here)
library(xtable)
library(stringr)
library(MobMixlogit)
library(ggplot2)
library(gridExtra)
#Load costome made functions
source(here::here('utils/utils_using_MobMixlogit.R'))

all_LC_MIXL_mo_files <- lapply(0:5, function(x){
  
  folder_files <- paste0("empirical_application/LC_MIXL_models/alloc_",x)
  
  ## Reading files in the folder
  filenames <- as.list(list.files(path       = folder_files,
                                  pattern    = "*.rds",
                                  full.names = TRUE ) )}) %>%
  unlist(recursive = F)


all_LC_mo <- lapply(all_LC_MIXL_mo_files, FUN = function(x){
  
  # Model Description identification
  model_description <- identify_LC_MIXL_model(x)
  # Final model estimates
  res_model <-   readRDS(file = x)
  res_final <- list(
    model_description  = model_description,
    res_model          = res_model
  )
})



get_time_models(all_LC_mo = all_LC_mo,type_model = "LC-MIXL")


tab_all<- create_comparisson_LC_table(all_LC_mo, 
                            type_model = "LC-MIXL",
                            write_on_disk = FALSE)


tab_all_2C <- tab_all %>% 
  select(Variables,starts_with("LC-MIXL-2")) %>% 
  filter(!Variables=="Demo.Alloc") %>% 
  filter(!str_detect(Variables, '\\_c$')) %>% 
  filter(!str_detect(Variables, '_c_')) %>% 
  filter(!str_detect(Variables, 'delta_a|asc3_a|asc3_b|asc3_c|gamma_a_nvisits|gamma_a_age|gamma_a_elec_bill|gamma_a_signed_oath|gamma_a_income')) %>% 
  LaTeX_makeup_LC_MIXL()

save_LaTeX_file(tab = tab_all_2C , 
                type_model= "LC-MIXL",
                file_name = "_2C_all.tex")


tab_all_3C <- tab_all %>% 
  select(Variables,starts_with("LC-MIXL-3")) %>% 
  filter(!Variables=="Demo.Alloc") %>% 
  filter(!str_detect(Variables, 'delta_a|asc3_a|asc3_b|asc3_c|gamma_a_nvisits|gamma_a_age|gamma_a_elec_bill|gamma_a_signed_oath|gamma_a_income')) %>% 
  LaTeX_makeup_LC_MIXL()

save_LaTeX_file(tab = tab_all_3C , 
                type_model= "LC-MIXL",
                file_name = "_3C_all.tex")


## Select best models

tab_LC_MIXL_selected <- merge(tab_all_2C,tab_all_3C, all = T) %>%
  select(Variables,
         "LC-MIXL-2-A-0","LC-MIXL-2-A-4",
         "LC-MIXL-3-A-0","LC-MIXL-3-A-2","LC-MIXL-3-A-4"  ) %>%
  dplyr::slice(match(tab_all_3C$Variables, Variables )) %>%
  filter(!`LC-MIXL-3-A-4` == "")


save_LaTeX_file(tab = tab_LC_MIXL_selected ,
                type_model= "LC-MIXL",
                file_name = "_selected.tex")




## Figures 
plot_LC_MIXL_2 <- get_LC_gof_from_tab(merge(tab_all_2C,tab_all_3C),
                                      type_model = "LC-MIXL-") %>%
  mutate(label_points = ifelse(color == FALSE,NaN,
                               format(round(value, 1), nsmall = 1))  ) %>% 
  mutate(`Information criterion` = Variables) %>% 
  mutate(Models = color) %>% 
  ggplot(aes(x=size_alloc_mo,
             y=value, )) +
  geom_line(aes(  group = `Information criterion` ,
                  linetype =`Information criterion` ),
            show.legend = T, 
            size = 0.5)+
  geom_point(aes(color = Models), 
             show.legend = T,
             size = 2) +
  geom_label(aes(label = label_points),
             hjust=0.2,
             vjust=-0.3,
             size=3.,
            alpha = 0.1 ,
            fontface = "bold")+
  scale_y_continuous(" ", position="right") +
  scale_colour_discrete(na.translate = F)+
  facet_grid(
    cols = vars(classes ),
    scales = c("free"),
    labeller=label_value,
    as.table = TRUE,
    switch="y"
  ) +
  xlab(" Number of variables in the allocation model ") +
  theme_bw() +
  guides(linetype = guide_legend(override.aes = list(shape = NA)))


plot_LC_MIXL_2
zoom_in <- 0.38

ggsave(filename = "empirical_application/LC_MIXL_models/AIC_BIC_LC_MIXL.pdf",
       plot = plot_LC_MIXL_2,
       width =  18.9*zoom_in , height =  7.57*zoom_in)

