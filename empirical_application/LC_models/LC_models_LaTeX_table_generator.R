library(here)
library(stringr)
library(MobMixlogit)
library(tidyr)
library(xtable)
library(ggplot2)
library(gridExtra)
source(here::here('utils/utils_using_MobMixlogit.R'))


## Current folder path for selecting the files to run
path_current_folder <- "empirical_application/LC_models/"


filenames <- as.list(list.files(path       = path_current_folder,
                                #pattern    = "\\.rds$",
                                pattern    = "fitted|\\.rds$",
                                full.names = TRUE ) )
filenames

all_LC_mo <- lapply(filenames, readRDS)


all_gof <- lapply(1:5, function(x){create_comparisson_LC_table(all_LC_mo = all_LC_mo[[x]], 
                                                               type_model =  "LC")})





tab_all = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Variables",all= T),all_gof) %>% 
   .[!duplicated(as.list(.))] %>% 
   select(!contains(".x")) %>%  
   select(!contains(".y")) %>% 
   dplyr::slice(match( all_gof[[5]]$Variables, Variables )) 
 
 
tab_all_2C <- tab_all %>% 
   select(Variables,starts_with("LC-2")) %>% 
   filter(!Variables=="Demo.Alloc") %>% 
   filter(!str_detect(Variables, '\\_c$')) %>% 
   filter(!str_detect(Variables, '_c_')) %>% 
   filter(!str_detect(Variables, 'delta_a|asc3_a|asc3_b|asc3_c|gamma_a_nvisits|gamma_a_age|gamma_a_elec_bill|gamma_a_signed_oath|gamma_a_income')) %>% 
   relocate(Variables,"LC-2-A-0") %>% 
   LaTeX_makeup_LC
 

save_LaTeX_file(tab = tab_all_2C , 
                type_model= "LC",
                file_name = "_2C_all.tex")
  

tab_all_3C <- tab_all %>% 
   select(Variables,starts_with("LC-3")) %>% 
   filter(!Variables=="Demo.Alloc") %>% 
   filter(!str_detect(Variables, 'delta_a|asc3_a|asc3_b|asc3_c|gamma_a_nvisits|gamma_a_age|gamma_a_elec_bill|gamma_a_signed_oath|gamma_a_income')) %>% 
   relocate(Variables,"LC-3-A-0") %>% 
   LaTeX_makeup_LC
 
save_LaTeX_file(tab = tab_all_3C , 
                type_model= "LC",
                file_name = "_3C_all.tex")



## Select best models

tab_LC_selected <- merge(tab_all_2C,tab_all_3C, all = T) %>%
  select(Variables,
         "LC-2-A-0","LC-2-A-4",
         "LC-3-A-0","LC-3-A-2","LC-3-A-4"  ) %>%
  dplyr::slice(match(tab_all_3C$Variables, Variables )) %>%
  filter(!`LC-3-A-4` == "")


save_LaTeX_file(tab = tab_LC_selected ,
                type_model= "LC",
                file_name = "_selected.tex")


## Figures
plot_LC <- get_LC_gof_from_tab(merge(tab_all_2C,tab_all_3C),
                                      type_model = "LC-") %>%
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
             show.legend = F,
             hjust=0.2,
             vjust=-0.3,
             size=3.,
             alpha = 0.5 ,
             position = position_dodge(0),
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


plot_LC

zoom_in <- 0.38
ggsave(filename = "empirical_application/LC_models/AIC_BIC_LC.pdf",
       plot = plot_LC,
       width =  20.9*zoom_in , height =  7.57*zoom_in)



