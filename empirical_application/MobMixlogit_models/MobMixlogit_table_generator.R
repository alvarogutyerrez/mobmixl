library(mlogit)
library(partykit)
library(strucchange)
library(sandwich)
library(here)
library(modelsummary)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(dotwhisker)
library(latex2exp)
source(here::here('utils/utils_using_MobMixlogit.R'))

# Route for 
route       <- "empirical_application/MobMixlogit_models/"
# Model Names  
all_models_names = list("MobMixlogit_Normal",
                        "MobMixlogit_Triangular")

# List with the new names used in the figures.
param_new_names  <- list(
  list(var_name="cte.sq"       , new_name = "ASC Status Quo" ),
  list(var_name="cte.a"        , new_name = "ASC altern. A"  ),
  list(var_name="land"         , new_name = "Land"           ),
  list(var_name="location"     , new_name = "Location"       ),
  list(var_name="cost"         , new_name = "Cost"           ),
  list(var_name="forest"       , new_name = "Forest (mean)"  ),
  list(var_name="sd.forest"    , new_name = "Forest (sd)"    ),
  list(var_name="morbidity"    ,new_name = "Morbidity (mean)"),
  list(var_name="sd.morbidity" ,new_name = "Morbidity (sd)" )
)

# Loop over the files to create LaTeX and Figures.
tabs_and_plots <- lapply(all_models_names, function(name_model_i){
  # Load the mob_mixlogit lists  
  all_mob_mixed  <- readRDS(file = paste0(route,name_model_i,".rds"))
  # Create LaTeX tabs and figures
  LaTeX_table_and_end_leaf_plots  <- get_LaTeX_and_plots_from_mob_mixlogit(
    all_mob_mixed=all_mob_mixed,
    route = route,
    model_name = name_model_i,
    param_new_names = param_new_names)
  
  
  
})



