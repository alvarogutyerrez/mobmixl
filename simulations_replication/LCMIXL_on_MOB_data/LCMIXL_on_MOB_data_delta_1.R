library(MobMixlogit)
# Degree of overlap
delta <- 1
# Simulate data in long format
data_long <- MobMixlogit::dgp_tree(N = 1500,
                                   t = 10  ,
                                   J = 3   ,
                                   delta = delta,
                                   xi = 0.5 )%>%
  dplyr::rename(choice_long = chosen) %>%
  dplyr::rename(id_choice = id_choice_situation_of_individual_n)

# transform it into wide format (for apollo)
data_wide <- MobMixlogit::long_to_wide(data_long)

#MNL estimates to be used as starting values 
mnl_estimates <- MobMixlogit::MNL_sim(data_wide)

# LC-MIXL estimates
LC_MIXL_3C_models <- MobMixlogit::LC_MIXL_3_sim(
  data_wide = data_wide,
  N_best_models = 3, 
  N_cores = 30,
  init_values_for_classes = mnl_estimates,
  N_draws = 500,
  nCandidates = 30,
  outliers = 200,
  max_iter_search_vals  = 300,
  max_iter_final_models = 400  
  )


delta_name <- stringr::str_replace(delta, "[.]", "")

saveRDS(
  object = LC_MIXL_3C_models,
  file = paste0("simulations_replication/LCMIXL_on_MOB_data/LCMIXL_on_MOB_data_delta_",delta_name,".rds")
  )
