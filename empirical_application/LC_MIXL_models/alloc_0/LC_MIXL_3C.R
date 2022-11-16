library(MobMixlogit)
library(stringr)
source(here::here('utils/utils_using_MobMixlogit.R'))

#Loading the data
data_wide <- (function(...)get(data(...,envir = new.env(),
                                    package ="MobMixlogit")))("data_DeLaMaza_2021")


model_description <- get_LC_MIXL_model_info_from_file_name(get_scriptpath())

# Starting values
MNL_estimates <- MNL_empirical(data_wide = data_wide)

# Model specs
mo_specs <- get_LC_MIXL_model_specs(N_classes = model_description$N_classes)

# Allocation model 
alloc_model_vars =  c(" ")

alloc_size <- length(alloc_model_vars)



# Model estimation
mo <- LC_empirical(

  LC_MIXL = TRUE,
  data_wide = data_wide,
  init_values_for_classes = MNL_estimates,

  # Model estimation specs
  N_cores               = mo_specs$N_cores,
  N_draws               = mo_specs$N_draws,
  nCandidates           = mo_specs$nCandidates,
  max_iter_search_vals  = mo_specs$max_iter_search_vals,
  max_iter_final_models = mo_specs$max_iter_final_models,
  N_best_models         = mo_specs$N_best_models,
  outliers = 100,

  # Info from the file name
  N_classes = model_description$N_classes,
  alloc_model_is_cte_only = TRUE,
  alloc_model_vars =  alloc_model_vars
)


# Recovering name of the model from the file name
model_name_plus_extension <- get_scriptname(get_scriptpath())
# Only model name (dropping ".R")
model_name <- stringr::str_sub(string = model_name_plus_extension,end    = -3)
# write on disk
saveRDS(
  object = mo,
  file   = paste0("empirical_application/LC_MIXL_models/alloc_",alloc_size,"/",model_name,"_alloc_",alloc_size,".rds")
)



