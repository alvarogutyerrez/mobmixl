library(MobMixlogit)
#Loading the data
data_wide <- (function(...)get(data(...,envir = new.env(),package ="MobMixlogit")))("data_DeLaMaza_2021")
#Starting values
MNL_estimates <- MNL_empirical(data_wide = data_wide)
# Model setup
N_cores  <- 1
outliers <- 100
nCandidates <- 100
N_best_models <- 5
max_iter_final_models <- 300
max_iter_search_vals  <- 100 
# Create the combination of models I need.
type_of_models <- expand.grid(N_classes = c(2,3), alloc_model_is_cte_only = c(TRUE,FALSE))

N_models <- nrow(type_of_models)

# Variables selected based on the MOB algorithm diagnostic
alloc_model_vars = c("nvisits" ,
                     "age" ,
                     "elec_bill" ,
                     "signed_oath",
                     "income"
)

for (id_alloc_model  in 1:length(alloc_model_vars)) {
  
  # Run for all the models. 
  fitted_LC_models <- lapply(1:N_models, 
                             function(i){
                               # Store the model desciption 
                               model_description <- list(
                                 N_classes               = type_of_models$N_classes[i],
                                 alloc_model_is_cte_only = type_of_models$alloc_model_is_cte_only[i]
                               )
                               
                               # Estimate LC model
                               res_model <- MobMixlogit::LC_empirical(
                                 data_wide = data_wide,
                                 #Change N_class and if allocation model is cte_only or not.
                                 N_classes = type_of_models$N_classes[i],
                                 alloc_model_is_cte_only = type_of_models$alloc_model_is_cte_only[i],    
                                 LC_MIXL = FALSE,
                                 init_values_for_classes =  MNL_estimates,
                                 N_cores = N_cores,
                                 nCandidates = nCandidates,
                                 N_best_models = N_best_models,
                                 outliers = outliers,
                                 max_iter_search_vals = max_iter_search_vals,
                                 max_iter_final_models =  max_iter_final_models,
                                 alloc_model_vars = alloc_model_vars[1:id_alloc_model]
                               )
                               
                               # Save model plus description 
                               res_final <- list(
                                 model_description  = model_description,
                                 res_model          = res_model
                               ) 
                               
                               return(res_final)})
  
  saveRDS(
    object = fitted_LC_models,
    file   = paste0("empirical_application/LC_models/fitted_LC_models_alloc_",id_alloc_model,".rds")
  ) 
  
}
