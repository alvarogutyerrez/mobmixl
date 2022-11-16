


get_name_LC_models <- function(LC_model_i, type_model  = "type"){
  # Create the name of the model 
  # 1) Collect information from model specification 
  
  
  get_size_alloc_model <- function(name_coef) {
    
    stringr::str_extract(string = name_coef, 
                         pattern = c("nvisit|age|elec_bill|signed_oath|income") ) %>% 
      .[!is.na(.)] %>% 
      unique() %>% length() %>% as.numeric()
  }
  # Vector of coefficients
  coef_names <- names(coef(LC_model_i$res_model$top_N_models[[1]]$final_model))
  # Size of the allocation model 
  size_alloc_model <- get_size_alloc_model(coef_names)
  # Number of Classes
  N_classes <- LC_model_i$model_description$N_classes 
  # 2) Put together the string with the model's name
  model_name <- paste0(type_model,"-",N_classes,"-A-" ,size_alloc_model)
  return(model_name)
}



format_model <- function(model, 
                         model_name = "DefaultName", 
                         N_decimals = 3){
  
  # Recover point paramters estimates
  Estimates = format(round(model$estimate, N_decimals), nsmall = N_decimals)
  # Robust Standard errors formated
  std.err.  = format(round(model$robse,    N_decimals), nsmall = N_decimals)
  # Correct NA values; replace by "  -" (The spaces are replaced created with "replicate()" to get aligned results)
  #std.err.[is.na(model$robse)] <- paste(replicate(max(nchar((std.err.))), "-"), collapse = "") # +3 bc of the spaced from format()
  std.err.[is.na(model$robse)] <- paste(replicate(nchar(max((std.err.))), " "), collapse = "") # +3 bc of the spaced from format()
  # p-values 
  # T-test against 0 (robust std.error)
  t_test <- -abs((model$estimate/model$robse))
  DoF    <- model$nObs -  sum(model$estimate!=0)
  ## P-values
  p_val  =  2*pt(t_test, df= DoF)
  
  ## stars
  symp <- symnum(p_val, 
                 corr = FALSE,
                 cutpoints = c(0    , .001, .01, .05, 1),
                 symbols = c(  "***", "**", "*", " "   ),na = FALSE)
  # Format the significant stars.
  symp_formated <- format(symp, nsmall = N_decimals,na.encode = F)
  # Add the point parameters' estimates to the table.
  df <-  data.frame(Variables = names(Estimates))
  # Concatenate results into a string.
  df$mo = cbind(paste0(Estimates,"(",std.err.,")", symp_formated))
  # Rename the model name 
  names(df)[names(df) == 'mo'] <- model_name
  
  return(df)
  
}




get_gof <- function(model, model_name ="default_name"  ) {
  
  
  f_deci <- function(metric, N_decimals = 2 ){
    format(round(metric, N_decimals), nsmall = N_decimals)
  }
  
  # N observations
  NOBS <- f_deci(model$nObs,N_decimals = 0)
  # Likelihood
  LL  <- f_deci(model$maximum)   

  # Estimated parameters.
  nFreeParams <- length(model$apollo_beta) - length(model$apollo_fixed)

  #information criterion
  
  bic  <- f_deci(metric = -2 * model$maximum + nFreeParams * log(model$nObs))
  aic  <- f_deci(metric = -2 * model$maximum + 2 * nFreeParams )
  
  # N param
  nParam <- f_deci(nFreeParams,N_decimals = 0)
  
  # create the vector with all info.
  temp <- c(NOBS, LL, nParam ,aic, bic )
  names(temp) <- c("N","LL","Num.Params" ,"AIC","BIC")
  
  # As dataframe
  gof <- as.data.frame(t(t(temp)))
  
  # Model Name
  names(gof)[names(gof) == 'V1'] <- model_name
  
  gof$Variables <- rownames(gof)
  
  rownames(gof) <- NULL
  return(gof)
  
}



get_LC_class_membership <- function(model, 
                                    model_name ="default_name", 
                                    digits = 2){
  
  
  round_df <- function(df, digits) {
    
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1)) 
    
    df[,nums] <- paste0("\\%",round(df[,nums]*100, digits = digits))
    
    (df)
  }
  LC_membership <- model$class_membership %>% 
    t() %>% 
    as.data.frame()
  
  names(LC_membership)[names(LC_membership) == 'V1'] <- model_name
  
  LC_membership$Variables <- row.names(LC_membership)
  
  LC_membership <- round_df(LC_membership,digits = digits)
  
  return(LC_membership)
}


unlist_models_into_LaTeX_table <- function(list_all_mo){
  
  # Merge all the models
  tab = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Variables", all = TRUE),list_all_mo)
  # Drom <NA's>
  tab[is.na(tab)] <- ""
  # Sort variables
  N_params_each_model <- lapply(1:length(list_all_mo), function(i){nrow(list_all_mo[[i]])})
  # Model with the largest number of parameters
  largest_model <- which.max(unlist(N_params_each_model))
  tab <-  tab %>% 
    dplyr::slice(match(list_all_mo[[ largest_model  ]]$Variables, Variables ))
  
}


LaTeX_makeup_LC <- function(tab){ 
  
  #### ASC Parameters ####
  tab[tab$Variables == "asc1_a",1] <- c("cte$_{sq,1}$")
  tab[tab$Variables == "asc2_a",1] <- c("cte$_{A ,1}$")
  #tab[tab$Variables == "asc3_a",1] <- c("cte$_{B ,1}$")
  
  tab[tab$Variables == "asc1_b",1] <- c("cte$_{sq,2}$")
  tab[tab$Variables == "asc2_b",1] <- c("cte$_{A ,2}$")
  #tab[tab$Variables == "asc3_b",1] <- c("cte$_{B ,2}$")
  
  tab[tab$Variables == "asc1_c",1] <- c("cte$_{sq,3}$")
  tab[tab$Variables == "asc2_c",1] <- c("cte$_{A ,3}$")
  #tab[tab$Variables == "asc3_c",1] <- c("cte$_{B ,3}$")
  
  # tab[tab$Variables == "asc1_d",1] <- c("cte$_{sq,4}$")
  # tab[tab$Variables == "asc2_d",1] <- c("cte$_{A ,4}$")
  # #tab[tab$Variables == "asc3_d",1] <- c("cte$_{B ,4}$")
  
  
  #### Forest Parameter ####
  tab[tab$Variables =="forest_a",1] <- c("Forest 1") 
  tab[tab$Variables =="forest_b",1] <- c("Forest 2") 
  tab[tab$Variables =="forest_c",1] <- c("Forest 3") 
  
  
  #### Morbidity parameter ####
  tab[tab$Variables =="morbidity_a",1] <- c("Morbidity 1") 
  tab[tab$Variables =="morbidity_b",1] <- c("Morbidity 2") 
  tab[tab$Variables =="morbidity_c",1] <- c("Morbidity 3") 
  
  
  #### Land parameter ####
  tab[tab$Variables =="land_a",1] <- c("Land 1") 
  tab[tab$Variables =="land_b",1] <- c("Land 2") 
  tab[tab$Variables =="land_c",1] <- c("Land 3") 
  
  
  #### Log Cost Parameter ####
  tab[tab$Variables =="cost_a",1] <- c("Cost 1") 
  tab[tab$Variables =="cost_b",1] <- c("Cost 2") 
  tab[tab$Variables =="cost_c",1] <- c("Cost 3") 
  
  
  #### Log Cost Parameter ####
  tab[tab$Variables =="location_a",1] <- c("Location 1") 
  tab[tab$Variables =="location_b",1] <- c("Location 2") 
  tab[tab$Variables =="location_c",1] <- c("Location 3") 
  
  
  
  
  #### Parameter Constants  ####
  #tab[tab$Variables =="delta_a",1] <- c("$(\\lambda_{1})$") 
  tab[tab$Variables =="delta_b",1] <- c("$\\lambda_{2}$") 
  tab[tab$Variables =="delta_c",1] <- c("$\\lambda_{3}$") 
  tab[tab$Variables =="delta_d",1] <- c("$\\lambda_{4}$") 
  
  
  
  #### Allocation Model ####
  
  #tab[tab$Variables =="gamma_a_income",1] <- c("Income $\\gamma_{1}$") 
  tab[tab$Variables =="gamma_b_income",1] <- c("Income $\\gamma_{2}$") 
  tab[tab$Variables =="gamma_c_income",1] <- c("Income $\\gamma_{3}$") 
  tab[tab$Variables =="gamma_d_income",1] <- c("Income $\\gamma_{4}$") 
  
  #tab[tab$Variables =="gamma_a_nvisit",1] <- c("Visit $\\gamma_{1}$") 
  tab[tab$Variables =="gamma_b_nvisits",1] <- c("Visit $\\gamma_{2}$") 
  tab[tab$Variables =="gamma_c_nvisits",1] <- c("Visit $\\gamma_{3}$") 
  tab[tab$Variables =="gamma_d_nvisits",1] <- c("Visit $\\gamma_{4}$") 
  
  #tab[tab$Variables =="gamma_a_age",1] <- c("Age $\\gamma_{1}$") 
  tab[tab$Variables =="gamma_b_age",1] <- c("Age $\\gamma_{2}$") 
  tab[tab$Variables =="gamma_c_age",1] <- c("Age $\\gamma_{3}$") 
  tab[tab$Variables =="gamma_d_age",1] <- c("Age $\\gamma_{4}$") 
  
  #tab[tab$Variables =="gamma_a_age",1] <- c("Age $\\gamma_{1}$") 
  tab[tab$Variables =="gamma_b_elec_bill",1] <- c("Electric bill $\\gamma_{2}$") 
  tab[tab$Variables =="gamma_c_elec_bill",1] <- c("Electric bill $\\gamma_{3}$") 
  tab[tab$Variables =="gamma_d_elec_bill",1] <- c("Electric bill $\\gamma_{4}$") 
  
  #tab[tab$Variables =="gamma_a_age",1] <- c("Age $\\gamma_{1}$") 
  tab[tab$Variables =="gamma_b_signed_oath",1] <- c("Signed oath $\\gamma_{2}$") 
  tab[tab$Variables =="gamma_c_signed_oath",1] <- c("Signed oath $\\gamma_{3}$") 
  tab[tab$Variables =="gamma_d_signed_oath",1] <- c("Signed oath $\\gamma_{4}$") 
  
  
  
  
  ## Drop parameters set to zero. 
  tab = tab[!(tab$Variables =="delta_a"),]
  tab = tab[!(tab$Variables =="asc3_a"),]
  tab = tab[!(tab$Variables =="asc3_b"),]
  tab = tab[!(tab$Variables =="asc3_c"),]
  tab = tab[!(tab$Variables =="asc3_d"),]
  
  tab = tab[!(tab$Variables =="gamma_a_income"),]
  tab = tab[!(tab$Variables =="gamma_a_nvisits"),]
  tab = tab[!(tab$Variables =="gamma_a_age"),]
  tab = tab[!(tab$Variables =="gamma_a_elec_bill"),]
  tab = tab[!(tab$Variables =="gamma_a_signed_oath"),]
  
  
  
  tab[tab$Variables =="class_a",1] <- c("$\\bar{\\pi}_{1}$") 
  tab[tab$Variables =="class_b",1] <- c("$\\bar{\\pi}_{2}$") 
  tab[tab$Variables =="class_c",1] <- c("$\\bar{\\pi}_{3}$") 
  
   
  
  
  

  return(tab)
}



get_LC_MIXL_model_info_from_file_name <- function(scriptpath){
  
  
  scriptname <- get_scriptname(scriptpath)
  
  scriptname <- gsub('.{2}$', '',get_scriptname(scriptpath))
  
  stripped_name <- strsplit(x = scriptname,split = "_")
  
  N_classes <- gsub('.{1}$', '',stripped_name[[1]][[3]])
  
  
  if      (length(stripped_name[[1]]) == 3) {alloc_model_is_cte_only <-  FALSE} 
  else if (length(stripped_name[[1]]) > 3 ) {alloc_model_is_cte_only <-  TRUE} 
  
  model_description <- list(
    N_classes               = as.numeric(N_classes),
    alloc_model_is_cte_only = alloc_model_is_cte_only
  )
  
}




identify_LC_MIXL_model <- function(scriptpath){
  

  scriptname <- get_scriptname(scriptpath)
  
  scriptname <- gsub('.{4}$', '',get_scriptname(scriptpath))
  
  stripped_name <- strsplit(x = scriptname,split = "_")
  
  N_classes <- gsub('.{1}$', '',stripped_name[[1]][[3]])
  
  
  if      (length(stripped_name[[1]]) == 3) {alloc_model_is_cte_only <-  FALSE} 
  else if (length(stripped_name[[1]]) > 3 ) {alloc_model_is_cte_only <-  TRUE} 

  model_description <- list(
    N_classes               = as.numeric(N_classes),
    alloc_model_is_cte_only = alloc_model_is_cte_only
  )
  
}


load_LC_MIXL_models <- function(){
  
  scriptpath <- get_scriptpath()
  scriptname <- get_scriptname(scriptpath)
  
  ## Current folder path for selecting the files to run
  path_current_folder <- stringr::str_remove(scriptpath, scriptname)
  
  ## Reading files in the folder
  filenames <- as.list(list.files(path       = path_current_folder,
                                  pattern    = "*.rds",
                                  full.names = TRUE ) )
  
  all_LC_mo <- lapply(filenames, FUN = function(x){
    
    # Model Description identification
    model_description <- identify_LC_MIXL_model(x)
    
    # Final model estimates
    res_model <-   readRDS(file = x)
    
    res_final <- list(
      model_description  = model_description,
      res_model          = res_model
    )
    

    
    return(res_final)
  })
  
}




LaTeX_makeup_LC_MIXL <- function(tab){ 
  
  
  #### ASC Parameters ####
  tab[tab$Variables == "asc1_a",1] <- c("cte$_{sq,1}$")
  tab[tab$Variables == "asc2_a",1] <- c("cte$_{A ,1}$")
  #tab[tab$Variables == "asc3_a",1] <- c("cte$_{B ,1}$")
  
  tab[tab$Variables == "asc1_b",1] <- c("cte$_{sq,2}$")
  tab[tab$Variables == "asc2_b",1] <- c("cte$_{A ,2}$")
  #tab[tab$Variables == "asc3_b",1] <- c("cte$_{B ,2}$")
  
  tab[tab$Variables == "asc1_c",1] <- c("cte$_{sq,3}$")
  tab[tab$Variables == "asc2_c",1] <- c("cte$_{A ,3}$")
  #tab[tab$Variables == "asc3_c",1] <- c("cte$_{B ,3}$")
  
  
  #### Forest Parameter ####
  tab[tab$Variables =="forest_mu_a",1] <- c("Forest  ($b_1$)") 
  tab[tab$Variables =="forest_mu_b",1] <- c("Forest  ($b_2$)") 
  tab[tab$Variables =="forest_mu_c",1] <- c("Forest  ($b_3$)") 
  
  tab[tab$Variables =="forest_sd_a",1] <- c("Forest  ($s_1$)") 
  tab[tab$Variables =="forest_sd_b",1] <- c("Forest  ($s_2$)") 
  tab[tab$Variables =="forest_sd_c",1] <- c("Forest  ($s_3$)") 
  
  
  
  #### Morbidity parameter ####
  tab[tab$Variables =="morbidity_mu_a",1] <- c("Morbidity ($b_1$)") 
  tab[tab$Variables =="morbidity_mu_b",1] <- c("Morbidity ($b_2$)") 
  tab[tab$Variables =="morbidity_mu_c",1] <- c("Morbidity ($b_3$)") 
  
  tab[tab$Variables =="morbidity_sd_a",1] <- c("Morbidity ($s_1$)") 
  tab[tab$Variables =="morbidity_sd_b",1] <- c("Morbidity ($s_2$)") 
  tab[tab$Variables =="morbidity_sd_c",1] <- c("Morbidity ($s_3$)") 
  
  
  #### Land parameter ####
  tab[tab$Variables =="land_a",1] <- c("Land 1") 
  tab[tab$Variables =="land_b",1] <- c("Land 2") 
  tab[tab$Variables =="land_c",1] <- c("Land 3") 
  
  
  #### Log Cost Parameter ####
  tab[tab$Variables =="cost_scale_a",1] <- c("Cost $(b_1 = s_1)$") 
  tab[tab$Variables =="cost_scale_b",1] <- c("Cost $(b_1 = s_2)$") 
  tab[tab$Variables =="cost_scale_c",1] <- c("Cost $(b_1 = s_3)$") 
  
  
  #### Log Cost Parameter ####
  tab[tab$Variables =="location_a",1] <- c("Location 1") 
  tab[tab$Variables =="location_b",1] <- c("Location 2") 
  tab[tab$Variables =="location_c",1] <- c("Location 3") 
  
  
  
  
  #### Parameter Constants  ####
  #tab[tab$Variables =="delta_a",1] <- c("$(\\lambda_{1})$") 
  tab[tab$Variables =="delta_b",1] <- c("$\\lambda_{2}$") 
  tab[tab$Variables =="delta_c",1] <- c("$\\lambda_{3}$") 
  tab[tab$Variables =="delta_d",1] <- c("$\\lambda_{4}$") 
  
  
  #### Allocation Model ####
  
  #tab[tab$Variables =="gamma_a_income",1] <- c("Income $\\gamma_{1}$") 
  tab[tab$Variables =="gamma_b_income",1] <- c("Income $\\gamma_{2}$") 
  tab[tab$Variables =="gamma_c_income",1] <- c("Income $\\gamma_{3}$") 
  tab[tab$Variables =="gamma_d_income",1] <- c("Income $\\gamma_{4}$") 
  
  #tab[tab$Variables =="gamma_a_nvisit",1] <- c("Visit $\\gamma_{1}$") 
  tab[tab$Variables =="gamma_b_nvisits",1] <- c("Visit $\\gamma_{2}$") 
  tab[tab$Variables =="gamma_c_nvisits",1] <- c("Visit $\\gamma_{3}$") 
  tab[tab$Variables =="gamma_d_nvisits",1] <- c("Visit $\\gamma_{4}$") 
  
  #tab[tab$Variables =="gamma_a_age",1] <- c("Age $\\gamma_{1}$") 
  tab[tab$Variables =="gamma_b_age",1] <- c("Age $\\gamma_{2}$") 
  tab[tab$Variables =="gamma_c_age",1] <- c("Age $\\gamma_{3}$") 
  tab[tab$Variables =="gamma_d_age",1] <- c("Age $\\gamma_{4}$") 
  
  #tab[tab$Variables =="gamma_a_age",1] <- c("Age $\\gamma_{1}$") 
  tab[tab$Variables =="gamma_b_elec_bill",1] <- c("Electric bill $\\gamma_{2}$") 
  tab[tab$Variables =="gamma_c_elec_bill",1] <- c("Electric bill $\\gamma_{3}$") 
  tab[tab$Variables =="gamma_d_elec_bill",1] <- c("Electric bill $\\gamma_{4}$") 
  
  #tab[tab$Variables =="gamma_a_age",1] <- c("Age $\\gamma_{1}$") 
  tab[tab$Variables =="gamma_b_signed_oath",1] <- c("Signed oath $\\gamma_{2}$") 
  tab[tab$Variables =="gamma_c_signed_oath",1] <- c("Signed oath $\\gamma_{3}$") 
  tab[tab$Variables =="gamma_d_signed_oath",1] <- c("Signed oath $\\gamma_{4}$") 
  ## Drop parameters set to zero. 
  tab = tab[!(tab$Variables =="delta_a"),]
  tab = tab[!(tab$Variables =="asc3_a"),]
  tab = tab[!(tab$Variables =="asc3_b"),]
  tab = tab[!(tab$Variables =="asc3_c"),]
  tab = tab[!(tab$Variables =="asc3_d"),]
  
  tab = tab[!(tab$Variables =="gamma_a_income"),]
  tab = tab[!(tab$Variables =="gamma_a_nvisits"),]
  tab = tab[!(tab$Variables =="gamma_a_age"),]
  tab = tab[!(tab$Variables =="gamma_a_elec_bill"),]
  tab = tab[!(tab$Variables =="gamma_a_signed_oath"),]
  
  
  tab[tab$Variables =="class_a",1] <- c("$\\bar{\\pi}_{1}$") 
  tab[tab$Variables =="class_b",1] <- c("$\\bar{\\pi}_{2}$") 
  tab[tab$Variables =="class_c",1] <- c("$\\bar{\\pi}_{3}$") 

  
  
  return(tab)
}















create_LaTeX_table <- function(all_LC_mo, 
                               type_model = "type",
                               write_on_disk = FALSE){
  
  
  if (!(type_model == "LC" | type_model == "LC-MIXL")) {
    stop("Only 'LC' or 'LC-MIXL' are valid inputs for 'type_model' ")
  }
  
  #------------------------------------#
  # Models' Coefficients
  list_all_mo <- lapply(seq_along(all_LC_mo),
                        FUN = function(x) { 
                          format_model(model = all_LC_mo[[x]]$res_model$top_N_models[[1]]$final_model,
                                       model_name = get_name_LC_models(LC_model_i = all_LC_mo[[x]],
                                                                       type_model = type_model), 
                                       N_decimals = 2) })
  tab <- unlist_models_into_LaTeX_table(list_all_mo)
  
  #------------------------------------#
  # Models' GoF 
  list_all_gof <- lapply(seq_along(all_LC_mo),
                         FUN = function(x) {
                           get_gof(model = all_LC_mo[[x]]$res_model$top_N_models[[1]]$final_model,
                                   model_name =  get_name_LC_models(LC_model_i = all_LC_mo[[x]],
                                                                    type_model = type_model)) }   
  )
  
  
  
  tab_gof <- unlist_models_into_LaTeX_table(list_all_gof)
  
  #------------------------------------#
  # Models' Class Membership 
  list_all_class_membership <- lapply(seq_along(all_LC_mo),
                                      FUN = function(x) {
                                        
                                        get_LC_class_membership(model = all_LC_mo[[x]]$res_model$top_N_models[[1]],
                                                                model_name =  get_name_LC_models(LC_model_i = all_LC_mo[[x]],
                                                                                                 type_model = type_model ))} 
  )
  
  tab_class_membership <- unlist_models_into_LaTeX_table(list_all_class_membership)
  
  
  #------------------------------------#
  # Final Table to LaTeX
  
  if   (type_model == "LC")         {LaTeX_makeup <- LaTeX_makeup_LC}
  else if (type_model == "LC-MIXL") {LaTeX_makeup <- LaTeX_makeup_LC_MIXL  }
  
  tab <- tab %>% 
    rbind(tab_gof) %>% 
    rbind(tab_class_membership) %>% 
    LaTeX_makeup()  %>% 
    select(order(colnames(tab))) %>% # sort columns names
    select("Variables", everything()) # put variables at the front
  
  
  if(write_on_disk == TRUE){
    ## Save to latex
    tab_to_latex<- print.xtable(xtable(tab),
                                include.rownames=F,
                                sanitize.text.function = function(x){x},
                                floating = F)
    
    if      (type_model == "LC")      {folder <- "LC"}
    else if (type_model == "LC-MIXL") {folder <- "LC_MIXL"  }
    
    route <- paste0("empirical_application/",folder,"_models/")
    fileConn <-file(paste0(route,folder,"_models_estimates.tex"))
    writeLines(tab_to_latex, fileConn)
    close(fileConn)
    
  }
  
  
  return(tab)
}




get_LC_MIXL_model_specs<- function(){
  
  res <- list(
    N_draws       = 5000,
    nCandidates   = 50  ,
    N_best_models = 3   ,
    max_iter_search_vals  = 300,
    max_iter_final_models = 400,
    N_cores = 45

    )
  return(res)
  
}




#### partykit + mlogit combo ####
fit_mob_mnl_LC_MIXL_data <- function(y, 
                                     x = NULL, 
                                     start = NULL, 
                                     weights = NULL, 
                                     offset = NULL,
                                             ...) {
  #cbind() x and y
  yx <- cbind(y,x)
  #First declare dfidx data 
  d <- dfidx(data = yx, 
             shape = "wide", 
             choice = "choice_wide",
             varying = 2:7, ## first if "y", then "x's" and finally "identifiers". 
             sep = "_",
             idx = list(c("id", "id_ind")),   
             idnames = c(NA, "altern"))
  
  # fit mlogit using d
  clogit_dfidx <- mlogit(formula = as.formula(choice_wide ~ x1 + x2 |0) ,  ##+0 to drop intercepts
                         data    = d,
                         panel = FALSE,
                         method = 'bhhh')  
  
  #clogit_dfidx$model <- clogit_dfidx$model %>% dplyr::select(dplyr::starts_with("idx") )
  
  return(clogit_dfidx)
}




LaTeX_makeup_LC_MIXL_3 <- function(tab){
  ## Drop parameters set to zero. 
  tab = tab[!(tab$Variables =="delta_a"),]
  tab = tab[!(tab$Variables =="gamma_a_z1"),]
  tab = tab[!(tab$Variables =="gamma_a_z2"),]
  tab = tab[!(tab$Variables =="gamma_a_z3"),]
  tab = tab[!(tab$Variables =="gamma_a_z4"),]
  tab = tab[!(tab$Variables =="gamma_a_z5"),]
  
  #### ASC Parameters ####
  tab[tab$Variables == "x1_mu_a",1] <- c("$x_{1} (\\mu_{1})$")
  tab[tab$Variables == "x1_mu_b",1] <- c("$x_{1} (\\mu_{2})$")
  tab[tab$Variables == "x1_mu_c",1] <- c("$x_{1} (\\mu_{3})$")
  
  tab[tab$Variables == "x1_sd_a",1] <- c("$x_{1} (\\sigma_{1}) $")
  tab[tab$Variables == "x1_sd_b",1] <- c("$x_{1} (\\sigma_{2}) $")
  tab[tab$Variables == "x1_sd_c",1] <- c("$x_{1} (\\sigma_{3}) $")
  
  
  tab[tab$Variables == "x2_mu_a",1] <- c("$x_{2} (\\mu_{1}) $")
  tab[tab$Variables == "x2_mu_b",1] <- c("$x_{2} (\\mu_{2}) $")
  tab[tab$Variables == "x2_mu_c",1] <- c("$x_{2} (\\mu_{3}) $")
  
  tab[tab$Variables == "x2_sd_a",1] <- c("$x_{2} (\\sigma_{1}) $")
  tab[tab$Variables == "x2_sd_b",1] <- c("$x_{2} (\\sigma_{2}) $")
  tab[tab$Variables == "x2_sd_c",1] <- c("$x_{2} (\\sigma_{3}) $")
  
  
  tab[tab$Variables == "delta_a",1] <- c("$\\lambda_{1}$")
  tab[tab$Variables == "delta_b",1] <- c("$\\lambda_{2}$")
  tab[tab$Variables == "delta_c",1] <- c("$\\lambda_{3}$")
  
  tab[tab$Variables == "gamma_a_z1",1] <- c("$Z_1 (\\gamma_{1})$")
  tab[tab$Variables == "gamma_a_z2",1] <- c("$Z_2 (\\gamma_{1})$")
  tab[tab$Variables == "gamma_a_z3",1] <- c("$Z_3 (\\gamma_{1})$")
  tab[tab$Variables == "gamma_a_z4",1] <- c("$Z_4 (\\gamma_{1})$")
  tab[tab$Variables == "gamma_a_z5",1] <- c("$Z_5 (\\gamma_{1})$")
  tab[tab$Variables == "gamma_b_z1",1] <- c("$Z_1 (\\gamma_{2})$")
  tab[tab$Variables == "gamma_b_z2",1] <- c("$Z_2 (\\gamma_{2})$")
  tab[tab$Variables == "gamma_b_z3",1] <- c("$Z_3 (\\gamma_{2})$")
  tab[tab$Variables == "gamma_b_z4",1] <- c("$Z_4 (\\gamma_{2})$")
  tab[tab$Variables == "gamma_b_z5",1] <- c("$Z_5 (\\gamma_{2})$")
  tab[tab$Variables == "gamma_c_z1",1] <- c("$Z_1 (\\gamma_{3})$")
  tab[tab$Variables == "gamma_c_z2",1] <- c("$Z_2 (\\gamma_{3})$")
  tab[tab$Variables == "gamma_c_z3",1] <- c("$Z_3 (\\gamma_{3})$")
  tab[tab$Variables == "gamma_c_z4",1] <- c("$Z_4 (\\gamma_{3})$")
  tab[tab$Variables == "gamma_c_z5",1] <- c("$Z_5 (\\gamma_{3})$")
  
  tab[tab$Variables =="class_a",1] <- c("$\\bar{\\pi}_{1}$") 
  tab[tab$Variables =="class_b",1] <- c("$\\bar{\\pi}_{2}$") 
  tab[tab$Variables =="class_c",1] <- c("$\\bar{\\pi}_{3}$") 
  
  return(tab)
}


create_LaTeX_table_LC_MIXL_on_MOB_data <- function(LC_MIXL_3C_models,
                                                   model_name = "default_model_name",
                                                   file_name = "default_name" ,
                                                   write_to_disk = FALSE){
  
  list_all_mo <- lapply(seq_along(LC_MIXL_3C_models[[1]]), 
                        FUN = function(i){
                          
                          format_model(model = LC_MIXL_3C_models[[1]][[i]]$final_model,
                                       model_name = paste0(model_name,i))
                          
                        })
  
  
  tab <- unlist_models_into_LaTeX_table(list_all_mo)
  
  
  # Models' GoF 
  list_all_gof <- lapply(seq_along(LC_MIXL_3C_models[[1]]),
                         FUN = function(i) {
                           get_gof(model = LC_MIXL_3C_models[[1]][[i]]$final_model,
                                   model_name = paste0(model_name,i) ) }   
  )
  
  
  tab_gof <- unlist_models_into_LaTeX_table(list_all_gof)
  
  # Models' Class Membership 
  list_all_class_membership <- lapply(seq_along(LC_MIXL_3C_models[[1]]),
                                      FUN = function(i) {
                                        get_LC_class_membership(model = LC_MIXL_3C_models[[1]][[i]],
                                                                model_name = paste0(model_name,i)  )} 
  )
  
  tab_class_membership <- unlist_models_into_LaTeX_table(list_all_class_membership)
  
  
  tab <- tab %>% 
    rbind(tab_gof) %>% 
    rbind(tab_class_membership) %>% 
    LaTeX_makeup_LC_MIXL_3()
  
  print(tab)
  
  if (write_to_disk) {
    tab_to_latex<- print.xtable(xtable(tab),
                                include.rownames=F,
                                sanitize.text.function = function(x){x},
                                floating = F)
    
    fileConn <-file(paste0("simulations_replication/LCMIXL_on_MOB_data/",file_name,".tex"))
    writeLines(tab_to_latex, fileConn)
    close(fileConn)
    
  }
  
  return(tab)
  
}



## Summary function inspired by: 
## https://stackoverflow.com/questions/65495322/partykit-modify-terminal-node-to-include-standard-deviation-and-significance-of/65500344#65500344
fn_summary_for_partykit_plots <- function(info, digits = 2) {
  n <- info$nobs
  individuals <- length(unique(info$object$model$idx$id))
  c(paste("T =", n),
    paste("N =", individuals))
  
}

get_party_plots <- function(partykit_obj, 
                            route, 
                            plot_name,
                            width =  9.20,
                            height =4.97  ){
  
  pdf(file   = paste0(route,plot_name),
      width  = width, 
      height = height )
  
  plot(partykit_obj,
       terminal_panel = node_terminal,
       tp_args = list(FUN = fn_summary_for_partykit_plots,
                      fill = "white"))
  
  dev.off()
  
}


.coef_rename_mob_mixlogit = c("cte.sq" = "cte$_{sq}$",
                "cte.a" = "cte$_{A}$",
                
                "location" = "Location",
                "land" = "Land ",
                
                "forest" = "Forest $(\\mu)$",
                "sd.forest" = "Forest $(\\sigma)$",
                
                "morbidity" = "Morbidity $(\\mu)$",
                "sd.morbidity" = "Morbidity $(\\sigma)$",
                
                "cost" = "Cost $(\\mu)$",
                "sd.cost" = "Cost $(\\sigma)$")




gen_dwplot <- function(pid_models, 
                       var_name = "cte.sq" , 
                       new_name = "new_name"  ){
  
  
  list_with_parameters <- lapply(names(pid_models), function(x){
    broom::tidy(pid_models[[x]]) %>% 
      filter(term == var_name) %>% 
      mutate(model = paste0("Node ",x))})
  
  
  tibble_with_parameters <-  do.call(rbind,list_with_parameters)
  
  g.parameter <- dwplot(tibble_with_parameters,
                        vline = geom_vline(
                          xintercept = 0,
                          colour = "grey60",
                          linetype = 2)) %>%
    relabel_predictors(
      c(var_name = new_name)
    ) +
    geom_vline(xintercept = 0,
               colour = "grey60",
               linetype = 2) +
    xlab(new_name) +  
    theme(
      plot.title = element_text(face = "bold"),
      legend.title = element_blank(),
      axis.text.y = element_blank()
      
    ) 
  
  return(g.parameter)
}





get_gof_mob_mixlogit <- function(x){
  nobs <-   nrow(x$gradient)
  LL   <- as.numeric(logLik(x))
  LL_0 <- attr(logLik(x), "null")
  nParam <- length(coef(x))
  aic  <- -2 * LL + 2 * nParam
  bic  <- -2 * LL + nParam * log(nobs)
  
  return(list(
    Num.Obs.   = nobs,
    Log.Lik.   = round( LL, digits = 2 ),
    Num.Param. = nParam,
    AIC        = round(aic,digits = 2),
    BIC        = round(bic, digits = 1)  ))
}


get_LaTeX_and_plots_from_mob_mixlogit <- function(all_mob_mixed,
                                                  route,
                                                  model_name,
                                                  param_new_names
){
  
  # Get the model with largest number of draws
  largest_model <- tail(all_mob_mixed,n = 1)
  # Select the partykit objet that contains the tree.
  mob_mixed <- largest_model[[1]]$MOB_MIXL
  
  #### Pre-pruning plot ####
  get_party_plots(partykit_obj = mob_mixed, 
                  route = route, 
                  plot_name = paste0(model_name,"_PRE_prunning.pdf"))
  
  #### PRUNING ####
  mob_mixed <- prune.modelparty(mob_mixed,  type = "BIC")
  
  #### Post-pruning plot ####
  get_party_plots(partykit_obj = mob_mixed, 
                  route = route, 
                  plot_name = paste0(model_name,"_POST_prunning.pdf"))
  
  #### Extract information from the nodes
  pid_models <- nodeapply(mob_mixed,
                          ids = nodeids(mob_mixed,
                                        terminal = TRUE),
                          FUN = function(x) x$info$object)
  
  root_node <- nodeapply(mob_mixed,
                         ids = 1,
                         FUN = function(x) x$info$object)
  
  pid_models$`1` <- root_node$`1`
  
  
  
  # Get GOF for each node.
  gof_all_mob <- lapply(pid_models, FUN = get_gof_mob_mixlogit) %>% 
    sapply(c) %>% 
    t() %>% t() %>%   
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Variables")
    
  

  # Goodness of Fit Full Model vs. Tree 
  nParam <- length(coef(root_node$`1`))
  LL   <- as.numeric(logLik(root_node$`1`))
  nobs<- nrow(root_node$`1`$gradient)
  bic.fullsample  <- -2 * LL + nParam * log(nobs)
  aic.fullsample  <- -2 * LL + 2 * nParam
  
  nParam <- length(coef(mob_mixed))
  LL_mob   <- as.numeric(logLik(mob_mixed))
  bic.tree  <- -2 * LL_mob + nParam * log(nobs)
  aic.tree  <- -2 * LL_mob + 2 * nParam
  
  
  cat("\n", "Goodness of Fit Full Model vs. Tree for :", model_name , " distribution ", "\n")
  gof_comparisson  <- data.frame(
    AIC = c(aic.fullsample, aic.tree , aic.fullsample - aic.tree),
    BIC = c(bic.fullsample, bic.tree , bic.fullsample - bic.tree  )  )
    
  gof_comparisson <- gof_comparisson %>%
    t() %>% 
    `colnames<-`(c("MIXL", "MOB-MIXL", "Improvement"))
  
  print(gof_comparisson)
  cat("\n")
  

  # Compute clustered corrected std.errors.
  vcov_to_msummary <- lapply(pid_models,
                             FUN = function(x){
    cluster_var <- x$model$idx$id[!duplicated(x$model$idx$id_choice) ]
    return(sandwich::vcovCL(x, cluster = cluster_var ))
  } )


  table_msummary <- modelsummary::msummary(pid_models,
                                           gof_omit = ".*",
                                           coef_map = .coef_rename_mob_mixlogit,
                                           #estimate =  "{estimate} ({std.error}){stars}" ,
                                           #statistic = NULL, 
                                           stars = c('*' = .1, '**' = .05, '***'= 0.01),
                                           fmt = 2 ,
                                           output = 'latex',
                                           vcov = vcov_to_msummary,
                                           escape =  FALSE,
                                           add_rows = gof_all_mob)
  

  
  
  writeLines(table_msummary, paste0(route,model_name,".tex"))
  
  
  #### Create Plots for Every end Leaf####
  param_plots <- lapply(seq_along(param_new_names), function(i){
    
    plot_i <- gen_dwplot(pid_models, 
                         var_name = param_new_names[[i]]$var_name , 
                         new_name = param_new_names[[i]]$new_name )
    
  })
  
  plots_end_leaves <- ggarrange(plotlist = param_plots, 
                                ncol = 2, 
                                nrow = 5,
                                common.legend = TRUE, 
                                legend = "bottom")
  
  
  ##### Collapse every graph into a single one ####
  ggsave(filename = paste0(route,"end_leaves_fix_param_",model_name,".pdf"),
         plot   = plots_end_leaves,
         width =  12.09 * 0.4 , 
         height =  15.57 *0.4)
  
  
  
  res <- list(table_msummary = table_msummary,
              plots_end_leaves = plots_end_leaves)
  
  return(res)
}


get_partitions_vars <- function(fitted_tree){
  # Get node id for the entire tree 
  node_ids <- nodeids(fitted_tree,terminal = FALSE)
  node_ids_terminals <- nodeids(fitted_tree,terminal = TRUE)
  
  
  node_id_non_terminal_nodes <- setdiff(node_ids,node_ids_terminals)
    
  # Recover the variables that were used as partitions
  partition_vars <- lapply(node_id_non_terminal_nodes , 
                           function(i){
                             # Select the p.values for each node
                             p_vals  <- sctest(fitted_tree, node = i) %>%
                               as.data.frame() %>%
                               tibble::rownames_to_column() %>%
                               filter(rowname == "p.value") 
                             # Select the partition variable for each node 
                             partition_var <- colnames(p_vals)[apply(p_vals,1,which.min)]
                           }) 
  
  return(partition_vars)
}



unlist_mob_simulations <- function(res){
  library(data.table)
  
  
  tmp = data.table(res)
  tmp = tmp[, t(res[1]), by=1:nrow(tmp)]
  
  tmp = tmp[,{ class(V1[[1]]$z1_correct) <- 'double'
  class(V1[[1]]$xi_error) <- 'double'
  V1[[1]]
  }, by=1:nrow(tmp)]
  
  df <- as.data.frame(tmp)
  #Turning variables into factors
  df$n.choices <- as.factor(df$n.choices)
  
  ## Generate the facet_grid LaTeX headers
  df$xi <-    as.factor(df$xi)
  levels(df$xi) <- c("0.5" = TeX("$\\xi = 0.5$"),
                     "0.8" = TeX("$\\xi = 0.8$"))
  
  df$delta <- as.factor(df$delta)
  levels(df$delta) <- c("0.5" = TeX("$\\delta = 0.5$"),
                        "1" = TeX("$\\delta = 1$"))
  
  df$scenario <- as.factor(df$scenario)
  levels(df$scenario) <- c("stump" = "Stump", 
                           "tree" = "Tree")
  
  df$n.indiv <- as.factor(df$n.indiv)
  
  return(df)
  
}




get_number_of_degenerate_classes <- function(df,class_var ){
  df %>% 
    dplyr::select({{ class_var }}) %>% 
    dplyr::mutate(class_var_rounded = round({{ class_var }}, digits = 1)) %>%
    dplyr::count(class_var_rounded ) %>% 
    dplyr::summarise(degenerate_class = sum(n[class_var_rounded == 1 | class_var_rounded == 0 ])) %>%
    as.numeric()
}

get_number_times_max_likelihood<- function(df,LL_var){
  df %>% 
    dplyr::select( {{ LL_var }} ) %>% 
    dplyr::mutate(LL_rounded = round(LL, digits = 0)) %>%
    dplyr::count(LL_rounded) %>% 
    dplyr::summarise(time_max = sum(n[LL_rounded == max(LL_rounded)])) %>% 
    as.numeric()
}


get_model_fitting_complexity <- function(all_LC_mo, 
                                         type_model = "detault_type_model",
                                         write_on_disk = FALSE){
  
  if (!(type_model == "LC" | type_model == "LC-MIXL")) {
    stop("Only 'LC' or 'LC-MIXL' are valid inputs for 'type_model' ")
  }
  
  
  model_fitting_complexity <- purrr::map(seq_along(all_LC_mo), function(x){
    
    df <- all_LC_mo[[x]]$res_model$fitted_possible_candidates
    
    n_trails <- nrow(df)
    
    model_name     <- get_name_LC_models(LC_model_i = all_LC_mo[[x]], 
                                         type_model = type_model)
    
    degen_class    <- get_number_of_degenerate_classes(df,class_a) / n_trails
    times_max_like <- get_number_times_max_likelihood(df, LL) / n_trails
    
    data.frame(c(model_name),c(degen_class),c(times_max_like)) %>% 
      `colnames<-`(c("Model", "Degenerate classes (\\%)", "Best likelihood (\\%)"))
    
  }) %>% 
    bind_rows() 
  
  
  if (write_on_disk){
    ## Save to latex
    tab_to_latex<- print.xtable(xtable(model_fitting_complexity),
                                include.rownames=F,
                                sanitize.text.function = function(x){x},
                                floating = F)
    
    if      (type_model == "LC")      {folder <- "LC"}
    else if (type_model == "LC-MIXL") {folder <- "LC_MIXL"  }
    
    route <- paste0("empirical_application/",folder,"_models/")
    fileConn <-file(paste0(route,folder,"_model_fitting_complexity.tex"))
    writeLines(tab_to_latex, fileConn)
    close(fileConn)
  }
  
  
  return(model_fitting_complexity)
  
}




get_time_models <- function(all_LC_mo, 
                            type_model = "detault_type_model"){
  
  if (!(type_model == "LC" | type_model == "LC-MIXL")) {
    stop("Only 'LC' or 'LC-MIXL' are valid inputs for 'type_model' ")
  }
  
  lapply(1:length(all_LC_mo),function(x){
    
    
    model_name     <- get_name_LC_models(LC_model_i = all_LC_mo[[x]], 
                                         type_model = type_model)
    
    mean_hours_taken <- mean(all_LC_mo[[x]]$res_model$fitted_possible_candidates$time_taken)/60
    
    data.frame(c(model_name),c(mean_hours_taken)) %>% 
      `colnames<-`(c("Model", "Time taken (Minutes)"))
    
    
    
  })  %>% 
    bind_rows() 
  
}



get_size_alloc_model <- function(name_coef) {
  
  stringr::str_extract(string = name_coef, 
                       pattern = c("nvisit|age|elec_bill|signed_oath|income") ) %>% 
    .[!is.na(.)] %>% 
    unique() %>% length() %>% as.numeric()
}



create_comparisson_LC_table <- function(all_LC_mo, 
                                        type_model = "type",
                                        write_on_disk = FALSE){
  
  
  if (!(type_model == "LC" | type_model == "LC-MIXL")) {
    stop("Only 'LC' or 'LC-MIXL' are valid inputs for 'type_model' ")
  }
  
  
  
  #------------------------------------#
  # Models' GoF 
  list_all_gof <- lapply(seq_along(all_LC_mo),
                         FUN = function(x) {

                           tab_param <- format_model(model = all_LC_mo[[x]]$res_model$top_N_models[[1]]$final_model,
                                                     model_name = get_name_LC_models(LC_model_i = all_LC_mo[[x]],
                                                                                     type_model = type_model), N_decimals = 2) %>%
                             relocate(Variables, .after = last_col())
                           
                           tab_gof <- get_gof(model = all_LC_mo[[x]]$res_model$top_N_models[[1]]$final_model,
                                              model_name =  get_name_LC_models(LC_model_i = all_LC_mo[[x]],
                                                                               type_model = type_model)) 
                           
                           
                           
                           tab_member<- get_LC_class_membership(model = all_LC_mo[[x]]$res_model$top_N_models[[1]],
                                                                model_name =  get_name_LC_models(LC_model_i = all_LC_mo[[x]],
                                                                                                 type_model = type_model ) )
                           
                           rownames(tab_member) <- NULL
                           
                           res <- rbind(tab_param,tab_gof,tab_member)
                           
                           ## Size of the allocation model
                           coef_names <- names(coef(all_LC_mo[[x]]$res_model$top_N_models[[1]]$final_model))
                           size_alloc_model <- get_size_alloc_model(coef_names)
                           extra_df <- data.frame(size_alloc_model,"Demo.Alloc")
                           names(extra_df)<- names(res) 
                           
                           res  <- rbind(res,extra_df)
                           
                           
                           
                           
                           return(res)
                         }   
  )
  
  # Get 
  # tab_gof <- unlist_models_into_LaTeX_table(list_all_gof) %>%
  #    tidyr::pivot_longer(-Variables) %>% 
  #    tidyr::pivot_wider(names_from  = Variables, values_from = value) 
  # 
  
  tab_gof <- unlist_models_into_LaTeX_table(list_all_gof)
  #------------------------------------#
  # Final Table to LaTeX
  
  
  return(tab_gof)
}




save_LaTeX_file<- function(tab, type_model,file_name){
  
  if (!(type_model == "LC" | type_model == "LC-MIXL")) {
    stop("Only 'LC' or 'LC-MIXL' are valid inputs for 'type_model' ")
  }
  
  tab_to_latex<- print.xtable(xtable(tab),
                              include.rownames=F,
                              sanitize.text.function = function(x){x},
                              floating = F)
  
  
  if      (type_model == "LC")      {folder <- "LC"}
  else if (type_model == "LC-MIXL") {folder <- "LC_MIXL"  }
  
  route <- paste0("empirical_application/",folder,"_models/")
  fileConn <-file(paste0(route,folder,file_name))
  writeLines(tab_to_latex, fileConn)
  close(fileConn)
}


get_LC_gof_from_tab <- function(tab_res,type_model = "LC-MIXL-"){
  
  tab_res %>%  
    filter(Variables=="AIC" |Variables=="BIC" ) %>% 
    tidyr::pivot_longer(-Variables) %>% 
    dplyr::mutate(value = as.numeric(value)) %>% 
    dplyr::mutate(classes = case_when(
      str_detect(name,pattern = paste0(type_model,"2-A")) ~paste0(type_model,"2"),
      str_detect(name,pattern = paste0(type_model,"3-A")) ~paste0(type_model,"3"),
      TRUE ~ NA_character_  )) %>% 
    dplyr::mutate(size_alloc_mo = str_sub(name, -1)) %>%
    dplyr::group_by(Variables, classes) %>% 
    dplyr::mutate(color = ifelse(test = (min(value) == value),
                                 yes = "Best model fit",
                                 no = NA)  ) %>%  #This allows me to highlight the smaller value
    dplyr::mutate(color = ifelse(test = (size_alloc_mo == 0),
                                 yes = "Constant only",
                                 no = color)  )
  
}



get_plot_LC_models <- function(df, type_model = "default_name"){
  df %>% 
    filter(classes ==type_model) %>% 
    ggplot(aes(x=size_alloc_mo,
               y=value,
               group = Variables ,
               color = Variables )) +
    geom_line(size = .75)+
    geom_point(aes(color = color),size = 1.5) +
    scale_y_continuous(" ", position="right") +
    scale_color_manual(values = c("black", "black","black", "red"))+
    facet_grid(
      rows = vars(Variables),
      cols = vars(classes),
     # scales = c("free"),
      labeller=label_value,
      as.table = TRUE,
      switch="y"
    ) +
    xlab(" ") +
    theme_bw() +
    theme(strip.text.x = element_text(size = 10, colour = "black"),
          strip.text.y = element_text(size = 10, colour = "black"))+
    theme(legend.position="none") 
  
}
