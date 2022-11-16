library(MobMixlogit)
library(partykit)
library(mlogit)
library(strucchange)
library(sandwich)
library(purrr)
library(tidyr)
library(plyr)
library(xtable)

# Functions
source(here::here('utils/utils_using_MobMixlogit.R'))



#Loading the data
database <- (function(...)get(data(...,envir = new.env(),
                                    package ="MobMixlogit")))("data_DeLaMaza_2021")




N_trials <- 100
partition_var_total_trial <- lapply(1:N_trials, 
                                    function(trial_i){
                                
                                        
  set.seed(trial_i)
  #Block Bootstraping the data
                                      
  data_wide_trial_i <- database %>% 
    split(.$id) %>%
    .[sample(names(.),replace = TRUE)] %>% 
    .[order(names(.))] %>% 
    bind_rows %>%  
    arrange(id)  %>% 
    dplyr::mutate(id_choice = 1:n()) %>% 
    as.data.frame()
    

start_time <- Sys.time()
fitted_tree <- MobMixlogit::mob_mnl(
  database    = data_wide_trial_i, 
  mob_alpha   =  0.05,
  mob_minsize = 240  )

end_time <- Sys.time()
time_iter <- round(end_time - start_time, digits = 4)

partition_vars <- get_partitions_vars(fitted_tree = fitted_tree)

partition_vars_pruned <- prune.modelparty(fitted_tree,
                                          type = "BIC") %>% get_partitions_vars()

cat("\n","Trial ",trial_i, "time taken: ", time_iter)

return(list(partition_vars = partition_vars,
            partition_vars_pruned = partition_vars_pruned))
})



# List for key metrics
all_partition_vars <- vector("list", N_trials)
all_partition_vars_pruned <- vector("list", N_trials)
all_main_node <- vector("list", N_trials)

counter <- 1
for (x in partition_var_total_trial) {
  
  if (length(x$partition_vars)==0) { all_main_node[[counter]] <- 0}
  else {  all_main_node[[counter]] <- x$partition_vars[[1]]}
  all_partition_vars[[counter]] <- table(unlist(x$partition_vars))
  all_partition_vars_pruned[[counter]] <- table(unlist(x$partition_vars_pruned))
  
  counter <-  counter + 1
}



df_first_partition_var <- as.data.frame(rbind(table(unlist(all_main_node))))

df_partition_vars <- plyr::ldply(all_partition_vars, rbind) %>% 
  apply(2, sum, na.rm=TRUE)%>% 
  rbind() %>% 
  as.data.frame()

df_partition_vars_pruned <- plyr::ldply(all_partition_vars_pruned, rbind)%>% 
  apply(2, sum, na.rm=TRUE) %>% 
  rbind() %>% 
  as.data.frame()

nm <- list(df_first_partition_var,
           df_partition_vars,
           df_partition_vars_pruned)


final_df <- plyr::ldply(nm, rbind) %>% 
  rbind() %>% 
  as.data.frame() %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  select(sort(names(.))) 

#percentages 
final_df <- final_df/ N_trials

rownames(final_df) <- c("Root node split (\\%)",
                        "Average number of splits",
                        "Average number of splits after pruning"
)

final_df <- final_df %>% dplyr::rename(
  None =`0`,
  `Electric bill` = Electric_Bill,
  `Signed oath` = Signed_Oath) %>%
  .[,order(-.[1,])] 


# Create LaTeX table
tab_to_latex<- print.xtable(xtable(final_df),
                            include.rownames=T,
                            sanitize.text.function = function(x){x},
                            floating = F)


fileConn <-file("empirical_application/MOB_as_diagnostic_tool/res_MOB_diagnostic.tex")
writeLines(tab_to_latex, fileConn)
close(fileConn)



