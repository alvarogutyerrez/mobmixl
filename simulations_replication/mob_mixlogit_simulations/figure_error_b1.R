library(data.table)
library(ggplot2) 
library(latex2exp)
source(here::here('utils/utils_using_MobMixlogit.R'))


# Load simulation results
res <- readRDS("simulations_replication/mob_mixlogit_simulations/mob_mixlogit_simulations.rds")

# Load Data Set
df <- unlist_mob_simulations(res)

y_axis <-  "err_beta_1"      
x_axis <- "n.indiv"   
group  <- "n.choices" 
model  <- "model"

y_axis_title <- TeX("MAE($\\hat{\\beta}_{1n}$)")
x_axis_title <- "Number of Individuals"
group_title  <- "Number of Choice Sets"


p<- ggplot(df, aes_string(x=x_axis,
                          y=y_axis,
                          fill = group)) +
  geom_boxplot(outlier.alpha = 0,
               outlier.shape = 16,
               width =0.75) + 
  facet_grid(
    rows = vars( scenario),
    cols = vars(delta , xi)   ,
    labeller=label_parsed
  ) +
  theme(strip.text.x = element_text(size = 35, colour = "black"),
        strip.text.y = element_text(size = 35, colour = "black"))+
  labs(x     = x_axis_title , 
       y     = y_axis_title,
       fill='Number of Choice Sets per Individual') +
  theme(legend.text=element_text(size=75),
        legend.title=element_text(size=55),
        legend.key.width = unit(1.5,"cm")) + theme_bw()

p <-  p  + theme(legend.position="top")

p

filename <- "figure_error_b1.pdf"
route <- "simulations_replication/mob_mixlogit_simulations/"
name_and_route <- paste0(route,filename)

ggsave(filename = name_and_route,
       plot = p,
       width =  9.09*1.1 , height =  3.57*1.1)

