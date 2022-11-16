library(data.table)
library(ggplot2) 
library(latex2exp)
source(here::here('utils/utils_using_MobMixlogit.R'))


# Load simulation results
res <- readRDS("simulations_replication/mob_mixlogit_simulations/mob_mixlogit_simulations.rds")

# Load Data Set
df <- unlist_mob_simulations(res)

## Drop observation from stump
df <- df[df$scenario=="Tree",]

# Graph info.
y_axis <- "ari"      
x_axis <- "n.indiv"   
group  <- "n.choices" 
model  <- "model"

y_axis_title <- "Adjusted Rand Index"
x_axis_title <- "Number of Individuals"
group_title  <- "Number of Questions per Individual"



# Figure
p <- ggplot(df, aes_string(x    = x_axis,
                           y    = y_axis,
                           fill = group)) +
  geom_boxplot(outlier.alpha = 0,
               outlier.shape = 16,
               width =0.55) + 
  facet_grid(
    rows = vars( scenario),
    cols = vars(delta , xi)   ,
    labeller=label_parsed
  ) +
  scale_y_continuous(limits = with(df, 
                                   quantile(get(y_axis), 
                                            c(0, 1), 
                                            na.rm = F))) + 
  theme(strip.text.x = element_text(size = 35, colour = "black"),
        strip.text.y = element_text(size = 35, colour = "black"))+
  labs(x     = x_axis_title , 
       y     = y_axis_title,
       fill='Number of Choice sets per Individual') +
  theme(legend.text=element_text(size=75),
        legend.title=element_text(size=55),
        legend.key.width = unit(1.5,"cm")) + theme_bw()

p <-  p  + theme(legend.position="top")

p

filename <- "figure_ari.pdf"
route <- "simulations_replication/mob_mixlogit_simulations/"
name_and_route <- paste0(route,filename)

ggsave(filename = name_and_route,
       plot = p,
       width =  9.09*1.1 , height =  3.57*1.1)
