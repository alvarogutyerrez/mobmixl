library(data.table)
library(ggplot2) 
library(latex2exp)

res <- readRDS("simulations_replication/mob_mixlogit_simulations/mob_mixlogit_simulations.rds")

tmp = data.table(res)
tmp = tmp[, t(res[1]), by=1:nrow(tmp)]

tmp = tmp[,{ class(V1[[1]]$z1_correct) <- 'double'
class(V1[[1]]$xi_error) <- 'double'
V1[[1]]
}, by=1:nrow(tmp)]


g = function(x) list(mean = mean(x),
                     median = median(x),
                     sd = sd(x),
                     max = max(x),
                     ci_min_continuous = mean(as.numeric(x)) - qnorm(1-0.05/2)*sd(as.numeric(x)) , 
                     ci_max_continuous = mean(as.numeric(x)) + qnorm(1-0.05/2)*sd(as.numeric(x)) ,
                     ci_min_prop = mean(x) + qnorm(1-0.05/2)* as.numeric(sqrt(as.numeric(mean(x)) *( 1 - as.numeric(mean(x))) / as.numeric(length(x)) )),
                     ci_max_prop = mean(x) - qnorm(1-0.05/2)* as.numeric(sqrt(as.numeric(mean(x)) *( 1 - as.numeric(mean(x))) / as.numeric(length(x)) )),
                     p_95 = as.numeric(quantile(x, na.rm = TRUE , probs = c(0.95))),
                     p_5  = as.numeric(quantile(x, na.rm = TRUE , probs = c(0.05))),
                     n_nan = sum(is.na(x))
)

d <- as.data.frame(
  tmp[, unlist(lapply(.SD, g), recursive=FALSE)
      , .SDcols=c("time_iter","err_beta_1", "err_beta_2",
                  "z1_correct", "xi_error",
                  "ari",
                  "LL", "bic", "aic", "caic"),
      , by=.(scenario, n.indiv, n.choices, n.alter, xi, delta)])


## keep observation from stump
df  <- d[d$scenario=="stump", c("n.choices" , "n.indiv", "scenario", "xi", "delta",
                                "z1_correct.mean",
                                "z1_correct.ci_min_prop",
                                "z1_correct.ci_max_prop" )]
# Graph vars
y_axis <- "z1_correct.mean"      
x_axis <- "n.indiv"   
group  <- "n.choices" 
model  <- "model"

#Turning variables into factors
df[group] <- as.factor(with(df , get(group)))
## Generate the facet_grid LaTeX headers
df$xi <-    as.factor(df$xi)
levels(df$xi) <- c("0.5" = TeX("$\\xi = 0.5$"),"0.8" = TeX("$\\xi = 0.8$"))

df$delta <- as.factor(df$delta)
levels(df$delta) <- c("0.5" = TeX("$\\delta = 0.5$"),"1" = TeX("$\\delta = 1$"))

df$scenario <- as.factor(df$scenario)
levels(df$scenario) <- c("stump" = "Stump", "tree" = "Tree")

df$n.indiv <- as.factor(df$n.indiv)


#### Graph makeup ####
y_axis_title= TeX("Selection Probability $(Z_{1})$")
x_axis_title = "Number of Individuals"
group_title = "Number of Questions per Individual"

line_wt <- 0.75

p<- ggplot(df, aes_string(x=x_axis,
                          y=y_axis,
                          group = group, 
                          colour = group)
) +
  geom_errorbar(aes_string(  #confidence bars
    ymin="z1_correct.ci_min_prop", #lower CI
    ymax="z1_correct.ci_max_prop",
    group = group, 
    colour = group) , #upper CI
    width = 0.8,
    position = position_dodge(0.4),
    lwd = line_wt) +
  stat_summary(
    fun = mean,
    show.legend = F,  
    size = line_wt,
    geom = 'line',
    aes_string(group = group, colour = group),
    position = position_dodge(width = 0.4) 
  )  +
  labs(x = x_axis_title,
       y = y_axis_title ) +
  facet_grid(
    rows = vars( scenario),
    cols = vars(delta , xi)   ,
    labeller=label_parsed
  ) +
  theme(strip.text.x = element_text(size = 15, colour = "black"),
        strip.text.y = element_text(size = 15, colour = "black"))+
  theme(legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.width = unit(1.5,"cm")) + theme_bw()

p <- p + theme(legend.position="top")

p <- p + scale_color_discrete(name = 'Number of Choice sets per Individual') 

p


filename <- "figure_z1_correct.pdf"
route <- "simulations_replication/mob_mixlogit_simulations/"
name_and_route <- paste0(route,filename)
ggsave(filename = name_and_route,
       plot = p,
       width =  9.09*0.85 , height =  3.57*0.85)


