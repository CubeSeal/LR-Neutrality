# Packages
library(readxl)
library(dplyr)
library(ggplot2)

# Functions
source("./functions.R")

# Import data
rba_money_aggregates.df <-
    read_excel("res/rba_money_aggregates.xls",
               skip = 10) %>%
    mutate(Date = format(`Series ID`, format="%Y-%m"))
abs_5206_key_aggregates.df <-
    read_excel("res/abs_5206_key_aggregates.xls",
               sheet = "Data1",
               skip = 9) %>%
    mutate(Date = format(`Series ID`, format="%Y-%m"))

# Isolate NGDP and Money Supply Growth
data.df <- inner_join(abs_5206_key_aggregates.df,
                       rba_money_aggregates.df,
                       by = 'Date') %>%
    select(Date,
           M = DMAM3N,
           Y = A2304334J,
           NY = A2304350J) %>%
    # Percentage growths + 1
    mutate(M_Growth = c(M[-1]/M[-length(M)], NA),
           NY_Growth = c(NY[-1]/NY[-length(NY)], NA),
           Y_Growth = c(Y[-1]/Y[-length(NY)], NA),
           P_Growth = NY_Growth/Y_Growth) %>%
    na.omit()

# Create aggregate df across t quarters
t <- 20

growth_aggs.df <-
    data.frame(M_Growth = log(agg_prod(data.df$M_Growth, t)), 
               Y_Growth = log(agg_prod(data.df$Y_Growth, t)),
               P_Growth = log(agg_prod(data.df$P_Growth, t)))

# LM Model
growth_aggs.lm <- lm(Y_Growth ~ M_Growth, growth_aggs.df)
summary(growth_aggs.lm)

# Graph relationship
ggplot(growth_aggs.df, aes(x = M_Growth, y = Y_Growth)) + 
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x) +
    geom_abline()
    