# Packages
library(readxl)
library(dplyr)
library(ggplot2)

# Functions
source("./functions.R")

# Import data
rba_money_aggregates.raw <-
    read_excel("res/rba_money_aggregates.xls",
               skip = 10) %>%
    mutate(Date = format(`Series ID`, format="%Y-%m"))
abs_5206_key_aggregates.raw <-
    read_excel("res/abs_5206_key_aggregates.xls",
               sheet = "Data1",
               skip = 9) %>%
    mutate(Date = format(`Series ID`, format="%Y-%m"))

# Isolate NGDP and Money Supply Growth
money.df <- inner_join(abs_5206_key_aggregates.raw,
                       rba_money_aggregates.raw,
                       by = 'Date') %>%
    select(Date, M = DMACN, NGDP_Q = A2304350J) %>%
    
    # Percentage growths + 1
    mutate(M_Growth = c(M[-1]/M[-length(M)], NA),
           NGDP_Q_Growth = c(NGDP_Q[-1]/NGDP_Q[-length(NGDP_Q)], NA)) %>%
    na.omit()

# Create aggregate df across t quarters
t <- 4*3

money_g_agg.df <-
    data.frame(M_Growth = agg_prod(money.df$M_Growth, t), 
               NGDP_Growth = agg_prod(money.df$NGDP_Q_Growth, t))

# LM Model
money.lm <- lm(NGDP_Growth ~ M_Growth, money_g_agg.df)
summary(money.lm)

# Graph relationship
ggplot(money_g_agg.df, aes(x = M_Growth, y = NGDP_Growth)) + 
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x) +
    geom_abline()
    