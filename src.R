# Packages
library(readxl)
library(dplyr)
library(ggplot2)

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
    select(Date, M1 = DMAM1N, NGDP_Q = A2304350J) %>%
    mutate(M1_Growth = c(M1[-1]/M1[-length(M1)], NA),
           NGDP_Q_Growth = c(NGDP_Q[-1]/NGDP_Q[-length(NGDP_Q)], NA)) %>%
    na.omit()

t <- 12

money_g_agg.df <-
    data.frame(M1_Growth = sapply(split(
        money.df$M1_Growth,
        1:nrow(money.df) %/% t
    ), prod),
    NGDP_Growth = sapply(split(
        money.df$NGDP_Q_Growth,
        1:nrow(money.df) %/% t), prod
    ))
    
# Graph relationship
ggplot(money_g_agg.df, aes(x = M1_Growth, y = NGDP_Growth)) + 
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x) +
    geom_abline()

# LM Model
money.lm <- lm(NGDP_Growth ~ M1_Growth, money_g_agg.df)
summary(money.lm)
    