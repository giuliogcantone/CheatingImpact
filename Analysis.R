library(tidyverse)
options(scipen = 999, digits = 6)


###
library(xtable)

Results$Cheater_p %>% cut_width(.04, boundary = 0) %>% table()
Results$Cheater_p %>% cut_width(.04, boundary = 0) -> Results$Cheat_lvl

mean(Results$Y_200 - Results$Y)

Results %>% summarise(n_ = mean(n),
                      sd_n = sd(n),
                      Y_ = mean(Y),
                      sdY = sd(Y),
                      J_ = mean(J),
                      sdJ = sd(J),
                      avg_t_ = mean(avg_t),
                      sd_avg_t = sd(avg_t),
                      p = mean(Cheater_p),
                      sd_p = sd(Cheater_p),
                      ATEYI = mean(Y_200 - Y),
                      sdATEYI = sd(Y_200 - Y),
                      ATEYII = mean(Y_Shuff - Y),
                      sdATEYII = sd(Y_Shuff - Y),
                      ATEJI = mean(J_200 - J),
                      sdATEJI = sd(J_200 - J),
                      ATEJII = mean(J_Shuff - J),
                      sdATEJII = sd(J_Shuff - J),
                      ) %>% View()
  xtable(type= "latex")

Results %>% group_by(Cheat_lvl) %>% summarise(median(Y),
                                              median(Y_200),
                                              median(Y_Shuff),
                                              median(J),
                                              median(J_200),
                                              median(J_Shuff)) %>%
  xtable(type= "latex")



###


Results$multipl %>% cut_width(300, boundary = 0) %>% table()
Results$multipl %>% cut_width(300, boundary = 0) -> Results$miu_levels

Results %>% group_by(miu_levels, Cheat_lvl) %>%
  summarise( n(),
  mean(Y_200 - Y),
  mean(Y_Shuff - Y),
  mean(J_200 - J),
  mean(J_Shuff - J)
  ) %>% 
  xtable(type= "latex")

?x



###




mean(Results$Y_200 - Results$Y)
mean(Results$Y_Shuff - Results$Y)

median(Results$J_200 - Results$J)
median(Results$J_Shuff - Results$J)

Results %>% mutate(Y1 = Y_200 - Y,
                   Y2 = Y_Shuff - Y,
                   J1 = J_200 - J,
                   J2 = J_Shuff - J) %>%
  group_by(Cheat_lvl) %>% summarise(mean(Y1),
                                    mean(Y2),
                                    median(J1),
                                    median(J2)) %>%
  xtable(type= "latex", digits =3)


###

ggplot(Results, aes(x = Y_200 - Y,
                    y = Cheater_p)) +
  geom_bin2d(bins = 35) +
  scale_fill_binned(type = "viridis",
                    breaks = c(50,100,250,500,750),
                    show.limit = T) +
  xlim(c(-.2,.2))+
  xlab("Y_200 - Y")+
  ylab("Cheating") +
  ggtitle("Impact on efficacy of small lotteries")+
  theme_classic() -> p1

ggplot(Results, aes(x = (J_200 - J) / 100,
                    y = Cheater_p)) +
  geom_bin2d(bins = 50)+
  scale_fill_binned(type = "viridis",
                    breaks = c(50,100,250,500,750,1000),
                    show.limit = T) +
  xlim(c(-.25,.75))+
  xlab("J_200 - J")+
  ylab("Cheating") +
  ggtitle("Impact on efficacy of small lotteries")+
  theme_classic() -> p2

ggplot(Results, aes(x = Y_Shuff - Y,
                    y = Cheater_p)) +
  geom_bin2d(bins = 28)+
  scale_fill_binned(type = "viridis",
                    breaks = c(50,100,250,500,750),
                    show.limit = T) +
  xlim(c(-.2,.2))+
  xlab("Y_% - Y")+
  ylab("Cheating") +
  ggtitle("Impact on fairness of big lotteries")+
  theme_classic() -> p3

ggplot(Results, aes(x = (J_Shuff - J) / 100,
                    y = Cheater_p)) +
  geom_bin2d(bins = 33)+
  scale_fill_binned(type = "viridis",
                    breaks = c(50,100,250,500,750,1000),
                    show.limit = T) +
  xlab("J_% - J")+
  ylab("Cheating") +
  xlim(c(-.25,.75))+
  ggtitle("Impact on Justice of big lotteries")+
  theme_classic() -> p4

###


###
library(broom)
options(scipen = 999, digits = 1)

lm(scale(Y_200 - Y) ~
     scale(Cheater_p) +
     scale(multipl) +
     scale(q), data = Results) %>% tidy() %>%
  mutate_if(is.numeric, round, 5)

lm(scale(Y_Shuff - Y) ~
     scale(Cheater_p) +
     scale(multipl) +
     scale(q), data = Results) %>% tidy() %>%
  mutate_if(is.numeric, round, 5)

lm(scale(J_200 - J) ~
     scale(Cheater_p) +
     scale(multipl) +
     scale(q), data = Results) %>% tidy() %>%
  mutate_if(is.numeric, round, 5)

lm(scale(J_Shuff - J) ~
     scale(Cheater_p) +
     scale(multipl) +
     scale(q), data = Results) %>% tidy() %>%
  mutate_if(is.numeric, round, 5)
