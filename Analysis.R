library(tidyverse)
options(scipen = 999, digits = 4)

attach(Results)
median(n)
sd(n)
median(avg_t)
sd(avg_t)
median(Y)
sd(Y)
median(Y_200)
sd(Y_200)
median(Y_Shuff)
sd(Y_Shuff)
median(J)
sd(J)
median(J_200)
sd(J_200)
median(J_Shuff)
sd(J_Shuff)
detach(Results)

###
library(xtable)

Results$Cheater_p %>% cut_width(.04, boundary = 0) %>% table()
Results$Cheater_p %>% cut_width(.04, boundary = 0) -> Results$Cheat_lvl

Results %>% group_by(Cheat_lvl) %>% summarise(median(Y),
                                              median(Y_200),
                                              median(Y_Shuff),
                                              median(J),
                                              median(J_200),
                                              median(J_Shuff)) %>%
  xtable(type= "latex")

###

Results$avg_t %>% cut(breaks = c(.3,.45,.55,.625,.675,.725)) %>% table()
Results$avg_t %>% cut(breaks = c(.3,.45,.55,.625,.675,.725)) -> Results$T_levels

Results %>% group_by(T_levels) %>% summarise( n(),
                                              median(Y),
                                              median(Y_200),
                                              median(Y_Shuff),
                                              median(J),
                                              median(J_200),
                                              median(J_Shuff)) %>%
  xtable(type= "latex")

xtable()

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
