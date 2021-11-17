library(tidyverse)

Cheating =  1

Results = tibble(
  multipl = NA,
  n = NA,
  q = NA,
  Cheater_p =  NA,
  avg_t = NA,
  sd_t = NA,
  Shuff_coeff = NA,
  Spearman = NA,
  n_Cheaters = NA,
  Y = NA,
  J = NA,
  Y_200 = NA,
  J_200 = NA,
  Y_Shuff = NA,
  J_Shuff = NA,
  .rows = 100000
)

for (i in 1:100000) {

  # This is nested in Eq. 1
Results$multipl[i] = runif(1,300,1800)

# Look at at Eq. 1
Results$n[i] = (200.5 + abs(rnorm(1,
                                 Results$multipl[i],
                                 (Results$multipl[i]+200)/4))) %>%
                                 as.integer()

# Look at Eq. 2
Results$q[i] = sample(c(50,60,70,80,90,100),1)

# Universal parameter of cheating Nested in Eq. 3
Results$Cheater_p[i] =  runif(1,0,.2) * Cheating

# The tibble "Agents" is the Universe.
tibble(Name = str_c("Agent ",1:Results$n[i])) -> Agents

# t is True Score (Eq. 4)
Agents$t <- rbeta(Results$n[i],
                          (Results$multipl[i]+200)/100,
                          8)
Results$avg_t[i] <- mean(Agents$t)
Results$sd_t[i] <- var(Agents$t)


# Eq. 3. Is this agent a cheater or not?
Agents$Cheater <- rbinom(Results$n[i],1,Results$Cheater_p[i])

Results$n_Cheaters[i] <- sum(Agents$Cheater)

# TrueHit is the amount of answers that the agent know with certainty
Agents$TrueHit <- rbinom(Results$n[i], Results$q[i], Agents$t)

# Cheat is the individual impact of cheating on the correct answers
Agents$Cheat <- rbinom(Results$n[i], Results$q[i] - Agents$TrueHit,
                      1 - (Agents$TrueHit/Results$q[i])) * Agents$Cheater

# Hit is the final score
Agents$Hit <- Agents$Cheat + Agents$TrueHit
Agents$RandomHit <- rbinom(Results$n[i], Results$q[i] - Agents$Hit, .25)
Agents$Hit <- Agents$Hit + Agents$RandomHit

# Miss is (q - Hit)
Agents$Miss <- Results$q[i] - Agents$Hit
Agents$Score = Agents$Hit / Results$q[i]

# Agents are ordered by their ranks
Agents %>% arrange(-Score) -> Agents


### Statistical evaluation of the Universe starts here

# Shuffle coefficient

Results$Shuff_coeff[i] = runif(1,.4,.6)

# Rank correlations
cor(Agents$Score,Agents$t,method = "spearman") -> Results$Spearman[i]


# Winners are found
Agents %>% arrange(-Score) -> Agents
Agents %>% head(100) -> Winners

# Max potential
Agents %>% arrange(-t) %>% head(100) %>% .$t %>% sum() -> MaxPot

# Y and J
sum(Winners$t) / MaxPot -> Results$Y[i]
100 - sum(Winners$Cheater) -> Results$J[i]

# 200 Agents are shuffled, new winners are found
head(Agents,200) %>% slice_sample(n = 100) -> Winners2

# Y_200 and J_200
sum(Winners2$t) / MaxPot -> Results$Y_200[i]
100 - sum(Winners2$Cheater) -> Results$J_200[i]

# Sandel's shuffle, new winners are found
head(Agents,abs(Results$n[i] * Results$Shuff_coeff[i])) %>% slice_sample(n = 100) -> Winners3

# Y_Shuff and J_Shuff
sum(Winners3$t) / MaxPot -> Results$Y_Shuff[i]
100 - sum(Winners3$Cheater) -> Results$J_Shuff[i]

if(i %in% seq(250,100000,by=250)){print(i)}
}