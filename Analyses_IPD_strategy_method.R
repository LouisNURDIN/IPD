library(tidyverse)
library(readxl)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(ggstats)
library(ggsci)  
library(viridis)
library(ggtext)
library(broom)
library(exact2x2)
library(TOSTER)
library(Exact)
library(viridis)
library(statpsych)
library(tidyr)
library(pwr)
library(tidyverse)


# Lire les données simulées
#df <- read_excel("Simulated data/data_IPD_simulated.xlsx")
df <- read.csv("data/data_all.csv")

df <- unique(df)
df <- df[!is.na(df$pd_in3_out3),]

glimpse(df)

# Préparer les variables stables par individu - autres que liées aux contributions conditionnelles
df$pd_uncond_contribution <- df$pd_uncond
df$ipd_uncond_contribution <- df$ipd_uncond
df$session
df$session_date
df$treatment
df$groupe
df$sous_groupe
df$gender
df$age
df$nationality
df$marital_status
df$socioprofessional_group
df$diplome
df$discipline
df$collecte

df$collecte <- factor(df$collecte,
                      levels = c(1, 2),
                      labels = c("2025", "2026"))

variables_cols <- c("session","session_date","treatment","svo_order","groupe","sous_groupe","gender","age","nationality","marital_status","socioprofessional_group","diplome","discipline","pd_uncond","pd_uncond_contribution","ipd_uncond_contribution")


# Extraire les colonnes PD et IPD conditionnelles
pd_cols <- grep("^pd_in\\d+_out\\d+", names(df), value = TRUE)
ipd_cols <- grep("^ipd_in\\d+_out\\d+", names(df), value = TRUE)



# Fonction corrélation robuste
cor_fun <- function(x, y) {
  if (sd(x, na.rm = TRUE) == 0 | sd(y, na.rm = TRUE) == 0) {
    return(0)
  } else {
    return(cor(x, y, use = "complete.obs"))
  }
}

# Format long PD + IPD
df_long <- df %>%
  select(
    participant, collecte,
    all_of(pd_cols), all_of(ipd_cols)
  ) %>%
  pivot_longer(
    cols = c(all_of(pd_cols), all_of(ipd_cols)),
    names_to = "condition",
    values_to = "decision"
  ) %>%
  mutate(
    game = if_else(str_detect(condition, "^pd_"), "PD", "IPD"),
    ingroup = as.integer(str_extract(condition, "(?<=in)\\d+")),
    outgroup = as.integer(str_extract(condition, "(?<=out)\\d+"))
  )


player_types <- df_long %>%
  group_by(participant, game, collecte) %>%
  summarise(
    sum_decision = sum(decision, na.rm = TRUE),
    mean_decision = mean(decision, na.rm = TRUE),
    sd_decision = sd(decision, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Unconditional_non_cooperator = sum_decision == 0,
    Unconditional_cooperator = mean_decision != 0 & sd_decision == 0
  )


ingroup_cond <- df_long %>%
  group_by(participant, game, collecte, outgroup) %>%
  summarise(
    cor_decision_ingroup = cor_fun(decision, ingroup),
    .groups = "drop"
  ) %>%
  group_by(participant, game, collecte) %>%
  summarise(
    mean_cor_ingroup = mean(cor_decision_ingroup, na.rm = TRUE),
    .groups = "drop"
  )

ingroup_mono <- df_long %>%
  group_by(participant, game, collecte, ingroup) %>%
  summarise(
    sum_decision_peringroup = sum(decision, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(participant, game, collecte, ingroup) %>%
  group_by(participant, game, collecte) %>%
  summarise(
    min_varia_ingroup = min(diff(sum_decision_peringroup), na.rm = TRUE),
    max_varia_ingroup = max(diff(sum_decision_peringroup), na.rm = TRUE),
    .groups = "drop"
  )

outgroup_cond <- df_long %>%
  group_by(participant, game, collecte, ingroup) %>%
  summarise(
    cor_decision_outgroup = cor_fun(decision, outgroup),
    .groups = "drop"
  ) %>%
  group_by(participant, game, collecte) %>%
  summarise(
    mean_cor_outgroup = mean(cor_decision_outgroup, na.rm = TRUE),
    .groups = "drop"
  )

outgroup_mono <- df_long %>%
  group_by(participant, game, collecte, outgroup) %>%
  summarise(
    sum_decision_peroutgroup = sum(decision, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(participant, game, collecte, outgroup) %>%
  group_by(participant, game, collecte) %>%
  summarise(
    min_varia_outgroup = min(diff(sum_decision_peroutgroup), na.rm = TRUE),
    max_varia_outgroup = max(diff(sum_decision_peroutgroup), na.rm = TRUE),
    .groups = "drop"
  )



player_types <- player_types %>%
  left_join(ingroup_cond, by = c("participant", "game", "collecte")) %>%
  left_join(ingroup_mono, by = c("participant", "game", "collecte")) %>%
  left_join(outgroup_cond, by = c("participant", "game", "collecte")) %>%
  left_join(outgroup_mono, by = c("participant", "game", "collecte")) %>%
  mutate(
    Ingroupconditional =
      mean_cor_ingroup >= 0.5 |
      (min_varia_ingroup >= 0 & max_varia_ingroup > 0),
    
    Outgroupconditional =
      mean_cor_outgroup >= 0.5 |
      (min_varia_outgroup >= 0 & max_varia_outgroup > 0),
    
    Player_type = case_when(
      Unconditional_non_cooperator ~ "Unconditional\nnon cooperator",
      Unconditional_cooperator ~ "Unconditional\ncooperator",
      Ingroupconditional & Outgroupconditional ~ "Ingroup and Outgroup\nconditional cooperator",
      Ingroupconditional & !Outgroupconditional ~ "Only Ingroup\nconditional cooperator",
      !Ingroupconditional & Outgroupconditional ~ "Only Outgroup\nconditional cooperator",
      TRUE ~ "Undefined"
    )
  )




player_types %>%
  ggplot(aes(x = "", fill = Player_type)) +
  geom_bar(position = "fill") +
  geom_text(
    aes(label = after_stat(scales::percent(count / tapply(count, PANEL, sum)[PANEL], accuracy = 1))),
    stat = "count",
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 4
  ) +
  facet_grid(~ game) +
  scale_fill_jama(name = "Player type") +
  scale_y_continuous(
    name = "Proportion of players",
    labels = scales::percent
  ) +
  scale_x_discrete(name = NULL) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank()
  )


ggsave(filename="Profiles_PD_IPD.jpg", device="jpg", height=6, width=6,units="in",dpi=300)



# # # Tests primary hypotheses

#player_types <- player_types[player_types$collecte=="2026",]

library(statpsych)

alpha_primary <- 0.05 / 3
delta <- 0.115 

# --- Préparation des données
primary_data <- player_types %>%
  mutate(
    UNC = Player_type == "Unconditional\nnon cooperator",
    iCC = Ingroupconditional,
    oCC = Outgroupconditional
  ) %>%
  select(participant, collecte, game, UNC, iCC, oCC, Player_type) %>%
  pivot_wider(
    names_from = game,
    values_from = c(UNC, iCC, oCC, Player_type),
    names_sep = "_"
  )

tab_H1 <- table(
  PD = primary_data$oCC_PD,
  IPD = primary_data$oCC_IPD
)

tab_H1

b_H1 <- tab_H1["FALSE", "TRUE"]
c_H1 <- tab_H1["TRUE", "FALSE"]

test_H1 <- binom.test(
  x = b_H1,
  n = b_H1 + c_H1,
  p = 0.5,
  alternative = "greater"
)

test_H1



primary_data <- primary_data %>%
  mutate(
    diff_iCC = as.numeric(iCC_IPD) - as.numeric(iCC_PD)
  )

# Statistiques
mean_diff <- mean(primary_data$diff_iCC, na.rm = TRUE)
sd_diff <- sd(primary_data$diff_iCC, na.rm = TRUE)
n <- sum(!is.na(primary_data$diff_iCC))

se_diff <- sd_diff / sqrt(n)

# --- TOST
t_lower <- (mean_diff + delta) / se_diff
p_lower <- 1 - pnorm(t_lower)

t_upper <- (mean_diff - delta) / se_diff
p_upper <- pnorm(t_upper)

p_TOST <- max(p_lower, p_upper)

test_H2 <- tibble(
  hypothesis = "H2: equivalence iCC IPD vs PD",
  mean_diff = mean_diff,
  delta = delta,
  p_lower = p_lower,
  p_upper = p_upper,
  p_TOST = p_TOST,
  reject_null_equivalence = p_TOST < alpha_primary
)

test_H2




tab_H3 <- table(
  PD = primary_data$UNC_PD,
  IPD = primary_data$UNC_IPD
)

tab_H3

b_H3 <- tab_H3["FALSE", "TRUE"]
c_H3 <- tab_H3["TRUE", "FALSE"]

test_H3 <- binom.test(
  x = b_H3,
  n = b_H3 + c_H3,
  p = 0.5,
  alternative = "greater"
)

test_H3

primary_results <- tibble(
  hypothesis = c(
    "H1: oCC higher in IPD than PD",
    "H2: iCC equivalent between IPD and PD",
    "H3: UNC higher in IPD than PD"
  ),
  test = c(
    "Exact McNemar/binomial (one-sided)",
    "Equivalence test (TOST)",
    "Exact McNemar/binomial (one-sided)"
  ),
  estimate = c(
    mean(primary_data$oCC_IPD, na.rm = TRUE) - mean(primary_data$oCC_PD, na.rm = TRUE),
    mean(primary_data$iCC_IPD, na.rm = TRUE) - mean(primary_data$iCC_PD, na.rm = TRUE),
    mean(primary_data$UNC_IPD, na.rm = TRUE) - mean(primary_data$UNC_PD, na.rm = TRUE)
  ),
  p_value = c(
    test_H1$p.value,
    test_H2$p_TOST,
    test_H3$p.value
  ),
  alpha = alpha_primary,
  reject_null = p_value < alpha_primary
)

primary_results

primary_descriptives <- primary_data %>%
  summarise(
    n = n(),
    
    oCC_PD = mean(oCC_PD, na.rm = TRUE),
    oCC_IPD = mean(oCC_IPD, na.rm = TRUE),
    diff_oCC = oCC_IPD - oCC_PD,
    
    iCC_PD = mean(iCC_PD, na.rm = TRUE),
    iCC_IPD = mean(iCC_IPD, na.rm = TRUE),
    diff_iCC = iCC_IPD - iCC_PD,
    
    UNC_PD = mean(UNC_PD, na.rm = TRUE),
    UNC_IPD = mean(UNC_IPD, na.rm = TRUE),
    diff_UNC = UNC_IPD - UNC_PD
  )

primary_descriptives




















# # # # BROUILLON ----

# First create a function to solve the problem of cases where the correlation cannot be calculated because sd=0 (constant contribution): in this case we'll consider the correlation to be zero.
cor_fun<- function(x,y){
  
  if (any(sapply(list(x, y), FUN = sd) == 0)) {
    return(0)
  } else {
    val <- cor(x,y)
    return(val)
  }
}

# Choix sessions
df_long <- df[df$collecte=="2026",]

# Passer en format long
df_long <- df_long %>%
  select(participant, all_of(pd_cols), all_of(ipd_cols)) %>%
  pivot_longer(-participant, names_to = "condition", values_to = "decision") %>%
  mutate(
    game = if_else(str_detect(condition, "^pd_"), "PD", "IPD"),
    ingroup = as.integer(str_extract(condition, "(?<=in)\\d+")),
    outgroup = as.integer(str_extract(condition, "(?<=out)\\d+"))
  )

# Choix jeu
df_long <- df_long[df_long$game=="PD",]

df_long$subject <- df_long$participant

# # # # Calculate the type of each random player 

# # Unconditional non cooperator 
# = 0 all the time
df_long <- df_long %>% 
  group_by(subject) %>% 
  mutate(sum_decision = sum(decision))

df_long$Unconditional_non_cooperator <- "No"
df_long$Unconditional_non_cooperator[df_long$sum_decision==0] <- "Unconditional non cooperator"

describe(df_long$Unconditional_non_cooperator)
describe(df_long$sum_decision)


# # Unconditional cooperator 
# = a constant non-null integer all the time (null variance of investments for a given player)
df_long <- df_long %>% 
  group_by(subject) %>% 
  mutate(mean_decision = mean(decision),
         sd_decision = sd(decision))

df_long$Unconditional_cooperator <- "No"
df_long$Unconditional_cooperator[df_long$mean_decision!=0 & df_long$sd_decision ==0] <- "Unconditional cooperator"

describe(df_long$Unconditional_cooperator)
describe(df_long$sd_decision)
describe(df_long$mean_decision)


# # Ingroup conditional cooperator
# = Average Pearson correlations for each level of outgroup contribution between player contribution and ingroup contribution equal to or greater than 0.5 

df_long <- df_long %>%
  group_by(subject, outgroup) %>%
  mutate(cor_decision_ingroup_peroutgroup=cor_fun(decision, ingroup))  %>% 
  ungroup()

df_long <- df_long %>% 
  group_by(subject) %>% 
  mutate(mean_cor_decision_ingroup_peroutgroup = mean(cor_decision_ingroup_peroutgroup))

describe(df_long$cor_decision_ingroup_peroutgroup)

df_long$Ingroupconditional <- "No"
df_long$Ingroupconditional[df_long$mean_cor_decision_ingroup_peroutgroup>=0.5] <- "Ingroup conditional cooperator"

# OR 
# = Monotonically increasing sum of tokens invested as a function of ingroup contributions

df_long <- df_long %>% 
  group_by(subject, ingroup) %>% 
  mutate(sum_decision_peringroup = sum(decision))

describe(df_long$sum_decision_peringroup)

df_long <- df_long[order(df_long$subject, df_long$ingroup),]

df_long <- df_long %>% 
  group_by(subject) %>% 
  mutate(min_varia_lag_ingroup = min(sum_decision_peringroup-lag(sum_decision_peringroup), na.rm=TRUE))

describe(df_long$min_varia_lag_ingroup)

df_long <- df_long[order(df_long$subject, df_long$ingroup),]

df_long <- df_long %>% 
  group_by(subject) %>% 
  mutate(max_varia_lag_ingroup = max(sum_decision_peringroup-lag(sum_decision_peringroup), na.rm=TRUE))

describe(df_long$max_varia_lag_ingroup)

df_long$Ingroupconditional[df_long$min_varia_lag_ingroup>=0&df_long$max_varia_lag_ingroup>0] <- "Ingroup conditional cooperator"

describe(df_long$Ingroupconditional)



# # Outgroup conditional cooperator
# = Average Pearson correlations for each level of ingroup contribution between player contribution and outgroup contribution equal to or greater than 0.5 

df_long <- df_long %>%
  group_by(subject, ingroup) %>%
  mutate(cor_decision_outgroup_peringroup=cor_fun(decision, outgroup))  %>% 
  ungroup()

df_long <- df_long %>% 
  group_by(subject) %>% 
  mutate(mean_cor_decision_outgroup_peringroup = mean(cor_decision_outgroup_peringroup))

describe(df_long$cor_decision_outgroup_peringroup)

df_long$Outgroupconditional <- "No"
df_long$Outgroupconditional[df_long$mean_cor_decision_outgroup_peringroup>=0.5] <- "Outgroup conditional cooperator"

# OR 
# = Monotonically increasing sum of tokens invested as a function of outgroup contributions

df_long <- df_long %>% 
  group_by(subject, outgroup) %>% 
  mutate(sum_decision_peroutgroup = sum(decision))

describe(df_long$sum_decision_peroutgroup)

df_long <- df_long[order(df_long$subject, df_long$outgroup),]

df_long <- df_long %>% 
  group_by(subject) %>% 
  mutate(min_varia_lag_outgroup = min(sum_decision_peroutgroup-lag(sum_decision_peroutgroup), na.rm=TRUE))

describe(df_long$min_varia_lag_outgroup)

df_long <- df_long[order(df_long$subject, df_long$outgroup),]

df_long <- df_long %>% 
  group_by(subject) %>% 
  mutate(max_varia_lag_outgroup = max(sum_decision_peroutgroup-lag(sum_decision_peroutgroup), na.rm=TRUE))

describe(df_long$max_varia_lag_outgroup)

df_long$Outgroupconditional[df_long$min_varia_lag_outgroup>=0&df_long$max_varia_lag_outgroup>0] <- "Outgroup conditional cooperator"

describe(df_long$Outgroupconditional)

# # Get one single variable defining the type of the subject

df_long$Player_type = "Undefined"
df_long$Player_type[df_long$Unconditional_cooperator=="Unconditional cooperator"] = "Unconditional cooperator"
df_long$Player_type[df_long$Unconditional_non_cooperator=="Unconditional non cooperator"] = "Unconditional non cooperator"
df_long$Player_type[df_long$Outgroupconditional=="Outgroup conditional cooperator" & df_long$Ingroupconditional=="Ingroup conditional cooperator"] = "Ingroup and Outgroup\nconditional cooperator"
df_long$Player_type[df_long$Outgroupconditional=="No" & df_long$Ingroupconditional=="Ingroup conditional cooperator"] = "Only Ingroup\nconditional cooperator"
df_long$Player_type[df_long$Outgroupconditional=="Outgroup conditional cooperator" & df_long$Ingroupconditional=="No"] = "Only Outgroup\nconditional cooperator"

describe(df_long$Player_type)

profil_type_plot <- df_long  %>% 
  ggplot(aes(group = Player_type, x=" "))+
  geom_bar(aes( fill = Player_type, x=" "),position = "fill")+
  geom_text(stat = "prop",color="white",position = position_fill(.5))+
  scale_fill_jama(name = "Player type")+
  scale_y_continuous(name = "Proportion of random players")+
  scale_x_discrete(name = " ")+
  theme_minimal()+
  theme(
    text = element_text(size = 14)
  )
ggsave(
  filename = "results/figures/player profil by game.png",
  plot = profil_type_plot,
  width = 10,
  height = 6,
  dpi = 300
)


#test diagramme alluvial
install.packages("ggalluvial")
library(ggalluvial)

df_alluvial <- primary_data %>%
  count(Player_type_PD, Player_type_IPD)

alv_plot <- ggplot(
  df_alluvial,
  aes(
    axis1 = Player_type_PD,
    axis2 = Player_type_IPD,
    y = n
  )
) +
  geom_alluvium(aes(fill = Player_type_PD)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(
    limits = c("PD", "IPD"),
    expand = c(.1, .1)
  ) +
  theme_minimal()
ggsave(
  filename = "results/figures/graphique alluvial pd-ipd.png",
  plot = alv_plot,
  width = 10,
  height = 6,
  dpi = 300
)

#Vérification des hypothèses ----

#H1 ----

#H2 ----

#H3 ----

#H4a ----

#H4b ----

#H4c ----

#H4d ----

#H5 ----



