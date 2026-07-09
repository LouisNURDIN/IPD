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
library(modelsummary)
library(plotly)



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

# --- Préparation des données ----
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


results_H2 <- tibble(
  hypothesis = "H2: equivalence iCC IPD vs PD",
  p_PD = mean(primary_data$iCC_PD, na.rm = TRUE),
  p_IPD = mean(primary_data$iCC_IPD, na.rm = TRUE),
  mean_diff = mean_diff,
  delta = delta,
  p_lower = p_lower,
  p_upper = p_upper,
  p_TOST = p_TOST,
  reject_null_equivalence = p_TOST < alpha_primary
)

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

Hmisc::describe(df_long$Unconditional_non_cooperator)
Hmisc::describe(df_long$sum_decision)


# # Unconditional cooperator 
# = a constant non-null integer all the time (null variance of investments for a given player)
df_long <- df_long %>% 
  group_by(subject) %>% 
  mutate(mean_decision = mean(decision),
         sd_decision = sd(decision))

df_long$Unconditional_cooperator <- "No"
df_long$Unconditional_cooperator[df_long$mean_decision!=0 & df_long$sd_decision ==0] <- "Unconditional cooperator"

Hmisc::describe(df_long$Unconditional_cooperator)
Hmisc::describe(df_long$sd_decision)
Hmisc::describe(df_long$mean_decision)


# # Ingroup conditional cooperator
# = Average Pearson correlations for each level of outgroup contribution between player contribution and ingroup contribution equal to or greater than 0.5 

df_long <- df_long %>%
  group_by(subject, outgroup) %>%
  mutate(cor_decision_ingroup_peroutgroup=cor_fun(decision, ingroup))  %>% 
  ungroup()

df_long <- df_long %>% 
  group_by(subject) %>% 
  mutate(mean_cor_decision_ingroup_peroutgroup = mean(cor_decision_ingroup_peroutgroup))

Hmisc::describe(df_long$cor_decision_ingroup_peroutgroup)

df_long$Ingroupconditional <- "No"
df_long$Ingroupconditional[df_long$mean_cor_decision_ingroup_peroutgroup>=0.5] <- "Ingroup conditional cooperator"

# OR 
# = Monotonically increasing sum of tokens invested as a function of ingroup contributions

df_long <- df_long %>% 
  group_by(subject, ingroup) %>% 
  mutate(sum_decision_peringroup = sum(decision))

Hmisc::describe(df_long$sum_decision_peringroup)

df_long <- df_long[order(df_long$subject, df_long$ingroup),]

df_long <- df_long %>% 
  group_by(subject) %>% 
  mutate(min_varia_lag_ingroup = min(sum_decision_peringroup-lag(sum_decision_peringroup), na.rm=TRUE))

Hmisc::describe(df_long$min_varia_lag_ingroup)

df_long <- df_long[order(df_long$subject, df_long$ingroup),]

df_long <- df_long %>% 
  group_by(subject) %>% 
  mutate(max_varia_lag_ingroup = max(sum_decision_peringroup-lag(sum_decision_peringroup), na.rm=TRUE))

Hmisc::describe(df_long$max_varia_lag_ingroup)

df_long$Ingroupconditional[df_long$min_varia_lag_ingroup>=0&df_long$max_varia_lag_ingroup>0] <- "Ingroup conditional cooperator"

Hmisc::describe(df_long$Ingroupconditional)



# # Outgroup conditional cooperator
# = Average Pearson correlations for each level of ingroup contribution between player contribution and outgroup contribution equal to or greater than 0.5 

df_long <- df_long %>%
  group_by(subject, ingroup) %>%
  mutate(cor_decision_outgroup_peringroup=cor_fun(decision, outgroup))  %>% 
  ungroup()

df_long <- df_long %>% 
  group_by(subject) %>% 
  mutate(mean_cor_decision_outgroup_peringroup = mean(cor_decision_outgroup_peringroup))

Hmisc::describe(df_long$cor_decision_outgroup_peringroup)

df_long$Outgroupconditional <- "No"
df_long$Outgroupconditional[df_long$mean_cor_decision_outgroup_peringroup>=0.5] <- "Outgroup conditional cooperator"

# OR 
# = Monotonically increasing sum of tokens invested as a function of outgroup contributions

df_long <- df_long %>% 
  group_by(subject, outgroup) %>% 
  mutate(sum_decision_peroutgroup = sum(decision))

Hmisc::describe(df_long$sum_decision_peroutgroup)

df_long <- df_long[order(df_long$subject, df_long$outgroup),]

df_long <- df_long %>% 
  group_by(subject) %>% 
  mutate(min_varia_lag_outgroup = min(sum_decision_peroutgroup-lag(sum_decision_peroutgroup), na.rm=TRUE))

Hmisc::describe(df_long$min_varia_lag_outgroup)

df_long <- df_long[order(df_long$subject, df_long$outgroup),]

df_long <- df_long %>% 
  group_by(subject) %>% 
  mutate(max_varia_lag_outgroup = max(sum_decision_peroutgroup-lag(sum_decision_peroutgroup), na.rm=TRUE))

Hmisc::describe(df_long$max_varia_lag_outgroup)

df_long$Outgroupconditional[df_long$min_varia_lag_outgroup>=0&df_long$max_varia_lag_outgroup>0] <- "Outgroup conditional cooperator"

Hmisc::describe(df_long$Outgroupconditional)

# # Get one single variable defining the type of the subject

df_long$Player_type = "Undefined"
df_long$Player_type[df_long$Unconditional_cooperator=="Unconditional cooperator"] = "Unconditional cooperator"
df_long$Player_type[df_long$Unconditional_non_cooperator=="Unconditional non cooperator"] = "Unconditional non cooperator"
df_long$Player_type[df_long$Outgroupconditional=="Outgroup conditional cooperator" & df_long$Ingroupconditional=="Ingroup conditional cooperator"] = "Ingroup and Outgroup\nconditional cooperator"
df_long$Player_type[df_long$Outgroupconditional=="No" & df_long$Ingroupconditional=="Ingroup conditional cooperator"] = "Only Ingroup\nconditional cooperator"
df_long$Player_type[df_long$Outgroupconditional=="Outgroup conditional cooperator" & df_long$Ingroupconditional=="No"] = "Only Outgroup\nconditional cooperator"

Hmisc::describe(df_long$Player_type)

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


#test diagramme alluvial ----
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
print(alv_plot)
ggsave(
  filename = "results/figures/graphique alluvial pd-ipd.png",
  plot = alv_plot,
  width = 10,
  height = 6,
  dpi = 300
)



#Heatmap ----
heatmap <- ggplot(
  df_alluvial,
  aes(
    x = Player_type_PD,
    y = Player_type_IPD,
    fill = n
  )
) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "white",
    high = "red"
  ) +
  scale_fill_gradient(
    low = "white",
    high = "red",
    breaks = c(0, 25, 50, 75, 100,139)
  )
  theme_minimal() +
  labs(
    x = "Type de joueur dans PD",
    y = "Type de joueur dans IPD",
    fill = "Nombre\nde joueurs"
  )
  print(heatmap)
ggsave(
  filename = "results/figures/heatmap pd-ipd.png",
  plot = heatmap,
  width = 10,
  height = 6,
  dpi = 300
)

#Heatmap avec proportion par groupes ----
df_alluvial_prop <- df_alluvial %>%
  group_by(Player_type_PD) %>%
  mutate(
    total_PD = sum(n),
    prop = n / total_PD * 100
  ) %>%
  ungroup() %>%
  mutate(
    total_all = sum(n),
    prop_all = n / total_all * 100
  )

heatmap_prop <- ggplot(
  df_alluvial_prop,
  aes(
    x = Player_type_PD,
    y = Player_type_IPD,
    fill = prop_all
  )
) +
  geom_tile(color = "white") +
  
  geom_text(aes(label = round(prop_all, 1))) +
  
  scale_fill_gradient(
    low = "white",
    high = "red",
    breaks = c(0, 5, 10, 20, 25)
  ) +
  
  theme_minimal() +
  labs(
    x = "Type de joueur dans PD",
    y = "Type de joueur dans IPD",
    fill = "% de joueurs (ensemble de l'échantillon)"
  )

print(heatmap_prop)
print(heatmap_prop)
ggsave(
  filename = "results/figures/heatmap pd_ipd proportion.png",
  plot = heatmap_prop,
  width = 10,
  height = 6,
  dpi = 300
)
#Vérification des hypothèses ----

#H1 ----
#La proportion de Outgroup conditional cooperator est plus importante en cas de conflit (IPD > PD)
#McNemar’schi-square test (one-sided): the null hypothesis is oCCIPD ≤ oCCPD.
table(primary_data$Player_type_IPD)  #53 oCC dans IPD
table(primary_data$Player_type_PD)   #26 oCC dans PD

tab_H1 <- table(
  factor(primary_data$oCC_PD, levels = c(FALSE, TRUE)),
  factor(primary_data$oCC_IPD, levels = c(FALSE, TRUE))
)

b_H1 <- tab_H1["FALSE", "TRUE"]
c_H1 <- tab_H1["TRUE", "FALSE"]

test_H1 <- binom.test(
  x = b_H1,
  n = b_H1 + c_H1,
  p = 0.5,
  alternative = "greater"
)

test_H1

test_H1$p.value
test_H1$statistic
test_H1$conf.int

result_H1 <- data.frame(
  Transition = c("PD → IPD (b)", "IPD → PD (c)", "p-value"),
  Value = c(b_H1, c_H1, signif(test_H1$p.value, 7))
)


#H2 ----
#La proportion de ingroup conditional cooperators est la même que ce soit en présence d'un conflit ou non
table(primary_data$Player_type_IPD)  #60 iCC dans IPD
table(primary_data$Player_type_PD)  #98 dans PD => plus de iCC en absence de conflit 

primary_data <- primary_data %>%
  mutate(
    iCC_PD  = Player_type_PD  == "Only Ingroup\nconditional cooperator",
    iCC_IPD = Player_type_IPD == "Only Ingroup\nconditional cooperator"
  )
p_PD  <- mean(primary_data$iCC_PD, na.rm = TRUE)
p_IPD <- mean(primary_data$iCC_IPD, na.rm = TRUE)

d <- p_IPD - p_PD

diff_iCC <- as.numeric(primary_data$iCC_IPD) -
  as.numeric(primary_data$iCC_PD)
mean_diff <- mean(diff_iCC, na.rm = TRUE)
sd_diff   <- sd(diff_iCC, na.rm = TRUE)
n         <- sum(!is.na(diff_iCC))

se_diff <- sd_diff / sqrt(n)
delta <- 0.115
z_lower <- (mean_diff + delta) / se_diff
p_lower <- 1 - pnorm(z_lower)

z_upper <- (mean_diff - delta) / se_diff
p_upper <- pnorm(z_upper)

p_TOST <- max(p_lower, p_upper)
result_H2 <- tibble(
  hypothesis = "H2: equivalence iCC IPD vs PD",
  mean_diff = mean_diff,
  delta = delta,
  p_lower = p_lower,
  p_upper = p_upper,
  p_TOST = p_TOST,
  reject_null_equivalence = p_TOST < alpha_primary
)

result_H2

#H3 ----
#La proportion d'inconditionnels non coopérateurs (contribuent jamais) est plus importante en présence de conflit
table(primary_data$Player_type_IPD)  #57 UNC dans IPD
table(primary_data$Player_type_PD)   #47 UNC dans PD


tab_H3 <- table(
  factor(primary_data$UNC_PD, levels = c(FALSE, TRUE)),
  factor(primary_data$UNC_IPD, levels = c(FALSE, TRUE))
)

b_H3 <- tab_H3["FALSE", "TRUE"]
c_H3 <- tab_H3["TRUE", "FALSE"]

test_H3 <- binom.test(
  x = b_H3,
  n = b_H3 + c_H3,
  p = 0.5,
  alternative = "greater"
)

test_H3

test_H3$p.value
test_H3$statistic
test_H3$conf.int

result_H3 <- data.frame(
  Transition = c("PD → IPD (b)", "IPD → PD (c)", "p-value"),
  Value = c(b_H3, c_H3, signif(test_H3$p.value, 8))
)

#H4a ----
#Les ICC ont un iSVO plus important que les UNC dans IPD
##Calcul de gSVO par joueur
df <- df %>%
  mutate(
    A_ingroup = rowMeans(
      select(., starts_with("gsvo_in_")),
      na.rm = TRUE
    ),
    A_outgroup = rowMeans(
      select(., starts_with("gsvo_out_")),
      na.rm = TRUE
    ),
    
    gSVO = atan2(
      A_outgroup - 50,
      A_ingroup - 50
    ) * 180 / pi
)
  

max(df$gSVO)
min(df$gSVO)
mean(df$gSVO)
names(df %>% select(starts_with("gsvo_out_")))

##Calcul du isvo par joueur 
df <- df %>%
  mutate(
    B_other = rowMeans(
      select(., starts_with("isvo_other_")),
      na.rm = TRUE
    ),
    B_self = rowMeans(
      select(., starts_with("isvo_self_")),
      na.rm = TRUE
    ),
    
    iSVO = atan2(
      B_other - 50,
      B_self - 50
    ) * 180 / pi
  )


max(df$iSVO)
min(df$iSVO)
mean(df$iSVO)
names(df %>% select(starts_with("isvo_self_")))

#Corrélation entre gSVO et iSVO   
cor(df$gSVO, df$iSVO, use = "complete.obs")

#Join score iSVO et gSVO dans mon df avec le profil de mes joueurs
primary_data <- primary_data %>%
  left_join(
    df %>% select(participant, gSVO, iSVO),
    by = "participant")

#Vérif des hypothèses ----
primary_data <- primary_data %>%
  mutate(Player_type_IPD_recode = NA)
primary_data <- primary_data %>%
  mutate(
    Player_type_IPD_recode = case_when(
      Player_type_IPD == "Unconditional\nnon cooperator" ~ "0",
      Player_type_IPD == "Unconditional\ncooperator" ~ "1",
      Player_type_IPD == "Only Ingroup\nconditional cooperator" ~ "2",
      Player_type_IPD == "Only Outgroup\nconditional cooperator" ~ "3",
      Player_type_IPD == "Ingroup and Outgroup\nconditional cooperator" ~ "4",
      Player_type_IPD == "Undefined" ~ "5",
      TRUE ~ Player_type_IPD
    )
  )
unique(primary_data$Player_type_IPD)
primary_data$Player_type_IPD <- factor(primary_data$Player_type_IPD_recode)
primary_data$Player_type_IPD <- relevel(primary_data$Player_type_IPD, 
                                        ref = "0")
levels(primary_data$Player_type_IPD)
library(nnet)
model_H4a <- multinom(
  Player_type_IPD_recode ~ iSVO + gSVO,
  data = primary_data
)
summary(model_H4a)

#Tab_final => tableau qui résume H4a et H4b
tab_final_H4Ab <- broom::tidy(model_H4a) %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    
    estimate = paste0(round(estimate, 3), stars)
  ) %>%
  select(y.level, term, estimate) %>%
  pivot_wider(
    names_from = y.level,
    values_from = estimate)

knitr::kable(tab_final_H4Ab)

tab_final_H4Ab <- tab_final_H4Ab %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercept, réf = UNC",
      TRUE ~ term
    )
  )
tab_final_H4Ab <- tab_final_H4Ab %>%
  rename(
    UC   = `1`,
    OiCC = `2`,
    OoCC = `3`,
    ioCC = `4`,
    `n.c.` = `5`
  )



tt <- broom::tidy(model_H4a)
# H4a : iSVO > 0 pour y.level 2 (OiCC) et 4 (ioCC)
subset(tt, term=="iSVO" & y.level %in% c("2","4")) |>
  transform(p_one_sided = 1 - pnorm(estimate/std.error))
# H4b : gSVO < 0 pour y.level 3 (OoCC) et 4 (ioCC)
subset(tt, term=="gSVO" & y.level %in% c("3","4")) |>
  transform(p_one_sided = pnorm(estimate/std.error))


#H4c ----
data_logit <- primary_data %>%
  filter(Player_type_IPD_recode %in% c("2","3")) %>%          # 2 = OiCC, 3 = OoCC
  mutate(OiCC_binary = ifelse(Player_type_IPD_recode == "2", 1, 0))

table(data_logit$OiCC_binary)   # sanity: doit contenir des 0 ET des 1

model_OiCC_vs_OoCC <- glm(OiCC_binary ~ iSVO + gSVO,
                          data = data_logit, family = binomial(link = "logit"))
summary(model_OiCC_vs_OoCC)

tc <- broom::tidy(model_OiCC_vs_OoCC)
# H4c : OiCC plus altruiste ingroup (iSVO plus élevé) -> coef iSVO > 0
subset(tc, term=="iSVO") |> transform(p_one_sided = 1 - pnorm(estimate/std.error))
# H4d : OoCC plus parochial (gSVO plus bas) <=> OiCC gSVO plus élevé -> coef gSVO > 0
subset(tc, term=="gSVO") |> transform(p_one_sided = 1 - pnorm(estimate/std.error))


tab <- broom::tidy(model_OiCC_vs_OoCC) %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3), stars)
  ) %>%
  select(term, estimate)
  knitr::kable(tab)
  tab <- tab %>%
    mutate(
      term = case_when(
        term == "(Intercept)" ~ "Intercept, réf = OOCC",
        TRUE ~ term
      )
    )
  
  
  modelsummary(
    list("oiCC VS ooCC" = model_OiCC_vs_OoCC),
    
    coef_map = c(
      "(Intercept)" = "Intercept, réf = ooCC",
      "iSVO" = "iSVO",
      "gSVO" = "gSVO"
    ),
    statistic = c("std.error", "p.value"),
    stars = c('*' = .05, '**' = .01, '***' = .001),
     fmt = 4,
     title = "H4c and H4d table"
  )

#H5 ----
#Calcul du nombre de jetons investis dans pd et ipd
  test <- t.test(df$ipd_uncond, df$pd_uncond,
                 paired = TRUE,
                 alternative = "greater")
  
  test
  tab_test <- data.frame(
    mean_tokens_uncond_ipd = mean(df$ipd_uncond, na.rm = TRUE),
    mean_tokens_uncond_pd  = mean(df$pd_uncond, na.rm = TRUE),
    diff     = mean(df$ipd_uncond, na.rm = TRUE) - mean(df$pd_uncond, na.rm = TRUE),
    t_value  = test$statistic,
    df       = test$parameter,
    p_value  = test$p.value
  )
  
  tab_test

  
  
  test_inverse <- t.test(df$pd_uncond, df$ipd_uncond,
                 paired = TRUE,
                 alternative = "greater")
  
  test_inverse
  tab_test_inverse <- data.frame(
    mean_tokens_uncond_pd = mean(df$pd_uncond, na.rm = TRUE),
    mean_tokens_uncond_ipd  = mean(df$ipd_uncond, na.rm = TRUE),
    diff     = mean(df$pd_uncond, na.rm = TRUE) - mean(df$ipd_uncond, na.rm = TRUE),
    t_value  = test_inverse$statistic,
    df       = test_inverse$parameter,
    p_value  = test_inverse$p.value
  )
  
  tab_test_inverse
  
  
  
  primary_data <- primary_data %>%
    left_join(
      df %>% select(participant, pd_uncond, ipd_uncond),
      by = "participant")
 
  #Graphique avec nuage de points et sans intervalle de confiance par groupe 
  ggplot(
    primary_data,
    aes(
      x = pd_uncond,
      y = ipd_uncond,
      color = Player_type_IPD
    )
  ) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_point(alpha = 0.7, size = 2) +
    labs(
      x = "Nombre de jetons investis en l'absence de conflit",
      y = "Nombre de jetons investis en présence de conflit",
      color = "Profil du joueur dans IPD"
    ) +
    theme_minimal()

  #Graphique avec nuage de points et intervalle de confiance par groupe
  ggplot(
    primary_data,
    aes(
      x = pd_uncond,
      y = ipd_uncond,
      color = Player_type_IPD
    )
  ) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE) +
    geom_point(alpha = 0.7, size = 2) +
    labs(
      x = "Nombre de jetons investis en l'absence de conflit",
      y = "Nombre de jetons investis en présence de conflit",
      color = "Profil du joueur dans IPD"
    ) +
    theme_minimal()
  
  
  token_ipd_vs_pd <- lm(
    ipd_uncond ~ pd_uncond,
    data = primary_data)
  summary(token_ipd_vs_pd)
  
  modelsummary(
    list("Contribution inconditionnelle IPD VS PD" = token_ipd_vs_pd),
    
    coef_map = c(
      "(Intercept)" = "Intercept",
      "pd_uncond" = "Contribution inconditionnelle PD"
    ),
    statistic = c("std.error", "p.value"),
    stars = c('*' = .05, '**' = .01, '***' = .001),
    fmt = 4,
    title = "Effet du nombre de jetons investis de manière inconditionnelle en l'absence de conflit sur le nombre de jetons investis de manière inconditionnelle en présence de conflit"
  )

  
  # Test sens inverse:
  test <- t.test(df$ipd_uncond, df$pd_uncond,
                 paired = TRUE,
                 alternative = "less")
  
  
  #H5 bonus ----
  #Somme de tous les jetons investis de manière conditionnelle par joueur (jeu des matrices)
  names(select(df, starts_with("pd_in")))
  names(select(df, starts_with("ipd_in")))
  
  df <- df %>%
    mutate(
      all_tokens_pd = rowSums(
        select(., starts_with("pd_in")),
        na.rm = TRUE
      ),
      all_tokens_ipd = rowSums(
        select(., starts_with("ipd_in")),
        na.rm = TRUE
      ),
      diff_tokens_pd_ipd = all_tokens_pd - all_tokens_ipd
    )
  
  names(select(df, starts_with("pd_in")))
  names(select(df, starts_with("ipd_in")))

  #Même test que dans H5  
  test_tokens_cond <- t.test(df$all_tokens_ipd, df$all_tokens_pd,
                 paired = TRUE,
                 alternative = "greater")
  
  test_tokens_cond
  tab_test_tokens_cond <- data.frame(
    mean_ipd = mean(df$all_tokens_ipd, na.rm = TRUE),
    mean_pd  = mean(df$all_tokens_pd, na.rm = TRUE),
    diff     = mean(df$all_tokens_ipd, na.rm = TRUE) - mean(df$all_tokens_pd, na.rm = TRUE),
    t_value  = test$statistic,
    df       = test$parameter,
    p_value  = test$p.value
  )
  
  tab_test_tokens_cond
 
  
#GRAPHIQUE avec les moyennes de jetons investis par groupe ----
  df <- df %>%
    mutate(
      mean_tokens_pd = rowMeans(
        select(., starts_with("pd_in")),
        na.rm = TRUE
      ),
      mean_tokens_ipd = rowMeans(
        select(., starts_with("ipd_in")),
        na.rm = TRUE
      )
    )

  primary_data <- primary_data %>%
    left_join(
      df %>%
        dplyr::select(participant,mean_tokens_pd,mean_tokens_ipd),
        by= "participant"
    )
  
  df_mean_pd_ipd <- df %>%
    mutate(
      mean_tokens_pd_in0 = rowMeans(
        select(., starts_with("pd_in0")),
        na.rm = TRUE
      ),
      mean_tokens_pd_in1 = rowMeans(
        select(., starts_with("pd_in1")),
        na.rm = TRUE
      ),
      mean_tokens_pd_in2 = rowMeans(
        select(., starts_with("pd_in2")),
        na.rm = TRUE
      ),
      mean_tokens_pd_in3 = rowMeans(
        select(., starts_with("pd_in3")),
        na.rm = TRUE
      ),
      mean_tokens_pd_in4 = rowMeans(
        select(., starts_with("pd_in4")),
        na.rm = TRUE
      ),
      mean_tokens_ipd_in0 = rowMeans(
        select(., starts_with("ipd_in0")),
        na.rm = TRUE
      ),
      mean_tokens_ipd_in1 = rowMeans(
        select(., starts_with("ipd_in1")),
        na.rm = TRUE
      ),
      mean_tokens_ipd_in2 = rowMeans(
        select(., starts_with("ipd_in2")),
        na.rm = TRUE
      ),
      mean_tokens_ipd_in3 = rowMeans(
        select(., starts_with("ipd_in3")),
        na.rm = TRUE
      ),
      mean_tokens_ipd_in4 = rowMeans(
        select(., starts_with("ipd_in4")),
        na.rm = TRUE
      ),
      mean_tokens_pd_out0 = rowMeans(
        select(., starts_with("pd") & ends_with("out0")),
        na.rm = TRUE
      ),
      mean_tokens_pd_out1 = rowMeans(
        select(., starts_with("pd") & ends_with("out1")),
        na.rm = TRUE
      ),
      mean_tokens_pd_out2 = rowMeans(
        select(., starts_with("pd") & ends_with("out2")),
        na.rm = TRUE
      ),
      mean_tokens_pd_out3 = rowMeans(
        select(., starts_with("pd") & ends_with("out3")),
        na.rm = TRUE
      ),
      mean_tokens_pd_out4 = rowMeans(
        select(., starts_with("pd") & ends_with("out4")),
        na.rm = TRUE
      ),
      mean_tokens_ipd_out0 = rowMeans(
        select(., starts_with("ipd") & ends_with("out0")),
        na.rm = TRUE
      ),
      mean_tokens_ipd_out1 = rowMeans(
        select(., starts_with("ipd") & ends_with("out1")),
        na.rm = TRUE
      ),
      mean_tokens_ipd_out2 = rowMeans(
        select(., starts_with("ipd") & ends_with("out2")),
        na.rm = TRUE
      ),
      mean_tokens_ipd_out3 = rowMeans(
        select(., starts_with("ipd") & ends_with("out3")),
        na.rm = TRUE
      ),
      mean_tokens_ipd_out4 = rowMeans(
        select(., starts_with("ipd") & ends_with("out4")),
        na.rm = TRUE
      )
      )  

  primary_data <- primary_data %>%
    left_join(
      df_mean_pd_ipd %>%
        select(participant, starts_with("mean")),
      by = "participant"
    )
  
  primary_data_means_pd <- primary_data %>%
    pivot_longer(
      cols = c(
        starts_with("mean_tokens_pd_in"),
        starts_with("mean_tokens_pd_out")
      ),
      names_to = "variable",
      values_to = "value"
    ) %>%
    group_by(Player_type_PD, variable) %>%
    summarise(
      n = sum(!is.na(value)),
      mean_jetons_joueurs_groupe = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      se = sd / sqrt(n),
      ic_inf = mean_jetons_joueurs_groupe - qt(0.975, df = n - 1) * se,
      ic_sup = mean_jetons_joueurs_groupe + qt(0.975, df = n - 1) * se,
      .groups = "drop"
    )
  
  primary_data_means_pd <- primary_data_means_pd %>%
    mutate(
      nbr_jetons_autres_joueurs = as.numeric(stringr::str_sub(variable, -1))
    )
  
  
  primary_data_means_ipd <- primary_data %>%
    pivot_longer(
      cols = c(
        starts_with("mean_tokens_ipd_in"),
        starts_with("mean_tokens_ipd_out")
      ),
      names_to = "variable",
      values_to = "value"
    ) %>%
    group_by(Player_type_IPD, variable) %>%
    summarise(
      n = sum(!is.na(value)),
      mean_jetons_joueurs_groupe = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      se = sd / sqrt(n),
      ic_inf = mean_jetons_joueurs_groupe -
        qt(0.975, df = n - 1) * se,
      ic_sup = mean_jetons_joueurs_groupe +
        qt(0.975, df = n - 1) * se,
      .groups = "drop"
    ) %>%
    mutate(
      nbr_jetons_autres_joueurs = as.numeric(stringr::str_sub(variable, -1))
    )
  
  
  #Tracer les graphiques ----
  ##Graphique jeu PD ----
  pd <- position_dodge(width = 0.2)
  
  tokens_pd_in_groups <- ggplot(df_plot_pd_in,
         aes(x = nbr_jetons_autres_joueurs,
             y = mean_jetons_joueurs_groupe,
             color = Player_type_PD,
             group = Player_type_PD)) +
    geom_errorbar(
      aes(ymin = ic_inf, ymax = ic_sup),
      position = pd,
      width = 0.15,
      linewidth = 1.1
    ) +
    geom_line(position = pd, linewidth = 1) +
    geom_point(position = pd, size = 3)+
    theme_minimal() +
    scale_color_discrete(
      labels = c(
        "Ingroup and Outgroup\nconditional cooperator"  = "ioCC (14%)",
        "Only Ingroup\nconditional cooperator"   = "iCC (21%)",
        "Only Outgroup\nconditional cooperator" = "oCC (6%)",
        "Unconditional\ncooperator" = "uCC (9%)",
        "Unconditional\nnon cooperator" = "uNC (10%)",
        "Undefined" = "n.c (40%)"
      )
    )+
    labs(
      x = "Nombre de jetons des autres joueurs ingroup",
      y = "Moyenne des jetons du groupe",
      color = "Type de joueur (PD)",
      title = "Nombre moyen de jetons investis par groupe en fonction des jetons ingroup dans le jeu PD")
  
  print(tokens_pd_in_groups)
 ggsave(
   filename = "results/figures/mean_tokens_pd_in_groups.png",
   plot = tokens_pd_in_groups, width = 10, height = 6,dpi = 300)
 
 
 
 df_plot_pd_out <- primary_data_means_pd %>%
   filter(str_detect(variable, "^mean_tokens_pd_out"))
 
 pd <- position_dodge(width = 0.3)
 tokens_pd_out_groups <- ggplot(df_plot_pd_out,
                               aes(x = nbr_jetons_autres_joueurs,
                                   y = mean_jetons_joueurs_groupe,
                                   color = Player_type_PD,
                                   group = Player_type_PD)) +
   geom_errorbar(
     aes(ymin = ic_inf, ymax = ic_sup),
     position = pd,
     width = 0.15,
     linewidth = 1.1
   ) +
   geom_line(position = pd, linewidth = 1) +
   geom_point(position = pd, size = 3)+
   theme_minimal() +
   scale_x_continuous(breaks = 0:4)+
 coord_cartesian(ylim = c(0,4))+
   scale_color_discrete(
     labels = c(
       "Ingroup and Outgroup\nconditional cooperator"  = "ioCC (14%)",
       "Only Ingroup\nconditional cooperator"   = "iCC (21%)",
       "Only Outgroup\nconditional cooperator" = "oCC (6%)",
       "Unconditional\ncooperator" = "uCC (9%)",
       "Unconditional\nnon cooperator" = "uNC (10%)",
       "Undefined" = "n.c (40%)"
     )
   )+
   labs(
     x = "Nombre de jetons des autres joueurs outgroup",
     y = "Moyenne des jetons du groupe",
     color = "Type de joueur (PD)",
     title = "Nombre moyen de jetons investis par groupe en fonction des jetons outgroup dans le jeu PD"
   )
 print(tokens_pd_out_groups)
 ggsave(
   filename = "results/figures/mean_tokens_pd_out_groups.png",
   plot = tokens_pd_out_groups, width = 10, height = 6,dpi = 300)
 
 
 ##Graphique jeu IPD ----
 df_plot_ipd_in <- primary_data_means_ipd %>%
   filter(str_detect(variable, "^mean_tokens_ipd_in"))
 
 ipd_position <- position_dodge(width = 0.3)
 tokens_ipd_in_groups <- ggplot(df_plot_ipd_in,
                                aes(x = nbr_jetons_autres_joueurs,
                                    y = mean_jetons_joueurs_groupe,
                                    color = Player_type_IPD,
                                    group = Player_type_IPD)) +
   geom_errorbar(
     aes(ymin = ic_inf, ymax = ic_sup),
     position = ipd_position,
     width = 0.15,
     linewidth = 1.1
   ) +
   geom_line(linewidth = 1) +
   geom_point(position = pd, size = 3)+
   theme_minimal() +
   scale_color_discrete(
     labels = c(
       "Ingroup and Outgroup\nconditional cooperator"  = "ioCC (15%)",
       "Only Ingroup\nconditional cooperator"   = "iCC (13%)",
       "Only Outgroup\nconditional cooperator" = "oCC (11%)",
       "Unconditional\ncooperator" = "uCC (6%)",
       "Unconditional\nnon cooperator" = "uNC (12%)",
       "Undefined" = "n.c (43%)"
     )
   )+
   labs(
     x = "Nombre de jetons des autres joueurs ingroup",
     y = "Moyenne des jetons du groupe",
     color = "Type de joueur (IPD)",
     title = "Nombre moyen de jetons investis par groupe en fonction des jetons ingroup dans le jeu IPD"
   )
 print(tokens_ipd_in_groups)
 ggsave(
   filename = "results/figures/mean_tokens_ipd_in_groups.png",
   plot =  tokens_ipd_in_groups, width = 10, height = 6,dpi = 300)
 
 
 ipd_position <- position_dodge(width = 0.3)
 tokens_ipd_out_groups <- ggplot(df_plot_ipd_out,
                                aes(x = nbr_jetons_autres_joueurs,
                                    y = mean_jetons_joueurs_groupe,
                                    color = Player_type_IPD,
                                    group = Player_type_IPD)) +
   geom_errorbar(
     aes(ymin = ic_inf, ymax = ic_sup),
     position = ipd_position,
     width = 0.15,
     linewidth = 1.1
   ) +
   geom_line(linewidth = 1) +
   geom_point(position = pd, size = 3)+
   theme_minimal() +
   scale_color_discrete(
     labels = c(
       "Ingroup and Outgroup\nconditional cooperator"  = "ioCC (15%)",
       "Only Ingroup\nconditional cooperator"   = "iCC (13%)",
       "Only Outgroup\nconditional cooperator" = "oCC (11%)",
       "Unconditional\ncooperator" = "uCC (6%)",
       "Unconditional\nnon cooperator" = "uNC (12%)",
       "Undefined" = "n.c (43%)"
     )
   )+
   scale_x_continuous(breaks = 0:4)+
   coord_cartesian(ylim = c(0,4)) +
   labs(
     x = "Nombre de jetons des autres joueurs outgroup",
     y = "Moyenne des jetons du groupe",
     color = "Type de joueur (IPD)",
     title = "Nombre moyen de jetons investis par groupe en fonction des jetons outgroup dans le jeu IPD"
   )
 print(tokens_ipd_out_groups)
 ggsave(
   filename = "results/figures/mean_tokens_ipd_out_groups.png",
   plot =  tokens_ipd_out_groups, width = 10, height = 6,dpi = 300)
 
 
 
 library(patchwork)
 
  (tokens_pd_in_groups | tokens_pd_out_groups) /
   (tokens_ipd_in_groups | tokens_ipd_out_groups) 

 #Graphique tous joueurs ----
 primary_data_means_pd_all <- primary_data %>%
   pivot_longer(
     cols = c(
       starts_with("mean_tokens_pd_in"),
       starts_with("mean_tokens_pd_out")
     ),
     names_to = "variable",
     values_to = "value"
   ) %>%
   group_by(variable) %>%
   summarise(
     n = sum(!is.na(value)),
     mean_jetons_all_players = mean(value, na.rm = TRUE),
     sd = sd(value, na.rm = TRUE),
     se = sd / sqrt(n),
     ic_inf = mean_jetons_all_players -
       qt(0.975, df = n - 1) * se,
     ic_sup = mean_jetons_all_players +
       qt(0.975, df = n - 1) * se,
     .groups = "drop"
   ) %>%
   mutate(
     nbr_jetons_autres_joueurs = as.numeric(stringr::str_sub(variable, -1))
   )
 
 primary_data_means_ipd_all <- primary_data %>%
   pivot_longer(
     cols = c(
       starts_with("mean_tokens_ipd_in"),
       starts_with("mean_tokens_ipd_out")
     ),
     names_to = "variable",
     values_to = "value"
   ) %>%
   group_by(variable) %>%
   summarise(
     n = sum(!is.na(value)),
     mean_jetons_all_players = mean(value, na.rm = TRUE),
     sd = sd(value, na.rm = TRUE),
     se = sd / sqrt(n),
     ic_inf = mean_jetons_all_players -
       qt(0.975, df = n - 1) * se,
     ic_sup = mean_jetons_all_players +
       qt(0.975, df = n - 1) * se,
     .groups = "drop"
   ) %>%
   mutate(
     nbr_jetons_autres_joueurs = as.numeric(stringr::str_sub(variable, -1))
   )
 
 
 
 ##Graphique PD all players ----
 df_plot_pd_in_all <- primary_data_means_pd_all %>%
   filter(str_detect(variable, "^mean_tokens_pd_in"))
 
 tokens_pd_in_all <- ggplot(df_plot_pd_in_all,
                                aes(x = nbr_jetons_autres_joueurs,
                                    y = mean_jetons_all_players)) +
   geom_errorbar(
     aes(ymin = ic_inf, ymax = ic_sup),
     width = 0.15,
     linewidth = 1
   )+
   geom_line(linewidth = 1) +
   geom_point(position = pd, size = 3)+
   geom_line(linewidth = 1) +
   scale_x_continuous(limits = c(0, 4), breaks = 0:4) +
   scale_y_continuous(limits = c(0, 4), breaks = 0:4) +
   theme_minimal() +
   labs(
     x = "Nombre de jetons des autres joueurs ingroup",
     y = "Moyenne des jetons de l'ensemble des joueurs",
     title = "Nombre moyen de jetons investis par l'ensemble des joueurs en fonction des jetons ingroup dans le jeu PD"
   )
   print( tokens_pd_in_all)
 ggsave(
   filename = "results/figures/mean_tokens_pd_in_all_players.png",
   plot = tokens_pd_in_all, width = 10, height = 6,dpi = 300)
 

 
 df_plot_pd_out_all <- primary_data_means_pd_all %>%
   filter(str_detect(variable, "^mean_tokens_pd_out"))
 tokens_pd_out_all <-  ggplot(df_plot_pd_out_all,
                             aes(x = nbr_jetons_autres_joueurs,
                                 y = mean_jetons_all_players,
                             )) +
   geom_errorbar(
     aes(ymin = ic_inf, ymax = ic_sup),
     width = 0.15,
     linewidth = 1
   )+
   geom_point(alpha = 2,size = 3) +
   geom_line(linewidth = 1) +
   scale_x_continuous(limits = c(0, 4), breaks = 0:4) +
   scale_y_continuous(limits = c(0, 4), breaks = 0:4) +
   theme_minimal() +
   labs(
     x = "Nombre de jetons des autres joueurs outgroup",
     y = "Moyenne des jetons de l'ensemble des joueurs",
     title = "Nombre moyen de jetons investis par l'ensemble des joueurs en fonction des jetons outgroup dans le jeu PD"
   )
 print(tokens_pd_out_all)
 ggsave(
   filename = "results/figures/mean_tokens_pd_out_all_players.png",
   plot = tokens_pd_out_all, width = 10, height = 6,dpi = 300)
 
##Graphiques IPD all players ----
 df_plot_ipd_in_all <- primary_data_means_ipd_all %>%
   filter(str_detect(variable, "^mean_tokens_ipd_in"))
 tokens_ipd_in_all <-  ggplot(df_plot_ipd_in_all,
                             aes(x = nbr_jetons_autres_joueurs,
                                 y = mean_jetons_all_players,
                             )) +
   geom_errorbar(
     aes(ymin = ic_inf, ymax = ic_sup),
     width = 0.15,
     linewidth = 1
   )+
   geom_point(alpha = 2,size = 3) +
   geom_line(linewidth = 1) +
   scale_x_continuous(limits = c(0, 4), breaks = 0:4) +
   scale_y_continuous(limits = c(0, 4), breaks = 0:4) +
   theme_minimal() +
   labs(
     x = "Nombre de jetons des autres joueurs ingroup",
     y = "Moyenne des jetons de l'ensemble des joueurs",
     title = "Nombre moyen de jetons investis par l'ensemble des joueurs en fonction des jetons ingroup dans le jeu IPD"
   )
 print(tokens_ipd_in_all)
 ggsave(
   filename = "results/figures/mean_tokens_ipd_in_all_players.png",
   plot = tokens_ipd_in_all, width = 10, height = 6,dpi = 300)
 
 
 
 df_plot_ipd_out_all <- primary_data_means_ipd_all %>%
   filter(str_detect(variable, "^mean_tokens_ipd_out"))
 tokens_ipd_out_all <-  ggplot(df_plot_ipd_out_all,
                              aes(x = nbr_jetons_autres_joueurs,
                                  y = mean_jetons_all_players,
                              )) +
   geom_errorbar(
     aes(ymin = ic_inf, ymax = ic_sup),
     width = 0.15,
     linewidth = 1
   )+
   geom_point(alpha = 2,size = 3) +
   geom_line(linewidth = 1) +
   scale_x_continuous(limits = c(0, 4), breaks = 0:4) +
   scale_y_continuous(limits = c(0, 4), breaks = 0:4) +
   theme_minimal() +
   labs(
     x = "Nombre de jetons des autres joueurs outgroup",
     y = "Moyenne des jetons de l'ensemble des joueurs",
     title = "Nombre moyen de jetons investis par l'ensemble des joueurs en fonction des jetons outgroup dans le jeu IPD"
   )
 print(tokens_ipd_out_all)
 ggsave(
   filename = "results/figures/mean_tokens_ipd_out_all_players.png",
   plot = tokens_ipd_out_all, width = 10, height = 6,dpi = 300)
 
 #Tous les graphiques all players ensemble
 (tokens_pd_in_all | tokens_pd_out_all) /
   (tokens_ipd_in_all | tokens_ipd_out_all)
 

 #Graphiques 3D ----
 ##pd 3D all ----
 df_plot_pd_3D_all <- df %>%
   summarise(
     across(
       starts_with("pd_in"),
       ~ mean(.x, na.rm = TRUE)
     )
   )
 
 df_plot_pd_3D_all <- df_plot_pd_3D_all %>%
   pivot_longer(
     cols = c(
       starts_with("pd_in"),
     ),
     names_to = "variable",
     values_to = "value"
   ) 


df_plot_pd_3D_all <- df_plot_pd_3D_all %>%
  mutate(
    value_ingroup = as.numeric(stringr::str_extract(variable, "(?<=pd_in)\\d"))
  )

df_plot_pd_3D_all <- df_plot_pd_3D_all %>%
  mutate(
    value_outgroup = as.numeric(stringr::str_extract(variable, "(?<=out)\\d"))
  )

x_vals_pd_all <- sort(unique(df_plot_pd_3D_all$value_ingroup))
y_vals_pd_all <- sort(unique(df_plot_pd_3D_all$value_outgroup))


df_wide_pd_all <- df_plot_pd_3D_all %>%
  group_by(value_ingroup, value_outgroup) %>%
  summarise(value = mean(value), .groups = "drop") %>%
  pivot_wider(
    names_from = value_outgroup,
    values_from = value
  ) %>%
  arrange(value_ingroup)

z_matrix_pd_all <- as.matrix(df_wide_pd_all[,-1])


tokens_pd_all_3D <- plot_ly(
  x = x_vals_pd_all,
  y = y_vals_pd_all,
  z = t(z_matrix_pd_all),
  type = "surface",
  colorscale = "Blues",
  reversescale = TRUE,
  cmin = 0,
  cmax = 4,
  colorbar = list(
    tickmode = "array",
    tickvals = 0:4,
    ticktext = 0:4
  )
) %>%
  layout(
    title = "Surface des investissements moyens dans le jeu PD",
    scene = list(
      xaxis = list(
        title = "Jetons ingroup",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      ),
      yaxis = list(
        title = "Jetons outgroup",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      ),
      zaxis = list(
        title = "Moyenne des jetons investis",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      )
    )
  )
print(tokens_pd_all_3D)
htmlwidgets::saveWidget(
  tokens_pd_all_3D,
  "results/figures/mean_tokens_3D_pd_all.html"
)

###Heatmap 3D PD all ----
heatmap_3D_pd_all <- ggplot(
  df_plot_pd_3D_all,
  aes(
    x = value_ingroup,
    y = value_outgroup,
    fill = value
  )
) +
  geom_tile(color = "white") +
  
  geom_text(aes(label = round(value, 2))) +
  
  scale_fill_gradient(
    low = "lightblue",
    high = "navy",
    breaks = c(0, 1, 2, 3, 4)
  )+
  theme_minimal() +
  labs(
    title = "Nombre moyen de jetons investis dans le jeu PD en fonction des investissements ingroup et outgroup (tous les joueurs)",
    x = "Jetons investis ingroup",
    y = "Jetons investis outgroup",
    fill = "Moyenne de jetons investis dans le jeu PD"
  )

print(heatmap_3D_pd_all)
ggsave(
  filename = "results/figures/heatmap jetons PD all players.png",
  plot = heatmap_3D_pd_all,
  width = 10,
  height = 6,
  dpi = 300
)

##ipd all 3D ----
df_plot_ipd_3D_all <- df %>%
  summarise(
    across(
      starts_with("ipd_in"),
      ~ mean(.x, na.rm = TRUE)
    )
  )

df_plot_ipd_3D_all <- df_plot_ipd_3D_all %>%
  pivot_longer(
    cols = c(
      starts_with("ipd_in"),
    ),
    names_to = "variable",
    values_to = "value"
  ) 


df_plot_ipd_3D_all <- df_plot_ipd_3D_all %>%
  mutate(
    value_ingroup = as.numeric(stringr::str_extract(variable, "(?<=ipd_in)\\d"))
  )

df_plot_ipd_3D_all <- df_plot_ipd_3D_all %>%
  mutate(
    value_outgroup = as.numeric(stringr::str_extract(variable, "(?<=out)\\d"))
  )

x_vals_ipd_all <- sort(unique(df_plot_ipd_3D_all$value_ingroup))
y_vals_ipd_all <- sort(unique(df_plot_ipd_3D_all$value_outgroup))


df_wide_ipd_all <- df_plot_ipd_3D_all %>%
  group_by(value_ingroup, value_outgroup) %>%
  summarise(value = mean(value), .groups = "drop") %>%
  pivot_wider(
    names_from = value_outgroup,
    values_from = value
  ) %>%
  arrange(value_ingroup)

z_matrix_ipd_all <- as.matrix(df_wide_ipd_all[,-1])


tokens_ipd_all_3D <- plot_ly(
  x = x_vals_ipd_all,
  y = y_vals_ipd_all,
  z = t(z_matrix_ipd_all),
  type = "surface",
  colorscale = "Blues",
  reversescale = TRUE,
  cmin = 0,
  cmax = 4,
  colorbar = list(
    tickmode = "array",
    tickvals = 0:4,
    ticktext = 0:4
  )
) %>%
  layout(
    title = "Surface des investissements moyens dans le jeu IPD",
    scene = list(
      xaxis = list(
        title = "Jetons ingroup",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      ),
      yaxis = list(
        title = "Jetons outgroup",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      ),
      zaxis = list(
        title = "Moyenne des jetons investis",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      )
    )
  )
print(tokens_ipd_all_3D)
htmlwidgets::saveWidget(
  tokens_ipd_all_3D,
  "results/figures/mean_tokens_3D_ipd_all.html"
)


###Heatmap 3D all ----
heatmap_3D_ipd_all <- ggplot(
  df_plot_ipd_3D_all,
  aes(
    x = value_ingroup,
    y = value_outgroup,
    fill = value
  )
) +
  geom_tile(color = "white") +
  
  geom_text(aes(label = round(value, 2))) +
  
  scale_fill_gradient(
    low = "lightblue",
    high = "navy",
    breaks = c(0, 1, 1.75)
  )+
  
  theme_minimal() +
  labs(
    title = "Nombre moyen de jetons investis dans le jeu IPD en fonction des investissements ingroup et outgroup (tous les joueurs)",
    x = "Jetons investis ingroup",
    y = "Jetons investis outgroup",
    fill = "Moyenne de jetons investis dans le jeu IPD"
  )

print(heatmap_3D_ipd_all)
ggsave(
  filename = "results/figures/heatmap jetons IPD all players.png",
  plot = heatmap_3D_ipd_all,
  width = 10,
  height = 6,
  dpi = 300
)


#Graphique 3D PD - Groups ----

df <- df %>%
  left_join(
    primary_data %>%
      select(participant,Player_type_PD,Player_type_IPD),
    by = "participant")

df <- df %>%
  mutate(Player_PD_recode = NA)
df <- df %>%
  mutate(
    Player_type_PD_recode = case_when(
      Player_type_PD == "Unconditional\nnon cooperator" ~ "0",
      Player_type_PD == "Unconditional\ncooperator" ~ "1",
      Player_type_PD == "Only Ingroup\nconditional cooperator" ~ "2",
      Player_type_PD == "Only Outgroup\nconditional cooperator" ~ "3",
      Player_type_PD == "Ingroup and Outgroup\nconditional cooperator" ~ "4",
      Player_type_PD == "Undefined" ~ "5",
      TRUE ~ Player_type_PD
    )
  )
unique(df$Player_type_PD)


#Graphique 3D PD pipeline ----
##Graphique 3D PD groupes ---- 
plot_surface_pd <- function(data, group_value, title = "") {
  
  df_plot <- data %>%
    filter(Player_type_PD == group_value) %>%
    summarise(across(starts_with("pd_in"), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(
      cols = starts_with("pd_in"),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      value_ingroup = as.numeric(str_extract(variable, "(?<=pd_in)\\d")),
      value_outgroup = as.numeric(str_extract(variable, "(?<=out)\\d"))
    )
  
  grid <- df_plot %>%
    group_by(value_ingroup, value_outgroup) %>%
    summarise(value = mean(value), .groups = "drop") %>%
    pivot_wider(names_from = value_outgroup, values_from = value) %>%
    arrange(value_ingroup)
  
  x_vals <- sort(unique(df_plot$value_ingroup))
  y_vals <- sort(unique(df_plot$value_outgroup))
  z_matrix <- as.matrix(grid[,-1])
  
  plot_ly(
    x = x_vals,
    y = y_vals,
    z = t(z_matrix),
    type = "surface",
    colorscale = "Blues",
    reversescale = TRUE,
    cmin = 0,
    cmax = 4,
    colorbar = list(
      tickmode = "array",
      tickvals = 0:4,
      ticktext = 0:4
    )
  ) %>%
    layout(
      title = title,
      scene = list(
        xaxis = list(title = "Ingroup (0–4)", tickvals = 0:4),
        yaxis = list(title = "Outgroup (0–4)", tickvals = 0:4),
        zaxis = list(title = "Mean tokens", tickvals = 0:4)
      )
    )
}
types <- unique(df$Player_type_PD)

plots <- lapply(types, function(t) {
  plot_surface_pd(df, t, title = t)
})

#Afficher les graphiques tracés ----
plots[[1]] #PC - Unconditional Cooperator
plots[[2]] #PC - Undefined
plots[[3]] #PD - ICC
plots[[4]] #PD - UNC
plots[[5]] #PD - IOCC
plots[[6]] #PD - OCC

library(htmlwidgets)
names_plots <- c(
  "iCC_PD",
  "nc_PD",
  "oCC_PD",
  "ioCC_PD",
  "UNC_PD",
  "UC_PD"
)

for(i in seq_along(plots)){
  saveWidget(
    plots[[i]],
    file = paste0("results/figures/figures 3D", names_plots[i], ".html"),
    selfcontained = TRUE
  )
}


#3D IPD pipeline ----
##Graphique 3D IPD groupes ---- 
plot_surface_ipd <- function(data, group_value, title = "") {
  
  df_plot_ipd <- data %>%
    filter(Player_type_IPD == group_value) %>%
    summarise(across(starts_with("ipd_in"), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(
      cols = starts_with("ipd_in"),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      value_ingroup = as.numeric(str_extract(variable, "(?<=ipd_in)\\d")),
      value_outgroup = as.numeric(str_extract(variable, "(?<=out)\\d"))
    )
  
  grid <- df_plot_ipd %>%
    group_by(value_ingroup, value_outgroup) %>%
    summarise(value = mean(value), .groups = "drop") %>%
    pivot_wider(names_from = value_outgroup, values_from = value) %>%
    arrange(value_ingroup)
  
  x_vals_ipd <- sort(unique(df_plot_ipd$value_ingroup))
  y_vals_ipd <- sort(unique(df_plot_ipd$value_outgroup))
  z_matrix_ipd <- as.matrix(grid[,-1])
  
  plot_ly(
    x = x_vals_ipd,
    y = y_vals_ipd,
    z = t(z_matrix_ipd),
    type = "surface",
    colorscale = "Blues",
    reversescale = TRUE,
    cmin = 0,
    cmax = 4,
    colorbar = list(
      tickmode = "array",
      tickvals = 0:4,
      ticktext = 0:4
    )
  ) %>%
    layout(
      title = title,
      scene = list(
        xaxis = list(title = "Ingroup (0–4)", tickvals = 0:4),
        yaxis = list(title = "Outgroup (0–4)", tickvals = 0:4),
        zaxis = list(title = "Mean tokens", tickvals = 0:4)
      )
    )
}
types <- unique(df$Player_type_IPD)

plots <- lapply(types, function(t) {
  plot_surface_ipd(df, t, title = t)
})

###Afficher les graphiques tracés ----
plots[[1]] #iCC - IPD 
plots[[2]] #n.c - IPD
plots[[3]] #oCC - IPD
plots[[4]] #ioCC - IPD
plots[[5]] #UNC - IPD
plots[[6]] #UC - IPD
library(htmlwidgets)
names_plots <- c(
  "iCC_IPD",
  "nc_IPD",
  "oCC_IPD",
  "ioCC_IPD",
  "UNC_IPD",
  "UC_IPD"
)

for(i in seq_along(plots)){
  saveWidget(
    plots[[i]],
    file = paste0("results/figures/figures 3D", names_plots[i], ".html"),
    selfcontained = TRUE
  )
}


#Graphique 3D individuel sans pipeline ----
##PD - UNC ----
df_plot_pd_unc_3D <- df %>%
  filter(Player_type_PD == "Unconditional\nnon cooperator")%>%
  summarise(
    across(
      starts_with("pd_in"),
      ~ mean(.x, na.rm = TRUE)
    )
  )

df_plot_pd_unc_3D <- df_plot_pd_unc_3D %>%
  pivot_longer(
    cols = c(
      starts_with("pd_in"),
    ),
    names_to = "variable",
    values_to = "value"
  ) 


df_plot_pd_unc_3D <- df_plot_pd_unc_3D %>%
  mutate(
    value_ingroup = as.numeric(stringr::str_extract(variable, "(?<=pd_in)\\d"))
  )

df_plot_pd_unc_3D <- df_plot_pd_unc_3D %>%
  mutate(
    value_outgroup = as.numeric(stringr::str_extract(variable, "(?<=out)\\d"))
  )

x_vals_unc_pd <- sort(unique(df_plot_pd_unc_3D$value_ingroup))
y_vals_unc_pd <- sort(unique(df_plot_pd_unc_3D$value_outgroup))


df_wide <- df_plot_pd_unc_3D %>%
  group_by(value_ingroup, value_outgroup) %>%
  summarise(value = mean(value), .groups = "drop") %>%
  pivot_wider(
    names_from = value_outgroup,
    values_from = value
  ) %>%
  arrange(value_ingroup)

z_matrix_unc_pd <- as.matrix(df_wide[,-1])


tokens_pd_unc_3D <- plot_ly(
  x = x_vals_unc_pd,
  y = y_vals_unc_pd,
  z = z_matrix_unc_pd,
  type = "surface",
  colorscale = "Blues",
  cmin = 0,
  cmax = 4
) %>%
  layout(
    scene = list(
      xaxis = list(
        title = "Jetons ingroup (0–4)",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      ),
      yaxis = list(
        title = "Jetons outgroup (0–4)",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      ),
      zaxis = list(
        title = "Moyenne des jetons investis",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      )
    )
  )
print(tokens_pd_unc_3D)
htmlwidgets::saveWidget(
  tokens_pd_unc_3D,
  "results/figures/mean_tokens_pd_unc_3D.html"
)


##PD - UC ----
df_plot_pd_uc_3D <- df %>%
  filter(Player_type_PD == "Unconditional\ncooperator")%>%
  summarise(
    across(
      starts_with("pd_in"),
      ~ mean(.x, na.rm = TRUE)
    )
  )

df_plot_pd_uc_3D <- df_plot_pd_uc_3D %>%
  pivot_longer(
    cols = c(
      starts_with("pd_in"),
    ),
    names_to = "variable",
    values_to = "value"
  ) 


df_plot_pd_uc_3D <- df_plot_pd_uc_3D %>%
  mutate(
    value_ingroup = as.numeric(stringr::str_extract(variable, "(?<=pd_in)\\d"))
  )

df_plot_pd_uc_3D <- df_plot_pd_uc_3D %>%
  mutate(
    value_outgroup = as.numeric(stringr::str_extract(variable, "(?<=out)\\d"))
  )


x_vals_uc_pd <- sort(unique(df_plot_pd_uc_3D$value_ingroup))
y_vals_uc_pd <- sort(unique(df_plot_pd_uc_3D$value_outgroup))


df_wide_uc_pd <- df_plot_pd_uc_3D %>%
  group_by(value_ingroup, value_outgroup) %>%
  summarise(value = mean(value), .groups = "drop") %>%
  pivot_wider(
    names_from = value_outgroup,
    values_from = value
  ) %>%
  arrange(value_ingroup)

z_matrix_uc_pd <- as.matrix(df_wide_uc_pd[,-1])


tokens_pd_uc_3D <- plot_ly(
  x = x_vals_uc_pd,
  y = y_vals_uc_pd,
  z = z_matrix_uc_pd,
  type = "surface",
  colorscale = "Blues",
  cmin = 0,
  cmax = 4
) %>%
  layout(
    scene = list(
      xaxis = list(
        title = "Jetons ingroup (0–4)",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      ),
      yaxis = list(
        title = "Jetons outgroup (0–4)",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      ),
      zaxis = list(
        title = "Moyenne des jetons investis",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      )
    )
  )
print(tokens_pd_uc_3D)
htmlwidgets::saveWidget(
  tokens_pd_uc_3D,
  "results/figures/mean_tokens_pd_uc_3D.html"
)

##PD - ioCC ----
df_plot_pd_ioCC_3D <- df %>%
  filter(Player_type_PD == "Ingroup and Outgroup\nconditional cooperator")%>%
  summarise(
    across(
      starts_with("pd_in"),
      ~ mean(.x, na.rm = TRUE)))

df_plot_pd_ioCC_3D <- df_plot_pd_ioCC_3D %>%
  pivot_longer(
    cols = c(
      starts_with("pd_in"),
      ),
    names_to = "variable",
    values_to = "value") 


df_plot_pd_ioCC_3D <- df_plot_pd_ioCC_3D %>%
  mutate(
    value_ingroup = as.numeric(stringr::str_extract(variable, "(?<=pd_in)\\d")))

df_plot_pd_ioCC_3D <- df_plot_pd_ioCC_3D %>%
  mutate(
    value_outgroup = as.numeric(stringr::str_extract(variable, "(?<=out)\\d")))

x_vals_iocc_pd <- sort(unique(df_plot_pd_ioCC_3D$value_ingroup))
y_vals_iocc_pd <- sort(unique(df_plot_pd_ioCC_3D$value_outgroup))

df_wide <- df_plot_pd_ioCC_3D %>%
  group_by(value_ingroup, value_outgroup) %>%
  summarise(value = mean(value), .groups = "drop") %>%
  pivot_wider(
    names_from = value_outgroup,
    values_from = value
  ) %>%
  arrange(value_ingroup)

z_matrix_iocc_pd <- as.matrix(df_wide[,-1])

tokens_pd_ioCC_3D <- plot_ly(
  x = x_vals_iocc_pd,
  y = y_vals_iocc_pd,
  z = z_matrix_iocc_pd,
  type = "surface",
  colorscale = "Blues",
  cmin = 0,
  cmax = 4
) %>%
  layout(
    scene = list(
      xaxis = list(
        title = "Jetons ingroup (0–4)",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      ),
      yaxis = list(
        title = "Jetons outgroup (0–4)",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      ),
      zaxis = list(
        title = "Moyenne des jetons investis",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      )
    )
  )
print(tokens_pd_ioCC_3D)
htmlwidgets::saveWidget(
  tokens_pd_ioCC_3D,
  "results/figures/mean_tokens_pd_ioCC_3D.html"
)

##PD - iCC ----
df_plot_pd_iCC_3D <- df %>%
  filter(Player_type_PD == "Only Ingroup\nconditional cooperator")%>%
  summarise(
    across(
      starts_with("pd_in"),
      ~ mean(.x, na.rm = TRUE)))

df_plot_pd_iCC_3D <- df_plot_pd_iCC_3D %>%
  pivot_longer(
    cols = c(
      starts_with("pd_in"),
    ),
    names_to = "variable",
    values_to = "value") 


df_plot_pd_iCC_3D <- df_plot_pd_iCC_3D %>%
  mutate(
    value_ingroup = as.numeric(stringr::str_extract(variable, "(?<=pd_in)\\d")))

df_plot_pd_iCC_3D <- df_plot_pd_iCC_3D %>%
  mutate(
    value_outgroup = as.numeric(stringr::str_extract(variable, "(?<=out)\\d")))

x_vals_icc_pd <- sort(unique(df_plot_pd_iCC_3D$value_ingroup))
y_vals_icc_pd <- sort(unique(df_plot_pd_iCC_3D$value_outgroup))


df_wide_icc_pd <- df_plot_pd_iCC_3D %>%
  group_by(value_ingroup, value_outgroup) %>%
  summarise(value = mean(value), .groups = "drop") %>%
  pivot_wider(
    names_from = value_outgroup,
    values_from = value
  ) %>%
  arrange(value_ingroup)

z_matrix_icc_pd <- as.matrix(df_wide_icc_pd[,-1])

tokens_pd_iCC_3D <- plot_ly(
  x = x_vals_icc_pd,
  y = y_vals_icc_pd,
  z = z_matrix_icc_pd,
  type = "surface",
  colorscale = "Blues",
  cmin = 0,
  cmax = 4
) %>%
  layout(
    scene = list(
      xaxis = list(
        title = "Jetons ingroup (0–4)",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      ),
      yaxis = list(
        title = "Jetons outgroup (0–4)",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      ),
      zaxis = list(
        title = "Moyenne des jetons investis",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      )
    )
  )
print(tokens_pd_iCC_3D)
htmlwidgets::saveWidget(
  tokens_pd_iCC_3D,
  "results/figures/mean_tokens_pd_iCC_3D.html"
)

##PD - oCC ----
df_plot_pd_oCC_3D <- df %>%
  filter(Player_type_PD == "Only Outgroup\nconditional cooperator")%>%
  summarise(
    across(
      starts_with("pd_in"),
      ~ mean(.x, na.rm = TRUE)))

df_plot_pd_oCC_3D <- df_plot_pd_oCC_3D %>%
  pivot_longer(
    cols = c(
      starts_with("pd_in"),
    ),
    names_to = "variable",
    values_to = "value") 


df_plot_pd_oCC_3D <- df_plot_pd_oCC_3D %>%
  mutate(
    value_ingroup = as.numeric(stringr::str_extract(variable, "(?<=pd_in)\\d")))

df_plot_pd_oCC_3D <- df_plot_pd_oCC_3D %>%
  mutate(
    value_outgroup = as.numeric(stringr::str_extract(variable, "(?<=out)\\d")))

x_vals_occ_pd <- sort(unique(df_plot_pd_oCC_3D$value_ingroup))
y_vals_occ_pd <- sort(unique(df_plot_pd_oCC_3D$value_outgroup))


df_wide_occ_pd <- df_plot_pd_oCC_3D %>%
  group_by(value_ingroup, value_outgroup) %>%
  summarise(value = mean(value), .groups = "drop") %>%
  pivot_wider(
    names_from = value_outgroup,
    values_from = value
  ) %>%
  arrange(value_ingroup)

z_matrix_occ_pd <- as.matrix(df_wide_occ_pd[,-1])


tokens_pd_oCC_3D <- plot_ly(
  x = x_vals_occ_pd,
  y = y_vals_occ_pd,
  z = z_matrix_occ_pd,
  type = "surface",
  colorscale = "Blues",
  cmin = 0,
  cmax = 4
) %>%
  layout(
    scene = list(
      xaxis = list(
        title = "Jetons ingroup (0–4)",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      ),
      yaxis = list(
        title = "Jetons outgroup (0–4)",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      ),
      zaxis = list(
        title = "Moyenne des jetons investis",
        range = c(0, 4),
        tickmode = "array",
        tickvals = 0:4
      )
    )
  )
print(tokens_pd_oCC_3D)
htmlwidgets::saveWidget(
  tokens_pd_oCC_3D,
  "results/figures/mean_tokens_pd_oCC_3D.html"
)

##PD - nc ----
df_plot_pd_nc_3D <- df %>%
  filter(Player_type_PD == "Undefined")%>%
  summarise(
    across(
      starts_with("pd_in"),
      ~ mean(.x, na.rm = TRUE)))

df_plot_pd_nc_3D <- df_plot_pd_nc_3D %>%
  pivot_longer(
    cols = c(
      starts_with("pd_in"),
    ),
    names_to = "variable",
    values_to = "value") 


df_plot_pd_nc_3D <- df_plot_pd_nc_3D %>%
  mutate(
    value_ingroup = as.numeric(stringr::str_extract(variable, "(?<=pd_in)\\d")))

df_plot_pd_nc_3D <- df_plot_pd_nc_3D %>%
  mutate(
    value_outgroup = as.numeric(stringr::str_extract(variable, "(?<=out)\\d")))


tokens_pd_nc_3D <- plot_ly(
  df_plot_pd_nc_3D %>% arrange(value_ingroup, value_outgroup),
  x = ~value_ingroup,
  y = ~value,
  z = ~value_outgroup,
  type = "scatter3d",
  mode = "lines+markers",
  color = ~factor(value_ingroup),
  line = list(width = 4)
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Jetons ingroup (0–4)", range = c(0, 4)),
      yaxis = list(title = "Jetons outgroup (0–4)", range = c(0, 4)),
      zaxis = list(title = "Moyenne des jetons investis dans le jeu PD par le groupe non défini", range = c(0, 4))
    )
  )
print(tokens_pd_nc_3D)
htmlwidgets::saveWidget(
  tokens_pd_nc_3D,
  "results/figures/mean_tokens_pd_nc_3D.html"
)

#Graphique 3D IPD - groups ----
df <- df %>%
  mutate(
    Player_type_IPD = case_when(
      Player_type_IPD == "0" ~ "Unconditional\nnon cooperator",
      Player_type_IPD == "1" ~ "Unconditional\ncooperator",
      Player_type_IPD == "2" ~ "Only Ingroup\nconditional cooperator",
      Player_type_IPD == "3" ~ "Only Outgroup\nconditional cooperator",
      Player_type_IPD == "4" ~ "Ingroup and Outgroup\nconditional cooperator",
      Player_type_IPD == "5" ~ "Undefined",
      TRUE ~ Player_type_IPD
    )
  )
unique(df$Player_type_IPD)

##IPD - UNC ----
df_plot_ipd_unc_3D <- df %>%
  filter(Player_type_IPD == "Unconditional\nnon cooperator")%>%
  summarise(
    across(
      starts_with("ipd_in"),
      ~ mean(.x, na.rm = TRUE)
    )
  )

df_plot_ipd_unc_3D <- df_plot_ipd_unc_3D %>%
  pivot_longer(
    cols = c(
      starts_with("ipd_in"),
    ),
    names_to = "variable",
    values_to = "value"
  ) 


df_plot_ipd_unc_3D <- df_plot_ipd_unc_3D %>%
  mutate(
    value_ingroup = as.numeric(stringr::str_extract(variable, "(?<=ipd_in)\\d"))
  )

df_plot_ipd_unc_3D <- df_plot_ipd_unc_3D %>%
  mutate(
    value_outgroup = as.numeric(stringr::str_extract(variable, "(?<=out)\\d"))
  )


tokens_ipd_unc_3D <- plot_ly(
  df_plot_ipd_unc_3D %>% arrange(value_ingroup, value_outgroup),
  x = ~value_ingroup,
  y = ~value,
  z = ~value_outgroup,
  type = "scatter3d",
  mode = "lines+markers",
  color = ~factor(value_outgroup),
  line = list(width = 4)
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Jetons ingroup (0–4)", range = c(0, 4)),
      yaxis = list(title = "Jetons outgroup (0–4)", range = c(0, 4)),
      zaxis = list(title = "Moyenne des jetons investis dans le jeu IPD par le groupe UNC", range = c(0, 4))
    )
  )
print(tokens_ipd_unc_3D)
htmlwidgets::saveWidget(
  tokens_ipd_unc_3D,
  "results/figures/mean_tokens_ipd_unc_3D.html"
)

##IPD - UC ----
df_plot_ipd_uc_3D <- df %>%
  filter(Player_type_IPD == "Unconditional\ncooperator")%>%
  summarise(
    across(
      starts_with("ipd_in"),
      ~ mean(.x, na.rm = TRUE)
    )
  )

df_plot_ipd_uc_3D <- df_plot_ipd_uc_3D %>%
  pivot_longer(
    cols = c(
      starts_with("ipd_in"),
    ),
    names_to = "variable",
    values_to = "value"
  ) 


df_plot_ipd_uc_3D <- df_plot_ipd_uc_3D %>%
  mutate(
    value_ingroup = as.numeric(stringr::str_extract(variable, "(?<=ipd_in)\\d"))
  )

df_plot_ipd_uc_3D <- df_plot_ipd_uc_3D %>%
  mutate(
    value_outgroup = as.numeric(stringr::str_extract(variable, "(?<=out)\\d"))
  )


tokens_ipd_uc_3D <- plot_ly(
  df_plot_ipd_uc_3D %>% arrange(value_ingroup, value_outgroup),
  x = ~value_ingroup,
  y = ~value,
  z = ~value_outgroup,
  type = "scatter3d",
  mode = "lines+markers",
  color = ~factor(value_outgroup),
  line = list(width = 4)
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Jetons ingroup (0–4)", range = c(0, 4)),
      yaxis = list(title = "Jetons outgroup (0–4)", range = c(0, 4)),
      zaxis = list(title = "Moyenne des jetons investis dans le jeu IPD par le groupe UC", range = c(0, 4))
    )
  )
print(tokens_ipd_uc_3D)
htmlwidgets::saveWidget(
  tokens_ipd_uc_3D,
  "results/figures/mean_tokens_ipd_uc_3D.html"
)

##IPD - ioCC ----
df_plot_ipd_ioCC_3D <- df %>%
  filter(Player_type_IPD == "Ingroup and Outgroup\nconditional cooperator")%>%
  summarise(
    across(
      starts_with("ipd_in"),
      ~ mean(.x, na.rm = TRUE)))

df_plot_ipd_ioCC_3D <- df_plot_ipd_ioCC_3D %>%
  pivot_longer(
    cols = c(
      starts_with("ipd_in"),
    ),
    names_to = "variable",
    values_to = "value") 


df_plot_ipd_ioCC_3D <- df_plot_ipd_ioCC_3D %>%
  mutate(
    value_ingroup = as.numeric(stringr::str_extract(variable, "(?<=ipd_in)\\d")))

df_plot_ipd_ioCC_3D <- df_plot_ipd_ioCC_3D %>%
  mutate(
    value_outgroup = as.numeric(stringr::str_extract(variable, "(?<=out)\\d")))


tokens_ipd_ioCC_3D <- plot_ly(
  df_plot_ipd_ioCC_3D %>% arrange(value_ingroup, value_outgroup),
  x = ~value_ingroup,
  y = ~value,
  z = ~value_outgroup,
  type = "scatter3d",
  mode = "lines+markers",
  color = ~factor(value_outgroup),
  line = list(width = 4)
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Jetons ingroup (0–4)", range = c(0, 4)),
      yaxis = list(title = "Jetons outgroup (0–4)", range = c(0, 4)),
      zaxis = list(title = "Moyenne des jetons investis dans le jeu IPD par le groupe ioCC", range = c(0, 4))
    )
  )
print(tokens_ipd_ioCC_3D)
htmlwidgets::saveWidget(
  tokens_ipd_ioCC_3D,
  "results/figures/mean_tokens_ipd_ioCC_3D.html"
)

##IPD - iCC ----
df_plot_ipd_iCC_3D <- df %>%
  filter(Player_type_IPD == "Only Ingroup\nconditional cooperator")%>%
  summarise(
    across(
      starts_with("ipd_in"),
      ~ mean(.x, na.rm = TRUE)))

df_plot_ipd_iCC_3D <- df_plot_ipd_iCC_3D %>%
  pivot_longer(
    cols = c(
      starts_with("ipd_in"),
    ),
    names_to = "variable",
    values_to = "value") 


df_plot_ipd_iCC_3D <- df_plot_ipd_iCC_3D %>%
  mutate(
    value_ingroup = as.numeric(stringr::str_extract(variable, "(?<=ipd_in)\\d")))

df_plot_ipd_iCC_3D <- df_plot_ipd_iCC_3D %>%
  mutate(
    value_outgroup = as.numeric(stringr::str_extract(variable, "(?<=out)\\d")))


tokens_ipd_iCC_3D <- plot_ly(
  df_plot_pd_iCC_3D %>% arrange(value_ingroup, value_outgroup),
  x = ~value_ingroup,
  y = ~value,
  z = ~value_outgroup,
  type = "scatter3d",
  mode = "lines+markers",
  color = ~factor(value_outgroup),
  line = list(width = 4)
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Jetons ingroup (0–4)", range = c(0, 4)),
      yaxis = list(title = "Jetons outgroup (0–4)", range = c(0, 4)),
      zaxis = list(title = "Moyenne des jetons investis dans le jeu IPD par le groupe iCC", range = c(0, 4))
    )
  )
print(tokens_ipd_iCC_3D)
htmlwidgets::saveWidget(
  tokens_ipd_iCC_3D,
  "results/figures/mean_tokens_ipd_iCC_3D.html"
)

##IPD - oCC ----
df_plot_ipd_oCC_3D <- df %>%
  filter(Player_type_IPD == "Only Outgroup\nconditional cooperator")%>%
  summarise(
    across(
      starts_with("ipd_in"),
      ~ mean(.x, na.rm = TRUE)))

df_plot_ipd_oCC_3D <- df_plot_ipd_oCC_3D %>%
  pivot_longer(
    cols = c(
      starts_with("ipd_in"),
    ),
    names_to = "variable",
    values_to = "value") 


df_plot_ipd_oCC_3D <- df_plot_ipd_oCC_3D %>%
  mutate(
    value_ingroup = as.numeric(stringr::str_extract(variable, "(?<=pd_in)\\d")))

df_plot_ipd_oCC_3D <- df_plot_ipd_oCC_3D %>%
  mutate(
    value_outgroup = as.numeric(stringr::str_extract(variable, "(?<=out)\\d")))


tokens_ipd_oCC_3D <- plot_ly(
  df_plot_ipd_oCC_3D %>% arrange(value_ingroup, value_outgroup),
  x = ~value_ingroup,
  y = ~value,
  z = ~value_outgroup,
  type = "scatter3d",
  mode = "lines+markers",
  color = ~factor(value_outgroup),
  line = list(width = 4)
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Jetons ingroup (0–4)", range = c(0, 4)),
      yaxis = list(title = "Jetons outgroup (0–4)", range = c(0, 4)),
      zaxis = list(title = "Moyenne des jetons investis dans le jeu IPD par le groupe oCC", range = c(0, 4))
    )
  )
print(tokens_ipd_oCC_3D)
htmlwidgets::saveWidget(
  tokens_ipd_oCC_3D,
  "results/figures/mean_tokens_ipd_oCC_3D.html"
)

##IPD - nc ----
df_plot_ipd_nc_3D <- df %>%
  filter(Player_type_IPD == "Undefined")%>%
  summarise(
    across(
      starts_with("ipd_in"),
      ~ mean(.x, na.rm = TRUE)))

df_plot_ipd_nc_3D <- df_plot_ipd_nc_3D %>%
  pivot_longer(
    cols = c(
      starts_with("ipd_in"),
    ),
    names_to = "variable",
    values_to = "value") 


df_plot_ipd_nc_3D <- df_plot_ipd_nc_3D %>%
  mutate(
    value_ingroup = as.numeric(stringr::str_extract(variable, "(?<=ipd_in)\\d")))

df_plot_ipd_nc_3D <- df_plot_ipd_nc_3D %>%
  mutate(
    value_outgroup = as.numeric(stringr::str_extract(variable, "(?<=out)\\d")))


tokens_ipd_nc_3D <- plot_ly(
  df_plot_ipd_nc_3D %>% arrange(value_ingroup, value_outgroup),
  x = ~value_ingroup,
  y = ~value,
  z = ~value_outgroup,
  type = "scatter3d",
  mode = "lines+markers",
  color = ~factor(value_outgroup),
  line = list(width = 4)
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Jetons ingroup (0–4)", range = c(0, 4)),
      yaxis = list(title = "Jetons outgroup (0–4)", range = c(0, 4)),
      zaxis = list(title = "Moyenne des jetons investis dans le jeu IPD par le groupe non défini", range = c(0, 4))
    )
  )
print(tokens_ipd_nc_3D)
htmlwidgets::saveWidget(
  tokens_pd_nc_3D,
  "results/figures/mean_tokens_ipd_nc_3D.html"
)







#Analyses exploratoires ----
#H6 ----
  
  
  
#Balance check ----
  ##Balance check 1ere partie
names(df)
df$diplome <- factor(df$diplome)
df$discipline <- factor(df$discipline)
library(gtsummary)
tbl_summary(
  data = df,
  by = part_1_selected_task_name,
  include = c(age, gender, diplome, discipline),
  missing = "no"
) %>%
  add_overall() %>%
  add_p(
    test = list(
      c(age) ~ "oneway.test",
      c(gender, diplome, discipline) ~ "chisq.test"
    ),
    test.args = list(
      c(age) ~ list(var.equal = TRUE),
      c(gender, diplome, discipline) ~ list(simulate.p.value = TRUE)
    )
  ) %>%
  bold_p()

##Balance check 2eme partie
tbl_summary(
  data = df,
  by = part_2_selected_task_name,
  include = c(iSVO,gSVO),
  missing = "no"
) %>%
  add_overall() %>%
  add_p(
    test = list(
      c(iSVO,gSVO) ~ "oneway.test"
    ),
    test.args = list(
      c(iSVO,gSVO) ~ list(var.equal = TRUE)
    )
  ) %>%
  bold_p()

#Test effet d'ordre ----
df <- df %>%
  left_join(
    primary_data %>%
      select(participant,Player_type_PD,Player_type_IPD),
    by = "participant")

df <- df %>%
  mutate(Player_PD_recode = NA)
df <- df %>%
  mutate(
    Player_type_PD_recode = case_when(
      Player_type_PD.y == "Unconditional\nnon cooperator" ~ "0",
      Player_type_PD.y == "Unconditional\ncooperator" ~ "1",
      Player_type_PD.y == "Only Ingroup\nconditional cooperator" ~ "2",
      Player_type_PD.y == "Only Outgroup\nconditional cooperator" ~ "3",
      Player_type_PD.y == "Ingroup and Outgroup\nconditional cooperator" ~ "4",
      Player_type_PD.y == "Undefined" ~ "5",
      TRUE ~ Player_type_PD.y
    )
  )

df <- df %>%
  mutate(
    Player_type_IPD_recode = case_when(
      Player_type_IPD.y == "Unconditional\nnon cooperator" ~ "0",
      Player_type_IPD.y  == "Unconditional\ncooperator" ~ "1",
      Player_type_IPD.y  == "Only Ingroup\nconditional cooperator" ~ "2",
      Player_type_IPD.y  == "Only Outgroup\nconditional cooperator" ~ "3",
      Player_type_IPD.y  == "Ingroup and Outgroup\nconditional cooperator" ~ "4",
      Player_type_IPD.y  == "Undefined" ~ "5",
      TRUE ~ Player_type_IPD.y 
    )
  )

##Effet d'ordre profil des joueurs ----
df$Player_type_IPD_recode <- as.factor(df$Player_type_IPD_recode)
df$Player_type_PD_recode <- as.factor(df$Player_type_PD_recode)
levels(df$Player_type_IPD_recode)
levels(df$Player_type_PD_recode)


tbl_summary(
  data = df,
  by = part_1_selected_task_name,
  include = c(Player_type_IPD_recode,Player_type_PD_recode),
  missing = "no",
  label = list(
    Player_type_IPD_recode ~ "Profil IPD",
    Player_type_PD_recode ~ "Profil PD"
  )
) %>%
  add_overall() %>%
  add_p(
    test = list(
      c(Player_type_IPD_recode,Player_type_PD_recode) ~ "chisq.test"
    ),
    test.args = list(
      c(Player_type_IPD_recode,Player_type_PD_recode) ~ list(simulate.p.value = TRUE)
    )
  ) %>%
  bold_p()

##Effet d'ordre sur le nombre de jetons investis inconditionnellement dans IPD ----
class(df$ipd_uncond)
is.numeric(df$ipd_uncond)
tbl_summary(
  data = df,
  by = part_1_selected_task_name,
  include = ipd_uncond,
  type = list(ipd_uncond ~ "continuous"),
  digits = list(
    ipd_uncond ~ 2
  ),
  missing = "no") %>%
  add_overall() %>%
  add_p(
    test = list(
      ipd_uncond ~ "wilcox.test")
  ) %>%
  bold_p()







######################################################################
#  Baseline proportions and IPD/PD correlation for H1, H2, H3 (lab data)
#  ------------------------------------------------------------------
#  Purpose: get a realistic sense of (i) the baseline proportion in the
#  PD and (ii) the within-subject correlation r of individual binary
#  outcomes across PD and IPD, for oCC (H1), iCC (H2) and UNC (H3).
#  These are exactly the two ingredients that feed the McNemar power
#  grid (mcnemar_cells(propPD, corr, diff) in the power-analysis script).
#
#  Run this AFTER building `primary_data` as in the main analysis script
#  (i.e. after the block that creates oCC_PD/oCC_IPD, iCC_PD/iCC_IPD,
#  UNC_PD/UNC_IPD via pivot_wider).
######################################################################

library(dplyr)

# Phi coefficient = Pearson correlation on 0/1 indicators; this matches
# the `corr` parameter used in the power-analysis formula.
phi_coef <- function(x, y) {
  x <- as.numeric(x); y <- as.numeric(y)
  ok <- complete.cases(x, y)
  cor(x[ok], y[ok])
}

summarize_outcome <- function(data, var_pd, var_ipd, label) {
  x_pd  <- data[[var_pd]]
  x_ipd <- data[[var_ipd]]
  
  tab <- table(PD = factor(x_pd, levels = c(FALSE, TRUE)),
               IPD = factor(x_ipd, levels = c(FALSE, TRUE)))
  
  n      <- sum(tab)
  p_pd   <- mean(x_pd, na.rm = TRUE)
  p_ipd  <- mean(x_ipd, na.rm = TRUE)
  diff   <- p_ipd - p_pd
  r_phi  <- phi_coef(x_pd, x_ipd)
  
  # discordant cells, as used directly in the McNemar test
  b <- tab["FALSE", "TRUE"]  # PD=No,  IPD=Yes -> gained under conflict
  c <- tab["TRUE", "FALSE"]  # PD=Yes, IPD=No   -> lost under conflict
  
  cat("\n==== ", label, " ====\n", sep = "")
  print(tab)
  cat(sprintf("n = %d\n", n))
  cat(sprintf("Baseline proportion in PD  (propPD) : %.3f\n", p_pd))
  cat(sprintf("Proportion in IPD                    : %.3f\n", p_ipd))
  cat(sprintf("Observed difference (IPD - PD)       : %+.3f\n", diff))
  cat(sprintf("Within-subject correlation (phi, r)  : %.3f\n", r_phi))
  cat(sprintf("Discordant cells: b (gained)=%d, c (lost)=%d\n", b, c))
  
  invisible(data.frame(hypothesis = label, n = n, propPD = p_pd, propIPD = p_ipd,
                       diff = diff, corr = r_phi, b = b, c = c))
}

res_H1 <- summarize_outcome(primary_data, "oCC_PD", "oCC_IPD", "H1: oCC (outgroup conditional cooperator)")
res_H2 <- summarize_outcome(primary_data, "iCC_PD", "iCC_IPD", "H2: iCC (ingroup conditional cooperator)")
res_H3 <- summarize_outcome(primary_data, "UNC_PD", "UNC_IPD", "H3: UNC (unconditional non-cooperator)")

# Combined summary table, handy to eyeball / paste into the power-analysis grid
summary_table <- bind_rows(res_H1, res_H2, res_H3)
print(summary_table)

# --- Optional: overlay these observed points on the online power grid ------
# Once you have summary_table, you can directly read the power (or required n)
# implied by these lab-based propPD/corr values, e.g.:
#
#   source("IPD_strategy_power_analysis_lab_and_online.R")  # defines mcnemar_cells(), pwr.mcnemar()
#   for (i in seq_len(nrow(summary_table))) {
#     row <- summary_table[i, ]
#     cc_ <- mcnemar_cells(row$propPD, row$corr, abs(row$diff))
#     pw  <- pwr.mcnemar(cc_[["p10"]], cc_[["p01"]], n = 691, alpha = (0.05/3)*2)
#     cat(sprintf("%s: achieved power at n=691 (using lab propPD/corr/diff) = %.3f\n",
#         row$hypothesis, pw[["power"]]))
#   }


