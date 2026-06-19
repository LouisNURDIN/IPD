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


#test diagramme alluvial ----
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
    prop = n / total_PD *100
  ) %>%
  ungroup()

heatmap_prop <- ggplot(
  df_alluvial_prop,
  aes(
    x = Player_type_PD,
    y = Player_type_IPD,
    fill = prop
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
    breaks = c(0,25,50,75,100)
  )
theme_minimal() +
  labs(
    x = "Type de joueur dans PD",
    y = "Type de joueur dans IPD",
    fill = "% de joueurs au sein du groupe"
  )
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



tab_raw <- broom::tidy(model_H4a)
subset(tab_raw, term == "iSVO")
subset(tab_raw, term == "gSVO")
z <- estimate / std.error
p_one_tailed <- 1 - pnorm(z)
p_one_tailed <- pnorm(z)


#H4c ----
data_logit <- primary_data %>%
  filter(Player_type_IPD %in% c("Only Ingroup\nconditional cooperator", 
                                "Only Outgroup\nconditional cooperator")) %>%
  mutate(
    OiCC_binary = ifelse(Player_type_IPD == "Only Ingroup\nconditional cooperator", 1, 0))

model_OiCC_vs_OoCC <- glm(
  OiCC_binary ~ iSVO + gSVO,
  data = data_logit,
  family = binomial(link = "logit"))
summary(model_OiCC_vs_OoCC)


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
    mean_ipd = mean(df$ipd_uncond, na.rm = TRUE),
    mean_pd  = mean(df$pd_uncond, na.rm = TRUE),
    diff     = mean(df$ipd_uncond, na.rm = TRUE) - mean(df$pd_uncond, na.rm = TRUE),
    t_value  = test$statistic,
    df       = test$parameter,
    p_value  = test$p.value
  )
  
  tab_test

  
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
 
  
#Graphique avec les moyennes de jetons investis par groupe
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
        starts_with("mean_tokens_pd_out"),
      ),
      names_to = "variable",
      values_to = "value"
    ) %>%
    group_by(Player_type_PD, variable) %>%
    summarise(
      mean_jetons_joueurs_groupe = mean(value, na.rm = TRUE),
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
        starts_with("mean_tokens_ipd_out"),
      ),
      names_to = "variable",
      values_to = "value"
    ) %>%
    group_by(Player_type_IPD, variable) %>%
    summarise(
      mean_jetons_joueurs_groupe = mean(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  primary_data_means_ipd <- primary_data_means_ipd %>%
    mutate(
      nbr_jetons_autres_joueurs = as.numeric(stringr::str_sub(variable, -1))
    )
  
  
  #Tracer les graphiques ----
  ##Graphique jeu PD ----
  df_plot_pd_in <- primary_data_means_pd %>%
    filter(str_detect(variable, "^mean_tokens_pd_in"))
  unique(df_plot_pd_in$Player_type_PD)
  tokens_pd_in_groups <- ggplot(
    df_plot_pd_in,
    aes(x = nbr_jetons_autres_joueurs,y = mean_jetons_joueurs_groupe,color = Player_type_PD,group = Player_type_PD)) +
    scale_y_continuous(limits = c(0, 4),breaks = 0:4) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
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
 tokens_pd_out_groups <- ggplot(df_plot_pd_out,
        aes(x = nbr_jetons_autres_joueurs,
            y = mean_jetons_joueurs_groupe,
            color = Player_type_PD)) +
   geom_line(linewidth = 1) +
   geom_point(alpha = 2,size = 3) +
   theme_minimal() +
   scale_x_continuous(limits = c(0, 4), breaks = 0:4) +
   scale_y_continuous(limits = c(0, 4), breaks = 0:4) +
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
 tokens_ipd_in_groups <- ggplot(df_plot_ipd_in,
        aes(x = nbr_jetons_autres_joueurs,
            y = mean_jetons_joueurs_groupe,
            color = Player_type_IPD)) +
   geom_point(alpha = 2,size = 3) +
   geom_line(linewidth = 1) +
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
 ggsave(
   filename = "results/figures/mean_tokens_ipd_in_groups.png",
   plot =  tokens_ipd_in_groups, width = 10, height = 6,dpi = 300)
 
 
 df_plot_ipd_out <- primary_data_means_ipd %>%
   filter(str_detect(variable, "^mean_tokens_ipd_out"))
 tokens_ipd_out_groups <- ggplot(df_plot_ipd_out,
        aes(x = nbr_jetons_autres_joueurs,
            y = mean_jetons_joueurs_groupe,
            color = Player_type_IPD)) +
   geom_line(linewidth = 1) +
   geom_point(alpha = 2,size = 3) +
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
   scale_x_continuous(limits = c(0, 4), breaks = 0:4) +
   scale_y_continuous(limits = c(0, 4), breaks = 0:4) +
   labs(
     x = "Nombre de jetons des autres joueurs outgroup",
     y = "Moyenne des jetons du groupe",
     color = "Type de joueur (IPD)",
     title = "Nombre moyen de jetons investis par groupe en fonction des jetons outgroup dans le jeu IPD"
   )
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
       starts_with("mean_tokens_pd_out"),
     ),
     names_to = "variable",
     values_to = "value"
   ) %>%
   group_by(variable) %>%
   summarise(
     mean_jetons_all_players = mean(value, na.rm = TRUE),
     .groups = "drop"
   )
 
 primary_data_means_pd_all <- primary_data_means_pd_all %>%
   mutate(
     nbr_jetons_autres_joueurs = as.numeric(stringr::str_sub(variable, -1))
   )
 
 
 primary_data_means_ipd_all <- primary_data %>%
   pivot_longer(
     cols = c(
       starts_with("mean_tokens_ipd_in"),
       starts_with("mean_tokens_ipd_out"),
     ),
     names_to = "variable",
     values_to = "value"
   ) %>%
   group_by(variable) %>%
   summarise(
     mean_jetons_all_players = mean(value, na.rm = TRUE),
     .groups = "drop"
   )
 
 primary_data_means_ipd_all <- primary_data_means_ipd_all %>%
   mutate(
     nbr_jetons_autres_joueurs = as.numeric(stringr::str_sub(variable, -1))
   )
 
 ##Graphique PD all players ----
 df_plot_pd_in_all <- primary_data_means_pd_all %>%
   filter(str_detect(variable, "^mean_tokens_pd_in"))
 tokens_pd_in_all <-  ggplot(df_plot_pd_in_all,
                                aes(x = nbr_jetons_autres_joueurs,
                                    y = mean_jetons_all_players,
                                    )) +
   geom_point(alpha = 2,size = 3) +
   geom_line(linewidth = 1) +
   scale_x_continuous(limits = c(0, 4), breaks = 0:4) +
   scale_y_continuous(limits = c(0, 4), breaks = 0:4) +
   theme_minimal() +
   labs(
     x = "Nombre de jetons des autres joueurs ingroup",
     y = "Moyenne des jetons de l'ensemble des joueurs",
     title = "Nombre moyen de jetons investis par l'ensemble des joueurs en fonction des jetons ingroup dans le jeu PD"
   )
 ggsave(
   filename = "results/figures/mean_tokens_pd_in_all_players.png",
   plot = tokens_pd_in_all, width = 10, height = 6,dpi = 300)
 

 
 df_plot_pd_out_all <- primary_data_means_pd_all %>%
   filter(str_detect(variable, "^mean_tokens_pd_out"))
 tokens_pd_out_all <-  ggplot(df_plot_pd_out_all,
                             aes(x = nbr_jetons_autres_joueurs,
                                 y = mean_jetons_all_players,
                             )) +
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
 ggsave(
   filename = "results/figures/mean_tokens_ipd_in_all_players.png",
   plot = tokens_ipd_in_all, width = 10, height = 6,dpi = 300)
 
 
 
 df_plot_ipd_out_all <- primary_data_means_ipd_all %>%
   filter(str_detect(variable, "^mean_tokens_ipd_out"))
 tokens_ipd_out_all <-  ggplot(df_plot_ipd_out_all,
                              aes(x = nbr_jetons_autres_joueurs,
                                  y = mean_jetons_all_players,
                              )) +
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

tokens_PD_all_3D <- plot_ly(
  df_plot_pd_3D_all %>% arrange(value_ingroup, value_outgroup),
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
      zaxis = list(title = "Moyenne des jetons investis (PD)", range = c(0, 4))
    )
  )
print(tokens_PD_all_3D)
htmlwidgets::saveWidget(
  tokens_PD_all_3D,
  "results/figures/mean_tokens_3D_pd_all.html"
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


tokens_IPD_all_3D <- plot_ly(
  df_plot_ipd_3D_all,
  x = ~value_ingroup,
  y = ~value_outgroup,
  z = ~value,
  type = "scatter3d",
  mode = "markers",
  color = ~value_ingroup
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Jetons ingroup (0–4)", range = c(0, 4)),
      yaxis = list(title = "Jetons outgroup (0–4)", range = c(0, 4)),
      zaxis = list(title = "Moyenne des jetons investis par l'ensemble des joueurs (IPD)", range = c(0, 4))
    )
  )
htmlwidgets::saveWidget(
  tokens_PD_all_3D,
  "results/figures/mean_tokens_3D_ipd_all.html"
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

x_vals <- sort(unique(df_plot_pd_unc_3D$value_ingroup))
y_vals <- sort(unique(df_plot_pd_unc_3D$value_outgroup))

df_wide <- df_plot_pd_unc_3D %>%
  group_by(value_ingroup, value_outgroup) %>%
  summarise(value = mean(value), .groups = "drop") %>%
  pivot_wider(
    names_from = value_outgroup,
    values_from = value
  ) %>%
  arrange(value_ingroup)

z_matrix <- as.matrix(df_wide[,-1])


tokens_pd_unc_3D <- plot_ly(
  x = x_vals,
  y = y_vals,
  z = z_matrix,
  type = "surface",
  colorscale = "Blues"
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Jetons ingroup (0–4)", range = c(0, 4)),
      yaxis = list(title = "Jetons outgroup (0–4)", range = c(0, 4)),
      zaxis = list(title = "Moyenne des jetons investis", range = c(0, 4))
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


tokens_pd_uc_3D <- plot_ly(
  df_plot_pd_uc_3D %>% arrange(value_ingroup, value_outgroup),
  x = ~value_ingroup,
  y = ~value,
  z = ~value_outgroup,
  type = "scatter3d",
  mode = "markers",
  color = ~value,
  colors = "Blues",
  line = list(width = 4)
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Jetons ingroup (0–4)", range = c(0, 4)),
      yaxis = list(title = "Jetons outgroup (0–4)", range = c(0, 4)),
      zaxis = list(title = "Moyenne des jetons investis dans le jeu PD par le groupe UC", range = c(0, 4))
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
  colorscale = "Blues"
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Jetons ingroup (0–4)", range = c(0, 4)),
      yaxis = list(title = "Jetons outgroup (0–4)", range = c(0, 4)),
      zaxis = list(title = "Moyenne des jetons investis", range = c(0, 4))
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


tokens_pd_iCC_3D <- plot_ly(
  df_plot_pd_iCC_3D %>% arrange(value_ingroup, value_outgroup),
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
      zaxis = list(title = "Moyenne des jetons investis dans le jeu PD par le groupe iCC", range = c(0, 4))
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


tokens_pd_oCC_3D <- plot_ly(
  df_plot_pd_oCC_3D %>% arrange(value_ingroup, value_outgroup),
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
      zaxis = list(title = "Moyenne des jetons investis dans le jeu PD par le groupe oCC", range = c(0, 4))
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
      Player_type_IPD.y == "Unconditional\ncooperator" ~ "1",
      Player_type_IPD.y == "Only Ingroup\nconditional cooperator" ~ "2",
      Player_type_IPD.y == "Only Outgroup\nconditional cooperator" ~ "3",
      Player_type_IPD.y == "Ingroup and Outgroup\nconditional cooperator" ~ "4",
      Player_type_IPD.y == "Undefined" ~ "5",
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
  missing = "no"
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




