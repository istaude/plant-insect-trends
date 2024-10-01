source("00-preamble.R")


# load data ---------------------------------------------------------------
d <- read_csv("RL_inter_plant_insect-JUNE2024.csv")
#View(d)

# for red list cats -------------------------------------------------------

# transf rl cats
d <- d %>%
  mutate(plants_RL_Kat._trans_num = case_when(
    RL_Kat._p == "*" ~ 0,
    RL_Kat._p == "V" ~ 1,
    RL_Kat._p == "R" ~ 1,
    RL_Kat._p == "3" ~ 2,
    RL_Kat._p == "G" ~ 2,
    RL_Kat._p == "2" ~ 3,
    RL_Kat._p == "1" ~ 4,
    RL_Kat._p == "0" ~ 5,
    RL_Kat._p == "D" ~ -1,
    RL_Kat._p == "♦" ~ -1,
    RL_Kat._p == "nb" ~ -1,
    TRUE ~ -1
  )) %>% 
  mutate(plants_RL_Kat._trans_cat = case_when(
    
    plants_RL_Kat._trans_num == -1 ~ "DD",
    plants_RL_Kat._trans_num == 0 ~ "LC",
    plants_RL_Kat._trans_num == 1 ~ "NT",
    plants_RL_Kat._trans_num == 2 ~ "VU",
    plants_RL_Kat._trans_num == 3 ~ "EN",
    plants_RL_Kat._trans_num == 4 ~ "CR",
    plants_RL_Kat._trans_num == 5 ~ "EX"
  )) 

# exclude extinct species from this analysis these are interactions
d <- d %>% 
  filter(plants_RL_Kat._trans_num != 5)


# attach all taxa
d <- bind_rows(d, 
          d %>% mutate(taxon_trivial = "All taxa")
          )

# function to assign energy scores to plants ------------------------------

assign_energy_scores <- function(n) {
  # if just one host plants, assign always 1
  if (n == 1) {
    return(1)
  }
  
  # if greater one, just sample from unif
  (scores <- runif(n, 0, 1))
  total <- sum(scores)
  
  # now check if smaller 1, if so, replace the last value, with the diff to 1
  if (total < 1) {
    scores <- c(scores[-n], 1 - scores[n-1])
  
  # otherwise leave as is
  } else {
    (scores <- scores)
  }
  
  return(scores)
}

# create a function that accepts a data frame,
# calculates energy scores, and returns the data frame with the new scores
assign_energy_scores_to_df <- function(df) {
  n <- nrow(df)
  scores <- assign_energy_scores(n)
  df$energy_score <- scores
  return(df)
}



# Scenario 1 --------------------------------------------------------------

# this scenario uses the uniform distribution to assign randomly energy scores
# between 0 and 1 to host plants. with one constraint. in the original, pre-ex
# tinction status, host plants have to have at least a cumulative energy score
# of 1, so that 100% of the insects energy is covered.

# now create loop that assigns 100 times energy scores
s1 <- data.frame()
for (i in 1:10){
# assign energy scores for plants within each insect species
dx <- d %>%
  select(taxon_trivial, insect_species, plant_species_RL, plants_RL_Kat._trans_num) %>% 
  group_by(taxon_trivial, insect_species) %>%
  group_modify(~ assign_energy_scores_to_df(.x))


# calculate energy sums
noextinctions <- dx %>%
group_by(taxon_trivial, insect_species) %>% 
  summarise(energy_original = sum(energy_score))

CRextintions <- dx %>%
  filter(plants_RL_Kat._trans_num < 4) %>%
  group_by(taxon_trivial, insect_species) %>% 
  summarise(energy_CRextintions = sum(energy_score))

CRtoENextintions <- dx %>%
  filter(plants_RL_Kat._trans_num < 3) %>%
  group_by(taxon_trivial, insect_species) %>% 
  summarise(energy_CRtoENextintions = sum(energy_score))

CRtoVUextintions <- dx %>%
  filter(plants_RL_Kat._trans_num < 2) %>%
  group_by(taxon_trivial, insect_species) %>% 
  summarise(energy_CRtoVUextintions = sum(energy_score))

CRtoNTextintions <- dx %>%
  filter(plants_RL_Kat._trans_num < 1) %>%
  group_by(taxon_trivial, insect_species) %>% 
  summarise(energy_CRtoNTextintions = sum(energy_score))

# bring together
dt <- noextinctions %>% 
  left_join(CRextintions) %>% 
  left_join(CRtoENextintions) %>% 
  left_join(CRtoVUextintions) %>% 
  left_join(CRtoNTextintions) %>% 
  mutate(across(everything(), ~replace_na(., 0)))

# now calculate for each taxa the fraction of species that had cumulative 
# energy scores below 1, these go extinct, calculate 1 minus this number for thos
# that survive
temp_df <- dt %>%
  pivot_longer(cols = starts_with("energy_"), 
               names_to = "extinction_step", 
               values_to = "energy") %>% 
  group_by(taxon_trivial, extinction_step) %>%
  summarize(percent_surviving = 100 - (sum(energy < 1) / n() * 100)) %>% 
  mutate(iteration = paste0("iteration", i))

s1 <- bind_rows(s1, temp_df)

}

df <- s1 %>% 
  filter(extinction_step != "energy_original") %>% 
  mutate(extinction_step = recode(extinction_step, 
                                  "energy_CRextintions" = "CR",
                                  "energy_CRtoENextintions" = "EN",
                                  "energy_CRtoVUextintions" = "VU",
                                  "energy_CRtoNTextintions" = "NT")) %>% 
  mutate(extinction_step = factor(extinction_step,
                               levels = c(
                                 "CR",
                                 "EN",
                                 "VU",
                                 "NT"))) %>% 
  mutate(taxon_trivial = factor(taxon_trivial,
                                levels = c("All taxa",
                                           "Bees", 
                                           "Butterflies & moths", 
                                           "Sawflies", 
                                           "Hoverflies")))

# mean line
df_mean <- df %>% group_by(taxon_trivial, extinction_step) %>% 
  summarise(mean_line = mean(percent_surviving))

# visualize
ggplot(data = df) +
  facet_grid(~taxon_trivial) +
  geom_line(aes(x = extinction_step, y = percent_surviving,  group = iteration), 
            alpha = 0.4, col = "grey") +
  #scale_y_continuous(limits = c(0, 100)) +
  geom_line(data = df_mean, 
            aes(x = extinction_step, y = mean_line, group = 1), inherit.aes = F) +
theme_minimal(base_family = "Arial Narrow") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(face = "italic", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size = 14)
  ) +
  guides(color = guide_legend( title.position = "top")) + 
  labs(x= "Threat status of host plants", 
       y = "Surviving insects (%)",
       title = "Scenario I: using a uniform distribution to assign energy scores to host plants")



# Scenario 2 --------------------------------------------------------------

# this scenario uses also the uniform distribution but assigns host plants
# with a larger range size a larger random energy score. 

# first transform range size cat into numeric
d %>% select(akt_Best_p) %>% distinct

d <- d %>%
  mutate(plants_akt_Best_trans_num = case_when(
    akt_Best_p == "sh" ~ 7,
    akt_Best_p == "h" ~ 6,
    akt_Best_p == "mh" ~ 5,
    akt_Best_p == "s" ~ 4,
    akt_Best_p == "ss" ~ 3,
    akt_Best_p == "es" ~ 2,
    akt_Best_p == "ex" ~ 1,
    akt_Best_p == "nb" ~ 0,
    akt_Best_p == "?" ~ 0,
    TRUE ~ 0
  ))



# create function for scenario 2 ------------------------------------------

# need to modify our assign energy function
assign_energy_scores_to_df2 <- function(df) {
  n <- nrow(df)
  
  if (n == 1) {
    df$energy_score <- 1
    return(df)
  }
  
  # generate random scores
  scores <- runif(n, 0, 1)
  total <- sum(scores)
  
  # ensure the sum is at least 1
  if (total < 1) {
    scores <- c(scores[-n], 1 - scores[n-1])
  }
  
  # sort by plants_akt_Best_trans_num
  df <- df %>% arrange(plants_akt_Best_trans_num)
  
  # assign sorted scores
  df$energy_score <- sort(scores, decreasing = FALSE)
  
  return(df)
}

# now create loop that assigns 100 times energy scores
s2 <- data.frame()
for (i in 1:10){
  # assign energy scores for plants within each insect species
  dx <- d %>%
    select(taxon_trivial, insect_species, plant_species_RL, plants_RL_Kat._trans_num, 
           plants_akt_Best_trans_num,
           ) %>% 
    group_by(taxon_trivial, insect_species) %>%
    group_modify(~ assign_energy_scores_to_df2(.x))
  
  
  # calculate energy sums
  noextinctions <- dx %>%
    group_by(taxon_trivial, insect_species) %>% 
    summarise(energy_original = sum(energy_score))
  
  CRextintions <- dx %>%
    filter(plants_RL_Kat._trans_num < 4) %>%
    group_by(taxon_trivial, insect_species) %>% 
    summarise(energy_CRextintions = sum(energy_score))
  
  CRtoENextintions <- dx %>%
    filter(plants_RL_Kat._trans_num < 3) %>%
    group_by(taxon_trivial, insect_species) %>% 
    summarise(energy_CRtoENextintions = sum(energy_score))
  
  CRtoVUextintions <- dx %>%
    filter(plants_RL_Kat._trans_num < 2) %>%
    group_by(taxon_trivial, insect_species) %>% 
    summarise(energy_CRtoVUextintions = sum(energy_score))
  
  CRtoNTextintions <- dx %>%
    filter(plants_RL_Kat._trans_num < 1) %>%
    group_by(taxon_trivial, insect_species) %>% 
    summarise(energy_CRtoNTextintions = sum(energy_score))
  
  # bring together
  dt <- noextinctions %>% 
    left_join(CRextintions) %>% 
    left_join(CRtoENextintions) %>% 
    left_join(CRtoVUextintions) %>% 
    left_join(CRtoNTextintions) %>% 
    mutate(across(everything(), ~replace_na(., 0)))
  
  # now calculate for each taxa the fraction of species that had cumulative 
  # energy scores below 1, these go extinct, calculate 1 minus this number for thos
  # that survive
  temp_df <- dt %>%
    pivot_longer(cols = starts_with("energy_"), 
                 names_to = "extinction_step", 
                 values_to = "energy") %>% 
    group_by(taxon_trivial, extinction_step) %>%
    summarize(percent_surviving = 100 - (sum(energy < 1) / n() * 100)) %>% 
    mutate(iteration = paste0("iteration", i))
  
  s2 <- bind_rows(s2, temp_df)
  
}

df2 <- s2 %>% 
  filter(extinction_step != "energy_original") %>% 
  mutate(extinction_step = recode(extinction_step, 
                                  "energy_CRextintions" = "CR",
                                  "energy_CRtoENextintions" = "EN",
                                  "energy_CRtoVUextintions" = "VU",
                                  "energy_CRtoNTextintions" = "NT")) %>% 
  mutate(extinction_step = factor(extinction_step,
                                  levels = c(
                                    "CR",
                                    "EN",
                                    "VU",
                                    "NT"))) %>% 
  mutate(taxon_trivial = factor(taxon_trivial,
                                levels = c(
                                  "All taxa",
                                  "Bees", 
                                  "Butterflies & moths", 
                                  "Sawflies", 
                                  "Hoverflies")))

# mean line
df_mean2 <- df2 %>% group_by(taxon_trivial, extinction_step) %>% 
  summarise(mean_line = mean(percent_surviving))

# visualize
ggplot(data = df2) +
  facet_grid(~taxon_trivial) +
  geom_line(aes(x = extinction_step, y = percent_surviving,  group = iteration), 
            alpha = 0.4, col = "grey") +
  # scale_y_continuous(limits = c(0, 100)) +
  geom_line(data = df_mean2, 
            aes(x = extinction_step, y = mean_line, group = 1), inherit.aes = F) +
  theme_minimal(base_family = "Arial Narrow") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(face = "italic", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size = 14)
  ) +
  guides(color = guide_legend( title.position = "top")) + 
  labs(x= "Threat status of host plants", 
       y = "Surviving insects (%)",
       title = "Scenario I: using a uniform distribution to assign energy scores to host plants")



# Scenario 3 --------------------------------------------------------------

# this scenario uses also the uniform distribution but assigns host plants
# with a smaller range size a larger random energy score. 

# need to modify our assign energy function
assign_energy_scores_to_df3 <- function(df) {
  n <- nrow(df)
  
  if (n == 1) {
    df$energy_score <- 1
    return(df)
  }
  
  # generate random scores
  scores <- runif(n, 0, 1)
  total <- sum(scores)
  
  # ensure the sum is at least 1
  if (total < 1) {
    scores <- c(scores[-n], 1 - scores[n-1])
  }
  
  # sort by plants_akt_Best_trans_num
  df <- df %>% arrange(plants_akt_Best_trans_num)
  
  # assign sorted scores
  df$energy_score <- sort(scores, decreasing = TRUE)
  
  return(df)
}

# now create loop that assigns 100 times energy scores
s3 <- data.frame()
for (i in 1:10){
  # assign energy scores for plants within each insect species
  dx <- d %>%
    select(taxon_trivial, insect_species, plant_species_RL, plants_RL_Kat._trans_num, 
           plants_akt_Best_trans_num,
    ) %>% 
    group_by(taxon_trivial, insect_species) %>%
    group_modify(~ assign_energy_scores_to_df3(.x))
  
  
  # calculate energy sums
  noextinctions <- dx %>%
    group_by(taxon_trivial, insect_species) %>% 
    summarise(energy_original = sum(energy_score))
  
  CRextintions <- dx %>%
    filter(plants_RL_Kat._trans_num < 4) %>%
    group_by(taxon_trivial, insect_species) %>% 
    summarise(energy_CRextintions = sum(energy_score))
  
  CRtoENextintions <- dx %>%
    filter(plants_RL_Kat._trans_num < 3) %>%
    group_by(taxon_trivial, insect_species) %>% 
    summarise(energy_CRtoENextintions = sum(energy_score))
  
  CRtoVUextintions <- dx %>%
    filter(plants_RL_Kat._trans_num < 2) %>%
    group_by(taxon_trivial, insect_species) %>% 
    summarise(energy_CRtoVUextintions = sum(energy_score))
  
  CRtoNTextintions <- dx %>%
    filter(plants_RL_Kat._trans_num < 1) %>%
    group_by(taxon_trivial, insect_species) %>% 
    summarise(energy_CRtoNTextintions = sum(energy_score))
  
  # bring together
  dt <- noextinctions %>% 
    left_join(CRextintions) %>% 
    left_join(CRtoENextintions) %>% 
    left_join(CRtoVUextintions) %>% 
    left_join(CRtoNTextintions) %>% 
    mutate(across(everything(), ~replace_na(., 0)))
  
  # now calculate for each taxa the fraction of species that had cumulative 
  # energy scores below 1, these go extinct, calculate 1 minus this number for thos
  # that survive
  temp_df <- dt %>%
    pivot_longer(cols = starts_with("energy_"), 
                 names_to = "extinction_step", 
                 values_to = "energy") %>% 
    group_by(taxon_trivial, extinction_step) %>%
    summarize(percent_surviving = 100 - (sum(energy < 1) / n() * 100)) %>% 
    mutate(iteration = paste0("iteration", i))
  
  s3 <- bind_rows(s3, temp_df)
  
}

df3 <- s3 %>% 
  filter(extinction_step != "energy_original") %>% 
  mutate(extinction_step = recode(extinction_step, 
                                  "energy_CRextintions" = "CR",
                                  "energy_CRtoENextintions" = "EN",
                                  "energy_CRtoVUextintions" = "VU",
                                  "energy_CRtoNTextintions" = "NT")) %>% 
  mutate(extinction_step = factor(extinction_step,
                                  levels = c(
                                    "CR",
                                    "EN",
                                    "VU",
                                    "NT"))) %>% 
  mutate(taxon_trivial = factor(taxon_trivial,
                                levels = c(
                                  "All taxa",
                                  "Bees", 
                                  "Butterflies & moths", 
                                  "Sawflies", 
                                  "Hoverflies")))

# mean line
df_mean3 <- df3 %>% group_by(taxon_trivial, extinction_step) %>% 
  summarise(mean_line = mean(percent_surviving))

# visualize
ggplot(data = df3) +
  facet_grid(~taxon_trivial) +
  geom_line(aes(x = extinction_step, y = percent_surviving,  group = iteration), 
            alpha = 0.4, col = "grey") +
  # scale_y_continuous(limits = c(0, 100)) +
  geom_line(data = df_mean3, 
            aes(x = extinction_step, y = mean_line, group = 1), inherit.aes = F) +
  theme_minimal(base_family = "Arial Narrow") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(face = "italic", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size = 14)
  ) +
  guides(color = guide_legend( title.position = "top")) + 
  labs(x= "Threat status of host plants", 
       y = "Surviving insects (%)",
       title = "Scenario I: using a uniform distribution to assign energy scores to host plants")


# put everything together -------------------------------------------------

df_combined <- bind_rows(
  list(
    df_mean %>% mutate(scenario = "Scenario 1: Random suitability"),
    df_mean2 %>% mutate(scenario = "Scenario 2: Larger-ranged plants higher suitability"),
    df_mean3 %>% mutate(scenario = "Scenario 3: Smaller-ranged plants higher suitability")
  )
)

# View(df_combined)

(
  ggplot(data = df_combined) +
  facet_grid(~taxon_trivial) +
  geom_line(aes(x = extinction_step, y = mean_line,  group = scenario, col = scenario)) +
  theme_minimal(base_family = "Arial Narrow") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(face = "italic", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_blank()
  ) +
  scale_color_manual(values = c("blue", "green", "magenta")) + 
  guides(color = guide_legend( title.position = "top")) + 
  labs(x= "Threat status of host plants", 
       y = "Surviving insects (%)",
       title = "Assessing the impact of host plant non-interchangeability on insect co-extinctions") -> figure5_supp
  )

showtext_opts(dpi=600)
ggsave(bg = "white",
       dpi = 600,
       "Figures/figure5-supp.svg",
       width = 12,
       height = 3.3)
showtext_opts(dpi=96)
