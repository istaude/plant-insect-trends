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


# extinctions from CR to LC/NE --------------------------------------------

noextinctions <- d %>%
  group_by(taxon_trivial, insect_species) %>% 
  summarise(nplants_original = n_distinct(plant_species_RL))

CRextintions <- d %>%
  filter(plants_RL_Kat._trans_num < 4) %>%
  group_by(taxon_trivial, insect_species) %>% 
  summarise(nplants_CRextintions = n_distinct(plant_species_RL))

CRtoENextintions <- d %>%
  filter(plants_RL_Kat._trans_num < 3) %>%
  group_by(taxon_trivial, insect_species) %>% 
  summarise(nplants_CRtoENextintions = n_distinct(plant_species_RL))

CRtoVUextintions <- d %>%
  filter(plants_RL_Kat._trans_num < 2) %>%
  group_by(taxon_trivial, insect_species) %>% 
  summarise(nplants_CRtoVUextintions = n_distinct(plant_species_RL))

CRtoNTextintions <- d %>%
  filter(plants_RL_Kat._trans_num < 1) %>%
  group_by(taxon_trivial, insect_species) %>% 
  summarise(nplants_CRtoNTextintions = n_distinct(plant_species_RL))


# bring together
dt <- noextinctions %>% 
  left_join(CRextintions) %>% 
  left_join(CRtoENextintions) %>% 
  left_join(CRtoVUextintions) %>% 
  left_join(CRtoNTextintions) %>% 
  mutate(across(everything(), ~replace_na(., 0))) %>% 
  # create percentage of host plants lost for each extinction step
  mutate(perc_CRextintions = 1 - nplants_CRextintions / nplants_original,
         perc_CRtoENextintions = 1 - nplants_CRtoENextintions / nplants_original,
         perc_CRtoVUextintions = 1 - nplants_CRtoVUextintions / nplants_original,
         perc_CRtoNTextintions = 1 - nplants_CRtoNTextintions / nplants_original)


#View(dt)

# based on this table, calculate the percentage of persisting insect species
# after each loss step, assuming four scenarios
threatlevels <- c("CR", "EN", "VU", "NT")
ext <- bind_rows(
  list(
    # 1) they survive if they have lost less than 100% of their plants
    data.frame(
      taxon_trivial = "All taxa",
      scenario = "<100",
      threatlevels,
      perc_surviving =
        c(
          dt %>% filter(perc_CRextintions < 1) %>% nrow / dt %>% nrow,
          dt %>% filter(perc_CRtoENextintions < 1) %>% nrow / dt %>% nrow,
          dt %>% filter(perc_CRtoVUextintions < 1) %>% nrow / dt %>% nrow,
          dt %>% filter(perc_CRtoNTextintions < 1) %>% nrow / dt %>% nrow
        )
    ),
    # 2) they survive if they have lost less than 75% of their plants
    data.frame(
      taxon_trivial = "All taxa",
      scenario = "<75",
      threatlevels,
      perc_surviving =
        c(
          dt %>% filter(perc_CRextintions < 0.75) %>% nrow / dt %>% nrow,
          dt %>% filter(perc_CRtoENextintions < 0.75) %>% nrow / dt %>% nrow,
          dt %>% filter(perc_CRtoVUextintions < 0.75) %>% nrow / dt %>% nrow,
          dt %>% filter(perc_CRtoNTextintions < 0.75) %>% nrow / dt %>% nrow
        )
    ),
    # 3) they survive if they have lost less than 50% of their plants
    data.frame(
      taxon_trivial = "All taxa",
      scenario = "<50",
      threatlevels,
      perc_surviving =
        c(
          dt %>% filter(perc_CRextintions < 0.5) %>% nrow / dt %>% nrow,
          dt %>% filter(perc_CRtoENextintions < 0.5) %>% nrow / dt %>% nrow,
          dt %>% filter(perc_CRtoVUextintions < 0.5) %>% nrow / dt %>% nrow,
          dt %>% filter(perc_CRtoNTextintions < 0.5) %>% nrow / dt %>% nrow
        )
    ),
    # 4) they survive if they have lost less than 25% of their plants
    data.frame(
      taxon_trivial = "All taxa",
      scenario = "<25",
      threatlevels,
      perc_surviving =
        c(
          dt %>% filter(perc_CRextintions < 0.25) %>% nrow / dt %>% nrow,
          dt %>% filter(perc_CRtoENextintions < 0.25) %>% nrow / dt %>% nrow,
          dt %>% filter(perc_CRtoVUextintions < 0.25) %>% nrow / dt %>% nrow,
          dt %>% filter(perc_CRtoNTextintions < 0.25) %>% nrow / dt %>% nrow
        )
    ),
    # 4) they only survive if they have not lost any of their plants (0%)
    data.frame(
      taxon_trivial = "All taxa",
      scenario = "0",
      threatlevels,
      perc_surviving =
        c(
          dt %>% filter(perc_CRextintions == 0) %>% nrow / dt %>% nrow,
          dt %>% filter(perc_CRtoENextintions == 0) %>% nrow / dt %>% nrow,
          dt %>% filter(perc_CRtoVUextintions == 0) %>% nrow / dt %>% nrow,
          dt %>% filter(perc_CRtoNTextintions == 0) %>% nrow / dt %>% nrow
        )
    )
  )
)


# for the taxa themselves
thresholds <- c(1, 0.75, 0.5, 0.25, 10e-08)
l <-  list()
for (i in seq_along(thresholds)) {
  threshold_value <- thresholds[i]
  l[[i]] <- bind_rows(
    list(
      dt %>% filter(perc_CRextintions < threshold_value) %>% group_by(taxon_trivial) %>% 
        summarise(n_surviving = n_distinct(insect_species)) %>%
        left_join(dt %>% group_by(taxon_trivial) %>% 
                    summarise(n = n_distinct(insect_species))) %>% 
        mutate(perc_surviving = n_surviving/n) %>% 
        mutate(threatlevels = "CR") %>% 
        select(-n_surviving, -n) %>% 
        mutate(scenario = paste0("<", thresholds[i]*100)),
      
      dt %>% filter(perc_CRtoENextintions < threshold_value) %>% group_by(taxon_trivial) %>% 
        summarise(n_surviving = n_distinct(insect_species)) %>%
        left_join(dt %>% group_by(taxon_trivial) %>% 
                    summarise(n = n_distinct(insect_species))) %>% 
        mutate(perc_surviving = n_surviving/n) %>% 
        mutate(threatlevels = "EN") %>% 
        select(-n_surviving, -n) %>% 
        mutate(scenario = paste0("<", thresholds[i]*100)),
      
      dt %>% filter(perc_CRtoVUextintions < threshold_value) %>% group_by(taxon_trivial) %>% 
        summarise(n_surviving = n_distinct(insect_species)) %>%
        left_join(dt %>% group_by(taxon_trivial) %>% 
                    summarise(n = n_distinct(insect_species))) %>% 
        mutate(perc_surviving = n_surviving/n) %>% 
        mutate(threatlevels = "VU") %>% 
        select(-n_surviving, -n) %>% 
        mutate(scenario = paste0("<", thresholds[i]*100)),
      
      dt %>% filter(perc_CRtoNTextintions < threshold_value) %>% group_by(taxon_trivial) %>% 
        summarise(n_surviving = n_distinct(insect_species)) %>%
        left_join(dt %>% group_by(taxon_trivial) %>% 
                    summarise(n = n_distinct(insect_species))) %>% 
        mutate(perc_surviving = n_surviving/n) %>% 
        mutate(threatlevels = "NT") %>% 
        select(-n_surviving, -n) %>% 
        mutate(scenario = paste0("<", thresholds[i]*100))
    )
  )
}

# bind everything together
ext <- bind_rows(l, ext)


# visualization red list cats removal -------------------------------------

# reorder factor levels, and make percentage go from 0 to 100
ext <- ext %>% 
  mutate(threatlevels = factor(threatlevels,
                               levels = c(
                                 "CR",
                                 "EN",
                                 "VU",
                                 "NT"))
  ) %>% 
  mutate(taxon_trivial = factor(taxon_trivial,
                                levels = c("All taxa", 
                                           "Bees", 
                                           "Butterflies & moths", 
                                           "Sawflies", 
                                           "Hoverflies"))) %>% 
  mutate(perc_surviving = 100*perc_surviving) %>% 
  mutate(scenario = recode(scenario, "<1e-05" = "0" ))



# number of plant species in each threat level
chicklet_data <- bind_rows(
  d %>% 
    group_by(plants_RL_Kat._trans_cat) %>% 
    summarise(n = n_distinct(plant_species_RL)) %>% 
    mutate(taxon_trivial = "All taxa"),
  d %>% 
    group_by(taxon_trivial, plants_RL_Kat._trans_cat) %>% 
    summarise(n = n_distinct(plant_species_RL)) 
) %>% 
  rename(threatlevels = plants_RL_Kat._trans_cat) %>% 
  filter(threatlevels != "LC" & threatlevels != "DD") %>% 
  mutate(taxon_trivial = factor(taxon_trivial,
                                levels = c("All taxa", 
                                           "Bees", 
                                           "Butterflies & moths", 
                                           "Sawflies", 
                                           "Hoverflies")))

# for second y axis
transformation_ratio <- max(chicklet_data$n) / max(ext$perc_surviving)


fig2a <- ggplot(data = ext) +
  facet_grid(~taxon_trivial) +
  geom_chicklet(data = chicklet_data, 
                aes(x = threatlevels, y = n / transformation_ratio),
                radius = unit(2, "pt"), alpha = .1,
                position = position_stack(reverse = FALSE)) +
  #geom_point(aes(x = threatlevels, 
  #               y = perc_surviving, 
  #               col = scenario), size = 0.8) + 
  geom_line(aes(x = threatlevels, y = perc_surviving,  group = scenario, col = scenario), 
            alpha = 0.7,
            arrow = arrow(type = "closed", length = unit(0.08, "inches"), ends = "last")) +
  scale_y_continuous(
    name = "Surviving insect species (%)",
    sec.axis = sec_axis(~ . * transformation_ratio, name = "Number of plant species lost")
  ) +
  scale_color_manual(
    values = c("<100" = "red", "<75" = "blue", "<50" = "cyan", "<25" = "green", "0" = "magenta"),
    labels = c(
      "<100" = ">0%",
      "<75" = ">25%",
      "<50" = ">50%",
      "<25" = ">75%",
      "0" = "100%"
    ),
    limits = c("<100", "<75", "<50", "<25", "0")
  ) +
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
       title = "Co-extinctions of insects from sequential loss of host plants by Red List category",
       color = "Insects survive if host plant diversity remains in their portfolio:")




# second plot showing the full histogram of the percentage of host plants remaining
# after sequential loss of threatened host plants
(
  fig2b <- dt %>% 
    bind_rows(dt %>% mutate(taxon_trivial = "All taxa")) %>% 
    mutate(taxon_trivial = factor(taxon_trivial,
                                  levels = c("All taxa", 
                                             "Bees", 
                                             "Butterflies & moths", 
                                             "Sawflies", 
                                             "Hoverflies"))) %>% 
    mutate(perc_CRtoNTextintions = 100* (1 - perc_CRtoNTextintions)) %>% 
    ggplot() +
    facet_wrap(~taxon_trivial, scales = "free", nrow = 1) +
    geom_histogram(aes(x = perc_CRtoNTextintions), fill = "grey", alpha = 0.4) +
    scale_color_identity() +
    theme_minimal(base_family = "Arial Narrow") +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(face = "italic", size = 14),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.position = "none"
    ) +
    labs(title= "Remaining percentage of host plant diversity in insect portfolio after sequential plant loss", 
         y = "Number of insect species",
         x = "Percentage of remaining host plant diversity in insect portfolio")
)




# figure showing the number of common species left

# which range size cats in RL
d %>% select(akt_Best_p) %>% distinct

highest_rs <- d %>%
  # exclude all threatened plants
  filter(plants_RL_Kat._trans_num < 1) %>%
  # want to know per insect species how often plant rarity categories appear
  group_by(taxon_trivial, insect_species) %>% 
  count(akt_Best_p) %>% 
  # recode
  mutate(akt_Best_p_numeric = recode(akt_Best_p, 
                                     "sh" = 5,
                                     "h" = 4,
                                     "mh" = 3,
                                     "s" = 2,
                                     "ss" = 1,
                                     "es" = 0,
                                     # some species unclear
                                     "nb" = -1,
                                     "?" = -1,
                                     .default = -1,  # default for any other values
                                     .missing = -1)) %>% # handle NA values 
  # now identify the highest range size of plants per insect species
  summarise(highest_range_size = max(akt_Best_p_numeric))

# now we also just want to know this for insect species that had their
# portfolio affected, i.e., insects that lost host plants, but also
# not those that lost all
highest_rs <- dt %>% ungroup %>% 
  filter(perc_CRtoNTextintions < .75) %>% 
  filter(perc_CRtoNTextintions != 0) %>% 
  select(insect_species) %>% 
  left_join(highest_rs)

range_cats <- data.frame(
  highest_range_size = as.character(-1:5),
  category_label = c("unknown", "extremly rare", "very rare", "rare", "moderately common", "common", "very common")
)

highest_rs <- highest_rs %>% 
  group_by(taxon_trivial) %>% 
  count(highest_range_size)  %>% 
  mutate(highest_range_size = as.character(highest_range_size)) %>% 
  right_join(range_cats, by = "highest_range_size") %>% 
  na.omit()

# cal percentages
highest_rs <- highest_rs %>%
  group_by(taxon_trivial) %>%
  mutate(total_n = sum(n)) %>%
  ungroup()

highest_rs <- highest_rs %>%
  mutate(percentage = (n / total_n) * 100)

# all taxa
highest_rs <- highest_rs %>% 
  group_by(category_label, highest_range_size) %>% 
  summarise(n = sum(n)) %>% 
  ungroup %>% 
  mutate(total_n = sum(n)) %>% 
  mutate(percentage = (n / total_n) * 100) %>% 
  mutate(taxon_trivial = "All taxa") %>% 
  bind_rows(
    highest_rs
  ) 

# order factor levels
highest_rs <- highest_rs %>% 
  mutate(taxon_trivial = factor(taxon_trivial,
                                levels = c("All taxa", 
                                           "Bees", 
                                           "Butterflies & moths", 
                                           "Sawflies", 
                                           "Hoverflies")))

# viz
(
  ggplot(highest_rs, aes(x = category_label, y = percentage)) +
    facet_wrap(~taxon_trivial,  nrow = 1) +
    geom_chicklet(fill = "blue",
                  radius = unit(2, "pt"), alpha = .6,
                  position = position_stack(reverse = FALSE)) +
    labs(title = "Insects surviving at the >25% threshold retain almost always common host plants",
         x = "Range size of largest-ranged host plant remaining in insect portfolio", 
         y = "Insect species (%)") +
    scale_x_discrete(limits = c("unknown", "very rare", "rare", "moderately common", "common", "very common")) +
    theme_minimal(base_family = "Arial Narrow") +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(face = "italic", size = 14),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.text = element_text(size = 14)
    ) -> fig2c
)

# how many common species in all taxa
highest_rs %>% filter(taxon_trivial == "All taxa")

# composite figure
fig2a /
  #fig2b /
  fig2c + 
  plot_annotation(tag_levels = 'a')

# save
showtext_opts(dpi=600)
ggsave(bg = "white",
       dpi = 600,
       "Figures/figure2-main-rev.png",
       width = 12,
       height = 8)
showtext_opts(dpi=96)

View(ext %>% mutate(lost = 100 - perc_surviving))

# SI Figure ---------------------------------------------------------------

fig2b

# now instead of the percentages show the numbers of host plants
fig2d <- dt %>% 
  bind_rows(dt %>% mutate(taxon_trivial = "All taxa")) %>% 
  mutate(taxon_trivial = factor(taxon_trivial,
                                levels = c("All taxa", 
                                           "Bees", 
                                           "Butterflies & moths", 
                                           "Sawflies", 
                                           "Hoverflies"))) %>% 
  mutate(diff = nplants_CRtoNTextintions - nplants_original) %>% 
  filter(diff < 0) %>% 
  filter(perc_CRtoNTextintions != 1) %>% 
  ggplot() +
  facet_wrap(~taxon_trivial, scales = "free", nrow = 1) +
  geom_histogram(aes(x = nplants_CRtoNTextintions), fill = "grey", alpha  = 0.4) +
  scale_x_log10()+
  theme_minimal(base_family = "Arial Narrow") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(face = "italic", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  ) +
  labs(title = "Remaining number of plant species in insect portfolio after sequential plant loss", 
       y = "Number of insect species",
       x = "Number of host plants remaining in insect portfolio")


fig2b /
  #fig2b /
  fig2d + 
  plot_annotation(tag_levels = 'a')


showtext_opts(dpi=600)
ggsave(bg = "white",
       dpi = 600,
       "Figures/figure4-supp.png",
       width = 12,
       height = 7)
showtext_opts(dpi=96)
