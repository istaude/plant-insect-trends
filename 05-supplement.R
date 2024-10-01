source("00-preamble.R")

# are specialized insects more threatened? --------------------------------

# prepare data
d <- read_csv("RL_inter_plant_insect-JUNE2024.csv")

# transf insect rl cats
d <- d %>%
  mutate(insect_RL_Kat._trans = case_when(
    RL_Kat. == "*" ~ 0,
    RL_Kat. == "V" ~ 1,
    RL_Kat. == "R" ~ 1,
    RL_Kat. == "3" ~ 2,
    RL_Kat. == "G" ~ 2,
    RL_Kat. == "2" ~ 3,
    RL_Kat. == "1" ~ 4,
    RL_Kat. == "0" ~ 5,
    RL_Kat. == "D" ~ NA,
    RL_Kat. == "♦" ~ NA,
    RL_Kat. == "nb" ~ NA,
    TRUE ~ NA_real_# Default case if none of the above conditions are met
  ))

# add specialization level following Cane and Sipes 2007
d <- d %>%
  group_by(insect_species) %>%
  mutate(
    n_species = n_distinct(plant_species_RL),
    n_genus = n_distinct(plant_genus),
    n_family = n_distinct(plant_family)
  ) %>% # adding the number of host plant species, genera and families
  ungroup() %>%
  mutate(specialization = if_else(
    n_species == 1,
    "mono",
    if_else(
      n_genus <= 4 & n_family == 1,
      "oligo",
      if_else(n_family <= 3, "meso", "poly")
    )
  ))

d <-
  d %>% 
  select(taxon_trivial, insect_species, specialization, insect_RL_Kat._trans, RL_Kat.) %>% distinct

d <- bind_rows(d, d %>% mutate(taxon_trivial = "All taxa")) %>%
  na.omit() 
#View(d)


# visualize ---------------------------------------------------------------
d$specialization <- 
  factor(d$specialization, levels = c("mono", "oligo", "meso", "poly"))
d$taxon_trivial <- factor(
  d$taxon_trivial,
  levels = c("All taxa", "Bees", "Butterflies & moths", "Sawflies", "Hoverflies")
)
d$RL_Kat. <- factor(
  d$RL_Kat.,
  levels = c("*", "V", "R", "G", "3", "2", "1", "0")
)

head(d)
(d %>%
    group_by(taxon_trivial) %>%
    count(specialization, `RL_Kat.`) %>%
    ggplot(aes(y = n, x = specialization, fill = `RL_Kat.`)) +
    geom_bar(position = "fill", stat = "identity") +
    facet_grid(. ~ taxon_trivial) +
    theme_minimal(base_family = "Arial Narrow") +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(face = "italic", size = 14),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)
    ) +
    labs(
      title = "Monophagous/-lectic insect species are more threatened than generalists",
      x = "Specialization insects",
      y = "Percent RL threat status") +
    scale_fill_paletteer_d(palette = "colorBlindness::Blue2Gray8Steps")  -> fig_supp_threat_spec_a)


# boxplot
(ggplot(data = d,
        aes(y = insect_RL_Kat._trans, x = specialization)) +
    facet_grid(. ~ taxon_trivial) +
    geom_boxplot(col = "grey40",fill = "grey90",
                 alpha = 0.5) +
    theme_minimal(base_family = "Arial Narrow") +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(face = "italic", size = 14),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.position = "none"
    ) +
    labs(
      x = "Specialization insects",
      y = "Threat status insects") -> fig_supp_threat_spec_b)


# visualize test statistic
# all p-values in one go
dt <- compare_means(insect_RL_Kat._trans ~ specialization, data = d, 
                    method = "wilcox.test", group.by = "taxon_trivial")
# assuming your data frame is named dt and you have already created the 'significant' column
dt$significance_category <- with(dt, ifelse(p.adj < 0.05, 'p < 0.05', 
                                            ifelse(p.adj < 0.1, 'p < 0.1', 'p >= 0.1')))

# melt the data
data_melted <- melt(dt, id.vars = c("taxon_trivial", "group1", "group2", "significance_category"),
                    measure.vars = "p.adj")

# convert group1 and group2 into ordered factors as before
data_melted <- data_melted %>% 
  mutate(group1 = factor(group1, levels = c("mono", "oligo", "meso", "poly"))) %>%
  mutate(group2 = factor(group2, levels = c("mono", "oligo", "meso", "poly")))

# sort
data_melted$taxon_trivial <- factor(
  data_melted$taxon_trivial,
  levels = c("All taxa", "Bees", "Butterflies & moths", "Sawflies", "Hoverflies")
)

# plot the heatmap
(ggplot(data_melted, aes(x = group1, y = group2, fill = significance_category)) + 
    geom_tile(color = "white", size = 0.2) +
    scale_fill_paletteer_d(palette = "colorBlindness::Blue2Gray8Steps") +
    facet_wrap(~ taxon_trivial, nrow = 1) +
    labs(x = "Specialization", y = "Specialization", fill = "Significance") +
    theme_minimal(base_family = "Arial Narrow") +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(face = "italic", size = 14),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    ) -> fig_supp_threat_spec_c)


# multipanel
fig_supp_threat_spec_a /
  fig_supp_threat_spec_b /
  fig_supp_threat_spec_c +
  plot_annotation(tag_levels = 'A')


showtext_opts(dpi=600)
ggsave("Figures/figure7-supp.svg",
       bg = "white",
       height = 10,
       width = 12,
       dpi = 600)
showtext_opts(dpi=96)



# restoration scenario ----------------------------------------------------

# for all taxa
d <- read_csv("RL_inter_plant_insect-JUNE2024.csv")
# ensure uniqueness within each plant-insect pair
data_unique <- distinct(d, plant_species_RL, insect_species)

# initialize lists to track progress
selected_species <- character()
cumulative_species <- numeric()
unique_insects <- character()

while(length(selected_species) <= 20) {
  remaining_species <- setdiff(unique(data_unique$plant_species_RL), selected_species)
  
  # store the count of new unique insects each genus would add
  species_unique_count <- sapply(remaining_species, function(species) {
    new_species <- setdiff(data_unique %>% filter(plant_species_RL == species) %>% pull(insect_species), unique_insects)
    return(length(new_species))
  })
  
  # find the genus that adds the most new unique insects
  max_species <- names(species_unique_count[which.max(species_unique_count)])
  selected_species <- c(selected_species, max_species)
  
  # update the list of unique insects
  unique_insects <- unique(c(unique_insects, data_unique %>% filter(plant_species_RL == max_species) %>% pull(insect_species)))
  
  # update the cumulative count
  cumulative_species <- c(cumulative_species, length(unique_insects))
}

# create a data frame for plotting
accumulation_data_all <- data.frame(
  PlantSpecies = selected_species, 
  PlantSpeciesIndex = 1:length(selected_species), 
  CumulativeSpecies = cumulative_species
)



# for apiformes
d <- read_csv("RL_inter_plant_insect-JUNE2024.csv") %>% filter(taxon == "Apiformes")
# ensure uniqueness within each plant-insect pair
data_unique <- distinct(d, plant_species_RL, insect_species)

# initialize lists to track progress
selected_species <- character()
cumulative_species <- numeric()
unique_insects <- character()

while(length(selected_species) <= 20) {
  remaining_species <- setdiff(unique(data_unique$plant_species_RL), selected_species)
  
  # store the count of new unique insects each genus would add
  species_unique_count <- sapply(remaining_species, function(species) {
    new_species <- setdiff(data_unique %>% filter(plant_species_RL == species) %>% pull(insect_species), unique_insects)
    return(length(new_species))
  })
  
  # find the genus that adds the most new unique insects
  max_species <- names(species_unique_count[which.max(species_unique_count)])
  selected_species <- c(selected_species, max_species)
  
  # update the list of unique insects
  unique_insects <- unique(c(unique_insects, data_unique %>% filter(plant_species_RL == max_species) %>% pull(insect_species)))
  
  # update the cumulative count
  cumulative_species <- c(cumulative_species, length(unique_insects))
}

# create a data frame for plotting
accumulation_data_all_apiformes <- data.frame(
  PlantSpecies = selected_species, 
  PlantSpeciesIndex = 1:length(selected_species), 
  CumulativeSpecies = cumulative_species
)



# for lepidoptera
d <- read_csv("RL_inter_plant_insect-JUNE2024.csv") %>% filter(taxon == "Lepidoptera")
# ensure uniqueness within each plant-insect pair
data_unique <- distinct(d, plant_species_RL, insect_species)

# initialize lists to track progress
selected_species <- character()
cumulative_species <- numeric()
unique_insects <- character()

while(length(selected_species) <= 20) {
  remaining_species <- setdiff(unique(data_unique$plant_species_RL), selected_species)
  
  # store the count of new unique insects each genus would add
  species_unique_count <- sapply(remaining_species, function(species) {
    new_species <- setdiff(data_unique %>% filter(plant_species_RL == species) %>% pull(insect_species), unique_insects)
    return(length(new_species))
  })
  
  # find the genus that adds the most new unique insects
  max_species <- names(species_unique_count[which.max(species_unique_count)])
  selected_species <- c(selected_species, max_species)
  
  # update the list of unique insects
  unique_insects <- unique(c(unique_insects, data_unique %>% filter(plant_species_RL == max_species) %>% pull(insect_species)))
  
  # update the cumulative count
  cumulative_species <- c(cumulative_species, length(unique_insects))
}

# create a data frame for plotting
accumulation_data_all_lepidoptera <- data.frame(
  PlantSpecies = selected_species, 
  PlantSpeciesIndex = 1:length(selected_species), 
  CumulativeSpecies = cumulative_species
)



# for symphyta
d <- read_csv("RL_inter_plant_insect-JUNE2024.csv") %>% filter(taxon == "Symphyta")
# ensure uniqueness within each plant-insect pair
data_unique <- distinct(d, plant_species_RL, insect_species)

# initialize lists to track progress
selected_species <- character()
cumulative_species <- numeric()
unique_insects <- character()

while(length(selected_species) <= 20) {
  remaining_species <- setdiff(unique(data_unique$plant_species_RL), selected_species)
  
  # store the count of new unique insects each genus would add
  species_unique_count <- sapply(remaining_species, function(species) {
    new_species <- setdiff(data_unique %>% filter(plant_species_RL == species) %>% pull(insect_species), unique_insects)
    return(length(new_species))
  })
  
  # find the genus that adds the most new unique insects
  max_species <- names(species_unique_count[which.max(species_unique_count)])
  selected_species <- c(selected_species, max_species)
  
  # update the list of unique insects
  unique_insects <- unique(c(unique_insects, data_unique %>% filter(plant_species_RL == max_species) %>% pull(insect_species)))
  
  # update the cumulative count
  cumulative_species <- c(cumulative_species, length(unique_insects))
}

# create a data frame for plotting
accumulation_data_all_symphyta <- data.frame(
  PlantSpecies = selected_species, 
  PlantSpeciesIndex = 1:length(selected_species), 
  CumulativeSpecies = cumulative_species
)


# for syrphidae
d <- read_csv("RL_inter_plant_insect-JUNE2024.csv") %>% filter(taxon == "Syrphidae")
# ensure uniqueness within each plant-insect pair
data_unique <- distinct(d, plant_species_RL, insect_species)

# initialize lists to track progress
selected_species <- character()
cumulative_species <- numeric()
unique_insects <- character()

while(length(selected_species) <= 20) {
  remaining_species <- setdiff(unique(data_unique$plant_species_RL), selected_species)
  
  # store the count of new unique insects each genus would add
  species_unique_count <- sapply(remaining_species, function(species) {
    new_species <- setdiff(data_unique %>% filter(plant_species_RL == species) %>% pull(insect_species), unique_insects)
    return(length(new_species))
  })
  
  # find the genus that adds the most new unique insects
  max_species <- names(species_unique_count[which.max(species_unique_count)])
  selected_species <- c(selected_species, max_species)
  
  # update the list of unique insects
  unique_insects <- unique(c(unique_insects, data_unique %>% filter(plant_species_RL == max_species) %>% pull(insect_species)))
  
  # update the cumulative count
  cumulative_species <- c(cumulative_species, length(unique_insects))
}

# create a data frame for plotting
accumulation_data_all_syrphidae <- data.frame(
  PlantSpecies = selected_species, 
  PlantSpeciesIndex = 1:length(selected_species), 
  CumulativeSpecies = cumulative_species
)

# before bringing everything together, calculate the number of insects
d <- read_csv("RL_inter_plant_insect-JUNE2024.csv")
d %>% select(taxon_trivial, insect_species, plant_species) %>% distinct %>% 
  group_by(taxon_trivial) %>% summarize(n_distinct(insect_species))
d %>% select(taxon_trivial, insect_species, plant_species_RL) %>% distinct %>% 
  summarize(n_distinct(insect_species))

# bring all taxa together, and add column of percentage covered
dt <- bind_rows(
  list(
    accumulation_data_all %>% mutate(
      taxon_trivial = "All taxa",
      percentage_insects_covered = CumulativeSpecies /
        2239 * 100
    ),
    accumulation_data_all_apiformes %>% mutate(
      taxon_trivial = "Bees",
      percentage_insects_covered = CumulativeSpecies /
        312 * 100
    ),
    accumulation_data_all_lepidoptera %>% mutate(
      taxon_trivial = "Butterflies & moths",
      percentage_insects_covered = CumulativeSpecies /
        1151 * 100
    ),
    accumulation_data_all_symphyta %>% mutate(
      taxon_trivial = "Sawflies",
      percentage_insects_covered = CumulativeSpecies /
        437 * 100
    ),
    accumulation_data_all_syrphidae %>% mutate(
      taxon_trivial = "Hoverflies",
      percentage_insects_covered = CumulativeSpecies /
        339 * 100
    )
  )
)
View(dt)
dt <- dt %>% select(taxon_trivial, PlantSpecies, CumulativeSpecies, percentage_insects_covered)
dt <- dt %>% mutate(percentage_insects_covered = round(percentage_insects_covered,0))
write.csv(dt, "restoration-scenario.csv")




# restoration scenario to bring back threatened insects -------------------


# for all taxa
d <- read_csv("RL_inter_plant_insect-JUNE2024.csv")

# transf insect rl cats
d <- d %>%
  mutate(insect_RL_Kat._trans = case_when(
    RL_Kat. == "*" ~ 0,
    RL_Kat. == "V" ~ 1,
    RL_Kat. == "R" ~ 1,
    RL_Kat. == "3" ~ 2,
    RL_Kat. == "G" ~ 2,
    RL_Kat. == "2" ~ 3,
    RL_Kat. == "1" ~ 4,
    RL_Kat. == "0" ~ 5,
    RL_Kat. == "D" ~ NA,
    RL_Kat. == "♦" ~ NA,
    RL_Kat. == "nb" ~ NA,
    TRUE ~ NA_real_# Default case if none of the above conditions are met
  ))

# only include threatened insect species
d <- d %>% filter(insect_RL_Kat._trans >= 2)

# ensure uniqueness within each plant-insect pair
data_unique <- distinct(d, plant_species_RL, insect_species)

# initialize lists to track progress
selected_species <- character()
cumulative_species <- numeric()
unique_insects <- character()

while(length(selected_species) <= 20) {
  remaining_species <- setdiff(unique(data_unique$plant_species_RL), selected_species)
  
  # store the count of new unique insects each genus would add
  species_unique_count <- sapply(remaining_species, function(species) {
    new_species <- setdiff(data_unique %>% filter(plant_species_RL == species) %>% pull(insect_species), unique_insects)
    return(length(new_species))
  })
  
  # find the genus that adds the most new unique insects
  max_species <- names(species_unique_count[which.max(species_unique_count)])
  selected_species <- c(selected_species, max_species)
  
  # update the list of unique insects
  unique_insects <- unique(c(unique_insects, data_unique %>% filter(plant_species_RL == max_species) %>% pull(insect_species)))
  
  # update the cumulative count
  cumulative_species <- c(cumulative_species, length(unique_insects))
}

# create a data frame for plotting
accumulation_data_all <- data.frame(
  PlantSpecies = selected_species, 
  PlantSpeciesIndex = 1:length(selected_species), 
  CumulativeSpecies = cumulative_species
)


# for apiformes
apiformes <- d %>% filter(taxon == "Apiformes")
# ensure uniqueness within each plant-insect pair
data_unique <- distinct(apiformes, plant_species_RL, insect_species)

# initialize lists to track progress
selected_species <- character()
cumulative_species <- numeric()
unique_insects <- character()

while(length(selected_species) <= 20) {
  remaining_species <- setdiff(unique(data_unique$plant_species_RL), selected_species)
  
  # store the count of new unique insects each genus would add
  species_unique_count <- sapply(remaining_species, function(species) {
    new_species <- setdiff(data_unique %>% filter(plant_species_RL == species) %>% pull(insect_species), unique_insects)
    return(length(new_species))
  })
  
  # find the genus that adds the most new unique insects
  max_species <- names(species_unique_count[which.max(species_unique_count)])
  selected_species <- c(selected_species, max_species)
  
  # update the list of unique insects
  unique_insects <- unique(c(unique_insects, data_unique %>% filter(plant_species_RL == max_species) %>% pull(insect_species)))
  
  # update the cumulative count
  cumulative_species <- c(cumulative_species, length(unique_insects))
}

# create a data frame for plotting
accumulation_data_all_apiformes <- data.frame(
  PlantSpecies = selected_species, 
  PlantSpeciesIndex = 1:length(selected_species), 
  CumulativeSpecies = cumulative_species
)



# for lepidoptera
lepidoptera <- d %>% filter(taxon == "Lepidoptera")
# ensure uniqueness within each plant-insect pair
data_unique <- distinct(lepidoptera, plant_species_RL, insect_species)

# initialize lists to track progress
selected_species <- character()
cumulative_species <- numeric()
unique_insects <- character()

while(length(selected_species) <= 20) {
  remaining_species <- setdiff(unique(data_unique$plant_species_RL), selected_species)
  
  # store the count of new unique insects each genus would add
  species_unique_count <- sapply(remaining_species, function(species) {
    new_species <- setdiff(data_unique %>% filter(plant_species_RL == species) %>% pull(insect_species), unique_insects)
    return(length(new_species))
  })
  
  # find the genus that adds the most new unique insects
  max_species <- names(species_unique_count[which.max(species_unique_count)])
  selected_species <- c(selected_species, max_species)
  
  # update the list of unique insects
  unique_insects <- unique(c(unique_insects, data_unique %>% filter(plant_species_RL == max_species) %>% pull(insect_species)))
  
  # update the cumulative count
  cumulative_species <- c(cumulative_species, length(unique_insects))
}

# create a data frame for plotting
accumulation_data_all_lepidoptera <- data.frame(
  PlantSpecies = selected_species, 
  PlantSpeciesIndex = 1:length(selected_species), 
  CumulativeSpecies = cumulative_species
)



# for symphyta
symphyta <- d %>% filter(taxon == "Symphyta")
# ensure uniqueness within each plant-insect pair
data_unique <- distinct(symphyta, plant_species_RL, insect_species)

# initialize lists to track progress
selected_species <- character()
cumulative_species <- numeric()
unique_insects <- character()

while(length(selected_species) <= 20) {
  remaining_species <- setdiff(unique(data_unique$plant_species_RL), selected_species)
  
  # store the count of new unique insects each genus would add
  species_unique_count <- sapply(remaining_species, function(species) {
    new_species <- setdiff(data_unique %>% filter(plant_species_RL == species) %>% pull(insect_species), unique_insects)
    return(length(new_species))
  })
  
  # find the genus that adds the most new unique insects
  max_species <- names(species_unique_count[which.max(species_unique_count)])
  selected_species <- c(selected_species, max_species)
  
  # update the list of unique insects
  unique_insects <- unique(c(unique_insects, data_unique %>% filter(plant_species_RL == max_species) %>% pull(insect_species)))
  
  # update the cumulative count
  cumulative_species <- c(cumulative_species, length(unique_insects))
}

# create a data frame for plotting
accumulation_data_all_symphyta <- data.frame(
  PlantSpecies = selected_species, 
  PlantSpeciesIndex = 1:length(selected_species), 
  CumulativeSpecies = cumulative_species
)


# for syrphidae
syrphidae <- d %>% filter(taxon == "Syrphidae")
# ensure uniqueness within each plant-insect pair
data_unique <- distinct(syrphidae, plant_species_RL, insect_species)

# initialize lists to track progress
selected_species <- character()
cumulative_species <- numeric()
unique_insects <- character()

while(length(selected_species) <= 20) {
  remaining_species <- setdiff(unique(data_unique$plant_species_RL), selected_species)
  
  # store the count of new unique insects each genus would add
  species_unique_count <- sapply(remaining_species, function(species) {
    new_species <- setdiff(data_unique %>% filter(plant_species_RL == species) %>% pull(insect_species), unique_insects)
    return(length(new_species))
  })
  
  # find the genus that adds the most new unique insects
  max_species <- names(species_unique_count[which.max(species_unique_count)])
  selected_species <- c(selected_species, max_species)
  
  # update the list of unique insects
  unique_insects <- unique(c(unique_insects, data_unique %>% filter(plant_species_RL == max_species) %>% pull(insect_species)))
  
  # update the cumulative count
  cumulative_species <- c(cumulative_species, length(unique_insects))
}

# create a data frame for plotting
accumulation_data_all_syrphidae <- data.frame(
  PlantSpecies = selected_species, 
  PlantSpeciesIndex = 1:length(selected_species), 
  CumulativeSpecies = cumulative_species
)

# before bringing everything together, calculate the number of host plant species
# that are supported in each insect taxon
# calculate insect species numbers
d %>% select(taxon_trivial, insect_species, plant_species) %>% distinct %>% 
  group_by(taxon_trivial) %>% summarize(n_distinct(insect_species))
d %>% select(taxon_trivial, insect_species, plant_species_RL) %>% distinct %>% 
  summarize(n_distinct(insect_species))

# bring all taxa together, and add column of percentage covered
dt <- bind_rows(
  list(
    accumulation_data_all %>% mutate(
      taxon_trivial = "All taxa",
      percentage_insects_covered = CumulativeSpecies /
        724 * 100
    ),
    accumulation_data_all_apiformes %>% mutate(
      taxon_trivial = "Bees",
      percentage_insects_covered = CumulativeSpecies /
        158 * 100
    ),
    accumulation_data_all_lepidoptera %>% mutate(
      taxon_trivial = "Butterflies & moths",
      percentage_insects_covered = CumulativeSpecies /
        367 * 100
    ),
    accumulation_data_all_symphyta %>% mutate(
      taxon_trivial = "Sawflies",
      percentage_insects_covered = CumulativeSpecies /
        94 * 100
    ),
    accumulation_data_all_syrphidae %>% mutate(
      taxon_trivial = "Hoverflies",
      percentage_insects_covered = CumulativeSpecies /
        105 * 100
    )
  )
)
View(dt)
dt <- dt %>% select(taxon_trivial, PlantSpecies, CumulativeSpecies, percentage_insects_covered)
dt <- dt %>% mutate(percentage_insects_covered = round(percentage_insects_covered,0))
write.csv2(dt, "restoration-scenario-for-threatened-insects.csv", row.names = FALSE)


# How many insects use endangered plants ----------------------------------


# load data ---------------------------------------------------------------
d <- read_csv("RL_inter_plant_insect-JUNE2024.csv")


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
    RL_Kat._p == "D" ~ NA,
    RL_Kat._p == "♦" ~ NA,
    RL_Kat._p == "nb" ~ NA,
    TRUE ~ NA_real_
  )) %>% 
  mutate(plants_RL_Kat._trans_cat = case_when(
    plants_RL_Kat._trans_num == 0 ~ "LC",
    plants_RL_Kat._trans_num == 1 ~ "NT",
    plants_RL_Kat._trans_num == 2 ~ "VU",
    plants_RL_Kat._trans_num == 3 ~ "EN",
    plants_RL_Kat._trans_num == 4 ~ "CR",
    plants_RL_Kat._trans_num == 5 ~ "EX"
  )) 

# exclude extinct species from this analysis these are interactions
# that no longer exist, exclude non-assessed species
d <- d %>% 
  filter(plants_RL_Kat._trans_cat != "EX") %>% 
  filter(!is.na(plants_RL_Kat._trans_cat))


# extinctions from CR to LC/NE --------------------------------------------

# loop that calculates for each taxa how many new additional insects are
# covered by higher threat levels, starting from LC+NE to CR
ext <- data.frame()
for (k in 1:5) {
  ext_all <- d %>%
    filter(plants_RL_Kat._trans_num < k) %>%
    summarise(insect_species = n_distinct(insect_species)) %>%
    mutate(ext = unique((d %>% filter(plants_RL_Kat._trans_num == k - 1))$plants_RL_Kat._trans_cat),
           taxon = "All taxa",
           cover = 
             insect_species / 
             d %>% summarise(insect_species = n_distinct(insect_species)) %>% pull
           * 100
    ) 
  
  for (j in c("Apiformes", "Lepidoptera", "Symphyta", "Syrphidae")) {
    ext_taxa <-
      d %>%
      filter(plants_RL_Kat._trans_num < k, taxon == j) %>%
      summarise(insect_species = n_distinct(insect_species)) %>%
      mutate(ext = unique((d %>% filter(plants_RL_Kat._trans_num == k - 1))$plants_RL_Kat._trans_cat),
             taxon = j,
             cover = 
               insect_species / d %>% filter(taxon == j) %>% 
               summarise(insect_species = n_distinct(insect_species)) %>% pull
             * 100
      )
    
    ext <- rbind(ext, ext_all, ext_taxa)  # binding the taxa
  }
}
#View(ext)

# prepare data frame
ext <- ext %>% 
  unnest(cover, names_repair = "universal") %>% 
  unique() # un-nest, rename and delete duplication, order levels

# to visualize the effects of an extinction of an entire threat category
# we need to take the cover value of the preceeding threat category. 
# for example: LC+NE covers some 2200 species, if it goes extinct, we end up
# with 0, if CR goes extinct we end up with the number of species that all threat
# categories up to EN still cover.
ext <- ext %>% 
  mutate(ext_real = case_when(
    ext == "LC" ~ "NT lost",
    ext == "NT" ~ "VU lost",
    ext == "VU" ~ "EN lost",
    ext == "EN" ~ "CR lost",
    ext == "CR" ~ "All survive"
  )) 

# remove the 100% All survive cat
ext <- ext %>% filter(ext_real != "All survive")



# reverse extinctions: from LC/NE to CR -----------------------------------
ext_rev <- data.frame()
for (k in 1:5) {
  ext_all <- d %>%
    filter(plants_RL_Kat._trans_num >= k ) %>%
    summarise(insect_species = n_distinct(insect_species)) %>%
    mutate(ext = unique((d %>% filter(plants_RL_Kat._trans_num == k - 1))$plants_RL_Kat._trans_cat),
           taxon = "All taxa",
           cover = 
             insect_species / 
             d %>% summarise(insect_species = n_distinct(insect_species)) %>% pull
           * 100
    ) 
  
  for (j in c("Apiformes", "Lepidoptera", "Symphyta", "Syrphidae")) {
    ext_taxa <-
      d %>%
      filter(plants_RL_Kat._trans_num >= k, taxon == j) %>%
      summarise(insect_species = n_distinct(insect_species)) %>%
      mutate(ext = unique((d %>% filter(plants_RL_Kat._trans_num == k - 1))$plants_RL_Kat._trans_cat),
             taxon = j,
             cover = 
               insect_species / d %>% filter(taxon == j) %>% 
               summarise(insect_species = n_distinct(insect_species)) %>% pull
             * 100
      )
    
    ext_rev <- rbind(ext_rev, ext_all, ext_taxa)  # binding the taxa
  }
}

# prepare data frame
ext_rev <- ext_rev %>% 
  unnest(cover, names_repair = "universal") %>% 
  unique() # un-nest, rename and delete duplication, order levels

ext_rev <- ext_rev %>% 
  mutate(ext_real = case_when(
    ext == "LC" ~ "LC lost",
    ext == "NT" ~ "NT lost",
    ext == "VU" ~ "VU lost",
    ext == "EN" ~ "EN lost",
    ext == "CR" ~ "CR lost"
  ))

ext_rev <- ext_rev %>% select(taxon,
                              ext_real, 
                              insect_species_rev = insect_species,
                              cover_rev = cover)

ext <- full_join(ext, ext_rev) %>% select(
  taxon,
  ext_real,
  insect_species,
  cover,
  insect_species_rev,
  cover_rev
) 


# add number of plant species ---------------------------------------------

ext <- ext %>% left_join(
  bind_rows(
    d %>% 
      group_by(plants_RL_Kat._trans_cat) %>% 
      summarise(n = n_distinct(plant_species_RL)) %>% 
      mutate(taxon = "All taxa"),
    d %>% 
      group_by(taxon, plants_RL_Kat._trans_cat) %>% 
      summarise(n = n_distinct(plant_species_RL)) 
  ) %>% 
    mutate(ext_real = case_when(
      plants_RL_Kat._trans_cat == "LC" ~ "LC lost",
      plants_RL_Kat._trans_cat == "NT" ~ "NT lost",
      plants_RL_Kat._trans_cat == "VU" ~ "VU lost",
      plants_RL_Kat._trans_cat == "EN" ~ "EN lost",
      plants_RL_Kat._trans_cat == "CR" ~ "CR lost"
    ))
)

# fill NAs with 0
ext <- ext %>% replace(is.na(.), 0)

# order
ext <- ext %>% 
  mutate(ext_real = factor(ext_real,
                           levels = c(
                             "CR lost",
                             "EN lost",
                             "VU lost",
                             "NT lost",
                             "LC lost")),
         plants_RL_Kat._trans_cat = factor(plants_RL_Kat._trans_cat,
                                           levels = c(
                                             "CR",
                                             "EN",
                                             "VU",
                                             "NT",
                                             "LC"))
  )


# visualization red list cats removal -------------------------------------

# trivial taxon names
ext <- ext %>% mutate(taxon_trivial = case_when(
  taxon == "All taxa" ~ "All taxa",
  taxon == "Apiformes" ~ "Bees",
  taxon == "Lepidoptera" ~ "Butterflies",
  taxon == "Symphyta" ~ "Sawflies",
  taxon == "Syrphidae" ~ "Hoverflies"
)) %>% mutate(taxon_trivial = factor(taxon_trivial,
                                     levels = c("All taxa", 
                                                "Bees", 
                                                "Butterflies", 
                                                "Sawflies", 
                                                "Hoverflies")))

# for second y axis
transformation_ratio <- max(ext$n) / max(ext$cover)

ggplot(data = ext) +
    facet_grid(~taxon_trivial) +
    geom_chicklet(aes(x = plants_RL_Kat._trans_cat, y = n / transformation_ratio),
                  radius = unit(2, "pt"), alpha = .1,
                  position = position_stack(reverse = FALSE)) +
    #geom_point(aes(x = plants_RL_Kat._trans_cat, y = cover, color = "Starting with CR")) + 
    #geom_line(aes(x = plants_RL_Kat._trans_cat, y = cover), color = "magenta", group = 1, 
    #          arrow = arrow(type = "closed", length = unit(0.1, "inches"), ends = "last")) +
    geom_point(aes(x = plants_RL_Kat._trans_cat, y = cover_rev, color = "Starting with LC")) + 
    geom_line(aes(x = plants_RL_Kat._trans_cat, y = cover_rev), col = "green", group = 1,
              arrow = arrow(type = "closed", length = unit(0.1, "inches"), ends = "first")) +
    scale_color_manual(values = c(
      #"Starting with CR" = "magenta", 
                                  "Starting with LC" = "green")) +
    scale_y_continuous(
      name = "Surviving insect species (%)",
      sec.axis = sec_axis(~ . * transformation_ratio, name = "Number of plant species lost")
    ) +
    theme_minimal(base_family = "Arial Narrow") +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(face = "italic", size = 14),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.position = "bottom",
      legend.text = element_text(size = 14)
    ) +
    labs(x= "Threat status of host plants", 
         title = "Sequential loss of host plants by Red List category",
         color = "")

showtext_opts(dpi=600)
ggsave(bg = "white",
       dpi = 600,
       "Figures/figure5-supp.svg",
       width = 12,
       height = 3.3)
showtext_opts(dpi=96)
