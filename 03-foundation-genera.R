source("00-preamble.R")

# load data ---------------------------------------------------------------
d <- read_csv("RL_inter_plant_insect-JUNE2024.csv")

# how many insect species per taxon
d %>% group_by(taxon) %>% summarise(n_distinct(insect_species))


# are interactions skewed? ------------------------------------------------

# calculate how many insects plant genera sustain
d <- d %>% select(taxon_trivial, insect_species, plant_species_RL, plant_genus) %>% 
  group_by(taxon_trivial, plant_genus) %>% 
  summarise(n = n_distinct(insect_species))

d <- bind_rows(
  d,
  read_csv("RL_inter_plant_insect-JUNE2024.csv") %>% 
    group_by(plant_genus) %>% 
    summarise(n = n_distinct(insect_species)) %>% 
    mutate(taxon_trivial = "All taxa")
)


# change order of facet labels
d$taxon_trivial <- factor(
  d$taxon_trivial,
  levels = c("All taxa", "Bees", "Butterflies & moths", "Sawflies", "Hoverflies")
)

# histogram
(ggplot(data = d, aes(x = n)) +
    facet_wrap(.~ taxon_trivial, scales = "free", ncol = 5) +
    geom_histogram(fill = "blue", alpha = 0.6) +
    theme_minimal(base_family = "Arial Narrow") +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(face = "italic", size = 14),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.position = "none"
    ) +
    labs(x = "Insect species richness",
         y = "Count of plant genera",
         title = "Plant-insect interactions are highly skewed") -> fig3a)


# top keystone plants -----------------------------------------------------

# mean percent of taxon hosted
d <- full_join(d,
               bind_rows(
               read_csv("RL_inter_plant_insect-JUNE2024.csv") %>% 
                 select(taxon_trivial, insect_species, plant_species_RL, plant_genus) %>% 
                 group_by(taxon_trivial) %>% 
                 summarise(n_tot = n_distinct(insect_species)),
               read_csv("RL_inter_plant_insect-JUNE2024.csv") %>% 
                 select(taxon_trivial, insect_species, plant_species_RL, plant_genus) %>% 
                 summarise(n_tot = n_distinct(insect_species)) %>% 
                 mutate(taxon_trivial = "All taxa")
               )
) %>% 
  mutate(perc_insect_hosted = n/n_tot)

# add rank column
d <- d %>% group_by(taxon_trivial) %>% arrange(desc(perc_insect_hosted)) %>% 
  mutate(plant_id = row_number())

# top genera for each insect group
top_genera <- d %>%
  group_by(taxon_trivial) %>%
  top_n(10, perc_insect_hosted) %>%
  ungroup()

# change order of facet labels
top_genera$taxon_trivial <- factor(
  top_genera$taxon_trivial,
  levels = c("All taxa", "Bees", "Butterflies & moths", "Sawflies", "Hoverflies")
)
d$taxon_trivial <- factor(
  d$taxon_trivial,
  levels = c("All taxa", "Bees", "Butterflies & moths", "Sawflies", "Hoverflies")
)

# plot
(ggplot(d %>% filter(plant_id <= 10),
        aes(x = -plant_id, y = perc_insect_hosted*100)) +
    geom_chicklet(fill = "magenta", col = "grey50", alpha = 0.1) +
    facet_wrap( ~ taxon_trivial, scales = "free", ncol = 5) +
    geom_text(
      data = top_genera,
      aes(x = -plant_id, y = 0, label = plant_genus),
      position = position_dodge(width = 0.5),
      hjust = -0,
      col = "grey20",
      family = "Arial Narrow",
      fontface = 3,
      size = 4
    ) +
    theme_minimal(base_family = "Arial Narrow") +
    theme(
      panel.grid.major.y  = element_blank(),
      panel.grid.minor.y  = element_blank(),
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(face = "italic", size = 14),
      axis.text = element_text(size = 12),
      axis.text.y = element_blank(),
      axis.title = element_text(size = 14)
    ) +
    labs(
      x = "",
      y = "Percentage of insect species hosted",
      title = "Top 10 foundation plant genera"
    ) +
    coord_flip() -> fig3b)


# accumulation curves -----------------------------------------------------

# for all taxa
d <- read_csv("RL_inter_plant_insect-JUNE2024.csv")
# ensure uniqueness within each plant-insect pair
data_unique <- distinct(d, plant_genus, insect_species)

# initialize lists to track progress
selected_genera <- character()
cumulative_species <- numeric()
unique_insects <- character()

while(length(selected_genera) < length(unique(data_unique$plant_genus))) {
  remaining_genera <- setdiff(unique(data_unique$plant_genus), selected_genera)
  
  # store the count of new unique insects each genus would add
  genus_unique_count <- sapply(remaining_genera, function(genus) {
    new_species <- setdiff(data_unique %>% filter(plant_genus == genus) %>% pull(insect_species), unique_insects)
    return(length(new_species))
  })
  
  # find the genus that adds the most new unique insects
  max_genus <- names(genus_unique_count[which.max(genus_unique_count)])
  selected_genera <- c(selected_genera, max_genus)
  
  # update the list of unique insects
  unique_insects <- unique(c(unique_insects, data_unique %>% filter(plant_genus == max_genus) %>% pull(insect_species)))
  
  # update the cumulative count
  cumulative_species <- c(cumulative_species, length(unique_insects))
}

# create a data frame for plotting
accumulation_data_all <- data.frame(
  PlantGenus = selected_genera, 
  PlantGenusIndex = 1:length(selected_genera), 
  CumulativeSpecies = cumulative_species
)

# for apiformes
d <- read_csv("RL_inter_plant_insect-JUNE2024.csv") %>% filter(taxon == "Apiformes")
data_unique <- distinct(d, plant_genus, insect_species)

selected_genera <- character()
cumulative_species <- numeric()
unique_insects <- character()
while(length(selected_genera) < length(unique(data_unique$plant_genus))) {
  remaining_genera <- setdiff(unique(data_unique$plant_genus), selected_genera)
  
  # store the count of new unique insects each genus would add
  genus_unique_count <- sapply(remaining_genera, function(genus) {
    new_species <- setdiff(data_unique %>% filter(plant_genus == genus) %>% pull(insect_species), unique_insects)
    return(length(new_species))
  })
  
  # find the genus that adds the most new unique insects
  max_genus <- names(genus_unique_count[which.max(genus_unique_count)])
  selected_genera <- c(selected_genera, max_genus)
  
  # update the list of unique insects
  unique_insects <- unique(c(unique_insects, data_unique %>% filter(plant_genus == max_genus) %>% pull(insect_species)))
  
  # update the cumulative count
  cumulative_species <- c(cumulative_species, length(unique_insects))
}

accumulation_data_apiformes <- data.frame(
  PlantGenus = selected_genera, 
  PlantGenusIndex = 1:length(selected_genera), 
  CumulativeSpecies = cumulative_species
)


# for lepidoptera
d <- read_csv("RL_inter_plant_insect-JUNE2024.csv") %>% filter(taxon == "Lepidoptera")
data_unique <- distinct(d, plant_genus, insect_species)

selected_genera <- character()
cumulative_species <- numeric()
unique_insects <- character()
while(length(selected_genera) < length(unique(data_unique$plant_genus))) {
  remaining_genera <- setdiff(unique(data_unique$plant_genus), selected_genera)
  
  # store the count of new unique insects each genus would add
  genus_unique_count <- sapply(remaining_genera, function(genus) {
    new_species <- setdiff(data_unique %>% filter(plant_genus == genus) %>% pull(insect_species), unique_insects)
    return(length(new_species))
  })
  
  # find the genus that adds the most new unique insects
  max_genus <- names(genus_unique_count[which.max(genus_unique_count)])
  selected_genera <- c(selected_genera, max_genus)
  
  # update the list of unique insects
  unique_insects <- unique(c(unique_insects, data_unique %>% filter(plant_genus == max_genus) %>% pull(insect_species)))
  
  # update the cumulative count
  cumulative_species <- c(cumulative_species, length(unique_insects))
}

accumulation_data_lepidoptera <- data.frame(
  PlantGenus = selected_genera, 
  PlantGenusIndex = 1:length(selected_genera), 
  CumulativeSpecies = cumulative_species
)

# for symphyta
d <- read_csv("RL_inter_plant_insect-JUNE2024.csv") %>% filter(taxon == "Symphyta")
data_unique <- distinct(d, plant_genus, insect_species)

selected_genera <- character()
cumulative_species <- numeric()
unique_insects <- character()
while(length(selected_genera) < length(unique(data_unique$plant_genus))) {
  remaining_genera <- setdiff(unique(data_unique$plant_genus), selected_genera)
  
  # store the count of new unique insects each genus would add
  genus_unique_count <- sapply(remaining_genera, function(genus) {
    new_species <- setdiff(data_unique %>% filter(plant_genus == genus) %>% pull(insect_species), unique_insects)
    return(length(new_species))
  })
  
  # find the genus that adds the most new unique insects
  max_genus <- names(genus_unique_count[which.max(genus_unique_count)])
  selected_genera <- c(selected_genera, max_genus)
  
  # update the list of unique insects
  unique_insects <- unique(c(unique_insects, data_unique %>% filter(plant_genus == max_genus) %>% pull(insect_species)))
  
  # update the cumulative count
  cumulative_species <- c(cumulative_species, length(unique_insects))
}

accumulation_data_symphyta <- data.frame(
  PlantGenus = selected_genera, 
  PlantGenusIndex = 1:length(selected_genera), 
  CumulativeSpecies = cumulative_species
)

# for syrphidae
d <- read_csv("RL_inter_plant_insect-JUNE2024.csv") %>% filter(taxon == "Syrphidae")
data_unique <- distinct(d, plant_genus, insect_species)

selected_genera <- character()
cumulative_species <- numeric()
unique_insects <- character()
while(length(selected_genera) < length(unique(data_unique$plant_genus))) {
  remaining_genera <- setdiff(unique(data_unique$plant_genus), selected_genera)
  
  # store the count of new unique insects each genus would add
  genus_unique_count <- sapply(remaining_genera, function(genus) {
    new_species <- setdiff(data_unique %>% filter(plant_genus == genus) %>% pull(insect_species), unique_insects)
    return(length(new_species))
  })
  
  # find the genus that adds the most new unique insects
  max_genus <- names(genus_unique_count[which.max(genus_unique_count)])
  selected_genera <- c(selected_genera, max_genus)
  
  # update the list of unique insects
  unique_insects <- unique(c(unique_insects, data_unique %>% filter(plant_genus == max_genus) %>% pull(insect_species)))
  
  # update the cumulative count
  cumulative_species <- c(cumulative_species, length(unique_insects))
}

accumulation_data_syrphidae <- data.frame(
  PlantGenus = selected_genera, 
  PlantGenusIndex = 1:length(selected_genera), 
  CumulativeSpecies = cumulative_species
)

# bring all taxa together, and add column of percentage covered
d <- bind_rows(
  list(
    accumulation_data_all %>% mutate(
      taxon_trivial = "All taxa",
      percentage_insects_covered = CumulativeSpecies /
        max(CumulativeSpecies) * 100
    ),
    accumulation_data_apiformes %>% mutate(
      taxon_trivial = "Bees",
      percentage_insects_covered = CumulativeSpecies /
        max(CumulativeSpecies) * 100
    ),
    accumulation_data_lepidoptera %>% mutate(
      taxon_trivial = "Butterflies & moths",
      percentage_insects_covered = CumulativeSpecies /
        max(CumulativeSpecies) * 100
    ),
    accumulation_data_symphyta %>% mutate(
      taxon_trivial = "Sawflies",
      percentage_insects_covered = CumulativeSpecies /
        max(CumulativeSpecies) * 100
    ),
    accumulation_data_syrphidae %>% mutate(
      taxon_trivial = "Hoverflies",
      percentage_insects_covered = CumulativeSpecies /
        max(CumulativeSpecies) * 100
    )
  )
)

# for plotting x intercept
x <- d %>% group_by(taxon_trivial) %>% 
  filter(percentage_insects_covered >= 90) %>% 
  summarise(total = last(PlantGenusIndex), 
            PlantGenusIndex = first(PlantGenusIndex), 
            percentage_insects_covered  = first(percentage_insects_covered)) %>% 
  mutate(percentage_plants_necessary = PlantGenusIndex/total)


# plot
# change facet labels
# change order of facet labels
x$taxon_trivial <- factor(
  x$taxon_trivial,
  levels = c("All taxa", "Bees", "Butterflies & moths", "Sawflies", "Hoverflies")
)
d$taxon_trivial <- factor(
  d$taxon_trivial,
  levels = c("All taxa", "Bees", "Butterflies & moths", "Sawflies", "Hoverflies")
)


(ggplot(data = d, aes(x = PlantGenusIndex, y = percentage_insects_covered)) +
  facet_wrap(.~ taxon_trivial, scales = "free_x", ncol = 5) +
  geom_line(color = "grey") +
  geom_text(data = x, aes(x = PlantGenusIndex, y = 90, 
                          label = paste(label_percent()
                          ( 
                           round(percentage_plants_necessary, 2)
                            )
                          , "plant gen.")),
            fontface = 3, family = "Arial Narrow",
            col = "blue", hjust = -.2, vjust = 1) +
  geom_segment(data = x, aes(x = PlantGenusIndex, xend = PlantGenusIndex, y = 0, yend = 90),
               color = "magenta", linetype = "dashed") +
  geom_segment(aes(x = 0, xend = PlantGenusIndex, y = 90, yend = 90),
               data = x,
               color = "magenta", linetype = "dashed") +
  scale_y_continuous(limits = c(0,100),breaks = c(seq(0, 100, by = 10), 90)) +
  labs(y = "Insect diversity covered (%)", 
       x = "Number of host plant genera added",
       title = "Few foundation plant genera support the majority of insects") +
  theme_minimal(base_family = "Arial Narrow") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(face = "italic", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"
  ) -> fig3c)


# multipanel --------------------------------------------------------------

fig3a /
  fig3c /
  fig3b + 
  plot_layout(guides = "collect") & 
  plot_annotation(tag_levels = 'a')

showtext_opts(dpi=600)
ggsave(bg = "white",
       dpi = 600,
       "Figures/figure3-main-rev.pdf",
       width = 12,
       height = 10)
showtext_opts(dpi=96)




