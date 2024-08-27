source("00-preamble.R")

# asymmetry data prep -------------------------------------------

d <- read_csv("RL_inter_plant_insect-JUNE2024.csv")

# transf rl cats
d <- d %>%
  mutate(plants_RL_Kat._trans = case_when(
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
  group_by(insect_species) %>% 
  summarize(
    taxon_trivial = first(taxon_trivial),
    insect_species = first(insect_species),
    specialization = first(specialization),
    plant_meanrlcat = mean(plants_RL_Kat._trans, na.rm = T)
  )

d <- bind_rows(d, d %>% mutate(taxon_trivial = "All taxa")) %>%
  na.omit()


# asymmetry model ---------------------------------------------------------
d$specialization <- 
  factor(d$specialization, levels = c("mono", "oligo", "meso", "poly"))
range(d$plant_meanrlcat)

mod_asy <- lm(plant_meanrlcat ~ specialization*taxon_trivial, d)
plot(mod_asy)

# pairwise wilcox test for each taxon
dt <- d %>% filter(taxon_trivial == "All taxa")
kruskal.test(plant_meanrlcat ~ specialization, dt)
pairwise.wilcox.test(dt$plant_meanrlcat, dt$specialization,
                     p.adjust.method = "BH")

dt <- d %>% filter(taxon_trivial == "Bees")
kruskal.test(plant_meanrlcat ~ specialization, dt)
pairwise.wilcox.test(dt$plant_meanrlcat, dt$specialization,
                     p.adjust.method = "BH")

dt <- d %>% filter(taxon_trivial == "Butterflies & moths")
kruskal.test(plant_meanrlcat ~ specialization, dt)
pairwise.wilcox.test(dt$plant_meanrlcat, dt$specialization,
                     p.adjust.method = "BH")

dt <- d %>% filter(taxon_trivial == "Sawflies")
kruskal.test(plant_meanrlcat ~ specialization, dt)
pairwise.wilcox.test(dt$plant_meanrlcat, dt$specialization,
                     p.adjust.method = "BH")

dt <- d %>% filter(taxon_trivial == "Hoverflies")
kruskal.test(plant_meanrlcat ~ specialization, dt)
pairwise.wilcox.test(dt$plant_meanrlcat, dt$specialization,
                     p.adjust.method = "BH")



# visualize ---------------------------------------------------------------

d$taxon_trivial <- factor(
  d$taxon_trivial,
  levels = c("All taxa", "Bees", "Butterflies & moths", "Sawflies", "Hoverflies")
)

(ggplot(data = d,
        aes(y = plant_meanrlcat, x = specialization)) +
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
    labs(title = "Host plants of specialized insects are less threatened",
         x = "Specialization insects",
         y = "Avg. threat status plants") -> fig4a)


# visualize test statistic
# all p-values in one go
dt <- compare_means(plant_meanrlcat ~ specialization, data = d, 
                    method = "wilcox.test", group.by = "taxon_trivial")
# assuming your data frame is named dt and you have already created the 'significant' column
dt$significance_category <- with(dt, ifelse(p.adj < 0.05, 'p < 0.05', 
                                            ifelse(p.adj < 0.1, 'p < 0.1', 'p >= 0.1')))

# melt the data
data_melted <- melt(dt, id.vars = c("taxon_trivial", "group1", "group2", 
                                    "significance_category"),
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
  scale_fill_manual(values = c('p < 0.05' = '#ff00bf', 'p < 0.1' = '#ffc4f0', 'p >= 0.1' = '#cdbbc9')) +
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
  ) -> fig4b)



# add number and percentages of specialization types for each taxa
dt <- d %>% group_by(taxon_trivial) %>% count(specialization) %>% left_join(
  d %>% group_by(taxon_trivial) %>%
    count(specialization) %>%
    group_by(taxon_trivial) %>%
    summarise(total = sum(n))
) %>% mutate(percentage = n / total)

# sort
dt$taxon_trivial <- factor(
  dt$taxon_trivial,
  levels = c("All taxa", "Bees", "Butterflies & moths", "Sawflies", "Hoverflies")
)


max_n <- max(dt$n)
(ggplot(dt, aes(x = specialization, y = n)) +
  facet_grid(. ~ taxon_trivial) +
  geom_chicklet(fill = "grey90") +
  geom_point(aes(y = percentage * max_n), color = "blue") + 
  geom_line(aes(y = percentage * max_n, group = taxon_trivial), linetype = "dotted", color = "blue") +
  scale_y_continuous(
    "Number of insect species",
    sec.axis = sec_axis(~ . / max_n * 100, name = "Percentage of insect species")
  ) +
  theme_minimal(base_family = "Arial Narrow") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(face = "italic", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"
  ) +
  labs(x = "Specialization insects",
       y = "Number of insect species",
       title = "Distribution along the specialization gradient varies by taxa") -> fig4c)

# multipanel
fig4c /
  fig4a /
  fig4b +
  plot_annotation(tag_levels = 'a')


showtext_opts(dpi=600)
ggsave("Figures/figure4-main-rev.pdf",
       bg = "white",
       height = 10,
       width = 12,
       dpi = 600)
showtext_opts(dpi=96)


