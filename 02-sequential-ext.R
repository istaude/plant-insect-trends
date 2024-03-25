source("00-preamble.R")

# load data ---------------------------------------------------------------
d <- read_csv("RL_inter_plant_insect-MARCH2024.csv")
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

#View(ext)



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

(ggplot(data = ext) +
  facet_grid(~taxon_trivial) +
  geom_chicklet(aes(x = plants_RL_Kat._trans_cat, y = n / transformation_ratio),
                radius = unit(2, "pt"), alpha = .1,
                position = position_stack(reverse = FALSE)) +
  geom_point(aes(x = plants_RL_Kat._trans_cat, y = cover, color = "Starting with CR")) + 
  geom_line(aes(x = plants_RL_Kat._trans_cat, y = cover), color = "magenta", group = 1, 
            arrow = arrow(type = "closed", length = unit(0.1, "inches"), ends = "last")) +
  geom_point(aes(x = plants_RL_Kat._trans_cat, y = cover_rev, color = "Starting with LC")) + 
  geom_line(aes(x = plants_RL_Kat._trans_cat, y = cover_rev), col = "green", group = 1,
            arrow = arrow(type = "closed", length = unit(0.1, "inches"), ends = "first")) +
  scale_color_manual(values = c("Starting with CR" = "magenta", 
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
       color = "") -> fig2a)



# same for range size of species ------------------------------------------

d %>% select(akt_Best_p) %>% count(akt_Best_p)
  
# transf rl cats
d <- d %>%
  mutate(plants_akt_Best_trans_num = case_when(
    akt_Best_p == "sh" ~ 0,
    akt_Best_p == "h" ~ 1,
    akt_Best_p == "mh" ~ 2,
    akt_Best_p == "s" ~ 3,
    akt_Best_p == "ss" ~ 4,
    akt_Best_p == "es" ~ 5,
    akt_Best_p == "ex" ~ 6,
    akt_Best_p == "nb" ~ NA,
    akt_Best_p == "?" ~ NA,
    TRUE ~ NA_real_
  )) %>% 
  mutate(plants_akt_Best_trans_cat = case_when(
    plants_akt_Best_trans_num == 0 ~ "very common",
    plants_akt_Best_trans_num == 1 ~ "common",
    plants_akt_Best_trans_num == 2 ~ "moderately common",
    plants_akt_Best_trans_num == 3 ~ "rare",
    plants_akt_Best_trans_num == 4 ~ "very rare",
    plants_akt_Best_trans_num == 5 ~ "extremely rare",
    plants_akt_Best_trans_num == 6 ~ "extinct"
  )) 

# exclude extinct species from this analysis these are interactions
# that no longer exist
d <- d %>% 
  filter(plants_akt_Best_trans_cat != "extinct") %>% 
  filter(!is.na(plants_akt_Best_trans_num))


# extinctions from extremely rare to very common  --------------------------

# loop that calculates for each taxa how many new additional insects are
# covered by successively lower range size cats
ext <- data.frame()
for (k in 1:6) {
  ext_all <- d %>%
    filter(plants_akt_Best_trans_num < k) %>%
    summarise(insect_species = n_distinct(insect_species)) %>%
    mutate(ext = unique((d %>% filter(plants_akt_Best_trans_num == k - 1))$plants_akt_Best_trans_cat),
           taxon = "All taxa",
           cover = 
             insect_species / 
             d %>% summarise(insect_species = n_distinct(insect_species)) %>% pull
           * 100
    ) 
  
  for (j in c("Apiformes", "Lepidoptera", "Symphyta", "Syrphidae")) {
    ext_taxa <-
      d %>%
      filter(plants_akt_Best_trans_num < k, taxon == j) %>%
      summarise(insect_species = n_distinct(insect_species)) %>%
      mutate(ext = unique((d %>% filter(plants_akt_Best_trans_num == k - 1))$plants_akt_Best_trans_cat),
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

# same matching as above
ext <- ext %>% 
  mutate(ext_real = case_when(
    ext == "very common" ~ "common lost",
    ext == "common" ~ "moderately common lost",
    ext == "moderately common" ~ "rare lost",
    ext == "rare" ~ "very rare lost",
    ext == "very rare" ~ "extremely rare lost",
    ext == "extremely rare" ~ "All survive",
  )) 

#View(ext)
# remove the 100% All survive cat
ext <- ext %>% filter(ext_real != "All survive")



# reverse extinctions: from very common to extremely rare ----------------
ext_rev <- data.frame()
for (k in 1:6) {
  ext_all <- d %>%
    filter(plants_akt_Best_trans_num >= k ) %>%
    summarise(insect_species = n_distinct(insect_species)) %>%
    mutate(ext = unique((d %>% filter(plants_akt_Best_trans_num == k - 1))$plants_akt_Best_trans_cat),
           taxon = "All taxa",
           cover = 
             insect_species / 
             d %>% summarise(insect_species = n_distinct(insect_species)) %>% pull
           * 100
    ) 
  
  for (j in c("Apiformes", "Lepidoptera", "Symphyta", "Syrphidae")) {
    ext_taxa <-
      d %>%
      filter(plants_akt_Best_trans_num >= k, taxon == j) %>%
      summarise(insect_species = n_distinct(insect_species)) %>%
      mutate(ext = unique((d %>% filter(plants_akt_Best_trans_num == k - 1))$plants_akt_Best_trans_cat),
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
    ext == "very common" ~ "very common lost",
    ext == "common" ~ "common lost",
    ext == "moderately common" ~ "moderately common lost",
    ext == "rare" ~ "rare lost",
    ext == "very rare" ~ "very rare lost",
    ext == "extremely rare" ~ "extremely rare lost",
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
      group_by(plants_akt_Best_trans_cat) %>% 
      summarise(n = n_distinct(plant_species_RL)) %>% 
      mutate(taxon = "All taxa"),
    d %>% 
      group_by(taxon, plants_akt_Best_trans_cat) %>% 
      summarise(n = n_distinct(plant_species_RL)) 
  ) %>% 
    mutate(ext_real = case_when(
      plants_akt_Best_trans_cat == "very common" ~ "very common lost",
      plants_akt_Best_trans_cat == "common" ~ "common lost",
      plants_akt_Best_trans_cat == "moderately common" ~ "moderately common lost",
      plants_akt_Best_trans_cat == "rare" ~ "rare lost",
      plants_akt_Best_trans_cat == "very rare" ~ "very rare lost",
      plants_akt_Best_trans_cat == "extremely rare" ~ "extremely rare lost",
    ))
)

# View(ext)
# fill NAs with 0
ext <- ext %>% replace(is.na(.), 0)

# order
ext <- ext %>% 
  mutate(ext_real = factor(ext_real,
                           levels = c(
                             "extremely rare lost",
                             "very rare lost",
                             "rare lost",
                             "moderately common lost",
                             "common lost",
                             "very common lost")),
         plants_akt_Best_trans_cat = factor(plants_akt_Best_trans_cat,
                                           levels = c(
                                             "extremely rare",
                                             "very rare",
                                             "rare",
                                             "moderately common",
                                             "common",
                                             "very common"))
  )

#View(ext)

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
transformation_ratio2 <- max(ext$n) / max(ext$cover)

(ggplot(data = ext) +
  facet_grid(~taxon_trivial) +
  geom_chicklet(aes(x = plants_akt_Best_trans_cat, y = n / transformation_ratio2),
                radius = unit(2, "pt"), alpha = .1,
                position = position_stack(reverse = FALSE)) +
  geom_point(aes(x = plants_akt_Best_trans_cat, y = cover, color = "Starting with extremely rare")) + 
  geom_line(aes(x = plants_akt_Best_trans_cat, y = cover), color = "magenta", group = 1, 
            arrow = arrow(type = "closed", length = unit(0.1, "inches"), ends = "last")) +
  geom_point(aes(x = plants_akt_Best_trans_cat, y = cover_rev, color = "Starting with very common")) + 
  geom_line(aes(x = plants_akt_Best_trans_cat, y = cover_rev), col = "green", group = 1,
            arrow = arrow(type = "closed", length = unit(0.1, "inches"), ends = "first")) +
  scale_color_manual(values = c("Starting with extremely rare" = "magenta", 
                                "Starting with very common" = "green")) +
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
    axis.text.x = element_text(angle = 45, hjust=1),
    legend.position = "bottom",
    legend.text = element_text(size = 14)
  ) +
  labs(title = "Sequential loss of host plants by range size", 
       x = "Range size of host plants",
       color = "") -> fig2b)



# multipanel --------------------------------------------------------------

fig2a /
  fig2b +
  plot_annotation(tag_levels = 'a')


showtext_opts(dpi=600)
ggsave("Figures/figure2-main.png",
       bg = "white",
       height = 8.5,
       width = 10.0,
       dpi = 600)
showtext_opts(dpi=96)
