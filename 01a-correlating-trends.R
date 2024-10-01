source("00-preamble.R")

# load data ---------------------------------------------------------------
d <- read_csv("RL_inter_plant_insect-JUNE2024.csv")
#View(d)


# short term trends -------------------------------------------------------

# transform trends into numeric categories

# insects
unique(d$kurz_Best)
d <- d %>%
  mutate(insects_kurz_Best_trans = case_when(
    kurz_Best == "(v)" ~ "v",
    kurz_Best == "vv" ~ "v",
    kurz_Best == "vvv" ~ "v",
    kurz_Best == "?" ~ NA,
    .default = as.character(kurz_Best)
  ))

# plants
unique(d$kurz_Best_p)
d <- d %>%
  mutate(plants_kurz_Best_trans = case_when(
    kurz_Best_p == "=" ~ 0,
    kurz_Best_p == "(v)" ~ -1,
    kurz_Best_p == "v" ~ -1,
    kurz_Best_p == "vv" ~ -2,
    kurz_Best_p == "vvv" ~ -3,
    kurz_Best_p == "^" ~ 1,
    kurz_Best_p == "?" ~ NA,
    TRUE ~ NA_real_ # Default case if none of the above conditions are met
  ))

# short term summarize data -----------------------------------------------
d %>% nrow
# how many interactions remain when NAs are excluded
d %>% select(taxon_trivial,
             insect_species,
             insects_kurz_Best_trans,
             plants_kurz_Best_trans) %>%
  na.omit %>% nrow


# need to calculate mean trend per insect species, per higher taxon
shortterm <- d %>%
  group_by(taxon_trivial, insect_species) %>%
  summarize(
    insect_species = first(insect_species),
    insect_shortterm = first(insects_kurz_Best_trans),
    plant_meanshortterm = mean(plants_kurz_Best_trans, na.rm = T)
  ) %>%
  filter(!is.na(insect_shortterm)) %>% 
  filter(!is.na( plant_meanshortterm))
 

# how many insects
shortterm %>% group_by(taxon_trivial) %>% summarize(n = length(insect_species))

# add a column for all taxa combined
shortterm <- bind_rows(
  shortterm %>% mutate(taxon_trivial = "All taxa"),
  shortterm
)

# short-term model --------------------------------------------------------

shortterm$insect_shortterm <- 
  factor(shortterm$insect_shortterm, 
         levels = c( "v", "=", "^"))

mod_st <- lm(plant_meanshortterm ~ insect_shortterm*taxon_trivial,
              shortterm)
# plot(mod_st)
# single models for reporting anova results in text
anova(lm(plant_meanshortterm ~ insect_shortterm,
   shortterm %>% filter(taxon_trivial == "Bees")))
anova(lm(plant_meanshortterm ~ insect_shortterm,
         shortterm %>% filter(taxon_trivial == "Butterflies & moths")))
anova(lm(plant_meanshortterm ~ insect_shortterm,
         shortterm %>% filter(taxon_trivial == "Sawflies")))
anova(lm(plant_meanshortterm ~ insect_shortterm,
         shortterm %>% filter(taxon_trivial == "Hoverflies")))

# sample sizes for insects
shortterm %>% group_by(taxon_trivial) %>% summarise(n = n_distinct(insect_species))

# visualize model ---------------------------------------------------------

(emmeans(mod_st, ~insect_shortterm|taxon_trivial) %>% as_tibble() %>% 
   mutate(taxon_trivial = factor(taxon_trivial,
                                 levels = c("All taxa", 
                                            "Bees", 
                                            "Butterflies & moths", 
                                            "Sawflies", 
                                            "Hoverflies"))) %>% 
  ggplot(aes(
    y = emmean, 
    x = insect_shortterm,
    ymin = lower.CL,
    ymax = upper.CL
  ))+
  facet_wrap(.~taxon_trivial, nrow = 1) +
  geom_point(col = "#0e52ff", pch = 21, size = 2) +
  geom_errorbar(width = 0.2, col = "magenta") +
  geom_path(group = 1, col = "#0e52ff") +
  theme_minimal(base_family = "Arial Narrow") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(face = "italic", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"
  ) +
  labs(
    y = "Avg. trend plants",
    x = "Trend insects",
    title = "Short-term trends (10 - 15 yrs)"
  ) -> fig1a)



# long-term trends --------------------------------------------------------

# transform trends into numeric categories

# insects
unique(d$lang_Best)
d <- d %>%
  mutate(insects_lang_Best_trans = case_when(
    lang_Best == "(<)" ~ "v",
    lang_Best == "<" ~ "v",
    lang_Best == "<<" ~ "v",
    lang_Best == "<<<" ~ "v",
    lang_Best == ">" ~ "^",
    lang_Best == "?" ~ NA,
    .default = as.character(lang_Best)
  ))

# plants
unique(d$lang_Best_p)
d <- d %>%
  mutate(plants_lang_Best_trans = case_when(
    lang_Best_p == "=" ~ 0,
    lang_Best_p == "(<)" ~ -1,
    lang_Best_p == "<" ~ -1,
    lang_Best_p == "<<" ~ -2,
    lang_Best_p == "<<<" ~ -3,
    lang_Best_p == ">" ~ 1,
    lang_Best_p == "?" ~ NA,
    TRUE ~ NA_real_ # Default case if none of the above conditions are met
  ))


# long-term summarize data -----------------------------------------------
d %>% nrow
# how many interactions remain when NAs are excluded
d %>% select(taxon_trivial,
             insect_species,
             insects_lang_Best_trans,
             plants_lang_Best_trans) %>%
  na.omit %>% nrow

# need to calculate mean trend per insect species, per higher taxon
longterm <- d %>%
  group_by(taxon_trivial, insect_species) %>%
  summarize(
    insect_species = first(insect_species),
    insect_longterm = first(insects_lang_Best_trans),
    plant_meanlongterm = mean(plants_lang_Best_trans, na.rm = T)
  ) %>%
  filter(!is.na(insect_longterm)) %>% 
  filter(!is.na(plant_meanlongterm))

# how many insects
longterm %>% group_by(taxon_trivial) %>% summarize(n = length(insect_species))

# add a column for all taxa combined
longterm <- bind_rows(
  longterm %>% mutate(taxon_trivial = "All taxa"),
  longterm
)


# long-term model --------------------------------------------------------

longterm$insect_longterm <- 
  factor(longterm$insect_longterm, 
         levels = c( "v", "=", "^"))

mod_lt <- lm(plant_meanlongterm ~ insect_longterm*taxon_trivial,
             longterm)
anova(mod_lt)
summary(mod_lt)
#plot(mod_t)

# for in text anova results per taxa
anova(lm(plant_meanlongterm ~ insect_longterm,
         longterm %>% filter(taxon_trivial == "Bees")))
anova(lm(plant_meanlongterm ~ insect_longterm,
         longterm %>% filter(taxon_trivial == "Butterflies & moths")))
anova(lm(plant_meanlongterm ~ insect_longterm,
         longterm %>% filter(taxon_trivial == "Sawflies")))
anova(lm(plant_meanlongterm ~ insect_longterm,
         longterm %>% filter(taxon_trivial == "Hoverflies")))

# sample sizes for insects
longterm %>% group_by(taxon_trivial) %>% summarise(n = n_distinct(insect_species))


# visualize model ---------------------------------------------------------

(emmeans(mod_lt, ~insect_longterm|taxon_trivial) %>% as_tibble() %>% 
   mutate(taxon_trivial = factor(taxon_trivial,
                                 levels = c("All taxa", 
                                            "Bees", 
                                            "Butterflies & moths", 
                                            "Sawflies", 
                                            "Hoverflies"))) %>% 
  ggplot(aes(
    y = emmean, 
    x = insect_longterm,
    ymin = lower.CL,
    ymax = upper.CL
  ))+
  facet_wrap(.~taxon_trivial, nrow = 1) +
  geom_point(col = "#0e52ff", pch = 21, size = 2) +
  geom_errorbar(width = 0.2, col = "magenta") +
  geom_path(group = 1, col = "#0e52ff") +
  theme_minimal(base_family = "Arial Narrow") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(face = "italic", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"
  ) +
  labs(
    y = "Avg. trend plants",
    x = "Trend insects",
    title = "Long-term trends (50 - 150 yrs)"
  ) -> fig1b)



# red list cat most recent ------------------------------------------------

# transform rl cats into numeric

# insects
unique(d$RL_Kat.)
d <- d %>%
  mutate(insects_RL_Kat._trans = case_when(
    RL_Kat. == "*" ~ "LC",
    RL_Kat. == "V" ~ "NT",
    RL_Kat. == "R" ~ "NT",
    RL_Kat. == "3" ~ "VU",
    RL_Kat. == "G" ~ "VU",
    RL_Kat. == "2" ~ "EN",
    RL_Kat. == "1" ~ "CR",
    RL_Kat. == "0" ~ NA,
    RL_Kat. == "D" ~ NA,
    RL_Kat. == "♦" ~ NA,
    RL_Kat. == "nb" ~ NA,
    .default = as.character(kurz_Best)
  ))

# plants
unique(d$RL_Kat._p)
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


# rlcat summarize data -----------------------------------------------
d %>% nrow
# how many interactions remain when NAs are excluded
d %>% select(taxon_trivial,
             insect_species,
             insects_RL_Kat._trans,
             plants_RL_Kat._trans) %>%
  na.omit %>% nrow

# need to calculate mean trend per insect species, per higher taxon
rlcat <- d %>%
  select(taxon_trivial,
         insect_species,
         insects_RL_Kat._trans,
         plants_RL_Kat._trans) %>%
  na.omit %>%
  group_by(taxon_trivial, insect_species) %>%
  summarize(
    insect_species = first(insect_species),
    insect_rlcat = first(insects_RL_Kat._trans),
    plant_meanrlcat = mean(plants_RL_Kat._trans, na.rm = T)
  ) %>%
  filter(!is.na(insect_rlcat)) %>% 
  filter(!is.na(plant_meanrlcat))

# how many insects
rlcat %>% group_by(taxon_trivial) %>% summarize(n = length(insect_species))

# add a column for all taxa combined
rlcat <- bind_rows(
  rlcat %>% mutate(taxon_trivial = "All taxa"),
  rlcat
)


# model red list ----------------------------------------------------------
rlcat$insect_rlcat <- factor(rlcat$insect_rlcat, 
                             levels = c("LC", "NT", "VU", "EN", "CR"))

mod_rl <- lm( plant_meanrlcat ~ insect_rlcat*taxon_trivial,
              rlcat)
#plot(mod_lt)
summary(mod_rl)

# for in text anova results per taxa
anova(lm(plant_meanrlcat ~ insect_rlcat,
         rlcat %>% filter(taxon_trivial == "All taxa")))
anova(lm(plant_meanrlcat ~ insect_rlcat,
         rlcat %>% filter(taxon_trivial == "Bees")))
anova(lm(plant_meanrlcat ~ insect_rlcat,
         rlcat %>% filter(taxon_trivial == "Butterflies & moths")))
anova(lm(plant_meanrlcat ~ insect_rlcat,
         rlcat %>% filter(taxon_trivial == "Sawflies")))
anova(lm(plant_meanrlcat ~ insect_rlcat,
         rlcat %>% filter(taxon_trivial == "Hoverflies")))


# sample sizes for insects
rlcat %>% group_by(taxon_trivial) %>% summarise(n = n_distinct(insect_species))


# visualize model ---------------------------------------------------------

(emmeans(mod_rl, ~insect_rlcat|taxon_trivial) %>% as_tibble() %>% 
   mutate(taxon_trivial = factor(taxon_trivial,
                                 levels = c("All taxa", 
                                            "Bees", 
                                            "Butterflies & moths", 
                                            "Sawflies", 
                                            "Hoverflies"))) %>% 
   ggplot(aes(
     y = emmean, 
     x = insect_rlcat,
     ymin = lower.CL,
     ymax = upper.CL
   ))+
   facet_wrap(.~taxon_trivial, nrow = 1) +
   geom_point(col = "#0e52ff", pch = 21, size = 2) +
   geom_errorbar(width = 0.2, col = "magenta") +
   geom_path(group = 1, col = "#0e52ff") +
   theme_minimal(base_family = "Arial Narrow") +
   theme(
     plot.title = element_text(size = 16, face = "bold"),
     strip.text = element_text(face = "italic", size = 14),
     axis.text = element_text(size = 12),
     axis.title = element_text(size = 14),
     legend.position = "none"
   ) +
   labs(
     y = "Avg. threat status plants",
     x = "Category insects",
     title = "Red List categories"
   ) -> fig1c)



# multipanel --------------------------------------------------------------

fig1a /
  fig1b /
#  fig1c +
  plot_layout(guides = "collect") & 
  plot_annotation(tag_levels = 'a')

showtext_opts(dpi=600)
ggsave("Figures/figure1-main_rev.pdf",
       bg = "white",
       height = 6.0,
       width = 8.0,
       dpi = 600)
showtext_opts(dpi=96)



# supplementary figures ---------------------------------------------------

# layering 
`-.gg` <- function(plot, layer) {
  if (missing(layer)) {
    stop("Cannot use `-.gg()` with a single argument. Did you accidentally put - on a new line?")
  }
  if (!is.ggplot(plot)) {
    stop('Need a plot on the left side')
  }
  plot$layers = c(layer, plot$layers)
  plot
}

# raw data plots
fig1a -   
  geom_jitter(data = shortterm %>% 
                mutate(taxon_trivial = factor(taxon_trivial,
                                              levels = c("All taxa", 
                                                         "Bees", 
                                                         "Butterflies & moths", 
                                                         "Sawflies", 
                                                         "Hoverflies"))) , 
              aes(x = insect_shortterm, y = plant_meanshortterm, group = taxon_trivial), 
              inherit.aes =F, 
              width = 0.1, 
              height = 0.1, 
              size = 2, 
              alpha = 0.2,
              col = "grey70") -> fig1a_supp

fig1b -   
  geom_jitter(data = longterm %>%    
                mutate(taxon_trivial = factor(taxon_trivial,
                                              levels = c("All taxa", 
                                                         "Bees", 
                                                         "Butterflies & moths", 
                                                         "Sawflies", 
                                                         "Hoverflies"))) , 
              aes(x = insect_longterm, y = plant_meanlongterm, group = taxon_trivial), 
              inherit.aes =F, 
              width = 0.1, 
              height = 0.1, 
              size = 2, 
              alpha = 0.2,
              col = "grey70") -> fig1b_supp

fig1c -   
  geom_jitter(data = rlcat %>% 
                mutate(taxon_trivial = factor(taxon_trivial,
                                              levels = c("All taxa", 
                                                         "Bees", 
                                                         "Butterflies & moths", 
                                                         "Sawflies", 
                                                         "Hoverflies"))) , 
              aes(x = insect_rlcat, y = plant_meanrlcat, group = taxon_trivial), 
              inherit.aes =F, 
              width = 0.1, 
              height = 0.1, 
              size = 2, 
              alpha = 0.2,
              col = "grey70") -> fig1c_supp

# multi
fig1a_supp /
  fig1b_supp /
#  fig1c_supp +
  plot_layout(guides = "collect") & 
  plot_annotation(tag_levels = 'A')

showtext_opts(dpi=600)
ggsave("Figures/figure1-supp-rev.svg",
       bg = "white",
       height = 6.0,
       width = 8.0,
       dpi = 600)
showtext_opts(dpi=96)



# pairwise comparisons
# short term
confint(pairs(emmeans(mod_st, ~insect_shortterm|taxon_trivial))) %>% as_tibble() %>% 
  mutate(contrast = factor(contrast,
                           levels = c( "= - ^",
                                       "v - ^",
                                       "v - ="))) %>% # order comps
  mutate(taxon_trivial = factor(taxon_trivial,
                                levels = c("All taxa", 
                                           "Bees", 
                                           "Butterflies & moths", 
                                           "Sawflies", 
                                           "Hoverflies"))) %>% 
  ggplot(aes(
    x = estimate, 
    y = contrast,
    xmin = lower.CL,
    xmax = upper.CL
  ))+
  facet_wrap(.~taxon_trivial, scales = "free_x", nrow = 1) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point(col = "#0e52ff", pch = 21, size = 2) +
  geom_errorbar(width = 0.1, col = "magenta") +
  theme_minimal(base_family = "Arial Narrow") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(face = "italic", size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45),
    axis.title = element_text(size = 14),
    legend.position = "none"
  ) +
  labs(
    y = "Comparison",
    x = "Mean difference",
    title = "Short-term trends (10 - 15 yrs)"
  ) -> fig1a_supp_pairwise


# long-term
confint(pairs(emmeans(mod_lt, ~insect_longterm|taxon_trivial))) %>% as_tibble() %>% 
  mutate(contrast = factor(contrast,
                           levels = c( "= - ^",
                                       "v - ^",
                                       "v - ="))) %>% # order comps
  mutate(taxon_trivial = factor(taxon_trivial,
                                levels = c("All taxa", 
                                           "Bees", 
                                           "Butterflies & moths", 
                                           "Sawflies", 
                                           "Hoverflies"))) %>% 
  ggplot(aes(
    x = estimate, 
    y = contrast,
    xmin = lower.CL,
    xmax = upper.CL
  ))+
  facet_wrap(.~taxon_trivial, scales = "free_x", nrow = 1) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point(col = "#0e52ff", pch = 21, size = 2) +
  geom_errorbar(width = 0.1, col = "magenta") +
  theme_minimal(base_family = "Arial Narrow") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(face = "italic", size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45),
    axis.title = element_text(size = 14),
    legend.position = "none"
  ) +
  labs(
    y = "Comparison",
    x = "Mean difference",
    title = "Long-term trends (50 - 150 yrs)"
  ) -> fig1b_supp_pairwise


# red list threat level
confint(pairs(emmeans(mod_rl, ~insect_rlcat|taxon_trivial), adjust = "none")) %>% as_tibble() %>% 
  filter(grepl("LC", contrast)) %>% # leave in all LC comparisons
  mutate(contrast = factor(contrast,
                                levels = c("LC - NT", 
                                           "LC - VU", 
                                           "LC - EN", 
                                           "LC - CR"))) %>% # order comps
  mutate(taxon_trivial = factor(taxon_trivial,
                                levels = c("All taxa", 
                                           "Bees", 
                                           "Butterflies & moths", 
                                           "Sawflies", 
                                           "Hoverflies"))) %>% 
  ggplot(aes(
    x = estimate, 
    y = contrast,
    xmin = lower.CL,
    xmax = upper.CL
  ))+
  facet_wrap(.~taxon_trivial, scales = "free_x", nrow = 1) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point(col = "#0e52ff", pch = 21, size = 2) +
  geom_errorbar(width = 0.1, col = "magenta") +
  theme_minimal(base_family = "Arial Narrow") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(face = "italic", size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45),
    axis.title = element_text(size = 14),
    legend.position = "none"
  ) +
  labs(
    y = "Comparison",
    x = "Mean difference",
    title = "Red List categories"
  ) -> fig1c_supp_pairwise



# multi
fig1a_supp_pairwise /
  fig1b_supp_pairwise /
#  fig1c_supp_pairwise +
  plot_layout(guides = "collect") & 
  plot_annotation(tag_levels = 'A')

showtext_opts(dpi=600)
ggsave("Figures/figure1-supp-pairwise-rev.svg",
       bg = "white",
       height = 6.0,
       width = 10.0,
       dpi = 600)
showtext_opts(dpi=96)
