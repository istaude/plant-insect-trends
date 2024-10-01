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

# short term model --------------------------------------------------------

d %>% nrow
# how many interactions remain when NAs are excluded
d %>% select(taxon_trivial,
             insect_species,
             insects_kurz_Best_trans,
             plants_kurz_Best_trans) %>%
  na.omit %>% nrow

d$insect_shortterm <- 
  factor(d$insects_kurz_Best_trans, 
         levels = c( "v", "=", "^"))

# per insect taxon
mod_st <- lmer(plants_kurz_Best_trans ~ 
                 insect_shortterm * taxon_trivial + 
                 (1|insect_species), 
               data = d)

# single models for reporting anova results in text
anova(lmer(plants_kurz_Best_trans ~ insect_shortterm + (1|insect_species),
         d %>% filter(taxon_trivial == "Bees")))
anova(lmer(plants_kurz_Best_trans ~ insect_shortterm + (1|insect_species),
           d %>% filter(taxon_trivial == "Butterflies & moths")))
anova(lmer(plants_kurz_Best_trans ~ insect_shortterm + (1|insect_species),
           d %>% filter(taxon_trivial == "Sawflies")))
anova(lmer(plants_kurz_Best_trans ~ insect_shortterm + (1|insect_species),
           d %>% filter(taxon_trivial == "Hoverflies")))


# viz
(
  emmeans(mod_st, ~insect_shortterm|taxon_trivial) %>% 
    as_tibble() %>% 
    mutate(taxon_trivial = factor(taxon_trivial,
                                  levels = c("All taxa", 
                                             "Bees", 
                                             "Butterflies & moths", 
                                             "Sawflies", 
                                             "Hoverflies"))) %>% 
    ggplot(aes(
      y = emmean, 
      x = insect_shortterm,
      ymin = asymp.LCL,
      ymax = asymp.UCL
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
      y = "Trend plants",
      x = "Trend insects",
      title = "Short-term trends (10 - 15 yrs)"
    ) -> fig2a_supp
)


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


# long-term model --------------------------------------------------------

d %>% nrow
# how many interactions remain when NAs are excluded
d %>% select(taxon_trivial,
             insect_species,
             insects_lang_Best_trans,
             plants_lang_Best_trans) %>%
  na.omit %>% nrow


d$insect_longterm <- 
  factor(d$insects_lang_Best_trans, 
         levels = c( "v", "=", "^"))


# per insect taxon
mod_lt <- lmer(plants_lang_Best_trans ~ 
                 insect_longterm * taxon_trivial + 
                 (1|insect_species), 
               data = d)

# single models for reporting anova results in text
anova(lmer(plants_lang_Best_trans ~ insect_longterm + (1|insect_species),
           d %>% filter(taxon_trivial == "Bees")))
anova(lmer(plants_lang_Best_trans ~ insect_longterm + (1|insect_species),
           d %>% filter(taxon_trivial == "Butterflies & moths")))
anova(lmer(plants_lang_Best_trans ~ insect_longterm + (1|insect_species),
           d %>% filter(taxon_trivial == "Sawflies")))
anova(lmer(plants_lang_Best_trans ~ insect_longterm + (1|insect_species),
           d %>% filter(taxon_trivial == "Hoverflies")))


# viz
(
  emmeans(mod_lt, ~insect_longterm|taxon_trivial) %>% 
    as_tibble() %>% 
    mutate(taxon_trivial = factor(taxon_trivial,
                                  levels = c("All taxa", 
                                             "Bees", 
                                             "Butterflies & moths", 
                                             "Sawflies", 
                                             "Hoverflies"))) %>% 
    ggplot(aes(
      y = emmean, 
      x = insect_longterm,
      ymin = asymp.LCL,
      ymax = asymp.UCL
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
      y = "Trend plants",
      x = "Trend insects",
      title = "Long-term trends (50 - 150 yrs)"
    ) -> fig2b_supp
)



# multi
fig2a_supp /
  fig2b_supp /
  #  fig1c_supp +
  plot_layout(guides = "collect") & 
  plot_annotation(tag_levels = 'A')

showtext_opts(dpi=600)
ggsave("Figures/figure1-hierarchical-supp-rev.svg",
       bg = "white",
       height = 6.0,
       width = 8.0,
       dpi = 600)
showtext_opts(dpi=96)
