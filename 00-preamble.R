
# preamble ----------------------------------------------------------------
library(readr)
library(tidyverse)
library(ggpmisc)
library(ggimage)
library(sysfonts)
library(showtext)
library(viridis)
library(patchwork)
library(ggchicklet)
library(ggpubr)
library(scales)
library(emmeans)
library(reshape2)
library(paletteer)
library(lme4)
library(lmerTest)
# add Arial Narrow for plotting
font_paths("C:/Windows/Fonts")
font_add("Arial Narrow", regular = "arialn.ttf", italic = "arialni.ttf", 
         bold = "arialnb.ttf")
showtext_auto()

