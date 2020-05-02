# Setup for new IHME models

library(tidyverse)
theme_set(theme_bw() + theme(panel.grid=element_blank()))
source("fn.R")


# recompile ihme.df
refresh_IHME_csv()

