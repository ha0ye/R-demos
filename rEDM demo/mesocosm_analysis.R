library(tidyverse)
library(cowplot)
library(rEDM)

load("rEDM demo/interpolated_abundances_no_nuts.Rdata")
all.data %>%
    group_by(Variable) %>%
    mutate(abundance = sqrt(sqrt(y / max(y, na.rm = TRUE)))) %>%
    ungroup %>%
    {.} -> all.data

abundance_plot <- ggplot(all.data, aes(x = Day, y = abundance)) + 
    geom_line() + 
    facet_wrap(~Variable, scales = "free_y") + 
    theme_cowplot()

print(abundance_plot)

all.data %>%
    select(Day, Variable, abundance) %>%
    spread(Variable, abundance) %>%
    {.} -> abundances

n <- NROW(abundances)
lib <- c(1, floor(2*n/3))
pred <- c(lib[2] + 1, n)
simplex(abundances$Rotifers, lib = lib, pred = lib)
simplex(abundances$Nanophytoplankton, lib = lib, pred = lib)

ccm(abundances, E = 6, tp = 0,
    lib_sizes = NROW(abundances), random_libs = FALSE, 
    lib_column = "Rotifers", target_column = "Nanophytoplankton",)

simplex(abundances$Nanophytoplankton, lib = lib, pred = pred, E = 2)


