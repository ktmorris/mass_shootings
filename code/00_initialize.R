### these are all the packages we use throughout the project (there may be extra here)
library(kableExtra)
library(readxl)
library(parallel)
library(doParallel)
library(rethnicity)
library(SearchTrees)
library(gstat)
library(rgeos)
library(rdrobust)
library(cowplot)
library(scales)
library(stargazer)
library(did)
library(plm)
library(raster)
library(ebal)
library(miceadds)
library(gtrendsR)
library(vroom)
library(ggeffects)
library(tidycensus)
library(fixest)
library(splitstackshape)
library(spdep)
library(AER)
library(maptools)
library(tigris)
library(rgdal)
library(modelsummary)
library(spatialreg)
library(sqldf)
library(lubridate)
library(rgdal)
library(tidyverse)
library(data.table)

### this is a quick function that allows us to clear memory without losing things we
### want to keep from one script to the next

options("modelsummary_format_numeric_latex" = "plain")

theme_bc <- function(base_size = 11, base_family = "BentonSans",
                     legend.position = "right", face = "plain", ...) {
  library(extrafont)
  half_line <- base_size/2
  theme_bw(base_family = base_family) %+replace%
    theme(plot.caption = element_text(size = rel(0.8), hjust = 0,
                                      family = base_family,
                                      vjust = 1, margin = margin(t = half_line)),
          plot.title = element_text(size = rel(1.2), hjust = 0.5,
                                    vjust = 1, margin = margin(b = half_line),
                                    family = base_family),
          plot.subtitle = element_text(hjust = 0.5, vjust = 1, margin = margin(b = half_line)),
          legend.position = legend.position,
          text = element_text(family = base_family, face = face,
                              colour = "black", size = base_size, lineheight = 0.9,
                              hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),
                              debug = FALSE),
          ...)
}

save <- c("db", "cleanup", "theme_bc", "save", "weighted.ttest.ci")

cleanup <- function(...){
  save2 <- c(save, ...)
  rm(list=ls(envir = .GlobalEnv)[! ls(envir = .GlobalEnv) %in% save2], envir = .GlobalEnv)
  gc()
}
