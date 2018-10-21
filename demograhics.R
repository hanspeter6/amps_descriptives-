# packages
library(tidyverse)

##read in files:

# set_full
set02 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/set02.rds")
set05 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/set05.rds")
set08 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/set08.rds")
set10 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/set10.rds")
set12 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/set12.rds")
set14 <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/set14.rds")

# set_all_simple
set02_simple <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/set02_simple.rds")
set05_simple <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/set05_simple.rds")
set08_simple <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/set08_simple.rds")
set10_simple <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/set10_simple.rds")
set12_simple <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/set12_simple.rds")
set14_simple <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/set14_simple.rds")

# set_simple_print
set02_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/set02_simple_print.rds")
# set05_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/set05_simple_print.rds")
set08_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/set08_simple_print.rds")
set10_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/set10_simple_print.rds")
set12_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/set12_simple_print.rds")
set14_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/set14_simple_print.rds")

# for demographics doesn't matter what set I use. Will use the full set

# create single pooled set of selected (chosen) demographics
add_year <- function(set, year) {
  set %>%
    mutate(year = year) %>%
    dplyr::select(qn, pwgt, year, age, sex, edu, hh_inc, race, lsm)
}

# bind into a single frame:
set_full <- rbind.data.frame(add_year(set02, 2002),
                             add_year(set05, 2005),
                             add_year(set08, 2008),
                             add_year(set10, 2010),
                             add_year(set12, 2012),
                             add_year(set14, 2014))

# name factor levels
# set factor labels (NB double check levels)
set_full$age <- factor(set_full$age, labels = c("15-24","25-44", "45-54","55+"), ordered = FALSE)
set_full$race <- factor(set_full$race,labels = c("black", "coloured", "indian", "white"), ordered = FALSE)
set_full$edu <- factor(set_full$edu, labels = c("<matric", "matric",">matric" ) ,ordered = FALSE)
set_full$lsm <- factor(set_full$lsm, labels = c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"), ordered = FALSE)
set_full$sex <- factor(set_full$sex, labels = c("male", "female"), ordered = FALSE)
set_full$hh_inc <- factor(set_full$hh_inc, labels = c("<R5000","R5000-R10999","R11000-R19999","R20000+"), ordered = FALSE)
set_full$year <- factor(set_full$year, ordered = FALSE)


# consider proportions of levels by category by year
# considering cluster by year...

plot_demogs <- function(set, category, palette = c("Accent", "Spectral", "Paired", "Pastel2", "Set2","Set3"), title = category) {
  by_year <- set %>%
    group_by(year) %>%
    count_(category) %>%
    mutate(total = sum(n))
  
  label <- paste0(round(100*(by_year$n/by_year$total)),"%")
  
  ggplot(by_year) +
    aes_string(x = "year", y = "n", fill = category, label = "label" ) +
    geom_bar(stat = 'identity') +
    geom_text(position = position_stack(vjust = 0.5), size = 4) +
    labs(y = "count", title = title) +
    scale_fill_brewer(palette = palette) +
    guides(fill = guide_legend(title = NULL))
}

jpeg('demog_sex.jpeg', quality = 100, type = "cairo")
plot_demogs(set_full, category = "sex", palette = "Accent", title = "Gender")
dev.off()
jpeg('demog_age.jpeg', quality = 100, type = "cairo")
plot_demogs(set_full, category = "age", palette = "Spectral", title = "Age Groups")
dev.off()
jpeg('demog_race.jpeg', quality = 100, type = "cairo")
plot_demogs(set_full, category = "race", palette = "Paired", title = "Population Groups")
dev.off()
jpeg('demog_edu.jpeg', quality = 100, type = "cairo")
plot_demogs(set_full, category = "edu", palette = "Pastel2", title = "Education")
dev.off()
jpeg('demog_hh_inc.jpeg', quality = 100, type = "cairo")
plot_demogs(set_full, category = "hh_inc", palette = "Set2", title = "Household Income")
dev.off()
jpeg('demog_lsm.jpeg', quality = 100, type = "cairo")
plot_demogs(set_full, category = "lsm", palette = "Set3", title = "Living Standards Measure")
dev.off()

