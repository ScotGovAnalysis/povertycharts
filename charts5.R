
# READ ME #####################################################################
# AUTHOR:            Maike Waldmann
# PURPOSE OF SCRIPT: Make poverty charts 5 
# CONTACTS:          contacts
# SOURCES:           sources
# NOTES:             notes
################################################################################ Make charts for poverty publication

# Charts5 - child poverty update one-pager (charts cp1, cp2, cp3)

# load data and functions

source("functions.R")
source("data_prep.R")

# get year levels for x axis ----

yearlevels <- formatyears(relpov) %>%
  select(year) %>%
  mutate(year = factor(year),
         year = fct_expand(year, "1920", "2021", "2122", "2223", 
                           "2324", "2425", "2526", "2627", 
                           "2728", "2829", "2930", "3031"))

yearlevels <- levels(yearlevels$year)

# get datasets ----

cp1data <- formatyears(relpov) %>%
  mutate(single = pct_ch_AHC,
         three = getcentmean(pct_ch_AHC),
         year = factor(year, levels = yearlevels)) %>%
  select(year, single, three) 

cp2data <- formatyears(abspov) %>%
  mutate(single = pct_ch_AHC,
         three = getcentmean(pct_ch_AHC),
         year = factor(year, levels = yearlevels)) %>%
  select(year, single, three) 

cp3data <- formatyears(matdep) %>%
  mutate(single = pct_ch_AHC,
         three = getcentmean(pct_ch_AHC),
         year = factor(year, levels = yearlevels),
         three = ifelse(year == "1011", NA, three)) %>%
  select(year, single, three) 

cp4data <- perspov %>%
  select(period, chAHC) %>%
  mutate(year = str_sub(period, -2L, -1L),
         year = str_c(lag(year), year),
         year = ifelse(is.na(year), "1314", year),
         year = factor(year, levels = yearlevels),
         single = chAHC/100,
         three = (single + lag(single) + lead(single))/3) %>%
  select(year, single, three, period)


# get charts ----

## Chart cp1 - Relpov ch ----

data <- cp1data
file <- "charts/cp1_relpovch_large.png"

interimtarget <- 0.18
finaltarget <- 0.10

ggplot(data, aes(x = year, 
                 y = three,
                 labels = percent(single, 1),
                 group = "all")) + 
  
  addtargetbars() +
  
  geom_line(size = 2,
            lineend = "round",
            colour = colours[1],
            show.legend = FALSE) +
  
  geom_point(aes(x = year, 
                 y = single),
             size = 1.5,
             shape = 21,
             colour = "grey20",
             fill = "grey20",
             alpha = 0.4) +
  
  adddatalabels() +
  
  annotate("text", x = 23.5, y = 0.5, 
           label = "Child Poverty\n (Scotland) Act",
           hjust = 1,
           size = 3,
           fontface = "bold",
           colour = "grey60") +
  
  annotate("text", x = 30, y = 0.45, 
           label = "Interim\n target",
           hjust = 1,
           size = 3,
           fontface = "bold",
           colour = "grey20") +
  
  annotate("text", x = 37, y = 0.4, 
           label = "Final\n target",
           hjust = 1,
           size = 3,
           fontface = "bold",
           colour = "grey20") +
  
  scale_x_discrete(drop = FALSE,
                   breaks = c("9495","1718", "2324", "3031"),
                   labels = c("1994/95", "2017/18", "23/24", "2030/31"),
                   expand = c(0.1, 0.1)) +
  
  addinterimtarget(y = interimtarget) +
  addfinaltarget(y = finaltarget) +
  addyaxis() +
  addsource()

ggsave(file, width = 9, height = 6.5, units = "cm", dpi = 300)

## Chart cp2 - Abspov ch ----

data <- cp2data
file <- "charts/cp2_abspovch_large.png"

interimtarget <- 0.14
finaltarget <- 0.05

ggplot(data, aes(x = year, 
                 y = three,
                 group = "all")) + 
  
  addtargetbars() +
  
  geom_line(size = 2,
            lineend = "round",
            colour = colours[1],
            show.legend = FALSE) +
  
  geom_point(aes(x = year, 
                 y = single),
             size = 1.5,
             shape = 21,
             colour = "grey20",
             fill = "grey20",
             alpha = 0.4) +
  
  adddatalabels() +
  
  scale_x_discrete(drop = FALSE,
                   breaks = c("9495","1718", "2324", "3031"),
                   labels = c("1994/95", "2017/18", "23/24", "2030/31"),
                   expand = c(0.1, 0.1)) +
  
  addinterimtarget(y = interimtarget) +
  addfinaltarget(y = finaltarget) +
  addyaxis() +
  addsource()

ggsave(file, width = 9, height = 6.5, units = "cm", dpi = 300)

## Chart cp3 - Matdep ch ----

data <- cp3data
file <- "charts/cp3_matdepch_large.png"

interimtarget <- 0.08
finaltarget <- 0.05

ggplot(data, aes(x = year, 
                 y = three,
                 group = "all")) + 
  
  geom_vline(aes(xintercept = 17),
             colour = breakcol,
             alpha = 0.9) +
  
  addtargetbars() +
  
  annotate("text", x = 16.7, y = 0.02, 
           label = "Methodology change",
           hjust = 1,
           size = 3,
           fontface = "bold",
           colour = "#c47c58") +
  
  geom_line(size = 2,
            lineend = "round",
            colour = colours[1],
            show.legend = FALSE) +
  
  geom_point(aes(x = year, 
                 y = single),
             size = 1.5,
             shape = 21,
             colour = "grey20",
             fill = "grey20",
             alpha = 0.4) +
  
  adddatalabels() +
  
  scale_x_discrete(drop = FALSE,
                   breaks = c("9495","1011","1718", "2324", "3031"),
                   labels = c("1994/95","2010/11", "17/18", "23/24", "2030/31"),
                   expand = c(0.1, 0.1)) +
  
  addinterimtarget(y = interimtarget) +
  addfinaltarget(y = finaltarget) +
  addyaxis() +
  addsource()

ggsave(file, width = 9, height = 6.5, units = "cm", dpi = 300)

## Chart cp4 - Pers pov ----

data <- cp4data
file <- "charts/cp4_perspov_large.png"

interimtarget <- 0.08
finaltarget <- 0.05

ggplot(data, aes(x = year, 
                 y = three,
                 group = "all")) + 
  
  addtargetbars() +
  
  geom_line(size = 2,
            lineend = "round",
            colour = colours[1],
            show.legend = FALSE) +
  
  geom_point(aes(x = year,
                 y = single),
             size = 1.5,
             shape = 21,
             colour = "grey20",
             fill = "grey20",
             alpha = 0.4) +
  
  adddatalabels() +
  
  scale_x_discrete(drop = FALSE,
                   breaks = c("9495","1718", "2324", "3031"),
                   labels = c("1994/95", "2017/18", "23/24", "2030/31"),
                   expand = c(0.1, 0.1)) +
  
  addinterimtarget(y = interimtarget) +
  addfinaltarget(y = finaltarget) +
  addyaxis() +
  labs(caption = "Source: Understanding Society Survey")

ggsave(file, width = 9, height = 6.5, units = "cm", dpi = 300)

