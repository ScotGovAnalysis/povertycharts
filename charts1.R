
# READ ME #####################################################################
# AUTHOR:            Maike Waldmann
# PURPOSE OF SCRIPT: Make poverty charts 1
# CONTACTS:          contacts
# SOURCES:           sources
# NOTES:             notes
################################################################################ Make charts for poverty publication

# Charts1 - general poverty (charts 1-12)


# load data and functions

source("functions.R")
source("data_prep.R")

# Chart 1 - Relpov pp ----

data <- relpovpp
file <- "charts/01_relpovpp.png"

plotdata(data) +
  
  scale_y_continuous(limits = c(0, 0.45)) +
  addnames(AHC_ypos = 0.28, BHC_ypos = 0.16) +
  
  addscales() +
  addxlabels() +
  addlabels() +
  addsource()

saveplot()

# Chart 2 - Abspov pp ----

data <- abspovpp
file <- "charts/02_abspovpp.png"

plotdata(data) +
  
  scale_y_continuous(limits = c(0, 0.45)) +
  addnames(AHC_ypos = 0.41, BHC_ypos = 0.18) +
  
  addscales() +
  addxlabels() +
  addlabels() +
  addsource()

saveplot()


# Chart 3 - Relpov ch ----

data <- relpovch
file <- "charts/03_relpovch.png"

plotdata(data) +
  
  scale_y_continuous(limits = c(0, 0.45)) +
  addnames(AHC_ypos = 0.36, BHC_ypos = 0.21) +
  
  addscales() +
  addxlabels() +
  addlabels() +
  addsource()

saveplot()

# Chart 4 - Abspov ch ----

data <- abspovch
file <- "charts/04_abspovch.png"

plotdata(data, up = 0.1) +
  
  scale_y_continuous(limits = c(0.1, 0.55)) +
  addnames(AHC_ypos = 0.53, BHC_ypos = 0.23) +
  
  addscales() +
  addxlabels() +
  addlabels() +
  addsource()

saveplot()


# Chart 5 - Matdep ch ----

data <- matdepch
file <- "charts/05_matdepch.png"

ggplot(data, aes(x = years, 
                 y = value, 
                 group = key, 
                 colour = key,
                 linetype = key,
                 label = percent(value, 1))) + 

  geom_line(aes(size = key),
            show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 0.45)) +
  addnames(AHC_ypos = 0.2, BHC_ypos = 0.12, xpos = 2) +
  
  geom_vline(aes(xintercept = 16),
             colour = breakcol,
           alpha = 0.9) +

  annotate("text",
           label = "Methodology \nchange",
           size = 3,
           colour = "#c47c58",
           x = 16.2, 
           y = 0.45,
           hjust = 0,
           vjust = 1) +
  
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994-97", "", "", "", "", "", "",
                              "", "", "", "", "", "", "", "", "2009-12", 
                              "", "", "", "", "", "", "2016-19"),
                   expand = c(0.1, 0.1)) +
  addscales() +
  addlabels() +
  addsource()

saveplot()


# Chart 6 - Workpov ch ----

data <- workpovch
file <- "charts/06_workpovch.png"

plotdata(data, up = 0.3) +
  
  scale_y_continuous(limits = c(0.3, 0.75)) +
  addnames(AHC_ypos = 0.53, BHC_ypos = 0.4) +
  
  addscales() +
  addxlabels() +
  addlabels() +
  addsource()

saveplot()
# Chart 7 - Relpov wa ----

data <- relpovwa
file <- "charts/07_relpovwa.png"

plotdata(data) +
  
  scale_y_continuous(limits = c(0, 0.45)) +
  addnames(AHC_ypos = 0.24, BHC_ypos = 0.13) +
  
  addscales() +
  addxlabels() +
  addlabels() +
  addsource()

saveplot()

# Chart 8 - Abspov wa ----

data <- abspovwa
file <- "charts/08_abspovwa.png"

plotdata(data) +
  
  scale_y_continuous(limits = c(0, 0.45)) +
  addnames(AHC_ypos = 0.33, BHC_ypos = 0.15) +
  
  addscales() +
  addxlabels() +
  addlabels() +
  addsource()

saveplot()


# Chart 9 - Workpov wa ----

data <- workpovwa
file <- "charts/09_workpovwa.png"

plotdata(data, up = 0.3) +
  
  scale_y_continuous(limits = c(0.3, 0.75)) +
  addnames(AHC_ypos = 0.56, BHC_ypos = 0.39) +
  
  addscales() +
  addxlabels() +
  addlabels() +
  addsource()

saveplot()

# Chart 10 - Relpov pn ----

data <- relpovpn
file <- "charts/10_relpovpn.png"

plotdata(data) +
  
  scale_y_continuous(limits = c(0, 0.45)) +
  addnames(AHC_ypos = 0.34, BHC_ypos = 0.17) +
  
  addscales() +
  addxlabels() +
  addlabels(uprAHC = -0.03, uprBHC = 0.03) +
  addsource()

saveplot()

# Chart 11 - Abspov pn ----

data <- abspovpn
file <- "charts/11_abspovpn.png"

plotdata(data, up=0.11) +
  
  scale_y_continuous(limits = c(0.11, 0.56)) +
  addnames(AHC_ypos = 0.18, BHC_ypos = 0.55) +
  
  addscales() +
  addxlabels() +
  addlabels(uprAHC = -0.02, uprBHC = 0.03,
            uplAHC = -0.03, uplBHC = 0.03) +
  addsource()

saveplot()


# Chart 12 - Matdep pn ----

data <- matdeppn
file <- "charts/12_matdeppn.png"

ggplot(data, aes(x = years, 
                 y = value, 
                 group = key, 
                 colour = key,
                 linetype = key,
                 label = percent(value, 1))) + 
  addbars() +
  
  geom_line(aes(size = key),
            show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 0.45)) +

  addxlabels() +
  addscales() +
  addlabels(uplAHC = - 0.015, uprAHC = - 0.015) +
  addsource()

saveplot()
