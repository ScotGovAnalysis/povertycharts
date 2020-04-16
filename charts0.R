# READ ME #####################################################################
# AUTHOR:            Maike Waldmann
# PURPOSE OF SCRIPT: Make poverty charts 0
# CONTACTS:          contacts
# SOURCES:           sources
# NOTES:             notes
###############################################################################

# Charts0 - frontpage (charts 0a, 0b, 0c)

# load data and functions

source("functions.R")
source("data_prep.R")

# Chart 0a - Relpov pp ----

data <- filter(relpovpp, key == "AHC")
file <- "charts/0a_relpovpp.png"

ggplot(data, aes(x = years, 
                 y = value, 
                 group = "all",
                 label = percent(value, 1))) + 
  
  geom_vline(aes(xintercept = 18.5),
                          alpha = 0.9,
                          colour = "grey90") +

  annotate("text",
                      label = "Welfare Reform \nAct 2012",
                      size = 3,
                      colour = "grey60",
                      hjust = 0,
                      vjust = 1,
                      x = 18.7, 
                      y = 0.35) +

  annotate("rect",
                     fill = "grey90",
                     alpha = 0.9,
                     xmin = 13.16,
                     xmax = 14.66,
                     ymin = -Inf,
                     ymax = Inf) +

  annotate("text",
                     label = "Recession",
                     size = 3,
                     colour = "grey60",
                     hjust = 1,
                     vjust = 1,
                     x = 13, 
                     y = 0.35) +
  
  geom_line(size = 1.2,
            colour = colours[1],
            lineend = "round",
            show.legend = FALSE) +
  
  geom_text(data = data[1,], 
          aes(x = years, y = value),
          nudge_x = -1.7,
          fontface = "bold",
          show.legend = FALSE,
          colour = colours[1]) +
  
  geom_text(data = tail(data, 1L),
            aes(x = years, y = value),
            nudge_x = 1.7,
            fontface = "bold",
            show.legend = FALSE,
            colour = colours[1]) +
  
  scale_y_continuous(limits = c(0.1, 0.35)) +
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994-97", "2005-08", "2016-19"),
                   expand = c(0.15, 0.15)) +
  addsource() 

ggsave(file, width = 8.3, height = 5.5, units = "cm", dpi = 300)


# Chart 0b - Palma ----

data <- palmabhc
file <- "charts/0b_palma.png"

ggplot(data, aes(x = years, 
                 y = value, 
                 group = "all",
                 label = percent(value, 1))) + 
  
  geom_vline(aes(xintercept = 18.5),
             alpha = 0.9,
             colour = "grey90") +
  
  annotate("rect",
           fill = "grey90",
           alpha = 0.9,
           xmin = 13.16,
           xmax = 14.66,
           ymin = -Inf,
           ymax = Inf) +
  
  geom_line(size = 1.2,
            lineend = "round",
            colour = colours[2],
            show.legend = FALSE) +
  
  geom_text(data = data[1,], 
            aes(x = years, y = value),
            nudge_x = -2.5,
            fontface = "bold",
            show.legend = FALSE,
            colour = colours[2]) +
  
  geom_text(data = tail(data, 1L),
            aes(x = years, y = value),
            nudge_x = 2.5,
            fontface = "bold",
            show.legend = FALSE,
            colour = colours[2]) +
  
  scale_y_continuous(limits = c(0.5, 1.9)) +
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994-97", "2005-08", "2016-19"),
                   expand = c(0.2, 0.2)) +
  addsource()

ggsave(file, width = 8.3, height = 5.5, units = "cm", dpi = 300)


# Chart 0c - Medians ----

data <- medianspp
file <- "charts/0c_medians.png"

ggplot(data, aes(x = years, 
                 y = value, 
                 group = key, 
                 colour = key,
                 linetype = key,
                 label = comma(value, 1, prefix = "£"))) + 

  geom_vline(aes(xintercept = 18.5),
             alpha = 0.9,
             colour = "grey90") +
  
  annotate("rect",
           fill = "grey90",
           alpha = 0.9,
           xmin = 13.16,
           xmax = 14.66,
           ymin = -Inf,
           ymax = Inf) +
  
  geom_line(aes(size = key),
            lineend = "round",
            show.legend = FALSE) + 
  
  geom_text(data = filter(data, key == "BHC")[1,], 
            aes(x = years, y = value),
            nudge_x = -2.5,
            show.legend = FALSE) +

  geom_text(data = filter(data, key == "AHC") %>% tail(1L),
            aes(x = years, y = value),
            nudge_x = 2.5,
            fontface = "bold",
            show.legend = FALSE) +

  geom_text(data = filter(data, key == "BHC") %>% tail(1L),
            aes(x = years, y = value),
            nudge_x = 2.5,
            show.legend = FALSE) +
  
  geom_text(data = filter(data, key == "AHC")[1,], 
            aes(x = years, y = value),
            nudge_x = -2.5,
            fontface = "bold",
            show.legend = FALSE) +
  
  annotate("text", x = 4, y = 300,
           label = "After housing costs", 
           hjust = 0,
           size = 3,
           fontface = "bold",
           colour = colours[1]) +

  annotate("text", x = 4, y = 530,
                label = "Before housing costs", 
                hjust = 0,
                size = 3,
                colour = colours[2]) +
  
  addscales() +
  scale_y_continuous(limits = c(250, 600)) +
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994-97", "2005-08", "2016-19"),
                   expand = c(0.2, 0.2)) +
  addsource() 

ggsave(file, width = 8.3, height = 5.5, units = "cm", dpi = 300)

