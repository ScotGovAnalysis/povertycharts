# READ ME #####################################################################
# AUTHOR:            Maike Waldmann
# PURPOSE OF SCRIPT: Make poverty charts 3
# CONTACTS:          contacts
# SOURCES:           sources
# NOTES:             notes
###############################################################################

# Charts3 - income (charts 20-24)

# load data and functions

source("functions.R")
source("data_prep.R")

# Chart 20 - Medians ----

data <- medianspp
file <- "charts/20_medians.png"

ggplot(data, aes(x = years, 
                 y = value, 
                 group = key, 
                 colour = key,
                 linetype = key,
                 label = comma(value, 1, prefix = "£"))) + 
  
  addbars(up = 599.55) +
  
  geom_line(aes(size = key),
            lineend = "round",
            show.legend = FALSE) +
  
  scale_y_continuous(limits = c(250, 600)) +
  addnames(AHC_ypos = 280, BHC_ypos = 490) +
  
  addscales() +
  addxlabels() +
  addlabels() +
  addsource()

saveplot()

# Chart 21 - Deciles ----

data <- filter(decilespp, key != "dec100")
file <- "charts/21_deciles.png"

ggplot(data, aes(x = key, 
                 y = value, 
                 fill = years,
                 label = comma(value, prefix = "£"))) + 
  
  geom_col(position = 'dodge',
           colour = "white") +

  scale_fill_manual(values = colours_ts) +

  scale_x_discrete(labels = c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th"),
                   expand = c(0.1, 0.1)) +
  
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  
  theme(axis.line.y = element_line(), 
        axis.text.y = element_text(hjust = 1, 
                                   margin = margin(0, 3, 0, 0, "pt")),
        axis.ticks.length = unit(2, "pt"),
        axis.ticks.y = element_line(),
        axis.title = element_blank()) +
  
  addsource()

saveplot()

# Chart 22 - Distribution ----

data <- dist
file <- "charts/22_distribution.png"

ggplot(filter(data, key == "pp"), 
       aes(x = income, weight = value)) + 
  
  geom_density(colour = NA,
               fill = colours[1],
               alpha = 0.6,
               adjust = 1/2) + 

  annotate("rect", fill = "white", alpha = 0.4,
           xmin = 0, xmax = decilepoints$value[1],
           ymin = -Inf, ymax = +Inf) +
  
  annotate("rect", fill = "white", alpha = 0.4,
           xmin = decilepoints$value[2], 
           xmax = decilepoints$value[3],
           ymin = -Inf, ymax = +Inf) +
  
  annotate("rect", fill = "white", alpha = 0.4,
           xmin = decilepoints$value[4], 
           xmax = decilepoints$value[5],
           ymin = -Inf, ymax = +Inf) +
  
  annotate("rect", fill = "white", alpha = 0.4,
           xmin = decilepoints$value[6], 
           xmax = decilepoints$value[7],
           ymin = -Inf, ymax = +Inf) +
  
  annotate("rect", fill = "white", alpha = 0.4,
           xmin = decilepoints$value[8], 
           xmax = decilepoints$value[9],
           ymin = -Inf, ymax = +Inf) +
  
  geom_text(data = decilepoints,
            aes(x = xpos, y = 1700, label = c(seq("1", "10"))),
            colour = "white",
            fontface = "bold") +
  
  geom_vline(aes(xintercept = povthresh),
             colour = colours[1],
             size = 1.2) +
  
  annotate("text", x = 260, y = 2.5E4, 
           label = str_c("Poverty threshold:\n ", 
                         comma(povthresh, 
                               prefix = "£",
                               accuracy = 1)),
           colour = colours[1],
           fontface = "bold",
           size = 3,
           hjust = 1) +
  
  geom_vline(aes(xintercept = UKmedian),
             colour = colours[1],
             size = 1.2) +
  
  annotate("text", x = 570, y = 2.5E4, 
           label = str_c("Median income: \n", 
                         comma(UKmedian, 
                               prefix = "£",
                               accuracy = 1)),
           colour = colours[1],
           fontface = "bold",
           size = 3,
           hjust = 0) +
  
  scale_x_continuous(labels = comma_format(prefix = "£"),
                     breaks = c(seq(0, 1200, 200)),
                     expand = c(0.1, 0.1)) +
  
  addsource()

saveplot()

# Chart 23 - Income sources ----

data <- sourcespp
file <- "charts/23_sources.png"

ggplot(data, aes(x = decile, 
                 y = value, 
                 fill = key,
                 width = 1)) + 
  
  geom_col(position = "fill",
           colour = "white") +
  
  scale_fill_manual(values = colours_sources) +
  
  scale_y_continuous(labels = percent_format(1)) +
  scale_x_continuous(breaks = c(seq(1, 10, 1)),
                     labels = c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th"),
                     expand = c(0.05, 0.05)) +
  
  theme(axis.line = element_line(), 
        axis.text.y = element_text(hjust = 1, 
                                   margin = margin(0, 3, 0, 0, "pt")),
        axis.title.y = element_text(hjust = 0.5),
        axis.ticks.length = unit(2, "pt"),
        axis.ticks.y = element_line(),
        legend.position = c(0.75, 0.22)) +
  
  ylab("Proportion of income") +

  addsource()

ggsave(file, width = 15, height = 12, units = "cm", dpi = 300)

# Chart 24 - Palma ----

data <- palmabhc
file <- "charts/24_palma.png"

ggplot(data, aes(x = years, 
                 y = value,
                 group = "all",
                 label = percent(value, 1))) +
  
  addbars(up = 1.35) +
  
  geom_line(size = 1.2,
            lineend = "round",
            show.legend = FALSE,
            colour = colours[1]) +
  
  geom_text(data = data[1,], 
            aes(x = years, y = value),
            nudge_x = -1.2,
            nudge_y = - 0.015,
            show.legend = FALSE,
            colour = colours[1],
            fontface = "bold") + 
  
  geom_text(data = tail(data, 1L),
            aes(x = years, y = value),
            nudge_x = 1.3,
            nudge_y = -0.015,
            show.legend = FALSE,
            colour = colours[1],
            fontface = "bold") +
  
  geom_text(data = filter(data, years == "2007-10"),
            aes(x = years, y = value),
            nudge_y = 0.08,
            show.legend = FALSE,
            colour = colours[1],
            fontface = "bold") +
  
  scale_y_continuous(limits = c(0.6, 1.8)) +

  scale_x_discrete(drop = FALSE,
                   breaks = c("1994-97", "", "", "", "", "", "",
                              "", "", "", "", "", "", "2007-10", "", "", 
                              "", "2011-14", "", "", "", "", "2016-19"),
                   expand = c(0.1, 0.2)) +
  addsource()

saveplot()
