# READ ME #####################################################################
# AUTHOR:            Maike Waldmann
# PURPOSE OF SCRIPT: Make poverty charts 2
# CONTACTS:          contacts
# SOURCES:           sources
# NOTES:             notes
###############################################################################

# Charts2 - poverty by equality group (charts 13-19)

# load data and functions

source("functions.R")
source("data_prep.R")

# Chart 13 - Gender wa ----

data <- genderwa
file <- "charts/13_genderwa.png"

ggplot(data, aes(x = years, 
                 y = value, 
                 group = key, 
                 colour = key,
                 linetype = key,
                 label = percent(value, 1))) + 
  
  geom_line(size = 1.2, 
            lineend = "round",
            show.legend = FALSE) +
  
  annotate("text", x = 1, y = 0.44,
           label = "Single men with or without children", 
           hjust = 0,
           size = 3,
           fontface = "bold",
           colour = colours_cat[1]) +
  
  annotate("text", x = 1, y = 0.24,
           label = "Single women without children", 
           hjust = 0,
           size = 3,
           fontface = "bold",
           colour = colours_cat[2]) +
  
  annotate("text", x = 1, y = 0.68,
           label = "Single women with children", 
           hjust = 0,
           size = 3,
           fontface = "bold",
           colour = colours_cat[3]) +
  
  geom_text(data = filter(data, key == "Men")[1,], 
            aes(x = years, y = value),
            nudge_x = -1.2,
            nudge_y = 0.015 ,
            fontface = "bold",
            show.legend = FALSE) +

  geom_text(data = filter(data, key == "Women with no children")[1,],
            aes(x = years, y = value),
            nudge_x = -1.2,
            fontface = "bold",
            show.legend = FALSE) +
  
  geom_text(data = filter(data, key == "Lone mothers")[1,], 
            aes(x = years, y = value),
            nudge_x = -1.2,
            fontface = "bold",
            show.legend = FALSE) +

  geom_text(data = filter(data, key == "Men") %>% tail(1L),
            aes(x = years, y = value),
            nudge_x = 1.2,
            nudge_y = -0.005,
            fontface = "bold",
            show.legend = FALSE) +

  geom_text(data = filter(data, key == "Women with no children") %>% tail(1L),
            aes(x = years, y = value),
            nudge_x = 1.2,
            nudge_y = -0.01,
            fontface = "bold",
            show.legend = FALSE) +
  
  geom_text(data = filter(data, key == "Lone mothers") %>% tail(1L),
            aes(x = years, y = value),
            nudge_x = 1.2,
            fontface = "bold",
            show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0.23, 0.68)) +
 
  scale_color_manual(values = colours_cat) +
  
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994-97", "", "", "", 
                              "", "", "", "", 
                              "", "", "", "2005-08", 
                              "", "", "", "", 
                              "", "", "", "",
                              "", "", "2016-19"),
                   expand = c(0.1, 0.1)) +
  addsource()

ggsave(file, width = 12, height = 5.4, units = "cm", dpi = 300)


# Chart 14 - Gender pn ----

data <- genderpn
file <- "charts/14_genderpn.png"

ggplot(data, aes(x = years, 
                 y = value, 
                 group = key, 
                 colour = key,
                 linetype = key,
                 label = percent(value, 1))) + 

  geom_line(size = 1.2, 
            lineend = "round",
            show.legend = FALSE) +
  
  annotate("text", x = 1, y = 0.16,
           label = "Single male pensioners", 
           hjust = 0,
           size = 3,
           fontface = "bold",
           colour = colours_cat[1]) +
  
  annotate("text", x = 1, y = 0.49,
           label = "Single female pensioners", 
           hjust = 0,
           size = 3,
           fontface = "bold",
           colour = colours_cat[2]) +

  geom_text(data = filter(data, key == "Male pensioners")[1,], 
            aes(x = years, y = value),
            nudge_x = -1.2,
            fontface = "bold",
            show.legend = FALSE) +
  
  geom_text(data = filter(data, key == "Female pensioners")[1,],
            aes(x = years, y = value),
            nudge_x = -1.2,
            fontface = "bold",
            show.legend = FALSE) +
  
  geom_text(data = filter(data, key == "Male pensioners") %>% tail(1L),
            aes(x = years, y = value),
            nudge_x = 1.2,
            nudge_y = -0.01,
            fontface = "bold",
            show.legend = FALSE) +
  
  geom_text(data = filter(data, key == "Female pensioners") %>% tail(1L),
            aes(x = years, y = value),
            nudge_x = 1.2,
            nudge_y = 0.01,
            fontface = "bold",
            show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0.05, 0.50)) +
  
  scale_color_manual(values = colours_cat) +
  
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994-97", "", "", "", 
                              "", "", "", "", 
                              "", "", "", "2005-08", 
                              "", "", "", "", 
                              "", "", "", "",
                              "", "", "2016-19"),
                   expand = c(0.1, 0.1)) +  
  
  addsource()

ggsave(file, width = 12, height = 5.4, units = "cm", dpi = 300)


# Chart 15 - Marital status ----

data <- arrange(marital, key)
file <- "charts/15_marital.png"

ggplot(data, aes(x = years, 
                 y = value, 
                 group = key, 
                 colour = key,
                 linetype = key,
                 label = percent(value, 1))) + 
  
  geom_line(size = 1.2, 
            lineend = "round",
            show.legend = FALSE) +
  
  geom_text(data = filter(data, 
                          years == "1994-97"),
            aes(x = 0.8, 
                y = ifelse(key == "Cohabiting", value + 0.005, value),
                label = str_c(key, " ", percent(value, 1)),
                colour = key),
            hjust = 1,
           fontface = "bold",
           show.legend = FALSE) +
  
  geom_text(data = filter(data, 
                          years == max(years)),
            aes(x = 23.5, 
                y = ifelse(key == "Divorced", value - 0.005, value),
                label = percent(value, 1),
                colour = key),
            hjust = 0,
            fontface = "bold",
            show.legend = FALSE) +
 
  scale_y_continuous(limits = c(0.05, 0.5)) +
  
  scale_color_manual(values = colours_cat) +
  
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994-97", "", "", "", "", "", "",
                              "", "", "", "", "2005-08", "", "", "", "", 
                              "", "", "", "", "", "", "2016-19"),
                   expand = c(0.35, 0)) +
  addsource()

  ggsave(file, width = 15, height = 13, units = "cm", dpi = 300)


# Chart 16 - Ethnic group ----

data <- arrange(ethnic, key)
file <- "charts/16_ethnic.png"

ggplot(data, aes(x = key, 
                 y =value,
                 label = percent(value, 1),
                 fill = key)) +
  
  geom_col(show.legend = FALSE) +
  
  geom_text(colour = "white",
            fontface = "bold",
            nudge_y = -0.04,
            show.legend = FALSE) +
 
  scale_fill_manual(values = colours_cat) +
  scale_colour_manual(values = colours_cat) +
  
  scale_x_discrete(labels = str_wrap(data$key, 18)) +
  
  coord_flip() +
  
  theme(axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 1)) +
  
  addsource()

ggsave(file, width = 12, height = 9.5, units = "cm", dpi = 300)


# Chart 17 - Religion ----

data <- arrange(religion, key)
file <- "charts/17_religion.png"

ggplot(data, aes(x = key, 
                 y =value,
                 label = percent(value, 1),
                 fill = key)) +
  
  geom_col(show.legend = FALSE) +
  
  geom_text(colour = "white",
            fontface = "bold",
            nudge_y = -0.05,
            show.legend = FALSE) +
  
  scale_fill_manual(values = colours_cat) +
  scale_colour_manual(values = colours_cat) +
  
  scale_x_discrete(labels = str_wrap(data$key, 18)) +
  
  coord_flip() +
  
  theme(axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 1)) +
  
  addsource()

ggsave(file, width = 12, height = 10, units = "cm", dpi = 300)

# Chart 18 - Disability 1 ----

data <- disability
file <- "charts/18_disability1.png"

ggplot(data, aes(x = years, 
                 y = value, 
                 group = key, 
                 colour = key,
                 linetype = key,
                 label = percent(value, 1))) + 
  
  geom_vline(aes(xintercept = 6.5),
             colour = breakcol,
             alpha = 0.9) +
  
  geom_vline(aes(xintercept = 8.5),
             colour = breakcol,
             alpha = 0.9) +
  
  geom_vline(aes(xintercept = 16.5),
             colour = breakcol,
             alpha = 0.9) +
  
  annotate("text",
           x = 16.3,
           y = 0.5,
           hjust = 1,
           vjust = 1,
           size = 3,
           colour = SGgrey,
           label = "Methodology changes") +
  
  geom_line(size = 1.2, 
            lineend = "round",
            show.legend = FALSE) +
  
  annotate("text", x = 2, y = 0.16,
           label = "No-one disabled", 
           hjust = 0,
           size = 3,
           fontface = "bold",
           colour = colours_cat[1]) +
  
  annotate("text", x = 2, y = 0.35,
           label = "Someone disabled", 
           hjust = 0,
           size = 3,
           fontface = "bold",
           colour = colours_cat[2]) +
  
  geom_text(data = filter(data, key == "No-one disabled")[1,], 
            aes(x = years, y = value),
            nudge_x = -1.2,
            fontface = "bold",
            show.legend = FALSE) +
  
  geom_text(data = filter(data, key == "Someone disabled")[1,],
            aes(x = years, y = value),
            nudge_x = -1.2,
            fontface = "bold",
            show.legend = FALSE) +
  
  geom_text(data = filter(data, key == "No-one disabled") %>% tail(1L),
            aes(x = years, y = value),
            nudge_x = 1.2,
            fontface = "bold",
            show.legend = FALSE) +
  
  geom_text(data = filter(data, key == "Someone disabled") %>% tail(1L),
            aes(x = years, y = value),
            nudge_x = 1.2,
            fontface = "bold",
            show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0.05, 0.5)) +
  
  scale_color_manual(values = colours_cat) +
  
  scale_x_discrete(drop = FALSE,
                   breaks = c("", "1995-98", "", "", 
                              "", "", "", "", 
                              "", "", "", "2005-08", 
                              "", "", "", "", 
                              "", "", "", "",
                              "", "", "2016-19"),
                   expand = c(0.1, 0.1)) +
  addsource()

saveplot()

# Chart 19 - Disability 2 ----

data <- disabilityalt
file <- "charts/19_disability2.png"

ggplot(data, aes(x = years, 
                 y = value, 
                 group = key, 
                 colour = key,
                 linetype = key,
                 label = percent(value, 1))) + 

  geom_vline(aes(xintercept = 6.5),
             colour = breakcol,
             alpha = 0.9) +
  
  geom_vline(aes(xintercept = 8.5),
             colour = breakcol,
             alpha = 0.9) +
  
  geom_vline(aes(xintercept = 16.5),
             colour = breakcol,
             alpha = 0.9) +
  
  annotate("text",
           x = 16.3,
           y = 0.5,
           hjust = 1,
           vjust = 1,
           size = 3,
           colour = SGgrey,
           label = "Methodology changes") +
  
  geom_line(size = 1.2, 
            lineend = "round",
            show.legend = FALSE) +
  
  annotate("text", x = 2, y = 0.16,
           label = "No-one disabled", 
           hjust = 0,
           size = 3,
           fontface = "bold",
           colour = colours_cat[1]) +
  
  annotate("text", x = 2, y = 0.42,
           label = "Someone disabled", 
           hjust = 0,
           size = 3,
           fontface = "bold",
           colour = colours_cat[2]) +
  
  geom_text(data = filter(data, key == "No-one disabled")[1,], 
            aes(x = years, y = value),
            nudge_x = -1.2,
            fontface = "bold",
            show.legend = FALSE) +
  
  geom_text(data = filter(data, key == "Someone disabled")[1,],
            aes(x = years, y = value),
            nudge_x = -1.2,
            fontface = "bold",
            show.legend = FALSE) +
  
  geom_text(data = filter(data, key == "No-one disabled") %>% tail(1L),
            aes(x = years, y = value),
            nudge_x = 1.2,
            fontface = "bold",
            show.legend = FALSE) +
  
  geom_text(data = filter(data, key == "Someone disabled") %>% tail(1L),
            aes(x = years, y = value),
            nudge_x = 1.2,
            fontface = "bold",
            show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0.05, 0.5)) +
  
  scale_color_manual(values = colours_cat) +
  
  scale_x_discrete(drop = FALSE,
                   breaks = c("", "1995-98", "", "", 
                              "", "", "", "", 
                              "", "", "", "2005-08", 
                              "", "", "", "", 
                              "", "", "", "",
                              "", "", "2016-19"),
                   expand = c(0.1, 0.1)) +
  addsource()

saveplot()
