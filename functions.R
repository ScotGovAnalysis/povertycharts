# READ ME #####################################################################
# AUTHOR:            Maike Waldmann
# PURPOSE OF SCRIPT: Packages, functions and ggplot theme for poverty charts
# CONTACTS:          contacts
# SOURCES:           sources
# NOTES:             notes
###############################################################################

# Load packages ----

library(tidyverse)
library(readxl)
library(scales)
library(labelled)

# Functions ----

# Format year variable
formatyears <- function(data){
  mutate(data,
         year = as.character(year),
         year = ifelse(nchar(year) <= 4, year, 
                       str_c(str_sub(year, 3L, 4L),
                             str_sub(year, 6L, 7L))),
         year = ifelse(nchar(year) == 3, str_c("0", year), 
                       ifelse(nchar(year) == 1, "0001", year)),
         year = factor(year, levels = unique(as.character(year))),
         year = ordered(year)) %>%
    arrange(year)}

# Get 3-year averages
get3yrmean <- function(v){(v + lag(v) + lag(v, 2L))/3}

# Get centered 3-year averages
getcentmean <- function(v){(v + lag(v) + lead(v))/3}

# Get 3-year period names
get3yrperiod <- function(data){
  
  mutate(data, years = str_c(str_sub(lag(year, 2L), 0, 2), "-", 
                             str_sub(year, 3, 4)),
         years = ifelse(str_sub(years, 0, 1) == "9", 
                        str_c("19", years),
                        str_c("20", years)),
         years = factor(years, 
                        levels = as.character(years), 
                        ordered = TRUE))}

# Add source

addsource <- function(){
  labs(caption = "Source: Family Resources Survey")
}

# Add annotations for recession and welfare reform

addbars <- function(up = 0){
  
  welf_rect <- geom_vline(aes(xintercept = 18.5),
                          alpha = 0.9,
                          colour = "grey90")
  
  welf_text <- annotate("text",
                        label = "Welfare Reform \nAct 2012",
                        size = 3,
                        colour = "grey60",
                        hjust = 0,
                        vjust = 1,
                        x = 18.7, 
                        y = 0.45 + up)
  
  rec_rect <- annotate("rect",
                       fill = "grey90",
                       alpha = 0.9,
                       xmin = 13.16,
                       xmax = 14.66,
                       ymin = -Inf,
                       ymax = Inf)
  
  rec_text <- annotate("text",
                       label = "Recession",
                       size = 3,
                       colour = "grey60",
                       hjust = 1,
                       vjust = 1,
                       x = 13, 
                       y = 0.45 + up)
  
  list(welf_rect,
       welf_text,
       rec_rect,
       rec_text)
}

addlabels <- function(uprAHC = 0, uprBHC = 0, 
                      uplAHC = 0, uplBHC = 0){
  
  AHCleft <- geom_text(data = filter(data, key == "AHC")[1,], 
                       aes(x = years, y = value),
                       nudge_x = -1.2,
                       nudge_y = 0.015 + uplAHC,
                       fontface = "bold",
                       show.legend = FALSE)
  
  BHCleft <- geom_text(data = filter(data, key == "BHC")[1,], 
                       aes(x = years, y = value),
                       nudge_x = -1.2,
                       nudge_y = - 0.015 + uplBHC,
                       show.legend = FALSE)
  
  AHCright <- geom_text(data = filter(data, key == "AHC") %>% tail(1L),
                        aes(x = years, y = value),
                        nudge_x = 1.2,
                        nudge_y = 0.015 + uprAHC,
                        fontface = "bold",
                        show.legend = FALSE)
  
  BHCright <- geom_text(data = filter(data, key == "BHC") %>% tail(1L),
                        aes(x = years, y = value),
                        nudge_x = 1.2,
                        nudge_y = -0.015 + uprBHC,
                        show.legend = FALSE)
  
  list(AHCleft,
       BHCleft,
       AHCright,
       BHCright)
}

addnames <- function(xpos = 2, AHC_ypos = 0.28, BHC_ypos = 0.16){
  
  AHC <- annotate("text", x = xpos, y = AHC_ypos,
                  label = "After housing costs", 
                  hjust = 0,
                  size = 3,
                  fontface = "bold",
                  colour = colours[1]) 
  
  BHC <- annotate("text", x = xpos, y = BHC_ypos,
                  label = "Before housing costs", 
                  hjust = 0,
                  size = 3,
                  colour = colours[2]) 
  
  list(AHC, BHC)
}

addxlabels <- function(){
  
  scale_x_discrete(drop = FALSE,
                   breaks = c("1994-97", "", "", "", 
                              "", "", "", "", 
                              "", "", "", "", 
                              "", "2007-10", "", "", 
                              "", "2011-14", "", "",
                              "", "", "2016-19"),
                   expand = c(0.1, 0.1))
}

addscales <- function(){
  
  list(scale_color_manual(values = colours), 
       scale_size_manual(values = linesize))
}

plotdata <- function(data = data, up = 0){
  
  ggplot(data, aes(x = years, 
                   y = value, 
                   group = key, 
                   colour = key,
                   linetype = key,
                   label = percent(value, 1))) + 
    addbars(up) +
    
    geom_line(aes(size = key),
              lineend = "round",
              show.legend = FALSE)
}

saveplot <- function(){
  
  ggsave(file, width = 12, height = 6.5, units = "cm", dpi = 300)
}

addinterimtarget <- function(y){
  
  a <- geom_point(aes(x = "2324", y = y), 
                  shape = 21, size = 5, 
                  fill = "grey20", 
                  alpha = 0.4)
  
  b <- geom_point(aes(x = "2324", y = y), 
                  shape = 21, size = 4, 
                  fill = "white", 
                  alpha = 0.4) 
  
  c <- geom_point(aes(x = "2324", y = y), 
                  shape = 21, size = 2.5, 
                  fill = "grey20", 
                  alpha = 0.4)
  
  d <- geom_point(aes(x = "2324", y = y), 
                  shape = 21, size = 1.5, 
                  fill = colours[1], 
                  alpha = 0.4) 
  
  e <-   geom_text(data = tail(data, 1L),
                   aes(x = 30.2, y = interimtarget + 0.05, 
                       label = percent(interimtarget, 1)),
                   size = 3,
                   fontface = "bold",
                   colour = "grey20") 
  
  list(a,b,c,d, e)
}

addfinaltarget <- function(y){
  
  a <- geom_point(aes(x = "3031", y = y), 
             shape = 21, size = 5, 
             fill = "grey20", 
             alpha = 0.4) 
    
  b <- geom_point(aes(x = "3031", y = y), 
               shape = 21, size = 4, 
               fill = "white") 
    
  c <- geom_point(aes(x = "3031", y = y), 
               shape = 21, size = 2.5, 
               fill = "grey20", 
               alpha = 0.4)
    
  d <- geom_point(aes(x = "3031", y = y), 
               shape = 21, size = 1.5, 
               fill = colours[1], 
               alpha = 0.4) 
  
  e <-   geom_text(data = tail(data, 1L),
                   aes(x = 37.2, y = finaltarget + 0.05, 
                       label = percent(finaltarget, 1)),
                   size = 3,
                   fontface = "bold",
                   colour = "grey20")
  
  list(a,b,c,d,e)
}

addtargetbars <- function(){
  
  a <- geom_vline(aes(xintercept = 24.36),
                  colour = "grey90",
                  alpha = 0.9) 
    
  b <-  annotate("rect", 
             xmin = 29, xmax = 31, 
             ymin = -Inf, ymax = Inf,
             fill = "grey90",
             alpha = 0.9)
    
  c <- annotate("rect", 
             xmin = 36, xmax = 38, 
             ymin = -Inf, ymax = Inf,
             fill = "grey90",
             alpha = 0.9) 
  
  list(a,b,c)
}

addyaxis <- function(){
  
  a <- scale_y_continuous(limits = c(0, 0.53), labels = percent_format(1)) 
    
  # b <- theme(axis.line.y = element_line(), 
  #         axis.text.y = element_text(hjust = 1, 
  #                                    margin = margin(0, 3, 0, 0, "pt")),
  #         axis.ticks.length = unit(2, "pt"),
  #         axis.ticks.y = element_line(),
  #         axis.title = element_blank()) 
  # 
  # list(a, b)
}

adddatalabels <- function(){
  
  a <- geom_text(data = head(data, 1L),
            aes(x = year, y = single + 0.04, 
                label = percent(single, 1)),
            size = 3,
            nudge_x = -0.4,
            colour = "grey20") 
    
  b <-  geom_text(data = tail(data, 1L),
              aes(x = year, y = single + 0.04, 
                  label = percent(single, 1)),
              size = 3,
              hjust = 0,
              colour = "grey20") 
  
  list(a,b)
}

# Define ggplot chart theme and colours ----

colours <- c("#da1f28", "#2da2bf", "#464646")
colours_ts <- c("#89bdd0", "#2da2bf", "#278fa9", "#1f798f")
colours_sources <- c("#464646", "#ffbf00", "#bebada",
                     "#da1f28", "#2da2bf")
colours_cat <- c("#464646", "#2da2bf", "#da1f28",
                 "#ffbf00", "#bebada", "#6fac47",
                 "#255f91")

breakcol <- "#fbdfd1"

linesize <- c(1.2, 1)

mytheme <- theme_grey() + 
  theme(text = element_text(colour = "grey20", size = 12 ), 
        
        line = element_line(colour = "grey20", 
                            linetype = 1, 
                            lineend = 2, 
                            size = 0.5), 
        
        plot.title = element_text(hjust = 0, colour = 'black'), 
        plot.subtitle = element_text(hjust = 0, colour = 'black'), 
        plot.caption = element_text(hjust = 1), 
        
        legend.position = "top", 
        legend.title = element_blank(), 
        
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        
        axis.line.x = element_line(), 
        axis.ticks.length = unit(2, "pt"),
        axis.ticks.y = element_blank(),
        
        axis.title = element_blank(),
        axis.text.y = element_blank()) 

theme_set(mytheme)
