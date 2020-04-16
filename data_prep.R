# READ ME #####################################################################
# AUTHOR:            Maike Waldmann
# PURPOSE OF SCRIPT: Import and prep data for poverty charts
# CONTACTS:          contacts
# SOURCES:           sources
# NOTES:             notes
###############################################################################

# Load functions

source("functions.R")

# Import data ----

## Poverty overall ----

path = "//s0177a/datashare/Social_Justice/Poverty/Prerelease/Poverty and Income Inequality 2018-19/2020 Publication/SAS outputs/Ass_Tables.xlsx"

relpov <- read_xlsx(path, sheet = "Relpov", skip = 3)
abspov <- read_xlsx(path, sheet = "Abspov", skip = 3)
workpov <- read_xlsx(path, sheet = "Workpov", skip = 3)
matdep <- read_xlsx(path, sheet = "Matdep", skip = 3)
pendep <- read_xlsx(path, sheet = "Pens", skip = 3)

## Persistent poverty ----

path = "//s0177a/datashare/Social_Justice/Poverty/Prerelease/Persistent Poverty in Scotland 2020 publication/Publication/Tables for publication - current year.xlsx"

perspov <- read_xlsx(path, sheet = "Table 1", range = "A7:E11",
                     col_names = c("period", "b", "c", "d", "chAHC"))

## Equality analysis ----

path = "//s0177a/datashare/Social_Justice/Poverty/Prerelease/Poverty and Income Inequality 2018-19/2020 Publication/Tables/Associated tables - working table.xlsx"

genderpov <- read_xlsx(path, sheet = "Gender", range = "A5:AB29",
                       col_names = c("year","b", "c", "waAHCpct", "e",
                                     "f", "g", "wamlAHCpct", "i",
                                     "j", "k", "walmAHCpct", "m",
                                     "n", "o","wafmAHCpct", "q",
                                     "r", "s","pnAHCpct", "u",
                                     "v", "w", "pnmlAHCpct", "y",
                                     "z", "aa", "pnfmAHCpct"))

disabilitypov <- read_xlsx(path, sheet = "Disability (household)", 
                           range = "A5:X28",
                       col_names = c("year", "b","c", "nodisAHC", 
                                     "e","f","g","disAHC",
                                     "i","j","k","l","m","n","o","p","q","r","s","nodisAHCalt", 
                                     "u", "v", "w", "disAHCalt"))

maritalpov <- read_xlsx(path, sheet = "Marital Status", 
                           range = "A5:T29",
                           col_names = c("year", "b","c", "MarriedAHC", 
                                         "e","f","g","CohabitingAHC",
                                         "i","j","k","SingleAHC",
                                         "m","n","o","WidowedAHC",
                                         "q","r","s","DivorcedSeparatedAHC"))



ethnicpov <- read_xlsx(path, sheet = "Ethnicity", 
                       range = "A14:B18",
                       col_names = c("key", "value"))

religionpov <- read_xlsx(path, sheet = "Religion", 
                       range = "A14:B20",
                       col_names = c("key", "value"))

## Income analysis ----

path = "//s0177a/datashare/Social_Justice/Poverty/Prerelease/Poverty and Income Inequality 2018-19/2020 Publication/SAS outputs/Ass_Tables.xlsx"

medians <- read_xlsx(path, sheet = "Weeklymedians", skip = 3)
deciles <- read_xlsx(path, sheet = "Deciles", skip = 3)
distribution <- read_xlsx(path, sheet = "Distribution2", skip = 3)
medians4dist <- read_xlsx(path, sheet = "MediansBHC", skip = 3)
sources <-  read_xlsx(path, sheet = "Sources", skip = 3)
palma <-  read_xlsx(path, sheet = "Palma", skip = 3)


# Prepare datasets ----

relpovdata <- formatyears(relpov) %>%
  get3yrperiod() %>%
  mutate(ppBHC = get3yrmean(pct_pp_BHC),
         chBHC = get3yrmean(pct_ch_BHC),
         waBHC = get3yrmean(pct_wa_BHC),
         pnBHC = get3yrmean(pct_pn_BHC),
         ppAHC = get3yrmean(pct_pp_AHC),
         chAHC = get3yrmean(pct_ch_AHC),
         waAHC = get3yrmean(pct_wa_AHC),
         pnAHC = get3yrmean(pct_pn_AHC)) %>%
  select(years, ppBHC:pnAHC) %>%
  tail(-2L)

abspovdata <- formatyears(abspov) %>%
  get3yrperiod() %>%
  mutate(ppBHC = get3yrmean(pct_pp_BHC),
         chBHC = get3yrmean(pct_ch_BHC),
         waBHC = get3yrmean(pct_wa_BHC),
         pnBHC = get3yrmean(pct_pn_BHC),
         ppAHC = get3yrmean(pct_pp_AHC),
         chAHC = get3yrmean(pct_ch_AHC),
         waAHC = get3yrmean(pct_wa_AHC),
         pnAHC = get3yrmean(pct_pn_AHC)) %>%
  select(years, ppBHC:pnAHC) %>%
  tail(-2L)

workpovdata <- formatyears(workpov) %>%
  get3yrperiod() %>%
  mutate(ppBHC = get3yrmean(pct_pp_BHC),
         chBHC = get3yrmean(pct_ch_BHC),
         waBHC = get3yrmean(pct_wa_BHC),
         ppAHC = get3yrmean(pct_pp_AHC),
         chAHC = get3yrmean(pct_ch_AHC),
         waAHC = get3yrmean(pct_wa_AHC)) %>%
  select(years, ppBHC:waAHC) %>%
  tail(-2L)

genderdata <- formatyears(genderpov) %>%
  get3yrperiod() %>%
  mutate(waml = get3yrmean(wamlAHCpct),
         wafm = get3yrmean(wafmAHCpct),
         walm = get3yrmean(walmAHCpct),
         pnml = get3yrmean(pnmlAHCpct),
         pnfm = get3yrmean(pnfmAHCpct)) %>%
  select(years, waml:pnfm) %>%
  tail(-2L)

disabilitydata <- formatyears(disabilitypov) %>%
  get3yrperiod() %>%
  mutate(nodis = get3yrmean(nodisAHC),
         dis = get3yrmean(disAHC),
         nodisalt = get3yrmean(nodisAHCalt),
         disalt = get3yrmean(disAHCalt)) %>%
  select(years, nodis:disalt) %>%
  tail(-2L)

maritaldata <- formatyears(maritalpov) %>%
  get3yrperiod() %>%
  mutate(Married = get3yrmean(MarriedAHC),
         Cohabiting = get3yrmean(CohabitingAHC),
         Single = get3yrmean(SingleAHC),
         Widowed = get3yrmean(WidowedAHC),
         Divorced = get3yrmean(DivorcedSeparatedAHC)) %>%
  select(years, Married:Divorced) %>%
  tail(-2L)

# Subset datasets for each chart ----
## Poverty charts ----

relpovpp <- relpovdata %>%
  select(years, ppAHC, ppBHC) %>%
  gather(key, value, -years) %>%
  mutate(key = ifelse(key == "ppAHC", "AHC", "BHC"))

abspovpp <- abspovdata %>%
  select(years, ppAHC, ppBHC) %>%
  gather(key, value, -years) %>%
  mutate(key = ifelse(key == "ppAHC", "AHC", "BHC"))

relpovch <- relpovdata %>%
  select(years, chAHC, chBHC) %>%
  gather(key, value, -years) %>%
  mutate(key = ifelse(key == "chAHC", "AHC", "BHC"))

abspovch <- abspovdata %>%
  select(years, chAHC, chBHC) %>%
  gather(key, value, -years) %>%
  mutate(key = ifelse(key == "chAHC", "AHC", "BHC"))

relpovwa <- relpovdata %>%
  select(years, waAHC, waBHC) %>%
  gather(key, value, -years) %>%
  mutate(key = ifelse(key == "waAHC", "AHC", "BHC"))

abspovwa <- abspovdata %>%
  select(years, waAHC, waBHC) %>%
  gather(key, value, -years) %>%
  mutate(key = ifelse(key == "waAHC", "AHC", "BHC"))

relpovpn <- relpovdata %>%
  select(years, pnAHC, pnBHC) %>%
  gather(key, value, -years) %>%
  mutate(key = ifelse(key == "pnAHC", "AHC", "BHC"))

abspovpn <- abspovdata %>%
  select(years, pnAHC, pnBHC) %>%
  gather(key, value, -years) %>%
  mutate(key = ifelse(key == "pnAHC", "AHC", "BHC"))

workpovch <- workpovdata %>%
  select(years, chAHC, chBHC) %>%
  gather(key, value, -years) %>%
  mutate(key = ifelse(key == "chAHC", "AHC", "BHC"),
         years = factor(years, levels = levels(relpovdata$years)))

workpovwa <- workpovdata %>%
  select(years, waAHC, waBHC) %>%
  gather(key, value, -years) %>%
  mutate(key = ifelse(key == "waAHC", "AHC", "BHC"),
         years = factor(years, levels = levels(relpovdata$years)))

## Matdep charts ----

matdepch <- formatyears(matdep) %>%
  get3yrperiod() %>%
  mutate(chBHC = get3yrmean(pct_ch_BHC),
         chAHC = get3yrmean(pct_ch_AHC),
         chBHC = ifelse(years == "2010-12", NA, chBHC),
         chAHC = ifelse(years == "2010-12", NA, chAHC)) %>%
  filter(years != "2009-11") %>%
  select(years, chBHC, chAHC) %>%
  gather(key, value, -years) %>%
  mutate(key = ifelse(key == "chAHC", "AHC", "BHC"),
         years = factor(years, levels = levels(relpovdata$years)))

matdepch$years[is.na(matdepch$years)] <- "2009-12"

matdeppn <- formatyears(pendep) %>%
  get3yrperiod() %>%
  mutate(value = get3yrmean(pct_pn)) %>%
  select(years, value) %>%
  tail(-2L) %>%
  mutate(key = "AHC",
         years = factor(years, levels = levels(relpovdata$years)))


## Equality charts ----

genderwa <- genderdata %>%
  select(years, waml, wafm, walm) %>%
  gather(key, value, -years) %>%
  mutate(key = ifelse(key == "waml", "Men", 
                      ifelse(key == "wafm", 
                             "Women with no children", 
                             "Lone mothers")),
         key = factor(key, levels = c("Men", 
                                      "Women with no children", 
                                      "Lone mothers")))

genderpn <- genderdata %>%
  select(years, pnml, pnfm) %>%
  gather(key, value, -years) %>%
  mutate(key = ifelse(key == "pnml", "Male pensioners", 
                      "Female pensioners"),
         key = factor(key, levels = c("Male pensioners", 
                                      "Female pensioners")))

disability <- disabilitydata %>%
  select(years, nodis, dis) %>%
  gather(key, value, -years) %>%
  mutate(key = ifelse(key == "nodis", "No-one disabled", 
                             "Someone disabled"),
         years = factor(years, levels = levels(relpovpp$years)))

disabilityalt <- disabilitydata %>%
  select(years, nodisalt, disalt) %>%
  gather(key, value, -years) %>%
  mutate(key = ifelse(key == "nodisalt", "No-one disabled", 
                      "Someone disabled"),
         years = factor(years, levels = levels(relpovpp$years)))

marital <- maritaldata %>%
  gather(key, value, -years) %>%
  mutate(key = factor(key, levels = c("Married",
                                      "Cohabiting",
                                      "Single",
                                      "Widowed",
                                      "Divorced")),
         years = factor(years, levels = levels(relpovpp$years)))

ethnic <- ethnicpov %>%
  mutate(key = factor(key, 
                      levels = c("All", 
                                 "White - British",
                                 "White - Other", 
                                 "Mixed, Black or Black British and Other",
                                 "Asian or Asian British"))) 
religion <- religionpov %>%
  mutate(key = factor(key, 
                      levels = c("All",
                                 "No Religion",
                                 "Church of Scotland",
                                 "Roman Catholic",
                                 "Other Christian",
                                 "Muslim",           
                                 "Other Religion")))

## Income charts ----

medianspp <- formatyears(medians) %>%
  get3yrperiod() %>%
  mutate(AHC = get3yrmean(median_pp_ahc),
         BHC = get3yrmean(median_pp_bhc)) %>%
  select(years, AHC, BHC) %>%
  tail(-2L) %>%
  gather(key, value, -years)

decilespp <- formatyears(deciles) %>%
  get3yrperiod() %>%
  mutate(dec10 = get3yrmean(bhcdec10),
         dec20 = get3yrmean(bhcdec20),
         dec30 = get3yrmean(bhcdec30),
         dec40 = get3yrmean(bhcdec40),
         dec50 = get3yrmean(bhcdec50),
         dec60 = get3yrmean(bhcdec60),
         dec70 = get3yrmean(bhcdec70),
         dec80 = get3yrmean(bhcdec80),
         dec90 = get3yrmean(bhcdec90),
         dec100 = get3yrmean(bhcdec100)) %>%
  select(years, dec10:dec100) %>%
  tail(-2L) %>%
  gather(key, value, -years) %>%
  mutate(key = ordered(key)) %>%
  filter(years >= "2013-16")

decilepoints <- decilespp %>% 
  filter(years == max(years)) %>%
  mutate(xpos = lag(value) + 1/2*(value-lag(value)),
         xpos = ifelse(is.na(xpos), value/2, xpos),
         xpos = ifelse(key == "dec100", (lag(value) + 100), xpos))

dist <- distribution  %>%
  rename(income = bhc,
         ch = GS_NEWCH,
         wa = GS_NEWWA,
         pn = GS_NEWPN,
         pp = GS_NEWPP) %>%
  filter(income > 0,
         income < 1200) %>%
  gather(key, value, -income) %>%
  arrange(income) 

medians4dist <- medians4dist %>%
  summarise(UKmedian = mean(UKmed_bhc),
            povthresh = mean(povthresh),
            Scotmedian = mean(Scotmed_bhc))
UKmedian <- medians4dist[["UKmedian"]]
Scotmedian <- medians4dist[["Scotmedian"]]
povthresh <- medians4dist[["povthresh"]]

sourcespp <- sources %>%
  filter(decile!= ".") %>%
  gather(key, value, -year, -decile) %>%
  group_by(decile, key) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  mutate(decile = as.numeric(decile),
         key = ifelse(key == "benratio", "Social security", 
                      ifelse(key == "earnsratio", "Earnings",
                             ifelse(key == "penratio", "Occupational pensions",
                                    ifelse(key == "invratio", "Investments",
                                           "Other")))),
         key = factor(key, levels = c("Earnings", 
                                      "Investments", 
                                      "Occupational pensions", 
                                      "Other", 
                                      "Social security")),
         key = fct_rev(key)) %>%
  arrange(decile, key)

palmabhc <-  formatyears(palma) %>%
  get3yrperiod() %>%
  mutate(value = get3yrmean(s90s40)) %>%
  select(years, value) %>%
  tail(-2L)

