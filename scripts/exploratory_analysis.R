#exploratory analysis to compare fall chinook runs in Hanford Reach, Deschutes River, and Snake River

#pull data from CAX

remotes:::install_github("nwfsc-cb/rCAX@*release")

library(rCAX)
library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)


plot_theme <- readRDS("plot_theme.RDS")

(rcax_hli("NOSA", type = "colnames"))

snake <- rcax_hli("NOSA", flist = list(popid = c(1, 515, 514)), 
                cols = c("commonname", 
                         "run", 
                         "majorpopgroup", 
                         "estimatetype", 
                         "spawningyear", 
                         "nosaij", 
                         "nosaej", 
                         "tsaij", 
                         "tsaej" 
                         ))
head(snake)
glimpse(snake)

#Hanford Reach and Deschutes River are not in CAX
table(snake$majorpopgroup)

snake %>%
  filter(estimatetype == "Escapement") %>%
  ggplot(aes(x = spawningyear, y = nosaij)) + 
  geom_line() + 
  geom_line(aes(y = tsaij), color = "blue")

hanford <- read_xlsx('hanford_deschutes_clean.xlsx', "hanford")
#does hanford reach adult count include hatchery origin? 

deschutes <- read_xlsx('hanford_deschutes_clean.xlsx', "deschutes")  
#Deschutes data also found here: https://www.streamnet.org/data/trends/trend/?trendid=50924#/

hanford %>%
  ggplot(aes(x = year, y = adults)) + 
  geom_line(linewidth = 1) + 
  plot_theme

deschutes %>%
  ggplot(aes(x = year, y = escapement)) + 
  geom_line() + 
  geom_line(aes(y = run_to_river), color = "blue")

snake_clean <- snake %>%
  filter(estimatetype == "Escapement") %>%
  select(spawningyear, nosaej) %>%
  rename(year = spawningyear, 
         total = nosaej) %>%
  mutate(pop = "snake")

deschutes_clean <- deschutes %>%
  select(year, escapement) %>%
  rename(total = escapement) %>%
  mutate(pop = "deschutes")

hanford_clean <- hanford %>%
  select(year, adults) %>%
  rename(total = adults) %>%
  mutate(pop = "hanford")

comp <- rbind(snake_clean, deschutes_clean, hanford_clean)

ggplot(comp, aes(x = year, y = total, group = pop)) + 
  geom_line(linewidth = 1, aes(color = pop)) + 
  plot_theme

comp <- comp %>%
  group_by(pop) %>%
  mutate(zscore = scale(total))

ggplot(comp, aes(x = year, y = zscore, group = pop)) + 
  geom_line(linewidth = 1, aes(color = pop)) + 
  plot_theme + 
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) + 
  scale_y_continuous(limits = c(-2, 5), expand = c(0, 0)) + 
  scale_x_continuous(limits = c(1975, 2025), expand = c(0, 0))

ggplot(comp, aes(x = year, y = total, group = pop)) + 
  geom_line(linewidth = 1, aes(color = pop)) + 
  plot_theme + 
  scale_y_continuous(limits = c(0, 250000), expand = c(0, 0)) + 
  scale_x_continuous(limits = c(1975, 2025), expand = c(0, 0)) + 
  ylab("NOSAej")

ggplot(comp, aes(x = year, y = log(total), group = pop)) + 
  geom_line(linewidth = 1, aes(color = pop)) + 
  plot_theme + 
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_continuous(limits = c(1975, 2025), expand = c(0, 0)) + 
  ylab("log(NOSAej)")


lmm_dat <- comp %>%
  mutate(trt = if_else(year >= 1977 & year <= 1995, "pre", 
                       if_else(year >= 2000 & year <= 2025, "post", NA))) %>%
  filter(!(is.na(trt)))

lmm_fit <- lmer(log(total) ~ pop*trt + (1|pop), data = lmm_dat)
lm_fit <- lm(log(total) ~ pop*trt, data = lmm_dat)

ggplot(lmm_dat, aes(x = pop, y = zscore, color = trt)) + 
  geom_boxplot(aes(color = trt)) + 
  plot_theme

ggplot(lmm_dat, aes(x = pop, y = log(total), fill = trt)) + 
  geom_boxplot(aes(fill = trt)) + 
  plot_theme + 
  ylab("log(NOSAej)") + 
  xlab("Population") + 
  scale_y_continuous(limits = c(4, 14), expand = c(0, 0)) + 
  scale_fill_brewer(palette = "Dark2", labels = c("post" = "Post (2000 - 2025)", "pre" ="Pre (1977 - 1995)"))

lm_trend <- lm(log(total) ~ year*pop*trt, data = lmm_dat)
summary(lm_trend)

lmm_trend <- lmer(log(total) ~ year*pop*trt + (1|pop), data = lmm_dat)
summary(lmm_trend)


#BACI trend figure

ggplot(lmm_dat, aes(x = year, y = log(total), fill = pop, shape = trt)) +
  geom_point(size = 3, color = "black") + 
  plot_theme + 
  #geom_vline(xintercept = 1995) + 
  geom_smooth(method = "lm", aes(color = pop)) + 
  scale_y_continuous(limits = c(4, 14), expand = c(0, 0)) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_shape_manual(values = c(21, 24), 
                     labels = c("post" = "Post (2000 - 2025)", "pre" ="Pre (1977 - 1995)"), 
                     name = ("Period")) + 
  scale_fill_brewer(palette = "Dark2", labels = c("Deschutes", "Hanford", "Snake"), 
                    name = "Population") +
  scale_color_brewer(palette = "Dark2", labels = c("Deschutes", "Hanford", "Snake"), 
                     name = "Population") +
  ylab("log(NOSAej)") + 
  xlab("Year")

ggplot(lmm_dat, aes(x = year, y = (zscore), fill = pop, shape = trt)) + 
  geom_point(size = 3, color = "black") + 
  plot_theme + 
  #geom_vline(xintercept = 1995) + 
  geom_smooth(method = "lm", aes(color = pop)) + 
  #scale_y_continuous(limits = c(4, 14), expand = c(0, 0)) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_shape_manual(values = c(21, 24), 
                     labels = c("post" = "Post (2000 - 2025)", "pre" ="Pre (1977 - 1995)"), 
                     name = ("Period")) + 
  scale_fill_brewer(palette = "Dark2", labels = c("Deschutes", "Hanford", "Snake"), 
                    name = "Population") +
  scale_color_brewer(palette = "Dark2", labels = c("Deschutes", "Hanford", "Snake"), 
                     name = "Population") +
  ylab("log(NOSAej)") + 
  xlab("Year")



