# ****************************
# Quality Control
# Figures for slides
# 
# Austin Hart
# ****************************

## Required packages
library(tidyverse)
library(magrittr)
library(ggeffects)
library(alpaca)
library(lme4)
library(patchwork)

## ggplot theme
  mytheme = theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  
# US PRES -------------------
  
  ## data  
  df = 
    haven::read_dta('presidents.dta') %>%
    mutate(
      margin = # incumbent vote margin
        if_else(dem_inc==1, demvote-repvote,repvote-demvote)
    )

  ## plot
  p1 =
    df %>%
    ggplot(aes(x=rdi, y=margin)) +
    geom_text(aes(label=year), size = 4) +
    geom_smooth(method = 'lm', se = F, 
                linetype=2, color = 'slateblue') +
    scale_y_continuous(limits = c(-15,25)) +
    scale_x_continuous(limits = c(-2,8)) +
    labs(x = "RDI growth (%)",
         y = "Incumbent party vote margin") +
    mytheme +
    theme(axis.line.x = element_line(color = 'gray20'))

  rm(df)
  
# COMP file -----------------  

  ## data
  load('CoMP PxR.RData') # choice-level
  ci = comp %>% # individual-level
    filter(p.exec == 1) %>%
    select(1:4,8:12,14)

  t1a = ci %>%
    group_by(econ) %>%
    summarize(
      n = n(),
      vote = mean(vote)
    )

t1a %>%
  ggplot(aes(x = econ, y = vote)) +
  geom_point(aes(size = n), shape = 21, fill = 'gray') +
  geom_smooth(data = ci, method = 'lm', se = F) +
  scale_size_continuous(range = c(2,8)) +
  scale_x_continuous(breaks=c(0, 0.5, 1), 
                     labels=c("Much\nWorse", "Same", "Much\nBetter")) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,.5,1),
                     labels = scales::percent) +
  labs(x="National economy in past year...", 
       y="Vote for incumbent",
       title = "Individual level (n = 77,354)") +
  mytheme

  
  
  ci %>%
    ggplot(aes(x = econ, y = vote)) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"),
                se = F) +
    scale_x_continuous(breaks=c(0, 0.5, 1), 
                       labels=c("Much\nWorse", "Same", "Much\nBetter")) +
    scale_y_continuous(limits = c(0,1), breaks = c(0,.5,1)) +
    labs(x="National economy in past year...", 
         y="Pr(Vote to reelect)",
         title = "Individual-level (n = 125,000)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())


  ## Estimate models
  e2 = glm(vote ~ econ + education + male + ideology + as_factor(e.id) + e.yr,
           data = ci, family = binomial(link = 'logit')) # Inc Full
    pp1 = tibble(ggpredict(e2,'econ')) # predicted probabilities
  
  e4 = glm(vote ~ wdi.econ + education + male + ideology + as_factor(e.id) + e.yr,
           data = ci, family = binomial(link = 'logit')) # Inc Full
    pp2 = tibble(ggpredict(e4, 'wdi.econ')) %>% # predicted probabilities
      mutate(
        group = '2',
        x = (x-min(x))/(max(x)-min(x))
      )
  
  ## plot
  pdf = 
    bind_rows(pp1,pp2) %>%
    mutate(group = if_else(group == 1, 'Ind Eval','GNI growth'))
    
  pdf %>%
    ggplot(aes(x = x, y = predicted, linetype = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                color = NA, fill = 'gray', alpha = 0.3) +
    geom_line(color = 'black') +
    scale_y_continuous(breaks = c(0,.5,1)) +
    scale_color_manual(values = c('gray', 'black')) +
    labs(x = 'Economy, past year',
         y = 'Vote to reelect government',
         title = 'Predicted probabilities, logit',
         color = 'Measure', fill = 'Measure', linetype = 'Measure') +
    mytheme +
    theme(axis.text.x = element_blank(),
          legend.position = c(.8,.2))

new %>%
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                    ymax = conf.high),
                fill = 'gray80') +
  geom_line() +
  scale_y_continuous(limits = c(0,1))








# Prep data ----------------------------------------------------

# import CoMP PxR data
load("coded data\\CoMP PxR.Rdata")
ci = comp %>%
  filter(econ>=0) %>%
  select(r.id, e.id, p.exec, p.family, vote, outside,
         econ, ideogap) %>%
  mutate( # set executive x economy interactions
    exec.x.econ = p.exec * econ,
    ideogap = if_else(outside == 1, 0, ideogap) # ideogap = 0 for "outside" options
  ) %>%
  rename_with(~gsub(".", "_", .x, fixed = TRUE))
 
  haven::write_dta(ci, path = 'CoMPixp23.dta')
  
# drop uncoded elections; set vars
comp %<>%
  mutate( # set executive x economy interactions
    exec.x.econ = p.exec * econ,
    ideogap = if_else(outside == 1, 0, ideogap) # ideogap = 0 for "outside" options
  )


# Estimate -----------------------------------------------------

# baseline  
m2 = 
  comp %>% 
  #filter(data != "CES") %>%
  feglm(
    vote ~ ideogap + outside + as.factor(p.family) + 
      p.exec + exec.econ | r.id + e.id | e.id,
    data = .,
    binomial("logit")
  )  

summary(m2, type = 'clustered', cluster = ~e.id)
rm(m2)  










