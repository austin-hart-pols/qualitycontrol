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
    scale_x_continuous(limits = c(-1,7)) +
    labs(x = "Income growth (%)",
         y = "Incumbent party vote margin") +
    theme_classic(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()
    )

  
# COMP file -----------------  

  ## data
  vf = 
    read_csv(file = "xmerged.csv") %>%
    filter(!econ %in% c(NA, 0.125, 0.875)) 

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
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

  
  
  ci %>%
    ggplot(aes(x = econ, y = vote)) +
    geom_smooth(se = F, method = "glm", method.args = list(family = "binomial")) +
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



e2 = glm(govvote ~ econ + edu + female + ideo + as_factor(mpcode) + yr,
         data = vf, family = binomial(link = 'logit')) # Inc Full

e4 = glm(govvote ~ wdi_gdpcapgr + edu + female + ideo + as_factor(mpcode) + yr,
         data = vf, family = binomial(link = 'logit')) # Inc Full

plot(ggpredict(e2, "econ"))
plot(ggpredict(e4,'wdi_gdpcapgr'))

new = tibble(ggpredict(e4,'wdi_gdpcapgr'))
new2 = tibble(ggpredict(e2, 'econ'))
new %>%
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                    ymax = conf.high),
                fill = 'gray80') +
  geom_line() +
  scale_y_continuous(limits = c(0,1))



# Set workspace -----------------------------------------------

# load packages 
library(magrittr)
library(tidyverse)
library(alpaca)

# directory
setwd("C:\\Users\\ahart\\Dropbox\\CoMP data")


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
m2 <- 
  ci %>% 
  #filter(data != "CES") %>%
  feglm(
    vote ~ ideogap + outside + as.factor(p_family) + p_exec + 
      exec_x_econ | r_id + e_id | e_id,
    data = .,
    binomial("logit")
  )  

summary(m2, type = 'clustered', cluster = ~e_id)
rm(m2)  

#m3 <- biasCorr(m2)
#  summary(m3)









