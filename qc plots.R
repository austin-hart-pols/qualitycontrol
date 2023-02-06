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
  library(lme4)

  ## ggplot theme
  mytheme = theme_classic(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()
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
    scale_y_continuous(limits = c(-15,30)) +
    scale_x_continuous(limits = c(-2,8)) +
    labs(x = "RDI growth (%)",
         y = "Inc-party margin",
         title = 'Across elections',
         caption = 'US Pres, 1948-2008') +
    mytheme

  rm(df)
  
  
# COMP FILE -----------------  

  ## data
  load('CoMP PxR.RData') # choice-level

  ## Estimates (20mins w ind REs; 45 w/nested)
  comp$parfam = as.character(comp$p.family)
  m3 = glmer(vote ~ ideogap + parfam +
               p.exec * econ + (1 | e.id/r.id),
             data = comp, family = binomial(link = 'logit'))

  pbase = m3@frame
  pbase$pp = predict(m3,type = 'response')
  pbm = pbase %>% 
    filter(p.exec==1) %>%
    group_by(econ) %>%
    summarise(
      n = n(),
      mean = mean(pp)
    )

p2 = pbase %>%
  filter(p.exec == 1) %>%
  ggplot(aes(x = econ, y = pp)) +
  geom_jitter(alpha=.2, color='gray', shape=1) +
  geom_line(data=pbm, aes(x=econ, y=mean), color = 'red') +
  geom_point(data = pbm, aes(x = econ, y = mean), color = 'black') +
  scale_y_continuous(limits = c(0,1), breaks = c(0,.5,1)) +
  scale_x_continuous(breaks = c(0,.5,1)) +
  labs(y = 'Pr(Vote|INC)',
       x = 'Economy, past year',
       title = 'Across voters & elections', 
       caption = 'ME logit, n = 875,445, k = 100') +
  mytheme

rm(comp,m3,pbase,pbm)


# MEXICO -------------------------

mx = haven::read_dta('mx06reg.dta')

mxm = lm(thermdiff ~ therm2 + ediff + econ1 + pid1 + ideology1, data = mx)

p4 = ggpredict(mxm, 'ediff') %>% 
  plot() + 
  scale_y_continuous(breaks = c(-30,-20,-10)) +
  scale_x_continuous(breaks = c(-.5,0,.5)) +
  labs(x=expression(Delta~"Econ eval, w1-2"),
       y=expression(Delta~"Incumb Fav, w2-3"),
       title = 'Within voter',
       caption = 'Mexico 2006, n = 1,980') + 
  mytheme
  
rm(mx,mxm)


library(patchwork)
layout <- "
AABBB
AACCC
"
rvplot = p1 + p2 + p4 + plot_layout(design = layout)
ggsave(
  filename ='rvplot.svg',
  plot = rvplot,
  height = 4, width = 7, dpi = 900
)
