# ****************************
# Quality Control
# Figures for slides
# 
# Austin Hart
# ****************************


  ## Required packages
  library(tidyverse)
  library(ggeffects)
  library(patchwork)
  library(lme4)
  library(showtext)
  
  font_add_google('Noto Sans')
  showtext_auto()
  
  ## ggplot theme
  mytheme = theme_minimal(base_size = 11) +
    theme(
      text=element_text(family = 'Noto Sans'),
      plot.title = element_text(hjust = 0.5, face = 'bold'),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.text = element_blank()
    )
  
  
# US PRES -------------------
  
  ## data  
  df = 
    haven::read_dta('data/presidents.dta') %>%
    mutate(
      margin = # incumbent vote margin
        if_else(dem_inc==1, demvote-repvote,repvote-demvote)
    )

  ## plot
  p1 =
    df %>%
    ggplot(aes(x=rdi, y=margin)) +
    geom_text(aes(label=year), size = 3) +
    geom_smooth(method = 'lm', se = F, 
                linetype=2, color = 'red') +
    scale_y_continuous(limits = c(-15,30)) +
    scale_x_continuous(limits = c(-1,7.5)) +
    labs(x = "RDI growth (%)",
         y = "Inc-party margin",
         title = 'Across elections',
         subtitle = 'US Pres, 1948-2008') +
    mytheme

  rm(df)
  
  
# COMP FILE -----------------  

  ## data
  load('data/CoMP PxR.RData') # choice-level
  comp$parfam = as.character(comp$p.family) # for ease in reg
  
  ## Estimates (25mins)
  m3 = glmer(vote ~ ideogap + parfam +
               p.exec * econ + (1 | e.id/r.id),
             data = comp, family = binomial(link = 'logit'))
  
## Predicted prob, individual
  pbase = m3@frame
  pbase$pp = predict(m3,type = 'response')
    rm(comp)
  
  ## Plot
  p2 = pbase %>%
    filter(p.exec == 1) %>%
    ggplot(aes(x = econ, y= pp, group=econ)) +
    geom_hline(aes(yintercept = 0.5), color = 'gray') +
    geom_violin(draw_quantiles = 0.5, scale = 'count', adjust = 2, fill = 'lightblue') +
    scale_y_continuous(limits = c(0,1), breaks = c(0,.5,1)) +
    scale_x_continuous(breaks = c(0,.5,1),
                       labels = c('Worse','Same','Better')) +
    labs(y = 'Pr(Vote|INC)',
         x = 'Economy, past year',
         title = 'Voters * Parties * Elections', 
         subtitle = 'ME logit, n = 875,445, k = 103') +
    mytheme

  rm(m3,pbase)

  
# MEXICO -------------------------

  ## Data
  mx = haven::read_dta('data/mx06reg.dta')

  ## Model
  mxm = 
    lm(thermdiff ~ therm2+ediff+econ1+pid1+ideology1,
       data = mx)

  ## Plot
  p3 = ggpredict(mxm, 'ediff') %>% 
    tibble() %>%
    ggplot(aes(x, y = predicted)) +
    geom_ribbon(aes(ymin=conf.low,ymax=conf.high),fill='darkgreen', alpha = 1/4) +
    geom_line(color = 'darkgreen')+
    scale_y_continuous(breaks = c(-30,-20,-10)) +
    scale_x_continuous(breaks = c(-.5,0,.5)) +
    labs(x=expression(Delta~"Econ eval, w1-2"),
         y=expression(Delta~"Incumb Fav, w2-3"),
         title = 'Voters * panel wave',
         subtitle = 'Mexico 2006, n = 1,980') + 
    mytheme
  
  rm(mx,mxm)


# COMPOSITE ----------------------

  ## layout
  layout = "
  AABBB
  AACCC
  "
  
  ## Arrange
  rvplot = p1 + p2 + p3 + 
    plot_layout(design = layout) &
    theme(plot.margin = margin(4,4,6,4, unit = 'pt'))
  
  ## Export
  ggsave(
    filename ='figures/rvplot.svg',
    plot = rvplot,
    height = 5, width = 7, dpi = 900
  )
