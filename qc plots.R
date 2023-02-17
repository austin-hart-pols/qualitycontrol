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
  library(scales)
  library(lme4)
  library(showtext)
  
  font_add_google('Noto Sans')
  showtext_auto()
  
  ## ggplot theme
  mytheme = theme_minimal(base_size = 11) +
    theme(
      text=element_text(family = 'Noto Sans'),
      plot.title = element_text(face = 'bold'),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.text = element_blank(),
      plot.title.position = 'plot',
      plot.margin=unit(c(.2,.2,.2,.2),"cm")
    )
  

# Context slide -----------------------
  
  ## US PRES ----------------
  
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
  
  
  ## COMP FILE -------------------  

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

  
  ## MEXICO ----------------------

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
    labs(x=expression(Delta~"Econ eval, pre"),
         y=expression(Delta~"Inc Fav, post"),
         title = 'Voters * panel wave',
         subtitle = 'Mexico 2006, n = 1,980') + 
    mytheme
  
  rm(mx,mxm)


  ## COMPOSITE -------------------

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

# Chap 4 ------------------------------  

  load('data/ch4 data.Rdata')

  ## Study 1  ---------------
  
  # Plot distribution of Incumbent types
  a = c4[['s1']] %>%
    select(Avg, Ya) %>%
    pivot_longer(cols = c(Avg, Ya)) %>%
    ggplot(aes(x = fct_rev(name), y = value, 
               fill = fct_rev(name))) +
    geom_hline(aes(yintercept = 1200), color = 'gray20') +
    geom_dotplot(binaxis='y', stackdir='center', alpha = .7, 
                 dotsize = .8, binwidth = 12) +
    scale_x_discrete(labels = c('Assigned \n type', 'Avg \n output')) +
    scale_y_continuous(limits = c(820,1580), breaks = seq(950,1450,250),
                       expand = expansion(mult = c(0, 0))) +
    scale_fill_viridis_d(option = 'cividis', direction = -1) +
    labs(x = NULL, y = 'Units per week',
         fill = NULL,
         title = 'Incumbent competence') +
    mytheme + theme(legend.position = 'none', 
                    axis.text = element_text(),
                    panel.grid = element_blank())
  
  
  # Plot retention functions
  b = c4[['s1']] %>%
    ggplot(aes(x = Avg, y = Vote)) +
    geom_hline(yintercept = 0.50, color = 'gray50') +
    geom_smooth(method = "glm", 
                method.args = list(family = "binomial"), 
                color = '#00204DFF') +
    labs(x = "Average output", y = "Vote to reappoint (pr)", 
         title = 'Response to treatment') +
    scale_x_continuous(limits = c(800,1600), breaks = c(900,1200,1500)) +
    scale_y_continuous(breaks = seq(0,1,0.5), limits = c(0,1)) +
    mytheme + theme(legend.position = 'none',
                    axis.text = element_text(),
                    panel.grid = element_blank())
  
  # combine plots and export  
  (study1sum = a + b)
  
  # Export
  ggsave(
    filename ='figures/study1sum.svg',
    plot = study1sum,
    height = 5, width = 6, dpi = 400
  )
  
  rm(a,b,study1sum)
  
  
  ## Study 2 ----------------
  
  # Graph retention functions
  a =
    c4[['s2']] %>%
    ggplot(aes(x = Avg, y = 100*Vote, fill = Noise, linetype = Noise, color = Noise)) +
    geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=0.6, aes(color = NULL)) +
    geom_line(stat='smooth', method = "lm", color = 'black') +
    labs(x = "Incumbent avg", y = "Vote to reappoint (%)", title = 'A. Retention functions',
         fill = 'Variance', linetype = 'Variance', color = 'Variance') +
    scale_x_continuous(limits = c(800,1600), breaks = c(900,1200,1500)) +
    scale_y_continuous(breaks = seq(0,100,25)) +
    scale_color_manual(values = c('gray30','black'), labels = c('High','Low')) +
    scale_fill_viridis_d(option = 'cividis',labels = c('High','Low')) +
    scale_linetype_manual(values = c(1,2),labels = c('High','Low')) +
    coord_cartesian(ylim = c(0, 102)) +
    theme_minimal() + 
    theme(legend.position = c(.8,.2), 
          plot.title.position = 'plot',
          plot.margin = unit(c(0,20,0,0), "pt"))   
  
  # Comparison with Study 1
  c12 = bind_rows(
    c4$s1 %>% select(study, Vote, Avg00), 
    c4$s2 %>% filter(Noise == 'High variance') %>% select(study, Vote, Avg00)
  )
  
  d = c12 %>%
    split( f = .$study ) %>%
    map_df(
      ~ broom::tidy( lm(Vote ~ Avg00, data = .x), ),
      .id = 'study'
    ) %>%
    filter( term != '(Intercept)' ) %>%
    select( -term, -p.value, -statistic ) %>%
    ggplot(aes(x = study, y = estimate)) +
    geom_col(fill = '#00204DFF', alpha = .6) +
    geom_errorbar(aes(ymin = estimate - 1.97*std.error, ymax = estimate + 1.97*std.error),
                  width = 0.1, size = .5, color = 'black') +
    scale_x_discrete(labels = c('Study 1', 'Study2')) +
    scale_y_continuous(limits = c(0,0.21), breaks = seq(0,.2,.1), 
                       expand = expansion(mult = c(0, 0))) +
    labs(x = 'High-variance Incumbent', y = 'Effect of Inc avg',
         title = 'B. Comparison to Study 1') +
    theme_minimal() + theme(plot.title.position = 'plot', 
                            panel.grid.major.x = element_blank())
  
  summary(lm(Vote ~ Avg00 * study, data = c12))
  
  # Combine plots
  study2sum = a + (d / table_png) &
    theme(plot.title = element_text(size = 10, face = 'bold'),
          axis.title = element_text(size = 9),
          axis.text = element_text(size = 8),
          legend.title = element_text(size = 9))
  study2sum
  
  ggsave(study2sum, file = 'study2sum.png', 
         width = 5, height = 6, dpi = 400) 
  
  rm(a,b,d,c12,study2sum,design)  
  
  
  ## Study 3 ----------------
  
  # Negativity bias from prior studies
  # gather data
  c12 = bind_rows(
    c4$s1 %>% select(study,Vote,Avg00,starts_with('w')),
    c4$s2 %>% select(study,Vote,Avg00,starts_with('w'))
  )
  
  # Plot trends for exposition
  s3i = read_csv('exp3 trends.csv')
  a =
    s3i %>%
    filter(treat != 'NeutralB') %>%
    ggplot(aes(x = wk, y = output, shape = treat, color = treat, fill = treat)) +
    geom_hline(aes(yintercept = 0), color = 'gray40') +
    geom_line(size = 1) +
    geom_point(size = 2, color = 'gray40') +
    scale_y_continuous(breaks = seq(-500,500,250)) +
    coord_flip() +
    labs(y = 'Weekly output \n (vs factory average)', x = NULL,
         shape = NULL, color = NULL, fill = NULL,
         title = 'A. Treatment profiles') +
    scale_color_viridis_d(option = 'D', direction = -1) +
    scale_fill_viridis_d(option = 'D', direction = -1) +
    scale_shape_manual(values = c(21,22,23,24)) +
    mytheme + theme(axis.text.y = element_blank(), 
                    legend.position = c(0.83,0.9),
                    plot.margin = unit(c(0,10,0,0), "pt"))
  
  # Plot estimates
  # generate model ests
  m = c4$s3 %>% filter(IP_block == 0 & IP_country == 'United States') %>% 
    lm(Vote ~ Type + 0, data = .)
  
  # estimates to tidy frame
  c = summary(m)$coefficients %>%
    data.frame(.) %>%
    select(1,2) %>%
    mutate(coef = c(1050,1200,1350), est = Estimate, se = Std..Error, .keep = 'unused') %>%
    ggplot(aes(x = coef, y = est, color = as.factor(coef), fill = as.factor(coef), shape = as.factor(coef))) +
    geom_ribbon(inherit.aes = F, data = c12, aes(y = Vote, x = Avg00*100),
                stat='smooth', method = "lm", alpha=0.1, color = '#00204DFF', linetype = 3) +
    geom_line(inherit.aes = F, data = c12, aes(y = Vote, x = Avg00*100),
              stat='smooth', method = "lm", color = '#00204DFF') +
    geom_errorbar(aes(ymin = est - 1.97*se, ymax = est + 1.97*se),
                  width = 0, size = 1) +
    geom_point(size = 2, color = 'gray40') +
    scale_color_viridis_d(option = 'D', direction = -1) +
    scale_fill_viridis_d(option = 'D', direction = -1) +
    scale_shape_manual(values = c(21,22,23)) +
    scale_y_continuous(breaks = seq(0,1,0.5), labels = c(0,50,'100%')) +
    scale_x_continuous(breaks = c(1050,1200,1350), labels = c('Neg','Neutral','Pos')) +
    coord_cartesian(ylim = c(0.03, 1.03),xlim = c(900,1500)) +
    labs(x = 'Incumbent avg', y = 'Vote to reappoint',
         title = 'D. Negativity bias in context') +
    mytheme + theme(plot.title.position = 'plot', 
                    plot.margin = unit(c(0,0,10,0), "pt"),
                    panel.grid.major.x = element_blank(),
                    legend.position = 'none',
                    plot.caption.position = 'plot',
                    plot.caption = element_text(hjust = 0))
  
  # text screen    
  text = paste("Week 7","Worker A produces 176 units","below the historical average.",sep='\n')
  p.t = tibble(x = 0:1,y=0:1) %>%
    ggplot(aes(x,y)) +
    annotate(geom = 'text', x = .01, y = .5, size = 2.5, label = text,hjust=0) + 
    coord_cartesian(xlim = c(0,1),ylim=c(0,1)) +
    labs(title = 'B. Output screen') +
    theme_void() +
    theme(
      plot.title.position = 'plot',
      plot.title = element_text(hjust = 0, size = 10, family = "sans", face = 'bold')
    )
  
  
  design = "ABD
            AC#"
  # Combine plots
  study3sum = a + p.t + table_png + c + plot_layout(design = design)
  
  ggsave(study3sum, file = 'study3sum.png', 
         width = 7, height = 3.5, dpi = 1000) 
  
  rm(m,mn,mnames,a,b,c,c12,study3sum,table_png,tmp,s3i)  
  
  
  ## Study 4 ---------------------
  
  # Profile of incumbents
  s4i = read_csv('data/exp4 profiles.csv')
  a = s4i %>%
    ggplot(aes(x = wk, y = output, shape = treat, linetype = treat, color = treat, fill = treat)) +
    geom_line(linewidth = 1) +
    geom_hline(aes(yintercept = 1200), color = 'gray50') +
    geom_segment(inherit.aes = F, aes(x = 5, xend = 20, y = 1580, yend = 1580),color = '#00204DFF') +
    geom_segment(inherit.aes = F, aes(x = 1, xend = 16, y = 500, yend = 500), color = '#FFEA46FF') +
    geom_point(size = 2, color = 'gray40') +
    scale_shape_manual(values = c(21,22,21)) +
    scale_color_manual(values = c('#FFEA46FF','#7C7B78FF','#00204DFF')) +
    scale_fill_manual(values = c('#FFEA46FF','#7C7B78FF','#00204DFF')) +
    scale_linetype_manual(values = c(2,1,2)) +
    scale_y_continuous(breaks = seq(600,1500,300), limits = c(390,1710)) +
    labs(x = NULL, y = 'Weekly output', title = 'Treatment profiles') +
    mytheme + theme(legend.position = 'none',
                    panel.grid = element_blank(),
                    axis.text.y = element_text(),
                    plot.title.position = 'plot') +
    annotate("text", x = 12.5, y = 1670, label = 'Late drop incumbent', size = 3) +
    annotate("text", x = 8.5, y = 410, label = 'Early drop incumbent', size = 3)
  

  # Plot results of some sort
  # generate model ests
  m = lm(Vote ~ Order, data = c4$s4) %>%
    ggpredict(., terms = c("Order")) %>% tibble() %>%
    mutate(x = factor(c('Early','Random','Late'), levels = c('Early','Random','Late')))
  
  
  b = m %>%
    ggplot(aes(x = x, y = predicted, 
               fill = x, color = x, shape = x)) +
    geom_hline(aes(yintercept = 0.5), color = 'gray20') +
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                  width = 0.05, linewidth = 1) +
    geom_point(size = 2, color = 'gray25') +
    scale_shape_manual(values = c(21,22,21)) +
    scale_color_manual(values = c('#FFEA46FF','#7C7B78FF','#00204DFF')) +
    scale_fill_manual(values = c('#FFEA46FF','#7C7B78FF','#00204DFF')) +
    scale_y_continuous(breaks = seq(0,1,0.5), labels = scales::percent,
                       limits = c(-0.01,1),
                       expand = expansion(mult = c(0, 0.03))) +
    labs(x = NULL, y = 'Reappoint',
         title = 'Response to treatment',
         shape = 'Treatment',
         fill = 'Treatment',
         color = 'Treatment') +
    mytheme + theme(axis.text.y = element_text(),
                    panel.grid = element_blank())
  
  # combine plots
  design = "AA
            BC"
  (study4sum = a + b + guide_area() + plot_layout(design = design, guides = 'collect'))
  
  ## Export
  ggsave(
    filename ='figures/study4sum.svg',
    plot = study4sum,
    height = 5, width = 6, dpi = 400
  )
 
  
  rm(a,b,m,s4est,study4sum,s4i,design,c4)    

  
# Chap 5 ------------------------------
  
  ## designs ----------------
  
  # mock factory data
  fd = 
    tibble(
      week = 1:16,
      comp = rnorm(16,1400,250),
      inc = c(rep(0,8),rnorm(8,1200,250)),
      fac = comp+inc
    ) %>%
    pivot_longer(-week,names_to = 'group', values_to = 'output')
    
  # extraction design
  pa = filter(fd,group == 'fac') %>%
    ggplot(aes(x=week,y=output)) +
    geom_area(data=filter(fd,group=='fac'&week<=8),fill='gray30') +
    geom_area(data=filter(fd,week>8&group=='fac'),fill='gray30') +
    geom_line(data=filter(fd,week>8&group=='inc'),
              color='red',linewidth=1) +
    geom_vline(aes(xintercept = 8.5),color = 'gray') +
    labs(x = ' ', y='Output',
         title='Extraction design',
         subtitle='Incumbent joins the Comparator') +
    scale_x_continuous(limits = c(0.5,16.5),
                       breaks = c(4.5,12.5),
                       labels = c(expression(F[pre]==C),expression(F[post]==C+I))) +
    scale_y_continuous(limits = c(0,3500)) +
    coord_cartesian(expand =F) +
    mytheme +
    theme(axis.text = element_text())
  
  # recognition design    
  pb = filter(fd, group=='inc') %>%
    ggplot(aes(x=week,y=output)) +
    geom_area(data=filter(fd,group=='inc'&week>8),
              fill='red',alpha=.7) +
    geom_area(data = filter(fd,group=='comp'&week<9),
              fill='gray30') +
    geom_vline(aes(xintercept = 8.5),color = 'gray') +
    labs(x = ' ', y='Output',
         title='Recognition design', 
         subtitle='Incumbent replaces the Comparator') +
    scale_x_continuous(limits = c(0.5,16.5),
                       breaks = c(4.5,12.5),
                       labels = c(expression(F[pre]==C),expression(F[post]==I))) +
    scale_y_continuous(limits = c(0,3500)) +
    coord_cartesian(expand =F) +
    mytheme +
    theme(axis.text = element_text())

  # patchwork
  (multi = pa / pb)

  # Export
  ggsave(
    filename ='figures/multidesign.svg',
    plot = multi,
    height = 5, width = 6, dpi = 400
  )
  
  rm(multi,pa,pb,fd)
  
  ## Study 5 ----------------
  
  load(url("https://www.dropbox.com/s/c9safb3z4bib6wb/HartMatthewsJOP.RData?dl=1"))
  
  df1 = 
    dfmain %>%
    filter(Study == 1) 
  
  # Tests: differences/interaction effect 
  fit = df1 %>%
    mutate(NoiseS1 = relevel(as.factor(NoiseS1), ref = 'Low (100)')) %>%
    glm(VoteB ~ NoiseS1*(AvgA + AvgAB), data = ., family = 'binomial') 
  
    my1 = ggpredict(fit, terms = c("AvgAB [800,900,1000,1100,1200,1300,1400,1500,1600]","NoiseS1")) 
    p.b = ggplot(my1, aes(x, predicted, linetype = group)) +
      geom_hline(aes(yintercept = .5), color = 'gray') +
      stat_smooth(formula = y ~ s(x, k = 8), method = "gam", 
                  se = F, color = 'black') +
      scale_y_continuous(limits = c(0,1), breaks = c(0,.5,1)) +
      scale_x_continuous(breaks = c(900,1200,1500)) +
      scale_linetype_manual(
        values = c('solid','dotted'),
        name = NULL,
      ) +
      labs(
        title = 'Incumbent-era output',
        x = 'Factory Avg',
        y = ' '
      ) +
      mytheme +
      theme(axis.text.x = element_text())
  
    my2 = ggpredict(fit, terms = c("AvgA [800,900,1000,1100,1200,1300,1400,1500,1600]","NoiseS1")) 
    p.a = ggplot(my2, aes(x, predicted, linetype = group)) +
      geom_hline(aes(yintercept = .5), color = 'gray') +
      stat_smooth(formula = y ~ s(x, k = 8), method = "gam", 
                  se = F, color = 'black') +
      scale_y_continuous(limits = c(0,1), breaks = c(0,.5,1)) +
      scale_x_continuous(breaks = c(900,1200,1500)) +
      scale_linetype_manual(
        values = c('solid','dotted'),
        name = NULL,
      ) +
      labs(
        title = 'Pre-Incumbent output',
        x = 'Factory Avg',
        y = 'pr(Vote = Incumbent)'
      ) +
      mytheme +
      theme(axis.text = element_text())
    
    study5sum = p.a + p.b + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
    ggsave(
      filename ='figures/study5sum.svg',
      plot = study5sum,
      height = 5, width = 6, dpi = 400
    )
    
    
# Chap 6 ------------------------------
  
  load('data/ch6 data.Rdata')
    c6f = map(c6, ~filter(., full == TRUE))
  
  # FctCaseWhen() - Create factor from case_when (maintaining level order)    
    FctCaseWhen <- function(...) {
      args <- as.list(match.call())
      levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
      levels <- levels[!is.na(levels)]
      factor(dplyr::case_when(...), levels = levels)
    }
  
  ## Study 8 ----------------

  # get data, combined selections
  b = c6f[['s8']] %>%
    group_by(design) %>%
    count(design,reportCombo) %>%
    mutate(p = 100*n/sum(n),
           value = fct_relevel(
             reportCombo,
             c('Benchmarked','Inc-centered','Irrelevant')
           ))
  
  # get individual selections  
  a = c6f[['s8']] %>%
    select(rep1,rep2,design) %>%
    pivot_longer(1:2) %>%
    count(design,value) %>%
    group_by(design) %>%
    mutate(p = 200*n/sum(n),
           value = case_match(
             value,
             'AvB' ~ 'Inc vs Comp',
             'B' ~ 'Inc avg',
             'Y' ~ 'Bonus',
             'A' ~ 'Comp avg',
             'None' ~ '(none)',
             .ptype = factor(levels = c('Inc vs Comp',
                                        'Inc avg',
                                        'Comp avg',
                                        'Bonus',
                                        '(none)'))
           ))
  
  # viz: selections
  v8a = a %>%
    ggplot(aes(x = value, y = p)) +
    geom_col(aes(fill = fct_rev(design)), position = 'dodge', color = 'black', alpha =.85) +
    labs(
      x = NULL,
      y = NULL,
      fill = 'Task design',
      title = 'Individual reports chosen'
    ) +
    scale_fill_viridis_d(option = 'inferno', direction = 1) +
    scale_y_continuous(breaks = seq(0, 100, 50),
                       labels = c('0','50','100%'),
                       limits = c(0,100),
                       expand = expansion(mult = c(0, .04))) +
    mytheme +
    theme(axis.text = element_text())
  
  # Combination viz  
  v8b = b %>%
    ggplot(aes(x = value, y = p)) +
    geom_col(aes(fill = fct_rev(design)), position = 'dodge', color = 'black', alpha =.85) +
    labs(
      x = NULL,
      y = NULL,
      fill = 'Task design',
      title = 'Combined selection'
    ) +
    scale_fill_viridis_d(option = 'inferno', direction = 1) +
    scale_y_continuous(breaks = seq(0, 100, 50),
                       labels = c('0','50','100%'),
                       limits = c(0,100),
                       expand = expansion(mult = c(0, .04))) +
    mytheme +
    theme(
      plot.title.position = 'plot',
      legend.position = 'none',
      axis.text.y = element_text(),
      axis.text.x = element_text(vjust = grid::unit(c(0, -2, 0), "points"))
    )
  
  # Patchwork
  layout = "
    AAAA
    BBBC
  "
  (study8sum = v8a + v8b + guide_area() + plot_layout(design = layout, guides = 'collect'))
  
  ggsave(
    filename ='figures/study8sum.svg',
    plot = study8sum,
    height = 5, width = 6, dpi = 400
  )

  rm(v8a,v8b,study8sum,a,b,layout,c6,c6f)  
  