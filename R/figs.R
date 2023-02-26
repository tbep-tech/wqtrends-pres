library(wqtrends)
library(tidyverse)
library(gratia)
library(here)
library(sf)
library(tbeptools)
library(ggmap)
library(patchwork)
library(EnvStats)
library(lubridate)

# gam example ---------------------------------------------------------------------------------

data(rawdat)

tomod <- rawdat %>% 
  filter(station %in% 30) %>% 
  filter(param %in% 'chl')

mod <- anlz_gam(tomod, trans = 'log10')

yrstr1 <- 1990
yrend1 <- 2007
yrstr2 <- 2007
yrend2 <- 2019
doystr <- 213
doyend <- 304

ylab <- 'Chl-a (ug/L)'
alpha <- 1

# get predictions
prds <- anlz_prd(mod)

# get transformation
trans <- unique(prds$trans)

# raw data
tobacktrans <- mod$model %>% 
  dplyr::mutate(
    trans = mod$trans
  )

moddat <- anlz_backtrans(tobacktrans) %>% 
  dplyr::mutate(
    date = lubridate::date_decimal(cont_year), 
    date = as.Date(date)
  )

# transformation used
trans <- mod$trans

# get seasonal averages
avgseason <- anlz_avgseason(mod, doystr = doystr, doyend = doyend) 

# get mixmeta models
mixmet1 <- anlz_mixmeta(avgseason, yrstr = yrstr1, yrend = yrend1)
mixmet2 <- anlz_mixmeta(avgseason, yrstr = yrstr2, yrend = yrend2)

# title
dts <- as.Date(c(doystr, doyend), origin = as.Date("2000-12-31"))
strt <- paste(lubridate::month(dts[1], label = T, abbr = T), lubridate::day(dts[1]))
ends <- paste(lubridate::month(dts[2], label = T, abbr = T), lubridate::day(dts[2]))
ttl <- paste0('Fitted averages with 95% confidence intervals: ', strt, '-',  ends)

# plot objects
toplo1 <- avgseason

toplo2 <- data.frame(
  yr = seq(yrstr1, yrend1, length = 50)
) %>% 
  dplyr::mutate( 
    avg = predict(mixmet1, newdata = data.frame(yr = yr)), 
    se = predict(mixmet1, newdata = data.frame(yr = yr), se = T)[, 2], 
    bt_lwr = avg - 1.96 * se,
    bt_upr = avg + 1.96 * se,
    bt_avg = avg
  )

# subtitle info
pval1 <- coefficients(summary(mixmet1)) %>% data.frame %>% .[2, 4] %>% anlz_pvalformat()
pval2 <- coefficients(summary(mixmet2)) %>% data.frame %>% .[2, 4] %>% anlz_pvalformat()

dispersion <- summary(mod)$dispersion

# backtransform mixmeta predictions
toplo2a <- data.frame(
  yr = seq(yrstr1, yrend1, length = 50)
) %>% 
  dplyr::mutate( 
    avg = predict(mixmet1, newdata = data.frame(yr = yr)), 
    se = predict(mixmet1, newdata = data.frame(yr = yr), se = T)[, 2], 
    bt_lwr = 10^((avg - 1.96 * se) + log(10) * dispersion / 2),
    bt_upr = 10^((avg + 1.96 * se) + log(10) * dispersion / 2),
    bt_avg = 10^(avg + log(10) * dispersion / 2)
  )

# backtransform mixmeta predictions
toplo2b <- data.frame(
  yr = seq(yrstr2, yrend2, length = 50)
) %>% 
  dplyr::mutate( 
    avg = predict(mixmet2, newdata = data.frame(yr = yr)), 
    se = predict(mixmet2, newdata = data.frame(yr = yr), se = T)[, 2], 
    bt_lwr = 10^((avg - 1.96 * se) + log(10) * dispersion / 2),
    bt_upr = 10^((avg + 1.96 * se) + log(10) * dispersion / 2),
    bt_avg = 10^(avg + log(10) * dispersion / 2)
  )

# for subtitle
slope1 <- lm(bt_avg ~ yr, toplo2a) %>% summary %>% coefficients %>% .[2, 1]
slope1 <- round(slope1, 2)
logslope1 <- summary(mixmet1)$coefficients[2, c(1, 5, 6)]
logslope1 <- round(logslope1, 2)
logslope1 <- paste0(logslope1[1], ' (', logslope1[2], ', ', logslope1[3], ')')
subttl1 <- paste0('Trend from ', yrstr1, ' to ', yrend1, ': approximate slope ', slope1, ', log-slope ', logslope1, ', ', pval1)
slope2 <- lm(bt_avg ~ yr, toplo2b) %>% summary %>% coefficients %>% .[2, 1]
slope2 <- round(slope2, 2)
logslope2 <- summary(mixmet2)$coefficients[2, c(1, 5, 6)]
logslope2 <- round(logslope2, 2)
logslope2 <- paste0(logslope2[1], ' (', logslope2[2], ', ', logslope2[3], ')')
subttl2 <- paste0('Trend from ', yrstr2, ' to ', yrend2, ': approximate slope ', slope2, ', log-slope ', logslope2, ', ', pval2)


bassz <- 14
ptsz <- 3

pobs <- ggplot2::ggplot(prds, ggplot2::aes(x = date)) + 
  ggplot2::geom_point(data = moddat, ggplot2::aes(y = value), size = 2) +
  # ggplot2::geom_line(ggplot2::aes(y = value), linewidth = 0.75, colour = 'deepskyblue3', alpha = alpha) +
  # coord_cartesian(ylim = c(0,45)) +
  scale_y_log10() + 
  # ggplot2::geom_line(ggplot2::aes(y = value), alpha = alpha, colour = 'deepskyblue3', linewidth = 1) +
  ggplot2::theme_minimal(base_family = 'serif', base_size = bassz) + 
  ggplot2::theme(
    legend.position = 'top', 
    legend.title = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(), 
    # panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  ) + 
  ggplot2::labs(
    y = ylab
  )

yrsmth <- draw(mod) + 
  theme_minimal(base_size = bassz)

p1 <- show_prddoy(mod, ylab = ylab) + 
  theme_minimal(base_size = bassz) + 
  theme(
    panel.grid.minor = element_blank(), 
    legend.position = 'top'
  ) + 
  labs(
    color = NULL
  )

p2 <- ggplot2::ggplot(prds, ggplot2::aes(x = date)) + 
  ggplot2::geom_point(data = moddat, ggplot2::aes(y = value), size = 2) +
  ggplot2::geom_ribbon(aes(ymin = value - sevalue, ymax = value + sevalue), fill = 'deepskyblue3', alpha = 0.5) + 
  ggplot2::geom_line(ggplot2::aes(y = value), linewidth = 0.75, colour = 'deepskyblue3', alpha = alpha) +
  # coord_cartesian(ylim = c(0,45)) +
  scale_y_log10() +
  # ggplot2::geom_line(ggplot2::aes(y = value), alpha = alpha, colour = 'deepskyblue3', linewidth = 1) +
  ggplot2::theme_minimal(base_family = 'serif', base_size = bassz) + 
  ggplot2::theme(
    legend.position = 'top', 
    legend.title = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(), 
    # panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  ) + 
  ggplot2::labs(
    y = ylab
  )

# plot output
p3 <- ggplot2::ggplot(data = toplo1, ggplot2::aes(x = yr, y = bt_met)) + 
  ggplot2::geom_point(colour = 'deepskyblue3', size = ptsz) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = bt_lwr, ymax = bt_upr), colour = 'deepskyblue3') +
  # ggplot2::geom_ribbon(data = toplo2, ggplot2::aes(ymin = bt_lwr, ymax = bt_upr), fill = 'pink', alpha = 0.4) +
  # ggplot2::geom_line(data = toplo2, color = 'pink') +
  ggplot2::theme_minimal(base_family = 'serif', base_size = bassz) + 
  scale_y_log10() + 
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(), 
    panel.grid.major = ggplot2::element_blank(),
    # panel.grid.minor = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    y = ylab
  )

# plot output
p4 <- ggplot2::ggplot(data = toplo1, ggplot2::aes(x = yr)) + 
  ggplot2::geom_point(aes( y = bt_met), colour = 'deepskyblue3', size = ptsz) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = bt_lwr, ymax = bt_upr), colour = 'deepskyblue3') +
  ggplot2::geom_ribbon(data = toplo2a, ggplot2::aes(ymin = bt_lwr, ymax = bt_upr), fill = 'pink', alpha = 0.4) +
  ggplot2::geom_line(data = toplo2a, aes(y = bt_avg), color = 'pink') +
  scale_y_log10() +
  ggplot2::theme_minimal(base_family = 'serif', base_size =bassz) + 
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(), 
    panel.grid.major = ggplot2::element_blank(),
    # panel.grid.minor = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    y = ylab,
    caption = subttl1
  )

# plot output
p5 <- ggplot2::ggplot(data = toplo1, ggplot2::aes(x = yr)) + 
  ggplot2::geom_point(aes(y = bt_met), colour = 'deepskyblue3', size = ptsz) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = bt_lwr, ymax = bt_upr), colour = 'deepskyblue3') +
  ggplot2::geom_ribbon(data = toplo2b, ggplot2::aes(ymin = bt_lwr, ymax = bt_upr), fill = 'pink', alpha = 0.4) +
  ggplot2::geom_line(data = toplo2b, aes(y = bt_avg), color = 'pink') +
  ggplot2::theme_minimal(base_family = 'serif', base_size =bassz) + 
  scale_y_log10() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(), 
    panel.grid.major = ggplot2::element_blank(),
    # panel.grid.minor = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    y = ylab, 
    caption = subttl2
  )

wd <- 9
hi <- 3

png('figs/prddoy.png', height = 4, width = 9, family = 'serif', units = 'in', res = 300)
p1
dev.off()

png('figs/graphabp2.png', height = hi, width = wd, family = 'serif', units = 'in', res = 300)
p2
dev.off()

png('figs/yrsmth.png', height = hi, width = wd / 2, family = 'serif', units = 'in', res = 300)
yrsmth
dev.off()

png('figs/graphabp3.png', height = hi, width = wd, family = 'serif', units = 'in', res = 300)
p3
dev.off()

png('figs/graphabp4.png', height = hi, width = wd, family = 'serif', units = 'in', res = 300)
p4
dev.off()

png('figs/graphabp5.png', height = hi, width = wd, family = 'serif', units = 'in', res = 300)
p5
dev.off()

# trends map ----------------------------------------------------------------------------------

load(file = here('data/tbchlamod.RData'))
load(file = here('data/tbtnmod.RData'))

tbchlamod <- tbchlamod %>% 
  mutate(
    param = 'chla'
  )
tbtnmod <- tbtnmod %>% 
  mutate(
    param = 'tn'
  )

modfit <- bind_rows(tbchlamod, tbtnmod) %>% 
  mutate(
    trn = purrr::pmap(list(station, param, mod), function(station, param, mod){
      
      cat(station, param, 'all \n')
      
      coefs1 <- anlz_metseason(mod, metfun = mean, doystr = 1, doyend = 364, nsim = 1000) %>% 
        anlz_mixmeta(., yrstr = 2012, yrend = 2022) %>% 
        summary() %>% 
        .$coefficients 
      
      pval <- coefs1[2, 4]
      slos <- coefs1[2, 1]
      
      outall <- tibble(
        pval = pval, 
        slos = slos,
        seas = 'seas1'
      )
      
      coefs2 <- anlz_metseason(mod, metfun = mean, doystr = 182, doyend = 304, nsim = 1000) %>% 
        anlz_mixmeta(., yrstr = 2012, yrend = 2022) %>% 
        summary() %>% 
        .$coefficients 
      
      pval <- coefs2[2, 4]
      slos <- coefs2[2, 1]
      
      outsum <- tibble(
        pval = pval, 
        slos = slos, 
        seas = 'seas2'
      )
      
      out <- bind_rows(outall, outsum)
      
      return(out)
      
    })
  ) %>% 
  select(-data, -mod) %>% 
  unnest(trn)

# basemap
dat_ext <- unname(st_bbox(tbseg))
bsmap1 <- get_stamenmap(bbox = dat_ext, maptype = 'terrain-background', zoom = 11)

# change opacity of basemap
mapatt <- attributes(bsmap1)
bsmap1_transparent <- matrix(adjustcolor(bsmap1, 
                                         alpha.f = 0.4), 
                             nrow = nrow(bsmap1))
attributes(bsmap1_transparent) <- mapatt

chlleglab <- expression(paste(log[10], " chl-a change (", italic(mu), "g ", L^-1, yr^-1, ")"))
tnleglab <- expression(paste(log[10], " TN change (mg ", L^-1, yr^-1, ")"))

pthm <- theme_bw(base_family = 'serif', base_size = 14) +
  theme(
    legend.position = 'top',
    legend.box = 'vertical', 
    strip.background = element_blank(),
    axis.title = element_blank(), 
    axis.text = element_text(size = 9),
    strip.text = element_text(size = 14)
  )

toplo <- modfit %>%
  mutate(
    pvalcol = ifelse(pval < 0.05, T, F), 
    coefsgn = sign(slos), 
    coefsgn = factor(coefsgn, levels = c('1', '-1'), labels = c('inc', 'dec')), 
    seas = factor(seas, levels = c('seas1', 'seas2'), labels = c('whole year', 'summer'))
  ) %>% 
  left_join(stations, by = c('segment' = 'bay_segment', 'station' = 'epchc_station'))

toplo1 <- toplo %>% 
  filter(param == 'tn')

toplo2 <- toplo %>% 
  filter(param == 'chla')

p1 <- ggmap(bsmap1_transparent) +
  geom_point(data = toplo1, aes(x = Longitude, y = Latitude, size = abs(slos), fill = slos, shape = coefsgn, color = pvalcol), stroke = 1) +
  facet_grid(~ seas) + 
  scale_fill_gradient2(tnleglab, low = 'green', mid = 'grey',  high = 'tomato1', midpoint = 0) +
  scale_color_manual(values = c(scales::alpha('black', 0), 'black'), guide = 'none', drop = FALSE) +
  coord_map() + 
  scale_shape_manual(values = c(24, 25), guide = 'none', drop = FALSE) +
  pthm +
  scale_size(range = c(1, 6), guide = F) +
  guides(fill = guide_colourbar(barheight = 0.7, barwidth = 16, title.position = 'top', title.hjust = 0.5)) 

p2 <- ggmap(bsmap1_transparent) +
  geom_point(data = toplo2, aes(x = Longitude, y = Latitude, size = abs(slos), fill = slos, shape = coefsgn, color = pvalcol), stroke = 1) +
  facet_grid(~ seas) + 
  scale_fill_gradient2(chlleglab, low = 'green', mid = 'grey',  high = 'tomato1', midpoint = 0) +
  scale_color_manual(values = c(scales::alpha('black', 0), 'black'), guide = 'none', drop = FALSE) +
  coord_map() + 
  scale_shape_manual(values = c(24, 25), guide = 'none', drop = FALSE) +
  pthm +
  scale_size(range = c(1, 6), guide = F) +
  guides(fill = guide_colourbar(barheight = 0.7, barwidth = 16, title.position = 'top', title.hjust = 0.5)) + 
  labs(
    caption = 'Outlines indicate p < 0.05'
  )

p <- p1 + p2 + plot_layout(ncol = 2)
png('figs/trndgam.png', height = 6, width = 11, family = 'serif', units = 'in', res = 300)
p
dev.off()

# kendall and mixmeta trend -----------------------------------------------

load(file = here('data/tbchlamod.RData'))
load(file = here('data/tbtnmod.RData'))

tbchlamod <- tbchlamod %>% 
  mutate(
    param = 'chla'
  )
tbtnmod <- tbtnmod %>% 
  mutate(
    param = 'tn'
  )

modcmp <- bind_rows(tbchlamod, tbtnmod) %>% 
  mutate(
    trn = purrr::pmap(list(station, param, mod, data), function(station, param, mod, data){
      
      cat(station, param, 'all \n')
      
      tomod <- data %>% 
        filter(yr > 2011) %>% 
        mutate(
          mo = month(date)
        )
      
      # run sk test
      ests_sk <- kendallSeasonalTrendTest(log10(value) ~ mo + yr, data = tomod)
      
      outsk <- tibble(
        pval = ests_sk$p.value[2], 
        slos = ests_sk$estimate[2],
        test = 'sk'
      )
      
      # run meta test
      coefs1 <- anlz_metseason(mod, metfun = mean, doystr = 1, doyend = 364, nsim = 1000) %>% 
        anlz_mixmeta(., yrstr = 2012, yrend = 2022) %>% 
        summary() %>% 
        .$coefficients 
      
      outmt <- tibble(
        pval = coefs1[2, 4],
        slos = coefs1[2, 1], 
        test = 'mt'
      )
      
      out <- bind_rows(outsk, outmt)
      
      return(out)
      
    })
  ) %>% 
  select(-data, -mod) %>% 
  unnest(trn)

# basemap
dat_ext <- unname(st_bbox(tbseg))
bsmap1 <- get_stamenmap(bbox = dat_ext, maptype = 'terrain-background', zoom = 11)

# change opacity of basemap
mapatt <- attributes(bsmap1)
bsmap1_transparent <- matrix(adjustcolor(bsmap1, 
                                         alpha.f = 0.4), 
                             nrow = nrow(bsmap1))
attributes(bsmap1_transparent) <- mapatt

chlleglab <- expression(paste(log[10], " chl-a change (", italic(mu), "g ", L^-1, yr^-1, ")"))
tnleglab <- expression(paste(log[10], " TN change (mg ", L^-1, yr^-1, ")"))

pthm <- theme_bw(base_family = 'serif', base_size = 14) +
  theme(
    legend.position = 'top',
    legend.box = 'vertical', 
    strip.background = element_blank(),
    axis.title = element_blank(), 
    axis.text = element_text(size = 9),
    strip.text = element_text(size = 14)
  )

toplo <- modcmp %>%
  mutate(
    pvalcol = ifelse(pval < 0.05, T, F), 
    coefsgn = sign(slos), 
    coefsgn = factor(coefsgn, levels = c('1', '-1'), labels = c('inc', 'dec')), 
    test = factor(test, levels = c('sk', 'mt'), labels = c('Seasonal Kendall', 'GAM/Mixed'))
  ) %>% 
  left_join(stations, by = c('segment' = 'bay_segment', 'station' = 'epchc_station'))

toplo1 <- toplo %>% 
  filter(param == 'tn')

toplo2 <- toplo %>% 
  filter(param == 'chla')

p1 <- ggmap(bsmap1_transparent) +
  geom_point(data = toplo1, aes(x = Longitude, y = Latitude, size = abs(slos), fill = slos, shape = coefsgn, color = pvalcol), stroke = 1) +
  facet_grid(~ test) + 
  scale_fill_gradient2(tnleglab, low = 'green', mid = 'grey',  high = 'tomato1', midpoint = 0) +
  scale_color_manual(values = c(scales::alpha('black', 0), 'black'), guide = 'none', drop = FALSE) +
  coord_map() + 
  scale_shape_manual(values = c(24, 25), guide = 'none', drop = FALSE) +
  pthm +
  scale_size(range = c(1, 6), guide = F) +
  guides(fill = guide_colourbar(barheight = 0.7, barwidth = 16, title.position = 'top', title.hjust = 0.5)) 

p2 <- ggmap(bsmap1_transparent) +
  geom_point(data = toplo2, aes(x = Longitude, y = Latitude, size = abs(slos), fill = slos, shape = coefsgn, color = pvalcol), stroke = 1) +
  facet_grid(~ test) + 
  scale_fill_gradient2(chlleglab, low = 'green', mid = 'grey',  high = 'tomato1', midpoint = 0) +
  scale_color_manual(values = c(scales::alpha('black', 0), 'black'), guide = 'none', drop = FALSE) +
  coord_map() + 
  scale_shape_manual(values = c(24, 25), guide = 'none', drop = FALSE) +
  pthm +
  scale_size(range = c(1, 6), guide = F) +
  guides(fill = guide_colourbar(barheight = 0.7, barwidth = 16, title.position = 'top', title.hjust = 0.5)) + 
  labs(
    caption = 'Outlines indicate p < 0.05'
  )

p <- p1 + p2 + plot_layout(ncol = 2)
png('figs/trndcmp.png', height = 6, width = 11, family = 'serif', units = 'in', res = 300)
p
dev.off()

# prdseries for each gam --------------------------------------------------

load(file = here('data/tbchlamod.RData'))
load(file = here('data/tbtnmod.RData'))

tbchlamod <- tbchlamod %>% 
  mutate(
    param = 'chla'
  )
tbtnmod <- tbtnmod %>% 
  mutate(
    param = 'tn'
  )

toprd <- bind_rows(tbchlamod, tbtnmod)

for(i in 1:nrow(toprd)){
  
  cat(i, 'of', nrow(toprd), '\n')
  
  dat <- toprd[i, ]
  prm <- dat$param
  modin <- dat$mod[[1]]
  sta <- dat$station
  flnm <- paste0(prm, 'sta', sta, '.png')  
  
  ylb <- case_when(
    prm == 'tn' ~ 'TN (mg/L)',
    prm == 'chla' ~ 'chl-a (ug/L)'
  )
  
  plt <- show_prdseries(modin, ylab = ylb) + 
    labs(
      subtitle = paste('Station', dat$station)
    )
  
  png(filename = here('figs', flnm), height = 2, width = 7, units = 'in', res = 200)
  print(plt)
  dev.off()
  
}