
## read in primary data
dists <- readRDS("temp/shooting_demos.rds")

bws <- readRDS("temp/primary_out_data.rds")

## loop over thresholds
out <- rbindlist(lapply(seq(0.25, 1, 0.05), function(threshold){
  ##keep the observations within the threshold closest to election day
  set_pre <- dists[dists$dist <= threshold & dists$pre,
                   .SD[date %in% max(date)], by=list(year, GEOID)]
  set_pre <- set_pre[, .SD[1], by = .(year, GEOID)] %>% 
    mutate(treated = T)
  
  ##keep the observations within the threshold closest to election day
  set_post <- dists[dists$dist <= threshold & !dists$pre,
                    .SD[date %in% min(date)], by=list(year, GEOID)]
  set_post <- set_post[!(paste0(set_post$GEOID, set_post$year) %in% paste0(set_pre$GEOID, set_pre$year)),
                       .SD[1], by = .(year, GEOID)] %>% 
    mutate(treated = F)
  
  full_treat <- bind_rows(set_pre, set_post) %>% 
    select(GEOID, id, date, dist, year, turnout,
           median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
           some_college, turnout_pre, treated) %>% 
    mutate(d2 = ifelse(year == "2020", as.integer(date - as.Date("2020-11-03")),
                       as.integer(date - as.Date("2016-11-08"))),
           t16 = year == "2016")
  
  
  full_treat <- full_treat[complete.cases(select(full_treat,
                                                 latino, nh_white, asian, nh_black, median_income, median_age,
                                                 pop_dens, turnout_pre, some_college)), ]
  
  ########################
  ## balance within 2016
  d16 <- filter(full_treat, year == "2016")
  
  mb <- ebalance(d16$treated,
                 select(d16,
                        nh_black, latino, nh_white, asian, median_income, median_age,
                        pop_dens, turnout_pre, some_college))
  
  d16 <- bind_rows(
    filter(d16, treated) %>%
      mutate(weight = 1),
    filter(d16, !treated) %>%
      mutate(weight = mb$w)
  )
  
  # balance within 2020
  d20 <- filter(full_treat, year == "2020")
  
  mb <- ebalance(d20$treated,
                 select(d20,
                        nh_black, latino, nh_white, asian, median_income, median_age,
                        pop_dens, turnout_pre, some_college))
  
  d20 <- bind_rows(
    filter(d20, treated) %>%
      mutate(weight = 1),
    filter(d20, !treated) %>%
      mutate(weight = mb$w)
  )
  
  full_treat <- bind_rows(d16, d20)
  
  ########################################
  ## redo the balancing without prior turnout for first-differencing models
  d16 <- filter(full_treat, year == "2016")
  
  mb <- ebalance(d16$treated,
                 select(d16,
                        nh_black, latino, nh_white, asian, median_income, median_age,
                        pop_dens, some_college))
  
  d16 <- bind_rows(
    filter(d16, treated) %>%
      mutate(weight2 = 1),
    filter(d16, !treated) %>%
      mutate(weight2 = mb$w)
  )
  
  d20 <- filter(full_treat, year == "2020")
  
  mb <- ebalance(d20$treated,
                 select(d20,
                        nh_black, latino, nh_white, asian, median_income, median_age,
                        pop_dens, some_college))
  
  d20 <- bind_rows(
    filter(d20, treated) %>%
      mutate(weight2 = 1),
    filter(d20, !treated) %>%
      mutate(weight2 = mb$w)
  )
  
  full_treat2 <- bind_rows(d16, d20) %>% 
    select(GEOID, year, weight2)
  
  full_treat <- left_join(full_treat, full_treat2) %>% 
    mutate(change_to = turnout - turnout_pre)
  ########################################
  
  ## main model with narrowest bandwidths
  la <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                 weights = full_treat$weight,
                 covs = select(full_treat,
                               latino, nh_white, asian,
                               nh_black, median_income, median_age,
                               pop_dens, turnout_pre, t16, some_college), h = min(bws$bw))
  ## main model with widest bandwidths
  lb <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                 weights = full_treat$weight,
                 covs = select(full_treat,
                               latino, nh_white, asian,
                               nh_black, median_income, median_age,
                               pop_dens, turnout_pre, t16, some_college), h = max(bws$bw))
  
  ## rdit without balancing
  l <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                covs = select(full_treat,
                              latino, nh_white, asian,
                              nh_black, median_income, median_age,
                              pop_dens, turnout_pre, t16, some_college))
  
  ## rdit without OLS adjustments
  l2 <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                 weights = full_treat$weight)
  ##rdit without any adjustments at all
  l3 <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id)
  
  ## rdit on first difference in turnout (note the changed dependent variable, different weight, and
  ## noninclusion of turnout_pre in the model)
  l4 <- rdrobust(y = full_treat$change_to, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                 weights = full_treat$weight2,
                 covs = select(full_treat,
                               latino, nh_white, asian,
                               nh_black, median_income, median_age,
                               pop_dens, t16, some_college))
  
  l4b <- rdrobust(y = full_treat$turnout_pre, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                  weights = full_treat$weight2,
                  covs = select(full_treat,
                                latino, nh_white, asian,
                                nh_black, median_income, median_age,
                                pop_dens, t16, some_college))
  
  ## grab nonparametric bandwidth for alternate specifications
  bw <- rdbwselect(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                   weights = full_treat$weight,
                   covs = select(full_treat,
                                 latino, nh_white, asian,
                                 nh_black, median_income, median_age,
                                 pop_dens, turnout_pre, t16, some_college))
  
  ## rdit using double the nonparametric bandwidth
  l5 <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                 weights = full_treat$weight,
                 covs = select(full_treat,
                               latino, nh_white, asian,
                               nh_black, median_income, median_age,
                               pop_dens, turnout_pre, t16, some_college), h = bw[["bws"]][1]*2)
  ## rdit using half the nonparametric bandwidth
  l5b <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                 weights = full_treat$weight,
                 covs = select(full_treat,
                               latino, nh_white, asian,
                               nh_black, median_income, median_age,
                               pop_dens, turnout_pre, t16, some_college), h = bw[["bws"]][1]/2)
  ## rdit using nonparametric bandwidth on treated side, 60 days on untreated side
  l6 <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                 weights = full_treat$weight,
                 covs = select(full_treat,
                               latino, nh_white, asian,
                               nh_black, median_income, median_age,
                               pop_dens, turnout_pre, t16, some_college), h = c(bw[["bws"]][1], 60))
  ## rdit using nonparametric bandwidth on treated side, 90 days on untreated side
  l7 <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                 weights = full_treat$weight,
                 covs = select(full_treat,
                               latino, nh_white, asian,
                               nh_black, median_income, median_age,
                               pop_dens, turnout_pre, t16, some_college), h = c(bw[["bws"]][1], 90))
  ## rdit using nonparametric bandwidth on treated side, 180 days on untreated side
  l7b <- rdrobust(y = full_treat$turnout, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                 weights = full_treat$weight,
                 covs = select(full_treat,
                               latino, nh_white, asian,
                               nh_black, median_income, median_age,
                               pop_dens, turnout_pre, t16, some_college), h = c(bw[["bws"]][1], 180))
  ## keep only the 2016 data
  o16 <- filter(full_treat, year == "2016")
  ## rdit on 2016 data alone
  l8 <- rdrobust(y = o16$turnout, x = o16$d2, p = 1, c = 0, cluster = o16$id,
                 weights = o16$weight,
                 covs = select(o16,
                               latino, nh_white, asian,
                               nh_black, median_income, median_age,
                               pop_dens, turnout_pre, some_college))
  ##keep only the 2020 data
  o20 <- filter(full_treat, year != "2016")
  #rdit on 2020 alone
  l9 <- rdrobust(y = o20$turnout, x = o20$d2, p = 1, c = 0, cluster = o20$id,
                 weights = o20$weight,
                 covs = select(o20,
                               latino, nh_white, asian,
                               nh_black, median_income, median_age,
                               pop_dens, turnout_pre, some_college))
  
  ## combine results of all these models
  f <- rbind(
    tibble(coef = la$coef,
           n = la$N_h[1] + la$N_h[2],
           bwidth = floor(la[["bws"]][2]),
           se = la$se, 
           pv = la$pv,
           p = threshold,
           t = "Narrowest",
           u = la[["ci"]][,2],
           l = la[["ci"]][,1]),
    tibble(coef = lb$coef,
           n = lb$N_h[1] + lb$N_h[2],
           bwidth = floor(lb[["bws"]][2]),
           se = lb$se, 
           pv = lb$pv,
           p = threshold,
           t = "Widest",
           u = lb[["ci"]][,2],
           l = lb[["ci"]][,1]),
    tibble(coef = l$coef,
           n = l$N_h[1] + l$N_h[2],
           bwidth = floor(l[["bws"]][2]),
           se = l$se, 
           pv = l$pv,
           p = threshold,
           t = "OLS",
           u = l[["ci"]][,2],
           l = l[["ci"]][,1]),
    tibble(coef = l2$coef,
           n = l2$N_h[1] + l2$N_h[2],
           bwidth = floor(l2[["bws"]][2]),
           se = l2$se, 
           pv = l2$pv,
           p = threshold,
           t = "Entropy Balancing",
           u = l2[["ci"]][,2],
           l = l2[["ci"]][,1]),
    tibble(coef = l3$coef,
           n = l3$N_h[1] + l3$N_h[2],
           bwidth = floor(l3[["bws"]][2]),
           se = l3$se, 
           pv = l3$pv,
           p = threshold,
           t = "No Adjustment",
           u = l3[["ci"]][,2],
           l = l3[["ci"]][,1]),
    tibble(coef = l4$coef,
           n = l4$N_h[1] + l4$N_h[2],
           bwidth = floor(l4[["bws"]][2]),
           se = l4$se, 
           pv = l4$pv,
           p = threshold,
           t = "First Difference in Turnout",
           u = l4[["ci"]][,2],
           l = l4[["ci"]][,1]),
    tibble(coef = l4b$coef,
           n = l4b$N_h[1] + l4b$N_h[2],
           bwidth = floor(l4b[["bws"]][2]),
           se = l4b$se, 
           pv = l4b$pv,
           p = threshold,
           t = "Prior Turnout",
           u = l4b[["ci"]][,2],
           l = l4b[["ci"]][,1]),
    tibble(coef = l5$coef,
           n = l5$N_h[1] + l5$N_h[2],
           bwidth = floor(l5[["bws"]][2]),
           se = l5$se, 
           pv = l5$pv,
           p = threshold,
           t = "Double Bandwidth",
           u = l5[["ci"]][,2],
           l = l5[["ci"]][,1]),
    tibble(coef = l5b$coef,
           n = l5b$N_h[1] + l5b$N_h[2],
           bwidth = floor(l5b[["bws"]][2]),
           se = l5b$se, 
           pv = l5b$pv,
           p = threshold,
           t = "Half Bandwidth",
           u = l5b[["ci"]][,2],
           l = l5b[["ci"]][,1]),
    tibble(coef = l6$coef,
           n = l6$N_h[1] + l6$N_h[2],
           se = l6$se, 
           pv = l6$pv,
           p = threshold,
           t = "Nonpara, 60",
           bwidth = paste0("(", floor(l6[["bws"]][1]), ", ", floor(l6[["bws"]][3]), ")"),
           u = l6[["ci"]][,2],
           l = l6[["ci"]][,1]),
    tibble(coef = l7$coef,
           n = l7$N_h[1] + l7$N_h[2],
           bwidth = paste0("(", floor(l7[["bws"]][1]), ", ", floor(l7[["bws"]][3]), ")"),
           se = l7$se, 
           pv = l7$pv,
           p = threshold,
           t = "Nonpara, 90",
           u = l7[["ci"]][,2],
           l = l7[["ci"]][,1]),
    tibble(coef = l7b$coef,
           n = l7b$N_h[1] + l7b$N_h[2],
           bwidth = paste0("(", floor(l7b[["bws"]][1]), ", ", floor(l7b[["bws"]][3]), ")"),
           se = l7b$se, 
           pv = l7b$pv,
           p = threshold,
           t = "Nonpara, 180",
           u = l7b[["ci"]][,2],
           l = l7b[["ci"]][,1]),
    tibble(coef = l8$coef,
           n = l8$N_h[1] + l8$N_h[2],
           bwidth = floor(l8[["bws"]][2]),
           se = l8$se, 
           pv = l8$pv,
           p = threshold,
           t = "Only 2016",
           u = l8[["ci"]][,2],
           l = l8[["ci"]][,1]),
    tibble(coef = l9$coef,
           n = l9$N_h[1] + l9$N_h[2],
           bwidth = floor(l9[["bws"]][2]),
           se = l9$se, 
           pv = l9$pv,
           p = threshold,
           t = "Only 2020",
           u = l9[["ci"]][,2],
           l = l9[["ci"]][,1])
  )
}))

saveRDS(out, "temp/alt_rdds2.rds")

## create figure A2
out <- readRDS("temp/alt_rdds2.rds") %>% 
  filter(t %in% c('Entropy Balancing','OLS','No Adjustment'))

out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))
out$t <- factor(out$t, levels = c('Entropy Balancing','OLS','No Adjustment'))

different_dists <- ggplot(out,
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(t~estimate) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10", base_size = 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)")
different_dists

saveRDS(different_dists, "temp/alt_proc_rdd.rds")

######################
# create figure A9
out <- readRDS("temp/alt_rdds2.rds") %>% 
  filter(t == "First Difference in Turnout")

out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

different_dists <- ggplot(out,
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~estimate) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10", base_size = 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)")
different_dists


saveRDS(different_dists, "temp/first_diff_plot.rds")
######################
#create figure A6
out <- readRDS("temp/alt_rdds2.rds") %>% 
  filter(t %in% c('Half Bandwidth', 'Double Bandwidth','Nonpara, 60','Nonpara, 90', 'Nonpara, 180')) %>% 
  mutate(t = ifelse(t == "Nonpara, 60", "60 Days",
                    ifelse(t == "Nonpara, 90", "90 Days",
                           ifelse(t == "Nonpara, 180", "180 Days", gsub(" ", "\n", t)))))

out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))
out$t <- factor(out$t, levels = c('Half\nBandwidth', 'Double\nBandwidth','60 Days','90 Days', '180 Days'))

different_dists <- ggplot(out,
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(t~estimate) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10", base_size = 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)")
different_dists

saveRDS(different_dists, "temp/alt_bws_rdd.rds")

######################
## create figure A3
out <- readRDS("temp/alt_rdds2.rds") %>% 
  filter(t %in% c('Only 2016', 'Only 2020'))

out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))
# out$t <- factor(out$t, levels = c('Double Bandwidth','30 Days','60 Days'))

different_dists <- ggplot(out,
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(t~estimate) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10", base_size = 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)")
different_dists

saveRDS(different_dists, "temp/individual_years.rds")

######################
## create figure AXX
out <- readRDS("temp/alt_rdds2.rds") %>% 
  filter(t %in% c("Prior Turnout"))

out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))
# out$t <- factor(out$t, levels = c('Double Bandwidth','30 Days','60 Days'))

different_dists <- ggplot(out,
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~estimate) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10", base_size = 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)")
different_dists
saveRDS(different_dists, "temp/placebo_prior.rds")

######################
## create figure AXX
out <- readRDS("temp/alt_rdds2.rds") %>% 
  filter(t %in% c("Narrowest", "Widest"))

out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))
# out$t <- factor(out$t, levels = c('Double Bandwidth','30 Days','60 Days'))

different_dists <- ggplot(out,
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(t~estimate) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10", base_size = 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)")
different_dists
saveRDS(different_dists, "temp/same_bws.rds")

