
all_p2b <- readRDS("temp/precinct_block.rds")

pop <- function(state){
  library(tidycensus)
  get_decennial(geography = "block", variable = "P001001", state = state)
}

cl <- makeCluster(8)  
registerDoParallel(cl)

clusterExport(cl, list("pop"))

states <- unique(filter(fips_codes, state_code <= "56")$state)

j <- rbindlist(parLapply(cl, states,
            fun = pop))

all_p2b <- left_join(all_p2b, select(j, GEOID, pop = value), by = c("GEOID10" = "GEOID"))

bgs_level <- all_p2b |> 
  mutate(GEOID = substring(GEOID10, 1, 12)) |> 
  group_by(state, precinct, GEOID, year) |> 
  summarize(pop = sum(pop, na.rm = T))

c16 <- readRDS("../regular_data/census_bgs_16.rds") %>%
  mutate(year = 2016) %>%
  select(GEOID, year, latino, asian, nh_white, nh_black, median_income, median_age,
         pop_dens, some_college)

c20 <- readRDS("../regular_data/census_bgs_19.rds") %>%
  mutate(year = 2020) %>%
  select(GEOID, year, latino, asian, nh_white, nh_black, median_income, median_age,
         pop_dens, some_college)
census <- bind_rows(c16, c20)

pct_level <- left_join(bgs_level, census)

pct_level <- pct_level[complete.cases(select(pct_level,
                                             latino, asian, nh_white, nh_black,
                                             median_income, median_age, pop_dens, some_college)), ]

pct_level <- pct_level |> 
  group_by(state, precinct, year) |> 
  summarize(across(c(latino, asian, nh_white, nh_black,
                     median_income, median_age, pop_dens, some_college),
                   ~ weighted.mean(., pop)))


saveRDS(pct_level, "temp/pct_demos.rds")
#################################################
## read in dists for each BG
dists <- left_join(readRDS("temp/precinct_demshare.rds"),
                   readRDS("temp/pct_demos.rds") |> 
                     mutate(GEOID = tolower(precinct))) %>% 
  mutate(year = as.character(floor(year(date) / 2) * 2),
         pre = (year == "2016" & date <= "2016-11-08") | (year == "2020" & date <= "2020-11-03"))

### loop over thresholds for RDITS
out <- rbindlist(lapply(seq(.25, 1, .05), function(threshold){
  
  ##keep the observations within the threshold closest to election day
  ## using data.table notation to speed this up
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
    select(GEOID, id, date, dist, year,
           median_income, nh_white, nh_black, median_age, pop_dens, latino, asian,
           some_college, treated, share_dem) %>% 
    mutate(d2 = ifelse(year == "2020", as.integer(date - as.Date("2020-11-03")),
                       as.integer(date - as.Date("2016-11-08"))),
           t16 = year == "2016")
  
  ## keep complete cases, necessary for balancing
  full_treat <- full_treat[complete.cases(select(full_treat, share_dem,
                                                 latino, nh_white, asian, nh_black, median_income, median_age,
                                                 pop_dens, some_college)), ]
  
  ########################
  
  ##balancing pre/post obs within 2016
  # d16 <- filter(full_treat, year == "2016")
  # 
  # mb <- ebalance(d16$treated,
  #                select(d16,
  #                       nh_black, latino, nh_white, asian, median_income, median_age,
  #                       pop_dens, some_college))
  # 
  # d16 <- bind_rows(
  #   filter(d16, treated) %>%
  #     mutate(weight = 1),
  #   filter(d16, !treated) %>%
  #     mutate(weight = mb$w)
  # )
  
  ##balancing pre/post obs within 2020
  d20 <- filter(full_treat, year == "2020")
  
  mb <- ebalance(d20$treated,
                 select(d20,
                        nh_black, latino, nh_white, asian, median_income, median_age,
                        pop_dens, some_college))
  
  d20 <- bind_rows(
    filter(d20, treated) %>%
      mutate(weight = 1),
    filter(d20, !treated) %>%
      mutate(weight = mb$w)
  )
  
  ## recombine
  full_treat <- bind_rows(d20)
  
  
  ## save half-mile observations for robustness checks, other analysis
  if(threshold == 0.3){
    saveRDS(full_treat, "temp/primary_third_dem.rds")
  }
  ########################################
  
  l <- rdrobust(y = full_treat$share_dem, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
                weights = full_treat$weight,
                covs = select(full_treat,
                              latino, nh_white, asian,
                              nh_black, median_income, median_age,
                              pop_dens, some_college))
  
  f <- tibble(coef = l$coef,
              se = l$se, 
              pv = l$pv,
              p = threshold,
              n = l$N_h[1] + l$N_h[2],
              bw = l[["bws"]][1],
              u = l[["ci"]][,2],
              l = l[["ci"]][,1])
}))

saveRDS(out, "temp/primary_out_data_dem.rds")
## create figure 3
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)

out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))

different_dists <- ggplot(out,
                          aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~estimate) +
  geom_point() +
  geom_errorbar(width = 0) + 
  theme_bc(base_family = "LM Roman 10", base_size = 14) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Radius Around Shooting (Miles)")
different_dists

saveRDS(different_dists, "temp/different_dists_primary.rds")

## create figure A4
sample_sizes <- ggplot(filter(out, estimate == "Traditional"),
                       aes(x = p, y = n)) +
  geom_line() +
  geom_point() +
  theme_bc(base_family = "LM Roman 10") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Effective Sample Size", x = "Radius Around Shooting (Miles)") +
  scale_y_continuous(labels = comma)
sample_sizes
saveRDS(sample_sizes, "temp/sample_size_plot.rds")
###########################
###########################
###########################
## read in half-mile observations
full_treat <- readRDS("temp/primary_third_dem.rds")

l <- rdrobust(y = full_treat$share_dem, x = full_treat$d2, p = 1, c = 0, cluster = full_treat$id,
              weights = full_treat$weight,
              covs = select(full_treat,
                            latino, nh_white, asian,
                            nh_black, median_income, median_age,
                            pop_dens, some_college))
########################################
## run with different polynomials for Figure A7
out <- rbindlist(lapply(c(1:5), function(x){
  l <- rdrobust(y = full_treat$share_dem, x = full_treat$d2, p = x, c = 0, cluster = full_treat$id,
                weights = full_treat$weight,
                covs = select(full_treat,
                              latino, nh_white, asian,
                              nh_black, median_income, median_age,
                              pop_dens, some_college))
  
  f <- tibble(coef = l$coef,
              se = l$se,
              n = l$N_h[1] + l$N_h[2],
              pv = l$pv,
              bwidth = l[["bws"]][1],
              p = x,
              u = l[["ci"]][,2],
              l = l[["ci"]][,1])
}))
saveRDS(out, "temp/alt_polys_data.rds")
out <- readRDS("temp/alt_polys_data.rds")
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)
out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))
out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$z <- out$p == 1

dd <- ggplot(out,
             aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~estimate) +
  geom_errorbar(width = 0) +
  geom_point(aes(color = z)) +
  theme_bc(base_family = "LM Roman 10") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Polynomial") +
  scale_color_manual(values = c("black", "red")) +
  theme(legend.position = "none")
dd
saveRDS(dd, "temp/diff_polys_primary.rds")
########################################
## run with different cutpoints for Figure A8
out <- rbindlist(lapply(c(-14:14), function(x){
  l <- rdrobust(y = full_treat$share_dem, x = full_treat$d2, p = 1, c = x, cluster = full_treat$id,
                weights = full_treat$weight,
                covs = select(full_treat,
                              latino, nh_white, asian,
                              nh_black, median_income, median_age,
                              pop_dens, some_college))
  
  f <- tibble(coef = l$coef,
              se = l$se,
              n = l$N_h[1] + l$N_h[2],
              bwidth = l[["bws"]][1],
              pv = l$pv,
              p = x,
              u = l[["ci"]][,2],
              l = l[["ci"]][,1])
}))
saveRDS(out, "temp/alt_cut_tab_data.rds")
out <- readRDS("temp/alt_cut_tab_data.rds")
out$estimate <- rep(c('Traditional','Bias-Adjusted','Robust'),nrow(out)/3)
out$estimate <- factor(out$estimate, levels = c('Traditional','Bias-Adjusted','Robust'))
out <- mutate_at(out, vars(coef, l, u), ~. * -1)

out$z <- out$p == 0

dd <- ggplot(out,
             aes(x = p, y = coef, ymin = l, ymax = u)) +
  facet_grid(~estimate) +
  geom_errorbar(width = 0) +
  geom_point(aes(color = z)) +
  theme_bc(base_family = "LM Roman 10") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Local Average Treatment Effect", x = "Cut-Point (Days from Election)") +
  scale_color_manual(values = c("black", "red")) +
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines"))
dd
saveRDS(dd, "temp/placebos.rds")
###########################

## create naive RDIT plot for Figure 2
j <- rdplot(y = full_treat$share_dem, x = full_treat$d2, c = 0, p = 3)[["rdplot"]]
j[["labels"]] <- j[["labels"]][-1]
j <- j +
  geom_point(aes(x = d2, y = share_dem), data = full_treat, shape = 21,
             color = "transparent", fill = "gray", alpha = 0.5) +
  theme_bc(base_family = "LM Roman 10") +
  labs(x = "Days Between Police Killing and Election",
       y = "Turnout") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = c(-180, -90, 0, 90, 180),
                     labels = c("Killing Occurs\n180 Days\nBefore Election", "90",
                                "Killing Occurs on\nElection Day",
                                "90", "Killing Occurs\n180 Days\nAfter Election"),
                     limits = c(-190, 190))
j

h <- j[["layers"]][[5]]

j[["layers"]][[5]] <- j[["layers"]][[1]]

j[["layers"]][[1]] <- h
j
saveRDS(j, "temp/rd_plot.rds")
#################################
## the rest of the script creates Table 1

## start with weighted means
demos <- full_treat %>% 
  group_by(year, treated) %>% 
  summarize(across(c("nh_black", "latino", "nh_white", "asian", "median_income", "median_age",
                     "pop_dens", "turnout_pre", "some_college"), ~ weighted.mean(., weight)),
            n = n_distinct(GEOID),
            n_shooting = n_distinct(id)) %>% 
  mutate(treated = ifelse(treated, "Treated", "Weighted Controls"))

## unweighted means
demos2 <- full_treat %>% 
  filter(!treated) %>%
  group_by(year) %>% 
  summarize(across(c("nh_black", "latino", "nh_white", "asian", "median_income", "median_age",
                     "pop_dens", "turnout_pre", "some_college"), mean),
            n = n_distinct(GEOID),
            n_shooting = n_distinct(id)) %>% 
  mutate(treated = "Unweighted Controls")

demos <- bind_rows(demos, demos2)


## keep only untreated / not in data observations
f <- anti_join(readRDS("temp/full_demos.rds"), full_treat, by = c("GEOID", "year")) %>% 
  mutate(turnout_pre = ballots_pre / cvap_pre) %>% 
  select(nh_black, latino, nh_white, asian, median_income, median_age,
         pop_dens, turnout_pre, year, GEOID, some_college)

f <- f[complete.cases(f), ] %>% 
  filter(is.finite(turnout_pre)) %>% 
  group_by(year) %>% 
  summarize(across(c("nh_black", "latino", "nh_white", "asian", "median_income", "median_age",
                     "pop_dens", "turnout_pre", "some_college"), mean),
            n = n_distinct(GEOID),
            n_shooting = 0) %>% 
  mutate(treated = "Not in Dataset")


demos <- bind_rows(demos, f)


demos <- pivot_longer(demos, cols = c("nh_black", "latino", "nh_white",
                                      "asian", "median_income", "median_age", "some_college",
                                      "pop_dens", "turnout_pre", "n", "n_shooting"))

demos <- pivot_wider(demos, id_cols = c("year", "name"), names_from = "treated", values_from = "value")

demos <- select(demos, year, name, `Not in Dataset`,
                `Treated`, `Unweighted Controls`, `Weighted Controls`)

###############################
##############################
vs <- fread("raw_data/var_orders.csv")

demos <- left_join(rename(demos, variable = name), vs)

demos <- demos %>% 
  mutate_at(vars(`Not in Dataset`,
                 `Treated`, `Unweighted Controls`, `Weighted Controls`),
            ~ ifelse(name == "Median Income", dollar(round(as.numeric(gsub(",", "", .)))), .)) %>% 
  mutate_at(vars(`Not in Dataset`,
                 `Treated`, `Unweighted Controls`, `Weighted Controls`),
            ~ ifelse(name == "Median Age", round(as.numeric(.), 1), .)) %>% 
  mutate_at(vars(`Not in Dataset`,
                 `Treated`, `Unweighted Controls`, `Weighted Controls`),
            ~ ifelse(name %in% c("Population Density", "Number of Block Groups",
                                 "Number of Killings"), comma(as.numeric(.), accuracy = 1), .)) %>% 
  mutate_at(vars(`Not in Dataset`,
                 `Treated`, `Unweighted Controls`, `Weighted Controls`),
            ~ ifelse(substring(name, 1, 1) == "%" |
                       name == "Previous Turnout", percent(as.numeric(.), accuracy = .1), .))
demos <- demos %>% 
  arrange(year, order) %>% 
  rename(Year = year) %>% 
  ungroup() %>% 
  select(-order, -variable) %>%
  select(Year, Variable = name, everything())

saveRDS(demos, "temp/demo_table_half_mile.rds")

  