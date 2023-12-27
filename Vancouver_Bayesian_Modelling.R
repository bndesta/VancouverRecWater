### Regression approach using BRMS ###

#### Model E.coli geometric mean outcome

#The proposed model

# Geomean_10 <- Intercept + PrevGeomean + Salinity + Rain48 + Meantemp24 +
#               UVmean24 +  DaysSinceRain  + Year + Beachname + Epselon

## Examine possible different priors for SD variables

priors <- tibble(student_t = rt(n = 10000, df = 3, ncp = 2.5),
                 exp = rexp(10000, rate = 1)) 

priors |> ggplot(aes(x = student_t)) + geom_density(colour = "blue") +
  geom_density(aes(x = exp)) + 
  xlim(0, 20) + theme_minimal()

# Starting with model with beach as varying effect

# Log transformed E. coli values - standardized predictors

get_prior(LogEc ~ LogEc24_s + Salinity_s + Rain48_s + Meantemp24_s +
            UVmean24_s + DaysSinceRain_s + Year + (1 | Beachname),
          data = Vancouver)

priors <- set_prior("normal(0,1)",class= "b")

#### Model E.coli geometric mean outcome - sample only the prior to check priors

model1_prior <- brm(LogEc ~ LogEc24_s + Salinity_s + Rain48_s + Meantemp24_s +
                      UVmean24_s + DaysSinceRain_s + Year + (1 | Beachname),
              data = Vancouver, family = gaussian(), prior = priors,
              iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, 
              sample_prior = TRUE, backend = "cmdstanr", 
              stan_model_args = list(stanc_options = list("O1")))

summary(model1_prior)
plot(model1_prior)
pp_check(model1_prior)
pp_check(model1_prior, type = "stat", stat = "mean", prefix = "ppd")
pp_check(model1_prior, ndraws=100, prefix = "ppd")
pp_check(model1_prior, type = "loo_pit_overlay")
conditional_effects(model1_prior)


### Run model again and sample from posterior now

model1 <- brm(LogEc ~ LogEc24_s + Salinity_s + Rain48_s + Meantemp24_s +
              UVmean24_s + DaysSinceRain_s + Year + (1 | Beachname),
                    data = Vancouver, family = gaussian(), prior = priors,
                    iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 9, 
                    backend = "cmdstanr", 
                    stan_model_args = list(stanc_options = list("O1")))

summary(model1)
get_variables(model1)
plot(model1)
pp_check(model1, ndraws=100)
pp_check(model1, type = "stat", stat = "mean")
pp_check(model1, type = "loo_pit_overlay")
mcmc_acf(model1, pars = vars(contains("_s")), lags = 10)
mcmc_pairs(model1, pars = vars(contains("_s")), diag_fun = "den", off_diag_fun = "hex")


# Model fit is not great as per pp check - given how skewed dist is
# Try Log-Normal model instead (use of raw Geomean outcome here)

model2 <- brm(Geomean10 ~ LogEc24_s + Salinity_s + Rain48_s + Meantemp24_s +
              UVmean24_s + DaysSinceRain_s + Year + (1 | Beachname),
                data = Vancouver, family = lognormal(), prior = priors,
                iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 9, 
                backend = "cmdstanr", 
                stan_model_args = list(stanc_options = list("O1")))

summary(model2)
get_variables(model2)
plot(model2) 
pp_check(model2, ndraws=100)
pp_check(model2, ndraws=100) + coord_cartesian(xlim = c(0, 500))
pp_check(model2, type = "loo_pit_overlay")
mcmc_acf(model2, pars = vars(contains("_s")), lags = 10)
mcmc_pairs(model2, pars = vars(contains("_s")), diag_fun = "den", off_diag_fun = "hex")

# Fit much improved - try adding varying slopes to see how fit improves

get_prior(Geomean10 ~ LogEc24_s + Salinity_s + Rain48_s + Meantemp24_s +
          UVmean24_s + DaysSinceRain_s + Year + (1 + LogEc24_s + Salinity_s + 
          Rain48_s + Meantemp24_s + UVmean24_s + DaysSinceRain_s | Beachname),
          data = Vancouver, family = lognormal())

priors4 <- c(set_prior("normal(0,1)",class= "b"),
             set_prior("lkj(2)", class= "cor"))

model3 <- brm(Geomean10 ~ LogEc24_s + Salinity_s + Rain48_s + Meantemp24_s +
              UVmean24_s + DaysSinceRain_s + Year + (1 + LogEc24_s + Salinity_s + 
              Rain48_s + Meantemp24_s + UVmean24_s + DaysSinceRain_s | Beachname),
              data = Vancouver, family = lognormal(), prior = priors4,
              iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 9, 
              backend = "cmdstanr", 
              stan_model_args = list(stanc_options = list("O1")))

summary(model3)
get_variables(model3)
plot(model3) 
pp_check(model3, ndraws=100)
pp_check(model3, ndraws=100) + coord_cartesian(xlim = c(0, 500))
pp_check(model3, type = "loo_pit_overlay")
mcmc_acf(model3, pars = vars(contains("_s")), lags = 10)
mcmc_pairs(model3, pars = vars(contains("_s")), diag_fun = "den", off_diag_fun = "hex")

loo(model2, model3)

conditional_effects(model3)

# Conditional adjusted prediction plots to check appropriateness of varying slopes 
# for each variable
plot_cap(model3, re_formula=NULL, condition = c("LogEc24_s", "Beachname"))
plot_cap(model3, re_formula=NULL, condition = c("Salinity_s", "Beachname"))
plot_cap(model3, re_formula=NULL, condition = c("Rain48_s", "Beachname"))
plot_cap(model3, re_formula=NULL, condition = c("Meantemp24_s", "Beachname"))
plot_cap(model3, re_formula=NULL, condition = c("UVmean24_s", "Beachname"))
plot_cap(model3, re_formula=NULL, condition = c("DaysSinceRain_s", "Beachname"))


# Reformulate model with Year as cross-classified varying effect 

model4 <- brm(Geomean10 ~ LogEc24_s + Salinity_s + Rain48_s + Meantemp24_s +
                UVmean24_s + DaysSinceRain_s + (1 | Year) + (1 + LogEc24_s + Salinity_s + 
                Rain48_s + Meantemp24_s + UVmean24_s + DaysSinceRain_s | Beachname),
              data = Vancouver, family = lognormal(), prior = priors2,
              iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 9, 
              backend = "cmdstanr", 
              stan_model_args = list(stanc_options = list("O1")))

summary(model4)
get_variables(model4)
plot(model4) 
pp_check(model4, ndraws=100)
pp_check(model4, ndraws=100) + coord_cartesian(xlim = c(0, 500))
pp_check(model4, type = "loo_pit_overlay")
mcmc_acf(model4, pars = vars(contains("_s")), lags = 10)
mcmc_pairs(model4, pars = vars(contains("_s")), diag_fun = "den", off_diag_fun = "hex")

loo(model3, model4)

# Limited benefit in selecting the cross-classified model - keep year as fixed effect
# Check change in fit with interaction between mean temperature and UV index

get_prior(Geomean10 ~ LogEc24_s + Salinity_s + Rain48_s + Meantemp24_s*UVmean24_s + 
            DaysSinceRain_s + Year + (1 + LogEc24_s + Salinity_s + 
            Rain48_s + Meantemp24_s*UVmean24_s + DaysSinceRain_s| Beachname),
          data = Vancouver, family = lognormal())

priors6 <- c(set_prior("normal(0,1)",class= "b"),
             set_prior("lkj(2)", class= "cor"))

model8 <- brm(Geomean10 ~ LogEc24_s + Salinity_s + Meantemp24_s*UVmean24_s + 
                Rain48_s + DaysSinceRain_s + Year + (1 + LogEc24_s + Salinity_s + 
               Meantemp24_s*UVmean24_s + Rain48_s + DaysSinceRain_s | Beachname),
              data = Vancouver, family = lognormal(), prior = priors6,
              iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 9, 
              backend = "cmdstanr", 
              stan_model_args = list(stanc_options = list("O1")))

#Models comparison without and with the interaction effect using loo compare
loo(model3, model8)

# Not treating DaySinceRAin as varying slope
get_prior(Geomean10 ~ LogEc24_s + Salinity_s + Rain48_s + Meantemp24_s*UVmean24_s + 
            DaysSinceRain_s + Year + (1 + LogEc24_s + Salinity_s + 
          Rain48_s + Meantemp24_s*UVmean24_s | Beachname),
          data = Vancouver, family = lognormal())

priors2 <- c(set_prior("normal(0,1)",class= "b"),
             set_prior("lkj(2)", class= "cor"))

model7 <- brm(Geomean10 ~ LogEc24_s + Salinity_s + Meantemp24_s*UVmean24_s + 
                Rain48_s + DaysSinceRain_s + Year + (1 + LogEc24_s + Salinity_s + 
                Meantemp24_s*UVmean24_s + Rain48_s | Beachname),
              data = Vancouver, family = lognormal(), prior = priors2,
              iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 9, 
              backend = "cmdstanr", 
              stan_model_args = list(stanc_options = list("O1")))

summary(model7)
plot(model7) 
pp_check(model7, ndraws=4000) + coord_cartesian(xlim = c(0, 500))
pp_check(model7, type = "loo_pit_overlay")
mcmc_acf(model7, pars = vars(contains("_s")), lags = 10)
mcplot <- mcmc_pairs(model7, pars = vars(contains("_s")), diag_fun = "den", off_diag_fun = "hex")
mcplot

#Models comparison without and with daysincerain as a varying effect using loo compare
loo(model8, model7)

# This model (model 7) fits the best

# Conditional adjusted prediction plots to check appropriateness of varying slopes 
# for each variable
plot_cap(model7, re_formula=NULL, condition = c("LogEc24_s", "Beachname"))
plot_cap(model7, re_formula=NULL, condition = c("Salinity_s", "Beachname"))
plot_cap(model7, re_formula=NULL, condition = c("Rain48_s", "Beachname"))
plot_cap(model7, re_formula=NULL, condition = c("Meantemp24_s", "Beachname"))
plot_cap(model7, re_formula=NULL, condition = c("UVmean24_s", "Beachname"))


### Plot predictions and calculate MARGINAL EFFECTS - i.e. average effects of variables for 
# "clusters (beaches) on average" # Using Model7


### Calculate effects for Previous Sample Day Log E. coli ###
Vancouver |> rstatix::get_summary_stats(LogEc24_s, LogEc24)
quantile(Vancouver$LogEc24_s, probs = c(0, 0.25, .50, 0.75, .95, 0.99), na.rm = TRUE)
quantile(Vancouver$LogEc24, probs = c(0, 0.25, .50, 0.75, .95, 0.99), na.rm = TRUE)


# Extract samples from posterior and plot draws at specific values of predictors
pred <- predictions(model7, re_formula = NULL, type = "response",
                    newdata = datagrid(model = model7, Beachname = unique, 
                                       Year = unique, 
                                       LogEc24_s = seq(-0.80, 4.76, by = 0.2))) |> 
  posteriordraws()

# Plot overall continuous effect of Previous Sample Day Log E. coli
# Last line of code will "uncenter" and "unstandardize" the x axis
at_LogEc24 <- c(-1, 0, 1, 2, 3, 4, 4.8)

ggplot(pred, aes(x = LogEc24_s, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Purples") +
  labs(x = "Log Previous Sample Day E. coli Geometric Mean",
       y = "Predicted E. coli Geometric Mean",
       fill = "") +
  theme_classic() + 
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0, 2000, by = 500)) +
  scale_x_continuous(breaks = at_LogEc24,
                     labels = round(at_LogEc24*sd(Vancouver$LogEc24, na.rm = TRUE) + 
                                      mean(Vancouver$LogEc24, na.rm = TRUE), 1))

# Now plot same effect stratified by beach

ggplot(pred, aes(x = LogEc24_s, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Purples") +
  labs(x = "Log Previous Sample Day E. coli Geometric Mean",
       y = "Predicted E. coli Geometric Mean",
       fill = "") +
  theme_classic() +
  scale_x_continuous(breaks = at_LogEc24,
                     labels = round(at_LogEc24*sd(Vancouver$LogEc24, na.rm = TRUE) + 
                                      mean(Vancouver$LogEc24, na.rm = TRUE), 1)) +
  facet_wrap(~ Beachname) + 
  theme(legend.position = "bottom") 

# Now calculate/plot the slope of the line (marginal effects) at different values
# We can use just the median and 95th percentile

mfx <- marginaleffects(model7, type = "response", re_formula = NULL,
                       variables = "LogEc24_s",
                       newdata = datagrid(model = model7, Year = unique,
                                          Beachname = unique,
                                          LogEc24_s = c(-0.4235989, 1.9509712))) |> 
  posteriordraws()

ggplot(mfx, aes(x = draw, fill = factor(LogEc24_s))) +
  stat_halfeye() +
  scale_fill_brewer(palette = "Set2", labels=c("Median", "95th Percentile")) + 
  labs(x = "Marginal Effect of Log Previous Sample Day Geometric Mean",
       y = "Posterior Density",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(0, 400))

#Extracting the median effect value and credible intervales from the plot
marginaleffects(model7, type = "response", re_formula = NULL,
                       variables = "LogEc24_s",
                       newdata = datagrid(model = model7, Year = unique,
                                          Beachname = unique,
                                          LogEc24_s = c(-0.4235989, 1.9509712))) |> 
  summary()



# Calculate beach-specific version 
mfx <- mfx |> mutate(LogEc24_s = ifelse(LogEc24_s == -0.4235989, 
                                            "Median", "95th Percentile")) 

ggplot(mfx, aes(x = draw, fill = fct_rev(LogEc24_s))) +
  stat_halfeye() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Marginal Effect of Log Previous Sample Day Geometric Mean",
       y = "Posterior Density",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(0, 400)) +
  facet_wrap(~ Beachname)



### Calculate effects for 48 hr rainfall ###
Vancouver |> rstatix::get_summary_stats(Rain48_s, Rain48)
quantile(Vancouver$Rain48, probs = c(0, .50, .95, 0.99), na.rm = TRUE)
quantile(Vancouver$Rain48_s, probs = c(0, .50, .95, 0.99, 0.999), na.rm = TRUE)

pred2 <- predictions(model7, re_formula = NULL, type = "response",
                    newdata = datagrid(model = model7, Beachname = unique, 
                                       Year = unique, 
                                       Rain48_s = seq(-0.5, 8.67, by = 0.3))) |> 
  posteriordraws()
remove(pred2)
# Plot overall continuous effect of Rainfall

at_Rain48 <- c(0, 1, 2, 3, 4, 5, 6, 7)

ggplot(pred2, aes(x = Rain48_s, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Purples") +
  labs(x = "48 hr Rainfall (mm)",
       y = "Predicted E. coli Geometric Mean",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = at_Rain48,
                     labels = round(at_Rain48*sd(Vancouver$Rain48, na.rm = TRUE) + 
                                      mean(Vancouver$Rain48, na.rm = TRUE), 1)) 

# Now plot same effect stratified by beach

ggplot(pred2, aes(x = Rain48_s, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Purples") +
  labs(x = "48 hr Rainfall (mm)",
       y = "Predicted E. coli Geometric Mean",
       fill = "") +
  theme_classic() +
  scale_x_continuous(breaks = at_Rain48,
                     labels = round(at_Rain48*sd(Vancouver$Rain48, na.rm = TRUE) + 
                                      mean(Vancouver$Rain48, na.rm = TRUE), 1)) +
  facet_wrap(~ Beachname) + 
  theme(legend.position = "bottom") 

# Now calculate/plot the slope of the line (marginal effects) at different values
# We can use just the median (which is 0 mm) and 95th percentile

remove(mfx)
mfx <- marginaleffects(model7, type = "response", re_formula = NULL,
                       variables = "Rain48_s",
                       newdata = datagrid(model = model7, Year = unique,
                                          Beachname = unique,
                                          Rain48_s = c(-0.4546764, 1.8359136))) |> 
  posteriordraws()

ggplot(mfx, aes(x = draw, fill = factor(Rain48_s))) +
  stat_halfeye() +
  scale_fill_brewer(palette = "Set2", labels=c("Median", "95th Percentile")) + 
  labs(x = "Marginal Effect of 48 h Rainfall",
       y = "Posterior Density",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(0, 150))

#Extracting the median effect value and credible intervales from the plot
marginaleffects(model7, type = "response", re_formula = NULL,
                variables = "Rain48_s",
                newdata = datagrid(model = model7, Year = unique,
                                   Beachname = unique,
                                   Rain48_s = c(-0.4546764, 1.8359136))) |> 
  summary()

# Calculate beach-specific version 
mfx <- mfx |> mutate(Rain48_s = ifelse(Rain48_s == -0.4546764,
                                        "Median", "95th Percentile")) 

ggplot(mfx, aes(x = draw, fill = fct_rev(Rain48_s))) +
  stat_halfeye() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Marginal Effect of 48 h Rainfall",
       y = "Posterior Density",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(0, 100)) +
  facet_wrap(~ Beachname)


### Calculate effects for Salinity ###
Vancouver |> rstatix::get_summary_stats(Salinity_s, Salinity)
quantile(Vancouver$Salinity_s, probs = c(0, .50, .95, 0.99, 0.999), na.rm = TRUE)
quantile(Vancouver$Salinity, probs = c(0, .50, .95, 0.99, 0.999), na.rm = TRUE)

pred3 <- predictions(model7, re_formula = NULL, type = "response",
                     newdata = datagrid(model = model7, Beachname = unique, 
                                        Year = unique, 
                                        Salinity_s = seq(-2.23, 2.65, by = 0.2))) |> 
  posteriordraws()

# Plot overall continuous effect of Salinity

at_Salinity <- c(-2, -1, 0, 1, 2, 3)

ggplot(pred3, aes(x = Salinity_s, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Purples") +
  labs(x = "Mean Salinity",
       y = "Predicted E. coli Geometric Mean",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = at_Salinity,
                     labels = round(at_Salinity*sd(Vancouver$Salinity, na.rm = TRUE) + 
                                      mean(Vancouver$Salinity, na.rm = TRUE), 1)) 

# Stratified by beach

ggplot(pred3, aes(x = Salinity_s, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Purples") +
  labs(x = "Mean Salinity",
       y = "Predicted E. coli Geometric Mean",
       fill = "") +
  theme_classic() +
  scale_x_continuous(breaks = at_Salinity,
                     labels = round(at_Salinity*sd(Vancouver$Salinity, na.rm = TRUE) + 
                                      mean(Vancouver$Salinity, na.rm = TRUE), 1)) +
  facet_wrap(~ Beachname) + 
  theme(legend.position = "bottom") 

# Now calculate/plot the slope of the line (marginal effects) at different values
# We can use just the median and 95th percentile

remove(mfx)
mfx <- marginaleffects(model7, type = "response", re_formula = NULL,
                       variables = "Salinity_s",
                       newdata = datagrid(model = model7, Year = unique,
                                          Beachname = unique,
                                          Salinity_s = c(0.07692692, 1.48038157))) |> 
  posteriordraws()

ggplot(mfx, aes(x = draw, fill = factor(Salinity_s))) +
  stat_halfeye() +
  scale_fill_brewer(palette = "Set2", labels=c("Median", "95th Percentile")) + 
  labs(x = "Marginal Effect of Mean Salinity",
       y = "Posterior Density",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(-100, 50))

#Extracting the median effect value and credible intervales from the plot
marginaleffects(model7, type = "response", re_formula = NULL,
                variables = "Salinity_s",
                newdata = datagrid(model = model7, Year = unique,
                                   Beachname = unique,
                                   Salinity_s = c(0.07692692, 1.48038157))) |> 
  summary()


# Calculate beach-specific version 
mfx <- mfx |> mutate(Salinity_s = ifelse(Salinity_s == 0.07692692,
                                       "Median", "95th Percentile")) 

ggplot(mfx, aes(x = draw, fill = fct_rev(Salinity_s))) +
  stat_halfeye() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Marginal Effect of Mean Salinity",
       y = "Posterior Density",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(-100, 50)) +
  facet_wrap(~ Beachname)



### Calculate effects for DaysSinceRain ###

Vancouver |> rstatix::get_summary_stats(DaysSinceRain_s, DaysSinceRain)
quantile(Vancouver$DaysSinceRain_s, probs = c(0, .50, .95, 0.99, 0.999), na.rm = TRUE)
quantile(Vancouver$DaysSinceRain, probs = c(0, .50, .95, 0.99, 0.999), na.rm = TRUE)

pred4 <- predictions(model7, re_formula = NULL, type = "response",
                     newdata = datagrid(model = model7, Beachname = unique, 
                                        Year = unique, 
                                        DaysSinceRain_s = seq(-0.725, 4.97, by = 0.2))) |> 
  posteriordraws()

# Plot overall continuous effect of Rainfall

at_DaysSinceRain <- c(-1, 0, 1, 2, 3, 4, 5)

ggplot(pred4, aes(x = DaysSinceRain_s, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Purples") +
  labs(x = "Antecedent Dry Days",
       y = "Predicted E. coli Geometric Mean",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = at_DaysSinceRain,
                     labels = round(at_DaysSinceRain*sd(Vancouver$DaysSinceRain, na.rm = TRUE) + 
                                      mean(Vancouver$DaysSinceRain, na.rm = TRUE), 1)) 

#Stratified by beach
ggplot(pred4, aes(x = DaysSinceRain_s, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Purples") +
  labs(x = "Antecedent Dry Days",
       y = "Predicted E. coli Geometric Mean",
       fill = "") +
  theme_classic() +
  scale_x_continuous(breaks = at_DaysSinceRain,
                     labels = round(at_DaysSinceRain*sd(Vancouver$DaysSinceRain, na.rm = TRUE) + 
                                      mean(Vancouver$DaysSinceRain, na.rm = TRUE), 1)) +
  facet_wrap(~ Beachname) + 
  theme(legend.position = "bottom") 


# Now calculate/plot the slope of the line (marginal effects) at different values
# We can use just the median and 95th percentile
remove(mfx)
mfx <- marginaleffects(model7, type = "response", re_formula = NULL,
                       variables = "DaysSinceRain_s",
                       newdata = datagrid(model = model7, Year = unique,
                                          Beachname = unique,
                                          DaysSinceRain_s = c(-0.3578197, 2.2158805))) |> 
  posteriordraws()

ggplot(mfx, aes(x = draw, fill = factor(DaysSinceRain_s))) +
  stat_halfeye() +
  scale_fill_brewer(palette = "Set2", labels=c("Median", "95th Percentile")) + 
  labs(x = "Marginal Effect of Antecedent Dry Days",
       y = "Posterior Density",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(-13, 15))

#Extracting the median effect value and credible intervals from the plot
marginaleffects(model7, type = "response", re_formula = NULL,
                variables = "DaysSinceRain_s",
                newdata = datagrid(model = model7, Year = unique,
                                   Beachname = unique,
                                   DaysSinceRain_s = c(-0.3578197, 2.2158805))) |> 
  summary()

# Calculate beach-specific version 
mfx <- mfx |> mutate(DaysSinceRain_s = ifelse(DaysSinceRain_s == -0.3578197,
                                         "Median", "95th Percentile")) 

ggplot(mfx, aes(x = draw, fill = fct_rev(DaysSinceRain_s))) +
  stat_halfeye() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Marginal Effect of Antecedent Dry Days",
       y = "Posterior Density",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(-20, 20)) +
  facet_wrap(~ Beachname)



# Marginal effects code for interaction effects of mean temperature and UV index

### Calculate effects for mean temperature ###
# Since part of interaction, show effect over 3 levels of uV index (minimum, median, 95th percentile)
Meantemp24_s*UVmean24_s
Vancouver |> rstatix::get_summary_stats(Meantemp24_s, Meantemp24)
quantile(Vancouver$Meantemp24_s, probs = c(0, .50, .95, 0.99, 0.999), na.rm = TRUE)
quantile(Vancouver$Meantemp24, probs = c(0, .50, .95, 0.99, 0.999), na.rm = TRUE)

Vancouver |> rstatix::get_summary_stats(UVmean24_s, UVmean24_s)
quantile(Vancouver$UVmean24_s, probs = c(0, .50, .95, 0.99), na.rm = TRUE)
quantile(Vancouver$UVmean24, probs = c(0, .50, .95, 0.99), na.rm = TRUE)

remove(pred, mfx)

pred <- predictions(model7, re_formula = NULL, type = "response",
                    newdata = datagrid(model = model7, Beachname = unique, 
                                       Year = unique, 
                                       UVmean24_s = c(-2.393851210, 0.002988696, 1.585497550),
                                       Meantemp24_s = seq(-5.02, 2.33, by = 0.3))) |> 
  posteriordraws()



# Plot overall continuous effect of MeanTemp24

at_Meantemp24 <- c(-5, -3.5, -2, -0.5, 1, 2)

pred <- pred |> mutate(UVmean24_s = case_when(
  UVmean24_s == -2.393851210 ~ "Minimum",
  UVmean24_s == 0.002988696 ~ "Median",
  UVmean24_s == 1.585497550 ~ "95th Percentile")) |> 
  mutate(UVmean24_s = as.factor(UVmean24_s)) |> 
  mutate(UVmean24_s = fct_rev(UVmean24_s))

ggplot(pred, aes(x = Meantemp24_s, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Purples") +
  labs(x = "24 hr Mean Temperature",
       y = "Predicted E. coli Geometric Mean",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = at_Meantemp24,
                     labels = round(at_Meantemp24*sd(Vancouver$Meantemp24, na.rm = TRUE) + 
                                      mean(Vancouver$Meantemp24, na.rm = TRUE), 1)) +
  facet_wrap(~ UVmean24_s)

#Stratified by beach

#for minimum UVmean24 values
remove(pred)
pred <- predictions(model7, re_formula = NULL, type = "response",
                    newdata = datagrid(model = model7, Beachname = unique, 
                                       Year = unique, 
                                       UVmean24_s = c(-2.393851210),
                                       Meantemp24_s = seq(-5.02, 2.33, by = 0.3))) |> 
  posteriordraws()

pred <- pred |> mutate(UVmean24_s = case_when(
  UVmean24_s == -2.393851210 ~ "Minimum")) |> 
  mutate(UVmean24_s = as.factor(UVmean24_s)) |> 
  mutate(UVmean24_s = fct_rev(UVmean24_s))

ggplot(pred, aes(x = Meantemp24_s, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Purples") +
  labs(x = "24 hr Mean Temperature",
       y = "Predicted E. coli Geometric Mean",
       fill = "") +
  theme_classic() +
  scale_x_continuous(breaks = at_Meantemp24,
                     labels = round(at_Meantemp24*sd(Vancouver$Meantemp24, na.rm = TRUE) + 
                                      mean(Vancouver$Meantemp24, na.rm = TRUE), 1)) +
  facet_wrap(~ Beachname) +
  theme(legend.position = "bottom") 


#for median UWmean24 values
remove(pred)
pred <- predictions(model7, re_formula = NULL, type = "response",
                    newdata = datagrid(model = model7, Beachname = unique, 
                                       Year = unique, 
                                       UVmean24_s = c(0.002988696),
                                       Meantemp24_s = seq(-5.02, 2.33, by = 0.3))) |> 
  posteriordraws()


pred <- pred |> mutate(UVmean24_s = case_when(
  UVmean24_s == 0.002988696 ~ "Median")) |> 
  mutate(UVmean24_s = as.factor(UVmean24_s)) |> 
  mutate(UVmean24_s = fct_rev(UVmean24_s))

ggplot(pred, aes(x = Meantemp24_s, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Purples") +
  labs(x = "24 hr Mean Temperature",
       y = "Predicted E. coli Geometric Mean",
       fill = "") +
  theme_classic() +
  scale_x_continuous(breaks = at_Meantemp24,
                     labels = round(at_Meantemp24*sd(Vancouver$Meantemp24, na.rm = TRUE) + 
                                      mean(Vancouver$Meantemp24, na.rm = TRUE), 1)) +
  facet_wrap(~ Beachname) +
  theme(legend.position = "bottom") 


#for 95th percentile UVmean24 values
remove(pred)
pred <- predictions(model7, re_formula = NULL, type = "response",
                    newdata = datagrid(model = model7, Beachname = unique, 
                                       Year = unique, 
                                       UVmean24_s = c(1.585497550),
                                       Meantemp24_s = seq(-5.02, 2.33, by = 0.3))) |> 
  posteriordraws()

pred <- pred |> mutate(UVmean24_s = case_when(
  UVmean24_s == 1.585497550 ~ "95th Percentile")) |> 
  mutate(UVmean24_s = as.factor(UVmean24_s)) |> 
  mutate(UVmean24_s = fct_rev(UVmean24_s))

ggplot(pred, aes(x = Meantemp24_s, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Purples") +
  labs(x = "24 hr Mean Temperature",
       y = "Predicted E. coli Geometric Mean",
       fill = "") +
  theme_classic() +
  scale_x_continuous(breaks = at_Meantemp24,
                     labels = round(at_Meantemp24*sd(Vancouver$Meantemp24, na.rm = TRUE) + 
                                      mean(Vancouver$Meantemp24, na.rm = TRUE), 1)) +
  facet_wrap(~ Beachname) +
  theme(legend.position = "bottom") 

# Now calculate/plot the slope of the line (marginal effects) at different values
# We can use just the median (which is 0 mm) and 95th percentile
remove(mfx)
mfx <- marginaleffects(model7, type = "response", re_formula = NULL,
                       variables = "Meantemp24_s",
                       newdata = datagrid(model = model7, Year = unique,
                                          Beachname = unique,
                                          UVmean24_s = c(-2.393851210, 0.002988696, 1.585497550),
                                          Meantemp24_s = c(0.1705406, 1.3344741))) |> 
  posteriordraws()

mfx <- mfx |> mutate(UVmean24_s = case_when(
  UVmean24_s == -2.393851210 ~ "Minimum",
  UVmean24_s == 0.002988696 ~ "Median",
  UVmean24_s == 1.585497550 ~ "95th Percentile")) |> 
  mutate(UVmean24_s = as.factor(UVmean24_s)) |> 
  mutate(UVmean24_s = fct_rev(UVmean24_s))

ggplot(mfx, aes(x = draw, fill = factor(Meantemp24_s))) +
  stat_halfeye() +
  scale_fill_brewer(palette = "Set2", labels=c("Median", "95th Percentile")) + 
  labs(x = "Marginal Effect of 24 hr Mean Temperature",
       y = "Posterior Density",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(-150, 400)) +
  facet_wrap(~ UVmean24_s)


#At minimum 24UVMean value
remove(mfx)
mfx <- marginaleffects(model7, type = "response", re_formula = NULL,
                       variables = "Meantemp24_s",
                       newdata = datagrid(model = model7, Year = unique,
                                          Beachname = unique,
                                          UVmean24_s = c(-2.393851210),
                                          Meantemp24_s = c(0.1705406, 1.3344741))) |> 
  posteriordraws()

mfx <- mfx |> mutate(UVmean24_s = case_when(
  UVmean24_s == -2.393851210 ~ "Minimum")) |> 
  mutate(UVmean24_s = as.factor(UVmean24_s)) |> 
  mutate(UVmean24_s = fct_rev(UVmean24_s))

ggplot(mfx, aes(x = draw, fill = factor(Meantemp24_s))) +
  stat_halfeye() +
  scale_fill_brewer(palette = "Set2", labels=c("Median", "95th Percentile")) +
  labs(x = "Marginal Effect of 24 hr Mean Temperature",
       y = "Posterior Density",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(-150, 400)) +
  facet_wrap(~ Beachname)

#Extracting the median effect value and credible intervals from the plot
marginaleffects(model7, type = "response", re_formula = NULL,
                variables = "Meantemp24_s",
                newdata = datagrid(model = model7, Year = unique,
                                   Beachname = unique,
                                   UVmean24_s = c(-2.393851210),
                                   Meantemp24_s = c(0.1705406, 1.3344741))) |> 
  summary()

#At median 24UVMean value
remove(mfx)
mfx <- marginaleffects(model7, type = "response", re_formula = NULL,
                       variables = "Meantemp24_s",
                       newdata = datagrid(model = model7, Year = unique,
                                          Beachname = unique,
                                          UVmean24_s = c(0.002988696),
                                          Meantemp24_s = c(0.1705406, 1.3344741))) |> 
  posteriordraws()

mfx <- mfx |> mutate(UVmean24_s = case_when(
  UVmean24_s == 0.002988696 ~ "Median")) |> 
  mutate(UVmean24_s = as.factor(UVmean24_s)) |> 
  mutate(UVmean24_s = fct_rev(UVmean24_s))

ggplot(mfx, aes(x = draw, fill = factor(Meantemp24_s))) +
  stat_halfeye() +
  scale_fill_brewer(palette = "Set2", labels=c("Median", "95th Percentile")) +
  labs(x = "Marginal Effect of 24 hr Mean Temperature",
       y = "Posterior Density",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(-150, 400)) +
  facet_wrap(~ Beachname)
#Extracting the median effect value and credible intervals from the plot
marginaleffects(model7, type = "response", re_formula = NULL,
                variables = "Meantemp24_s",
                newdata = datagrid(model = model7, Year = unique,
                                   Beachname = unique,
                                   UVmean24_s = c(0.002988696),
                                   Meantemp24_s = c(0.1705406, 1.3344741))) |> 
  summary()

#At 95th percentile value
remove(mfx)
mfx <- marginaleffects(model7, type = "response", re_formula = NULL,
                       variables = "Meantemp24_s",
                       newdata = datagrid(model = model7, Year = unique,
                                          Beachname = unique,
                                          UVmean24_s = c(1.585497550),
                                          Meantemp24_s = c(0.1705406, 1.3344741))) |> 
  posteriordraws()

mfx <- mfx |> mutate(UVmean24_s = case_when(
  UVmean24_s == 1.585497550 ~ "95th Percentile")) |> 
  mutate(UVmean24_s = as.factor(UVmean24_s)) |> 
  mutate(UVmean24_s = fct_rev(UVmean24_s))

ggplot(mfx, aes(x = draw, fill = factor(Meantemp24_s))) +
  stat_halfeye() +
  scale_fill_brewer(palette = "Set2", labels=c("Median", "95th Percentile")) +
  labs(x = "Marginal Effect of 24 hr Mean Temperature",
       y = "Posterior Density",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(-150, 500)) +
  facet_wrap(~ Beachname)

#Extracting the median effect value and credible intervals from the plot
marginaleffects(model7, type = "response", re_formula = NULL,
                variables = "Meantemp24_s",
                newdata = datagrid(model = model7, Year = unique,
                                   Beachname = unique,
                                   UVmean24_s = c(1.585497550),
                                   Meantemp24_s = c(0.1705406, 1.3344741))) |> 
  summary()


## Test model with full Bayesian missing data imputation as sensitivity analysis

# Create version of dataset with only variables used in final model
Vancouver_model <- Vancouver |> select(Geomean10, LogEc24_s, Salinity_s, Rain48_s, Meantemp24_s, 
                                       UVmean24_s, DaysSinceRain_s, Year, Beachname, Date)

# Examine missingness pattern
md.pattern(Vancouver_model)
# There are 291 rows with at least one missing data value for variables in the model,
# 4536 completely observed rows.

## Use Bayesian imputation during model fitting
# Need to tell brms which variables have missing values, and use non-linear model syntax to fit
mice::md.pattern(Vancouver_model)

# Start with only rainfall as this had the most missing values
missing_rain <- 
  bf(Geomean10 ~ LogEc24_s + Salinity_s + mi(Rain48_s) + Meantemp24_s*UVmean24_s + 
       DaysSinceRain_s + Year + (1 + LogEc24_s + Salinity_s + mi(Rain48_s) + 
       Meantemp24_s*UVmean24_s | Beachname), family = lognormal()) +
  bf(Rain48_s | mi() ~ LogEc24_s + Salinity_s + Meantemp24_s*UVmean24_s + 
       DaysSinceRain_s + Year + (1 + LogEc24_s + Salinity_s + Meantemp24_s*UVmean24_s | Beachname), 
       family = gaussian()) + set_rescor(FALSE)

get_prior(missing_rain, data = Vancouver)

prior_missing <- c(set_prior("lkj(2)", class= "cor"),
                   set_prior("normal(0,1)",class= "b", resp = "Geomean10"),
                   set_prior("student_t(3,0,2.5)", class = "sd", resp = "Geomean10"),
                   set_prior("student_t(3,0,2.5)", class = "sigma", resp = "Geomean10"),
                   set_prior("normal(0,1)",class= "b", resp = "Rain48s"),
                   set_prior("student_t(3,0,2.5)", class = "sd", resp = "Rain48s"),
                   set_prior("student_t(3,0,2.5)", class = "sigma", resp = "Rain48s"))

model7_missing <- brm(missing_rain, data = Vancouver, prior = prior_missing,
                      iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 9, 
                      backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(model7_missing)
plot(model7_missing)
pp_check(model7_missing, ndraws=4000) + coord_cartesian(xlim = c(0, 500))
pp_check(model7_missing, type = "loo_pit_overlay")
mcmc_acf(model7_missing, pars = vars(contains("_s")), lags = 10)
mcplot <- mcmc_pairs(model7_missing, pars = vars(contains("_s")), diag_fun = "den", off_diag_fun = "hex")
mcplot
conditional_effects(model7_missing)

# Now examine model with missing imputation for all variables

missing_full <- 
  bf(Geomean10 | mi() ~ mi(LogEc24_s) + mi(Salinity_s) + mi(Rain48_s) + mi(Meantemp24_s)*UVmean24_s + 
       DaysSinceRain_s + Year + (1 + mi(LogEc24_s) + mi(Salinity_s) + mi(Rain48_s) + 
                                   mi(Meantemp24_s)*UVmean24_s | Beachname), family = lognormal()) +
  bf(Rain48_s | mi() ~ mi(LogEc24_s) + mi(Salinity_s) + mi(Meantemp24_s)*UVmean24_s + 
       DaysSinceRain_s + Year + (1 + mi(LogEc24_s) + mi(Salinity_s) + mi(Meantemp24_s)*UVmean24_s | Beachname), 
     family = gaussian()) +
  bf(LogEc24_s | mi() ~ mi(Rain48_s) + mi(Salinity_s) + mi(Meantemp24_s)*UVmean24_s + 
       DaysSinceRain_s + Year + (1 + mi(Rain48_s) + mi(Salinity_s) + mi(Meantemp24_s)*UVmean24_s | Beachname), 
     family = gaussian()) +
  bf(Salinity_s | mi() ~ mi(LogEc24_s) + mi(Rain48_s) + mi(Meantemp24_s)*UVmean24_s + 
       DaysSinceRain_s + Year + (1 + mi(LogEc24_s) + mi(Rain48_s) + mi(Meantemp24_s)*UVmean24_s | Beachname), 
     family = gaussian()) +
  bf(Meantemp24_s | mi() ~ mi(LogEc24_s) + mi(Salinity_s) + mi(Rain48_s) + UVmean24_s + 
       DaysSinceRain_s + Year + (1 + mi(LogEc24_s) + mi(Salinity_s) + mi(Rain48_s) + UVmean24_s | Beachname), 
     family = gaussian()) + set_rescor(FALSE)

get_prior(missing_full, data = Vancouver)

prior_missing_full <- c(set_prior("lkj(2)", class= "cor"),
                   set_prior("normal(0,1)",class= "b", resp = "Geomean10"),
                   set_prior("student_t(3,0,2.5)", class = "sd", resp = "Geomean10"),
                   set_prior("student_t(3,0,2.5)", class = "sigma", resp = "Geomean10"),
                   set_prior("normal(0,1)",class= "b", resp = "Rain48s"),
                   set_prior("student_t(3,0,2.5)", class = "sd", resp = "Rain48s"),
                   set_prior("student_t(3,0,2.5)", class = "sigma", resp = "Rain48s"),
                   set_prior("normal(0,1)",class= "b", resp = "LogEc24s"),
                   set_prior("student_t(3,0,2.5)", class = "sd", resp = "LogEc24s"),
                   set_prior("student_t(3,0,2.5)", class = "sigma", resp = "LogEc24s"),
                   set_prior("normal(0,1)",class= "b", resp = "Salinitys"),
                   set_prior("student_t(3,0,2.5)", class = "sd", resp = "Salinitys"),
                   set_prior("student_t(3,0,2.5)", class = "sigma", resp = "Salinitys"),
                   set_prior("normal(0,1)",class= "b", resp = "Meantemp24s"),
                   set_prior("student_t(3,0,2.5)", class = "sd", resp = "Meantemp24s"),
                   set_prior("student_t(3,0,2.5)", class = "sigma", resp = "Meantemp24s"))

model7_missing_full <- brm(missing_full, data = Vancouver, prior = prior_missing_full,
                      iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 9, 
                      backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(model7_missing_full)
plot(model7_missing_full)
pp_check(model7_missing_full, ndraws=4000) + coord_cartesian(xlim = c(0, 500))
pp_check(model7_missing_full, type = "loo_pit_overlay")
mcmc_acf(model7_missing_full, pars = vars(contains("_s")), lags = 10)
mcplot <- mcmc_pairs(model7_missing_full, pars = vars(contains("_s")), diag_fun = "den", off_diag_fun = "hex")
mcplot

conditional_effects(model7_missing_full, "Rain48_s")
