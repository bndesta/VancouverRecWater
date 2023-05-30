
pacman::p_load(rio,
               here,
               brms,
               Matrix,
               tidyverse,  
               tsibble, 
               janitor,
               lubridate,
               tidybayes, 
               bayesplot,
               marginaleffects,
               modelr)

install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::check_cmdstan_toolchain(fix = TRUE)
library(cmdstanr)
install_cmdstan(dir="C:/My project", overwrite=T)
library(rstanarm)

# Load datasets
Vancouver <- import(here("C:/My project", "VancouverMerged15052023.xlsx"))

print(Vancouver)
summary(Vancouver)
# Clean up data 

#Changing the values of the geometric means 0 to 10, as that's the default value Metro Vancouver use
Vancouver["Geomean10"][Vancouver["Geomean10"] == 0] <- 10
Vancouver["PrevGeomean10"][Vancouver["PrevGeomean10"] == 0] <- 10


## Preparing the data 
# Changing variable formats
Vancouver <- Vancouver |> 
  mutate(Beachname = as.factor(BeachName)) |> 
  mutate(Year = as.factor(Year.x)) |> 
  mutate(Geomean10 = as.numeric(Geomean10)) |> 
  mutate(PrevGeomean10 = as.numeric(PrevGeomean10)) |> 
  mutate(Salinity = as.numeric(MeanSalinty)) |> 
  mutate(Rain48 = as.numeric(Rain48.x)) |> 
  mutate(Rain48_y = as.numeric(Rain48.y)) |> 
  mutate(Meantemp24 = as.numeric(MeanTemp24)) |> 
  mutate(UVmean24 = as.numeric(MeanUV24)) |> 
  mutate(DaysSinceRain = as.numeric(DaysSinceRain.x)) |> 
  mutate(DaysSinceRain_y = as.numeric(DaysSinceRain.y))

summary(Vancouver)
print(Vancouver)

# Explore data

# Check differences by beach

Vancouver |> group_by(Beachname) |> 
  summarize(count = n(), 
            mean_Ec = mean(Geomean10, na.rm=TRUE))

Vancouver |> 
  ggplot(aes(x = Geomean10)) +
  geom_histogram(binwidth = 250)

Vancouver |> 
  ggplot(aes(x = Year, y = Geomean10, colour = Beachname)) +
  geom_point()

#Checking relation between variables
Vancouver |> 
  ggplot(aes(x = Rain48, y = DaysSinceRain)) +
  geom_point()

Vancouver |> select(Rain48, DaysSinceRain) |> corrr::correlate()

Vancouver |> 
  ggplot(aes(x = Meantemp24, y = UVmean24)) +
  geom_point()

Vancouver |> select(Meantemp24, UVmean24) |> corrr::correlate()


#Summary of all the predictor variables
Vancouver  |> 
  rstatix::get_summary_stats(PrevGeomean, Salinity, Rain48, Meantemp24, UVmean24,
                             DaysSinceRain, Geomean10, type = "full")


#Log transforming variables (Geomean E.coli and Previous day Geomean E.coli)

Vancouver <- Vancouver |> 
  mutate(LogEc = log(Geomean10)) |>  
  relocate(LogEc, .after=Geomean10)

Vancouver <- Vancouver |> 
  mutate(LogEc24 = log(PrevGeomean10)) |>  
  relocate(LogEc24, .after=PrevGeomean10)

#Plotting to see the distribution
Vancouver |> 
  ggplot(aes(x = LogEc)) +
  geom_histogram()

Vancouver |> 
  ggplot(aes(x = LogEc24)) +
  geom_histogram()

### Alternate approach to centering and standardizing 
# to keep everything in same dataframe with new variable names

Vancouver <- Vancouver |> 
  mutate(Salinity_s = (Salinity - mean(Salinity, na.rm=TRUE)) / sd(Salinity, na.rm=TRUE)) |>
  mutate(Rain48_s = (Rain48 - mean(Rain48, na.rm=TRUE)) / sd(Rain48, na.rm=TRUE)) |> 
  mutate(Meantemp24_s = (Meantemp24 - mean(Meantemp24, na.rm=TRUE)) / sd(Meantemp24, na.rm=TRUE)) |> 
  mutate(UVmean24_s = (UVmean24 - mean(UVmean24, na.rm=TRUE)) / sd(UVmean24, na.rm=TRUE)) |> 
  mutate(DaysSinceRain_s = (DaysSinceRain - mean(DaysSinceRain, na.rm=TRUE)) / sd(DaysSinceRain, na.rm=TRUE)) |> 
  mutate(LogEc24_s = (LogEc24 - mean(LogEc24, na.rm=TRUE)) / sd(LogEc24, na.rm=TRUE)) |> 
  mutate(PrevGeomean_s = (PrevGeomean10 - mean(PrevGeomean10, na.rm=TRUE)) / sd(PrevGeomean10, na.rm=TRUE)) 


Vancouver  |> 
  rstatix::get_summary_stats(LogEc24_s, Salinity_s, Rain48_s, Meantemp24_s, 
                             UVmean24_s, DaysSinceRain_s, LogEc, type = "full")


Vancouver |> summarize_all(~ sum(is.na(.)))

## Examine possible different priors for SD variables

priors <- tibble(student_t = rt(n = 10000, df = 3, ncp = 2.5),
                 exp = rexp(10000, rate = 1)) 

priors |> ggplot(aes(x = student_t)) + geom_density(colour = "blue") +
  geom_density(aes(x = exp)) + 
  xlim(0, 20) + theme_minimal()


#Plotting mean of the geomeans by year 

summary_table <- Vancouver %>%                                        
  group_by(Year,Beachname) %>%                                             
  summarise(                                                         
    Ecoli_mean  = round(mean(Geomean10, na.rm=T), digits = 1)  
  )

summary_table  # print

#Plot mean of the Ecoli geomeans by year
ggplot(summary_table, aes(x = Year, y = Ecoli_mean, color = Beachname)) +
  geom_line(aes(group = Beachname), alpha = 1, size = 1.2) +
  theme_bw() +
  labs(,
       x = "Year",
       y = "Year Average of Daily Geometric Mean of E.coli(CFU/100ml)",
       color = NULL
  )