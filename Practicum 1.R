#Title: Practicum 1
#Author: Kerri Rose Riley
#Purpose: Replicate Figures 1 and 2 from Bartels (2000), “Partisanship and Voting Behavior”
#Requires: Cumulative CCES Dataset (2006-2024)
#Output: Figures and Quarto Document typeset as a .pdf

# Packages
library(tidyverse) # Load the tidyverse package for data wrangling and analysis
library(haven) # Load haven so read_dta() can be used to import Stata data files
library(ggplot2) # Load ggplot2 for creating graphs and plots

# Set the working directory
setwd("~/Desktop/Current Classes/R/Practicum 1")

# Read the data file
ces <- read_dta("~/Desktop/Current Classes/R/Practicum 1/Data/ces.dta")
 
# Select the variables Bartel would use
ces_1 <- ces |> select(year, pid7, vv_turnout_gvm)
 
table(ces_1$year)
table(ces_1$pid7)
table(ces_1$vv_turnout_gvm)

# Recode party identification for Figure 1
ces_1 <- ces_1 |>
  mutate(
    # Exclude non-substantive responses
    pid = if_else(pid7 %in% 1:7, as.numeric(pid7), NA_real_),

    # Strong partisans
    strong = case_when(
      pid %in% c(1, 7) ~ 1,
      pid %in% 2:6 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Weak partisans
    weak = case_when(
      pid %in% c(2, 6) ~ 1,
      pid %in% c(1, 3, 4, 5, 7) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Party Leaners
    leaners = case_when(
      pid %in% c(3, 5) ~ 1,
      pid %in% c(1, 2, 4, 6, 7) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Pure Independents
    indeps = case_when(
      pid == 4 ~ 1,
      pid %in% c(1, 2, 3, 5, 6, 7) ~ 0,
      TRUE ~ NA_real_
    )
    
  )


#ggplot
# Figure 1A: Strong and Weak Partisans
fig1a <- ggplot(ces_1, aes(x = year)) +
  geom_line(aes(y = strong, linetype = "Strong Identifiers"),
            linewidth = 1.2) +
  geom_line(aes(y = weak, linetype = "Weak Identifiers"),
            linewidth = 1.2) +
  geom_point(aes(y = strong), size = 2) +
  geom_point(aes(y = weak), size = 2) +
  scale_x_continuous(
    limits = c(2006, 2024),
    breaks = seq(2006, 2024, by = 2)
  ) +
  scale_y_continuous(
    limits = c(0, 0.5),
    breaks = seq(0, 0.5, by = 0.1)
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("Strong Identifiers" = "solid",
               "Weak Identifiers" = "dashed")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Figure 1A. Strong and Weak Partisans"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# Display the graphs
fig1a


# Figure 2 Recoding
figure2_data <- ces_1 |>
  mutate(
    # Convert turnout variable to readable labels
    turnout_label = as_factor(vv_turnout_gvm),
    
    # Identify party identifiers
    identifiers = case_when(
      pid %in% c(1, 2, 6, 7) ~ 1,
      pid %in% c(3, 4, 5) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Party identifiers among voters
    identvote = if_else(
      turnout_label == "Voted",
      identifiers,
      NA_real_
    ),
    
    # Party identifiers among those with no voting record
    identnovote = if_else(
      turnout_label == "No Record of Voting",
      identifiers,
      NA_real_
    ),
    
    # Recode turnout
    turnout = case_when(
      turnout_label == "Voted" ~ 1,
      turnout_label == "No Record of Voting" ~ 0,
      TRUE ~ NA_real_
    )
  )

# Figure 2: 


# Figure 1B: Pure Independents and Independent Leaners
fig1b <- ggplot(ces_1, aes(x = year)) +
  geom_line(aes(y = indeps, linetype = "Pure Independents"),
            linewidth = 1.2) +
  geom_line(aes(y = leaners, linetype = "Independent Leaners"),
            linewidth = 1.2) +
  geom_point(aes(y = indeps), size = 2) +
  geom_point(aes(y = leaners), size = 2) +
  scale_x_continuous(
    limits = c(2006, 2024),
    breaks = seq(2006, 2024, by = 2)
  ) +
  scale_y_continuous(
    limits = c(0, 0.5),
    breaks = seq(0, 0.5, by = 0.1)
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("Pure Independents" = "solid",
               "Independent Leaners" = "dashed")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Figure 1B. Pure Independents and Independent Leaners"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

fig1b