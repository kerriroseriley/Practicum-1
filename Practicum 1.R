#Title: Practicum 1
#Author: Kerri Rose Riley
#Purpose: Replicate Figures 1 and 2 from Bartels (2000), “Partisanship and Voting Behavior”
#Requires: Cumulative CCES Dataset (2006-2024)
#Output: Figures as PDFs (ggplot only) 

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
    
    # Party leaners
    leaners = case_when(
      pid %in% c(3, 5) ~ 1,
      pid %in% c(1, 2, 4, 6, 7) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Pure independents
    indeps = case_when(
      pid == 4 ~ 1,
      pid %in% c(1, 2, 3, 5, 6, 7) ~ 0,
      TRUE ~ NA_real_
    )
    
  )

# Figure 2 coding
ces_1 <- ces_1 |>
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



# Create two plots stacked vertically
par(mfrow = c(2, 1), mar = c(3, 4, 2, 2))

# Top graph: Strong and weak partisans
plot(x = 0, type = "n",
     xlim = range(figure1_data$year),
     ylim = c(0, 0.5),
     xlab = "",
     ylab = "Proportion",
     main = "Distribution of Party Identification")

lines(figure1_data$year, figure1_data$strong,
      type = "o", col = "black", lty = 1)

lines(figure1_data$year, figure1_data$weak,
      type = "o", col = "black", lty = 2)

legend("topright",
       legend = c("Strong Identifiers", "Weak Identifiers"),
       lty = c(1, 2), pch = 1, bty = "n")

# Bottom graph: Independents and leaners
plot(x = 0, type = "n",
     xlim = range(figure1_data$year),
     ylim = c(0, 0.5),
     xlab = "Year",
     ylab = "Proportion")

lines(figure1_data$year, figure1_data$indeps,
      type = "o", col = "black", lty = 1)

lines(figure1_data$year, figure1_data$leaners,
      type = "o", col = "black", lty = 2)

legend("topright",
       legend = c("Pure Independents", "Independent Leaners"),
       lty = c(1, 2), pch = 1, bty = "n")

# Reset plotting layout
par(mfrow = c(1, 1))

