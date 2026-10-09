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



# Graphing Section

# Figure 1A: Strong and weak partisans
plot(x = 0, type = "n",
     xlim = range(figure1_data$year),xaxt = "n",
     ylim = c(0, 0.5), yaxt = "n",
     xlab = " ",
     ylab = " ",
     main = "Distribution of Party Identification",)

# Add axis labels
axis(side = 1, at = seq(2006, 2024, by = 8))
axis(side = 2, at = seq(0, 0.5, by = 0.1))

lines(figure1_data$year, figure1_data$strong,
      type = "o", col = "black", lty = 1, pch=16)

lines(figure1_data$year, figure1_data$weak,
      type = "o", col = "black", lty = 2)

legend("bottom",
       legend = c("Strong Identifiers", "Weak Identifiers"),
       lty = c(2, 1), pch = c(1, 16),
       bty = "n")

# Figure 1B: Independents and leaners
plot(x = 0, type = "n",
     xlim = range(figure1_data$year), xaxt = "n",
     ylim = c(0, 0.5), yaxt = "n",
     xlab = " ",
     ylab = " ",
     main=" ")

lines(figure1_data$year, figure1_data$indeps,
      type = "o", col = "black", lty = 1, pch=16)

lines(figure1_data$year, figure1_data$leaners,
      type = "o", col = "black", lty = 2)

# Add axis labels
axis(side = 1, at = seq(2006, 2024, by = 8))
axis(side = 2, at = seq(0, 0.5, by = 0.1))

legend("topleft",
       legend = c("Pure Independents", "Independent Leaners"),
       lty = c(2, 1), pch = c(1, 16),
       bty = "n")


# Figure 2


