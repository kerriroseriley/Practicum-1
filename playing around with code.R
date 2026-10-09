#ggplot

# Figure 1A: Strong and Weak Partisans
fig1a <- ggplot(figure1_data, aes(x = year)) +
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

# Figure 1B: Pure Independents and Independent Leaners
fig1b <- ggplot(figure1_data, aes(x = year)) +
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

# Display the graphs
fig1a
fig1b

# Lattice:
# Figure 1A: Strong and Weak Partisans
library(lattice)
 
xyplot(
  strong + weak ~ year,
  data = figure1_data,
  type = "o",
  lwd = 1.2,
  lty = c(1, 2),
  pch = 16,
  xlab = "",
  ylab = "",
  main = "Figure 1A. Strong and Weak Partisans",
  scales = list(
    x = list(limits = c(2006, 2024), at = seq(2006, 2024, by = 2)),
    y = list(limits = c(0, 0.5), at = seq(0, 0.5, by = 0.1))
  ),
  auto.key = list(
    text = c("Strong Identifiers", "Weak Identifiers"),
    lines = TRUE,
    points = TRUE
  )
)

# Figure 1B: Pure Independents and Independent Leaners
xyplot(
  indeps + leaners ~ year,
  data = figure1_data,
  type = "o",
  lwd = 1.2,
  lty = c(1, 2),
  pch = 16,
  xlab = "",
  ylab = "",
  main = "Figure 1B. Pure Independents and Independent Leaners",
  scales = list(
    x = list(limits = c(2006, 2024), at = seq(2006, 2024, by = 2)),
    y = list(limits = c(0, 0.5), at = seq(0, 0.5, by = 0.1))
  ),
  auto.key = list(
    text = c("Pure Independents", "Independent Leaners"),
    lines = TRUE,
    points = TRUE
  )
)