

### The R Lab: Simulating a Supply Shock (Stagflation)
# Let’s simulate the "Nightmare Scenario" of the 1970s: An Oil Price Shock that 
#   shifts the SRAS curve inward, causing both higher prices and lower output.

library(tidyverse)

# 1. Setup the GDP (Y) Range
y_range <- seq(50, 150, by = 1)

# 2. Define the Structural Curves
df_adas <- tibble(Y = y_range) %>%
  mutate(
    # AD Curve: P = (Fixed_M / Y) * 10
    ad_base = 150 - 1.0 * Y,
    
    # SRAS Base: P = 20 + 0.5 * Y
    sras_base = 20 + 0.5 * Y,
    
    # SRAS Shock (Costs rise by 30 units)
    sras_shock = 50 + 0.5 * Y
  )

# 3. Equilibrium Points (Math check)
# Base: 150 - Y = 20 + 0.5Y => 130 = 1.5Y => Y = 86.6, P = 63.3
# Shock: 150 - Y = 50 + 0.5Y => 100 = 1.5Y => Y = 66.6, P = 83.3

# 4. Visualise the "Stagflation" (Price on Vertical, GDP on Horizontal)
ggplot(df_adas, aes(x = Y)) +
  # AD Curve
  geom_line(aes(y = ad_base, color = "Aggregate Demand"), linewidth = 1.2) +
  # SRAS Curves
  geom_line(aes(y = sras_base, color = "SRAS (Normal)"), linewidth = 1.2) +
  geom_line(aes(y = sras_shock, color = "SRAS (Oil Shock)"), linewidth = 1.2, linetype = "dashed") +
  # LRAS (Full Employment at Y = 100)
  geom_vline(xintercept = 100, color = "darkgreen", linetype = "dotted", linewidth = 1) +
  
  # Points
  annotate("point", x = 86.6, y = 63.3, size = 3) + # Original
  annotate("point", x = 66.6, y = 83.3, size = 3, color = "red") + # Shocked
  
  # Styling
  scale_color_manual(values = c("Aggregate Demand" = "blue", 
                                "SRAS (Normal)" = "green4", 
                                "SRAS (Oil Shock)" = "red")) +
  coord_cartesian(ylim = c(40, 120)) +
  labs(title = "Macro Lab: AD-AS & Stagflation",
       subtitle = "A Supply Shock (Red Dash) creates the 'Worst of Both Worlds': Higher P and Lower Y.",
       x = "Real GDP (Y)", y = "Price Level (P)", color = "Market Curves") +
  theme_minimal() +
  theme(legend.position = "bottom")

library(gert)
git_add(".")
git_commit("Add AD-AS Stagflation Simulation Lab")
git_push()
