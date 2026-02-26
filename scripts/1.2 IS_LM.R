library(tidyverse)
library(gert)

### The R Lab: Simulating the "Policy Mix"
#     We will simulate a "shock" where the government increases spending (G), shifting the IS curve out.

library(tidyverse)

# 1. Setup a wider Income Range to see the full interaction
y_range <- seq(200, 1200, by = 10)

# 2. Define the Structural Parameters
# IS: r = (A - (1-c)Y) / b 
# LM: r = (kY - MS) / h    
df_islm <- tibble(Y = y_range) %>%
  mutate(
    # IS Curve (Base: A=400, b=10, c=0.8)
    is_base = (400 - 0.2 * Y) / 10,
    # IS Expansion (G increases by 100, shifting A to 500)
    is_shifter = (500 - 0.2 * Y) / 10,
    # LM Curve (MS=100, k=0.3, h=10)
    lm_base = (0.3 * Y - 100) / 10
  )

# 3. Visualise (Legend moved to the bottom to avoid blocking)
ggplot(df_islm, aes(x = Y)) +
  # Draw the Curves
  geom_line(aes(y = is_base, color = "IS (Base)"), linewidth = 1.2) +
  geom_line(aes(y = is_shifter, color = "IS (Expansion)"), linewidth = 1.2, linetype = "dashed") +
  geom_line(aes(y = lm_base, color = "LM (Money Market)"), linewidth = 1.2) +
  
  # Add Equilibrium Points
  # Intersection 1: 0.5Y = 500 => Y=1000, r=20
  # Intersection 2: 0.5Y = 600 => Y=1200, r=26
  annotate("point", x = 1000, y = 20, size = 4, color = "black") +
  annotate("text", x = 1000, y = 18, label = "Original Eq.", size = 3) +
  
  # Styling
  scale_color_manual(values = c("IS (Base)" = "blue", 
                                "IS (Expansion)" = "darkblue", 
                                "LM (Money Market)" = "red")) +
  coord_cartesian(ylim = c(0, 40)) + # Limits to keep the "X" shape clear
  labs(title = "Macro Lab: IS-LM Policy Interaction",
       subtitle = "A Fiscal Expansion (Dashed Blue) increases GDP (Y) but 'Crowds Out' investment by raising r.",
       x = "National Income (Y)", y = "Interest Rate (r)", color = "Market Curves") +
  theme_minimal() +
  # --- CRITICAL FIX: Move Legend to Bottom ---
  theme(legend.position = "bottom") 

library(gert)
git_add(".")
git_commit("Fix IS-LM plot legend and update coordinates")
git_push()

