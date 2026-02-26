library(tidyverse)
library(gert)

### The R Lab: Simulating the "Policy Mix"
#     We will simulate a "shock" where the government increases spending (G), shifting the IS curve out.

library(tidyverse)

# 1. Setup Data: We expand the range to 1300 to see the new Equilibrium
df_islm <- tibble(Y = seq(200, 1300, by = 10)) %>%
  mutate(
    # IS Curve Base (A=400, b=10, mpc=0.8)
    is_base = (400 - 0.2 * Y) / 10,
    # IS Expansion (G increases by 100, shifting A to 500)
    is_shifter = (500 - 0.2 * Y) / 10,
    # LM Curve (MS=100, k=0.3, h=10)
    lm_base = (0.3 * Y - 100) / 10
  )

# 2. Plotting with explicit points for both Equilibria
ggplot(df_islm, aes(x = Y)) +
  # Draw the Curves
  geom_line(aes(y = is_base, color = "IS (Base)"), linewidth = 1.2) +
  geom_line(aes(y = is_shifter, color = "IS (Expansion)"), linewidth = 1.2, linetype = "dashed") +
  geom_line(aes(y = lm_base, color = "LM (Money Market)"), linewidth = 1.2) +
  
  # Original Equilibrium (Y=1000, r=20)
  annotate("point", x = 1000, y = 20, size = 4, color = "blue") +
  annotate("text", x = 1000, y = 17, label = "Original Eq.", color = "blue", fontface="bold") +
  
  # New Equilibrium (Y=1200, r=26)
  annotate("point", x = 1200, y = 26, size = 4, color = "darkred") +
  annotate("text", x = 1200, y = 23, label = "New Eq.", color = "darkred", fontface="bold") +
  
  # Styling
  scale_color_manual(values = c("IS (Base)" = "blue", 
                                "IS (Expansion)" = "darkblue", 
                                "LM (Money Market)" = "red")) +
  coord_cartesian(ylim = c(0, 40)) + 
  labs(title = "Macro Lab: IS-LM Policy Interaction",
       subtitle = "The expansion shifts IS right, increasing Y but also pushing up r (Crowding Out).",
       x = "National Income (Y)", y = "Interest Rate (r)", color = "Market Curves") +
  theme_minimal() +
  theme(legend.position = "bottom") # Legend at bottom so it doesn't block the 'X'


