


### 2. The R Lab: Simulating the "Volcker Shock"
#       We will simulate a central bank that tries to "buy" lower unemployment with inflation, 
#         only to find that expectations shift the curve against them.

library(tidyverse)

library(tidyverse)

# 1. Setup Unemployment Range
df_pc <- tibble(u = seq(2, 10, by = 0.1)) %>%
  mutate(
    # SRPC: Inflation = Expected_Inf - 1.0 * (u - 5)
    srpc_low = 2 - 1.0 * (u - 5),
    srpc_high = 6 - 1.0 * (u - 5)
  )

# 2. Plotting with the Vertical NAIRU
ggplot(df_pc, aes(x = u)) +
  geom_line(aes(y = srpc_low, color = "SRPC (2% Exp)"), linewidth = 1.2) +
  geom_line(aes(y = srpc_high, color = "SRPC (6% Exp)"), linewidth = 1.2, linetype = "dashed") +
  
  # --- THE NAIRU (Natural Rate of Unemployment) ---
  geom_vline(xintercept = 5, color = "darkgreen", linetype = "dotted", linewidth = 1.2) +
  annotate("text", x = 5.3, y = 9, label = "NAIRU (u*)", color = "darkgreen", fontface = "bold") +
  
  # Points
  annotate("point", x = 5, y = 2, size = 3, color = "blue") +
  annotate("point", x = 5, y = 6, size = 3, color = "red") +
  
  # Styling
  scale_color_manual(values = c("SRPC (2% Exp)" = "blue", "SRPC (6% Exp)" = "red")) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(title = "Macro Lab: The Phillips Curve & NAIRU",
       subtitle = "In the Long Run, trying to lower unemployment below 5% just shifts the curve up.",
       x = "Unemployment Rate (%)", y = "Inflation Rate (%)", color = "Short Run Curves") +
  theme_minimal() +
  theme(legend.position = "bottom")


library(gert)
git_add(".")
git_commit("Add Expectations-Augmented Phillips Curve Lab")
git_push()

library(gert)
git_add(".")
git_commit("Corrected IS-LM and Phillips Curve (NAIRU) plots")
git_push()

library(gert)
git_add(".")
git_commit("Finalising foundational Macro labs before pause")
git_push()

