library(tidyverse)
library(patchwork)

### The R Utility-Lab: Visualising the "Optimal Bundle"
##     We will simulate a consumer with £100 income, where Medicine costs £5 and 
#       Food costs £2.

# STEPS
# 1. Setup Parameters
income <- 100
px <- 5
py <- 2
alpha <- 0.5  # Preference for Medicine
beta <- 0.5   # Preference for Food

# 2. Calculate the Optimal Bundle (The Calculus result)
# X* = (alpha / (alpha + beta)) * (Income / Px)
x_opt <- (alpha / (alpha + beta)) * (income / px) # 10
y_opt <- (beta / (alpha + beta)) * (income / py)  # 25
u_max <- (x_opt^alpha) * (y_opt^beta)             # ~15.8

# 3. Create the Budget Line and Indifference Curve data
x_range <- seq(0.1, 20, by = 0.1)
df_utility <- tibble(x = x_range) %>%
  mutate(
    # Budget Line: Y = (I - Px*X) / Py
    budget_y = (income - px * x) / py,
    # Indifference Curve: Y = (U_max / X^alpha)^(1/beta)
    indiff_y = (u_max / (x^alpha))^(1/beta)
  ) %>%
  filter(budget_y >= 0) # Keep it in the positive quadrant

# 4. Plotting the "Marshallian" Consumer Choice
ggplot(df_utility, aes(x = x)) +
  # The Budget Constraint (Straight Line)
  geom_line(aes(y = budget_y, color = "Budget Constraint"), linewidth = 1.2) +
  # The Indifference Curve (Curved Line)
  geom_line(aes(y = indiff_y, color = "Indifference Curve (U_max)"), linewidth = 1.2) +
  # The Tangency Point (The Optimal Choice)
  annotate("point", x = x_opt, y = y_opt, color = "darkred", size = 4) +
  annotate("text", x = x_opt + 2, y = y_opt + 5, 
           label = paste0("Optimal Bundle\n(X=", x_opt, ", Y=", y_opt, ")"), size = 3) +
  # Aesthetic tweaks
  scale_color_manual(values = c("Budget Constraint" = "black", 
                                "Indifference Curve (U_max)" = "blue")) +
  coord_cartesian(ylim = c(0, 50)) +
  labs(title = "Micro Lab: Consumer Choice & Utility Maximisation",
       subtitle = "The point where the budget line is tangent to the highest possible indifference curve.",
       x = "Medicine (X)", y = "Food/Other (Y)", color = "Constraint/Preference") +
  theme_minimal()


## In postgraduate work, we don't always have a simple Cobb-Douglas function. We use 
#     R's numerical optimisers to find the answer for any complex function.

# Define a 'Negative Utility' function (because optimisers look for the minimum)
calc_neg_utility <- function(X, px, py, income) {
  Y <- (income - px * X) / py
  if (Y < 0) return(1e10) # Penalty for breaking the budget
  utility <- (X^0.5) * (Y^0.5)
  return(-utility) # Return negative to find the maximum
}

# Run the optimiser
result <- optim(par = 1, fn = calc_neg_utility, px = px, py = py, income = income, method = "Brent", lower = 0, upper = 20)

cat("Optimised Quantity of X:", round(result$par, 2), "\n")

## The R "Price Pivot" Lab (Substitution vs. Income Effects)

# 1. Define the Utility Function (Cobb-Douglas: U = X^0.5 * Y^0.5)
# We want to MAXIMIZE utility, but optim() MINIMIZES.
# So we return the Negative Utility.
calc_neg_u <- function(X, income, px, py) {
  Y <- (income - px * X) / py
  if (X <= 0 | Y <= 0) return(1e10) # Penalty for impossible bundles
  u <- (X^0.5) * (Y^0.5)
  return(-u)
}

# 2. SCENARIO A: Original Price (Px = 5)
res_old <- optim(par = 5, fn = calc_neg_u, income = 100, px = 5, py = 2, 
                 method = "Brent", lower = 0.1, upper = 20)
x_old <- res_old$par # Result: 10

# 3. SCENARIO B: New Price (Px = 2)
res_new <- optim(par = 5, fn = calc_neg_u, income = 100, px = 2, py = 2, 
                 method = "Brent", lower = 0.1, upper = 50)
x_new <- res_new$par # Result: 25

# 4. Data for Visualizing the 'Pivot'
x_range <- seq(0.1, 50, by = 0.5)
df_pivot <- tibble(x = x_range) %>%
  mutate(
    budget_old = (100 - 5 * x) / 2,
    budget_new = (100 - 2 * x) / 2
  ) %>%
  filter(budget_old >= 0 | budget_new >= 0)

# 5. Plot the Change
ggplot(df_pivot, aes(x = x)) +
  geom_line(aes(y = budget_old, color = "Old Budget (Px=5)"), linewidth = 1) +
  geom_line(aes(y = budget_new, color = "New Budget (Px=2)"), linewidth = 1) +
  # Points
  annotate("point", x = x_old, y = (100 - 5*x_old)/2, color = "black", size = 3) +
  annotate("point", x = x_new, y = (100 - 2*x_new)/2, color = "red", size = 3) +
  # Labels
  annotate("text", x = x_old + 3, y = 30, label = "Original Choice") +
  annotate("text", x = x_new + 3, y = 30, label = "New Choice", color = "red") +
  coord_cartesian(ylim = c(0, 50)) +
  labs(title = "Micro Lab: Price Pivot & Choice",
       subtitle = "When Px falls, the budget line pivots outward.",
       x = "Medicine (X)", y = "Food (Y)", color = "Budget") +
  theme_minimal()


