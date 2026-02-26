library(tidyverse)
library(shiny)
library(bslib)
library(gert)

### VISUALISATION AND INTERACTIVITY


library(shiny)
library(ggplot2)

# --- UI: The Control Panel ---
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),
  titlePanel("Scarth (1988) IS-LM Equilibrium Solver"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Policy Shocks (The 'B' Vector)"),
      sliderInput("dG", "Fiscal Shock (dG):", min = -50, max = 50, value = 0),
      sliderInput("dM", "Monetary Shock (dM):", min = -50, max = 50, value = 0),
      
      hr(),
      
      h4("2. Structural Parameters (Matrix 'A')"),
      sliderInput("Cy", "Marginal Propensity to Consume (Cy):", 
                  min = 0.1, max = 0.9, value = 0.8, step = 0.05),
      
      # FIX 1: Updated Slider range for steeper slope AND added the comma below
      sliderInput("Ir", "Inv. Sensitivity to Interest (Ir):", 
                  min = -200, max = -20, value = -50, step = 10), 
      
      sliderInput("Ly", "Money Demand to Income (Ly):", 
                  min = 0.1, max = 2.0, value = 0.5),
      sliderInput("Lr", "Money Demand to Interest (Lr):", 
                  min = -500, max = -10, value = -100),
      
      hr(),
      helpText("Base Equilibrium: Y=1000, R=5%")
    ),
    
    mainPanel(
      # The Hero Visual
      plotOutput("islmPlot", height = "450px"),
      
      # The Math Output
      br(),
      fluidRow(
        column(6, 
               h4("Matrix Solution (dY, dR)"),
               verbatimTextOutput("matrix_solution")
        ),
        column(6, 
               h4("Stability Check"),
               verbatimTextOutput("stability_check")
        )
      )
    )
  )
)

# --- SERVER: The Calculation Engine ---
server <- function(input, output) {
  
  # 1. Reactive Calculation of A and B
  model_solution <- reactive({
    t <- 0.2 
    
    # Matrix A (LHS coefficients)
    a11 <- 1 - input$Cy * (1 - t)
    a12 <- -input$Ir
    a21 <- input$Ly
    a22 <- input$Lr
    
    A_matrix <- matrix(c(a11, a12, a21, a22), nrow = 2, byrow = TRUE)
    B_vector <- c(input$dG, input$dM)
    
    # Solve Ax = B with error handling
    solution <- tryCatch({
      solve(A_matrix, B_vector)
    }, error = function(e) return(c(0, 0)))
    
    list(
      dY = solution[1],
      dR = solution[2],
      matrix = A_matrix,
      det = det(A_matrix)
    )
  })
  
  # 2. Text Outputs
  output$matrix_solution <- renderPrint({
    sol <- model_solution()
    cat(sprintf("Change in Output (dY):   %.2f\n", sol$dY))
    cat(sprintf("Change in Interest (dR): %.4f", sol$dR))
  })
  
  output$stability_check <- renderPrint({
    det_val <- model_solution()$det
    cat("Determinant of A:", round(det_val, 2), "\n")
    if(det_val < 0) {
      cat("STATUS: Stable System (Normal Case)")
    } else {
      cat("STATUS: UNSTABLE! (Check Slopes)")
    }
  })
  
  # 3. Visualization Logic (ggplot2)
  output$islmPlot <- renderPlot({
    sol <- model_solution()
    
    # Base levels
    Y_base <- 1000
    R_base <- 5
    
    # New Equilibrium levels
    Y_star <- Y_base + sol$dY
    R_star <- R_base + sol$dR
    
    # FIX 2: Corrected Slope Logic (Removed extra negative sign)
    t <- 0.2
    # IS Slope: (1 - Cy(1-t)) / Ir -> Result is Negative because Ir is Negative
    slope_IS <- (1 - input$Cy * (1 - t)) / input$Ir
    
    # LM Slope: -Ly / Lr -> Result is Positive because Lr is Negative
    slope_LM <- -input$Ly / input$Lr
    
    # Dummy range for plotting
    plot_range <- data.frame(Y = c(800, 1200))
    
    ggplot(plot_range, aes(x = Y)) +
      # Plot IS Curve
      stat_function(fun = function(y) slope_IS * (y - Y_star) + R_star, 
                    geom = "line", color = "#2c3e50", size = 1.5) +
      annotate("text", x = Y_base + 180, y = slope_IS * (180) + R_star + 0.5, 
               label = "IS", color = "#2c3e50", size = 6, fontface = "bold") +
      
      # Plot LM Curve
      stat_function(fun = function(y) slope_LM * (y - Y_star) + R_star, 
                    geom = "line", color = "#e74c3c", size = 1.5) +
      annotate("text", x = Y_base + 180, y = slope_LM * (180) + R_star - 0.5, 
               label = "LM", color = "#e74c3c", size = 6, fontface = "bold") +
      
      # Equilibrium Point (Using annotate to avoid row-mismatch warning)
      annotate("point", x = Y_star, y = R_star, size = 6, color = "black") +
      
      # Aesthetics
      labs(title = "IS-LM Equilibrium (Scarth Framework)",
           subtitle = paste("Equilibrium: Y =", round(Y_star, 1), "| R =", round(R_star, 2), "%"),
           x = "Output (Y)", y = "Interest Rate (R)") +
      theme_minimal(base_size = 14) +
      coord_cartesian(ylim = c(0, 10), xlim = c(800, 1200)) 
  })
}

shinyApp(ui = ui, server = server)

# Commit and Push to GitHub repo




