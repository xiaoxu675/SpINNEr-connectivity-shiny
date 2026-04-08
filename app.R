library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(igraph)
library(shinythemes)

# ================================
# USER INTERFACE
# ================================

ui <- fluidPage(
  navbarPage(
    "Functional Connectivity Visualization (SpINNEr Model)",
    theme = shinytheme("lumen"),
    
    tabPanel(
      "Connectivity Results",
      sidebarLayout(
        sidebarPanel(
          
          radioButtons(
            inputId = "condition",
            label = h3("Condition"),
            choices = c("High Sucrose" = "HS", "Low Sucrose" = "LS"),
            selected = "HS"
          ),
          
          radioButtons(
            inputId = "direction",
            label = h3("Direction"),
            choices = c("Positive", "Negative", "All"),
            selected = "Positive"
          ),
          
          numericInput(
            inputId = "stability",
            label = h3("Bootstrap Stability (%)"),
            value = 70,
            min = 0,
            max = 100
          ),
          
          actionButton("run", "Update")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Network Plot", plotOutput("networkPlot")),
            tabPanel("Edge Table", DTOutput("edgeTable")),
            tabPanel("Heatmap", plotOutput("heatmapPlot"))
          )
        )
      )
    )
  )
)

# ================================
# SERVER LOGIC
# ================================

server <- function(input, output, session) {
  
  # Load and filter edge data
  edge_data <- eventReactive(input$run, {
    
    if (input$condition == "HS") {
      df <- read.csv("data/edges_HS.csv")
    } else {
      df <- read.csv("data/edges_LS.csv")
    }
    
    # Apply stability threshold
    df <- df %>%
      filter(stability >= input$stability)
    
    # Filter by direction
    if (input$direction == "Positive") {
      df <- df %>% filter(sign == "Positive")
    } else if (input$direction == "Negative") {
      df <- df %>% filter(sign == "Negative")
    }
    
    df
  })
  
  # ================================
  # TABLE OUTPUT
  # ================================
  
  output$edgeTable <- renderDT({
    edge_data()
  })
  
  # ================================
  # NETWORK PLOT
  # ================================
  
  output$networkPlot <- renderPlot({
    
    df <- edge_data()
    
    if (nrow(df) == 0) return(NULL)
    
    g <- graph_from_data_frame(df, directed = FALSE)
    
    edge_colors <- ifelse(df$sign == "Positive", "orange", "blue")
    
    plot(
      g,
      edge.color = edge_colors,
      vertex.size = 5,
      vertex.label = NA,
      layout = layout_in_circle(g)
    )
  })
  
  # ================================
  # HEATMAP
  # ================================
  
  output$heatmapPlot <- renderPlot({
    
    if (input$condition == "HS") {
      hm <- read.csv("data/heatmap_HS.csv", row.names = 1)
    } else {
      hm <- read.csv("data/heatmap_LS.csv", row.names = 1)
    }
    
    hm_mat <- as.matrix(hm)
    
    image(
      hm_mat,
      col = colorRampPalette(c("white", "blue"))(50),
      main = "Connectivity Heatmap"
    )
  })
}

# ================================
# RUN APPLICATION
# ================================

shinyApp(ui = ui, server = server)