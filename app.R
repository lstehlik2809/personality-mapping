## 1. SETUP: LOAD LIBRARIES AND DEFINE UI
# --------------------------------------------------------------------------
# Load all required libraries
library(shiny)
library(plotly)
library(dplyr)
library(tidyr)

# Load pre-prepared data
final_data = readRDS("data/final_data.RDS")
percentile_data = readRDS("data/percentile_data.RDS")

# --- Define the UI for the Shiny App ---
ui <- fluidPage(
  titlePanel("Mapping Personality across 10,000 Big Five Profiles"),
  
  fluidRow(
    # The 3D scatterplot will take up the majority of the space
    column(8,
           h4("3D UMAP with HDBSCAN Clustering of Big Five Profiles"),
           p("Click on a point to see the average profile for its cluster (Cluster '0' represents points classified as noise)."),
           plotlyOutput("scatter3d_plot", height = "700px")
    ),
    # The line chart will appear on the side
    column(4,
           h4("Cluster Profile"),
           plotlyOutput("profile_line_chart", height = "500px")
    )
  )
)


## 2. SERVER LOGIC: DATA PROCESSING AND PLOTTING
# --------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- REACTIVE PLOTTING ---
  
  # Render the 3D plot
  output$scatter3d_plot <- renderPlotly({
    plot_ly(
      data = final_data, x = ~Dim1, y = ~Dim2, z = ~Dim3, color = ~cluster,
      type = "scatter3d", mode = "markers", marker = list(size = 4, opacity = 0.7),
      customdata = ~cluster, source = "scatter_source"
    ) 
  })
  
  selected_cluster <- reactiveVal()
  observeEvent(event_data("plotly_click", source = "scatter_source"), {
    selected_cluster(event_data("plotly_click", source = "scatter_source")$customdata)
  })
  
  # Render the cluster profile chart
  output$profile_line_chart <- renderPlotly({
    req(selected_cluster())
    
    # Calculate the size of the selected cluster 
    cluster_size <- sum(final_data$cluster == selected_cluster())
    
    individual_profiles <- percentile_data %>%
      filter(cluster == selected_cluster()) %>%
      pivot_longer(
        cols = all_of(names(final_data[,1:5])),
        names_to = "variable",
        values_to = "percentile_score"
      ) %>%
      mutate(variable = factor(variable, levels = rev(names(final_data[,1:5]))))
    
    average_profile <- individual_profiles %>%
      group_by(variable) %>%
      summarise(average_percentile = mean(percentile_score), .groups = 'drop')
    
    # Build the layered plot with swapped axes
    plot_ly() %>%
      # Add the individual profiles
      add_trace(
        data = individual_profiles, x = ~percentile_score, y = ~variable,
        group = ~id, type = 'scatter', mode = 'lines',
        line = list(color = 'grey', width = 0.5),
        opacity = 0.3, hoverinfo = 'none', showlegend = FALSE
      ) %>%
      # Add the average profile
      add_trace(
        data = average_profile, x = ~average_percentile, y = ~variable,
        type = 'scatter', mode = 'lines+markers',
        line = list(color = 'black', width = 4),
        marker = list(color = 'black', size = 6),
        name = "Cluster Average", showlegend = FALSE
      ) %>%
      # Add the final layout details for the new orientation
      layout(
        title = list(
          text = paste0("Cluster ", selected_cluster(), " (N=", cluster_size, ")"),
          font = list(size = 13),
          x = 0.65,
          xanchor = 'center'
        ),
        xaxis = list(
          title = list(
            text = "Percentile Score",
            font = list(size = 13)
          ), 
          range = c(0, 100)
        ),
        yaxis = list(title = ""),
        shapes = list(
          list(
            type = 'line', x0 = 50, x1 = 50, y0 = 0, y1 = 1,
            yref = 'paper',
            line = list(color = 'grey', dash = 'dash', width = 1.5)
          )
        )
      )
  })
}

## 3. CREATE & RUN THE SHINY APP
# --------------------------------------------------------------------------
shinyApp(ui, server)