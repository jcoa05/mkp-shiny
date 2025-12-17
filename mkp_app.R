# ============================================================================
# MoustiKAP-P: Behavioral Adaptation to Aedes Colonization
# Interactive Dashboard for Climate-Health Decision Support
# ============================================================================
# 
# This Shiny app visualizes:
# 1. Behavioral adaptation trajectories over time since Aedes colonization
# 2. Climate-behavior concordance analysis
# 3. Geographic patterns and intervention priorities
#
# Dependencies: shiny, bslib, leaflet, plotly, tidyverse, DT
# ============================================================================

# Load packages
library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(tidyverse)
library(DT)

# ----------------------------------------------------------------------------
# Load Pre-computed Data
# ----------------------------------------------------------------------------

# Load app data (created by R/00_prepare_app_data.R)
app_data <- readRDS("data/app_data.rds")

# Extract components for easier access
df_dept <- app_data$df_dept
df_region <- app_data$df_region
model_stats <- app_data$model_stats
model_predictions <- app_data$model_predictions
adaptation_metrics <- app_data$adaptation_metrics
summary_stats <- app_data$summary_stats

# ----------------------------------------------------------------------------
# UI Theme and Styling
# ----------------------------------------------------------------------------

# Custom theme using bslib
app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2c3e50",
  secondary = "#18bc9c",
  success = "#18bc9c",
  info = "#3498db",
  warning = "#f39c12",
  danger = "#e74c3c",
  base_font = font_google("Open Sans"),
  heading_font = font_google("Montserrat"),
  font_scale = 0.95
)

# Color palettes
colors_zone <- c(
  "South" = "#e74c3c",
  "Central" = "#f39c12", 
  "North" = "#3498db"
)

colors_concordance <- c(
  "Under-protected" = "#e74c3c",
  "Concordant" = "#2ecc71",
  "Over-protected" = "#3498db"
)

colors_priority <- c(
  "High priority" = "#c0392b",
  "Medium priority" = "#e74c3c",
  "On track" = "#f39c12",
  "Above average" = "#27ae60",
  "Exemplary" = "#16a085",
  "Not applicable" = "#95a5a6"
)

colors_models <- c(
  "linear" = "#3498db",
  "logarithmic" = "#e74c3c",
  "quadratic" = "#2ecc71",
  "sqrt" = "#9b59b6"
)

# ----------------------------------------------------------------------------
# UI Definition
# ----------------------------------------------------------------------------

ui <- page_navbar(
  title = tags$span(
    tags$img(src = "logo.png", height = "30px", style = "margin-right: 10px;",
             onerror = "this.style.display='none'"),
    "MoustiKAP-P: Behavioral Adaptation Dashboard"
  ),
  id = "main_navbar",
  theme = app_theme,
  fillable = TRUE,
  
  # Header with project info
  header = tags$head(
    tags$style(HTML("
      .value-box-title { font-size: 0.85rem !important; }
      .value-box-value { font-size: 1.8rem !important; }
      .leaflet-container { background: #f8f9fa; }
      .card-header { font-weight: 600; }
      .nav-link { font-weight: 500; }
      .plotly .modebar { top: 5px !important; }
      .info-text { color: #6c757d; font-size: 0.9rem; }
      .highlight-box { 
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 1rem;
        border-radius: 0.5rem;
        margin-bottom: 1rem;
      }
      .metric-card {
        text-align: center;
        padding: 1rem;
        border-radius: 0.5rem;
        background: #f8f9fa;
        margin-bottom: 0.5rem;
      }
      .metric-value { font-size: 2rem; font-weight: 700; color: #2c3e50; }
      .metric-label { font-size: 0.8rem; color: #6c757d; text-transform: uppercase; }
    "))
  ),
  
  # ==========================================================================
  # TAB 1: Overview
  # ==========================================================================
  nav_panel(
    title = "Overview",
    icon = icon("home"),
    
    layout_columns(
      col_widths = c(12),
      
      # Hero section
      card(
        class = "highlight-box",
        card_body(
          h3("Behavioral Adaptation to Aedes albopictus Colonization in France", 
             class = "mb-3"),
          p("This dashboard explores how protective behaviors against mosquito-borne 
            diseases evolve following the establishment of Aedes albopictus (Asian tiger 
            mosquito) in French departements. Understanding these adaptation trajectories 
            informs targeted public health communication strategies.",
            class = "mb-0")
        )
      )
    ),
    
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      
      value_box(
        title = "Total Respondents",
        value = format(summary_stats$n_total, big.mark = ","),
        showcase = icon("users"),
        theme = "primary"
      ),
      
      value_box(
        title = "Departements",
        value = summary_stats$n_dept,
        showcase = icon("map"),
        theme = "secondary"
      ),
      
      value_box(
        title = "Colonized",
        value = paste0(summary_stats$n_colonized_dept, " (", 
                       round(summary_stats$n_colonized_dept / summary_stats$n_dept * 100), "%)"),
        showcase = icon("bug"),
        theme = "warning"
      ),
      
      value_box(
        title = "Mean Protection Intensity",
        value = round(summary_stats$mean_protection_intensity, 2),
        showcase = icon("shield-halved"),
        theme = "success"
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        card_header("Key Research Questions"),
        card_body(
          tags$ul(
            tags$li(tags$strong("Adaptation trajectory:"), 
                    " How does protection behavior change with years of Aedes exposure?"),
            tags$li(tags$strong("Saturation effects:"), 
                    " Is there a plateau in behavioral adaptation?"),
            tags$li(tags$strong("Climate concordance:"), 
                    " Does protection behavior match local climate risk?"),
            tags$li(tags$strong("Intervention targeting:"), 
                    " Which departements require priority intervention?")
          )
        )
      ),
      
      card(
        card_header("Data Sources"),
        card_body(
          tags$ul(
            tags$li(tags$strong("KAP Survey:"), " MoustiKAP-P simulated data (N = 4,000)"),
            tags$li(tags$strong("Colonization:"), " Sante Publique France surveillance (2004-2024)"),
            tags$li(tags$strong("Climate:"), " Derived suitability index based on temperature and latitude"),
            tags$li(tags$strong("Geography:"), " 96 French departements across 13 regions")
          ),
          hr(),
          p(class = "info-text mb-0",
            icon("info-circle"), " This dashboard uses simulated data for demonstration. 
            The MoustiKAP-P survey will be deployed in 2025.")
        )
      )
    )
  ),
  
  # ==========================================================================
  # TAB 2: Adaptation Curve
  # ==========================================================================
  nav_panel(
    title = "Adaptation Curve",
    icon = icon("chart-line"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Analysis Options",
        width = 300,
        
        selectInput(
          "outcome_var",
          "Outcome Variable",
          choices = c(
            "Protection Intensity" = "mean_protection_intensity",
            "Protection Count" = "mean_protection_count",
            "Knowledge Score" = "mean_knowledge",
            "Risk Perception" = "mean_risk_perception",
            "Self-Efficacy" = "mean_efficacy"
          ),
          selected = "mean_protection_intensity"
        ),
        
        checkboxGroupInput(
          "models_show",
          "Models to Display",
          choices = c(
            "Linear" = "linear",
            "Logarithmic" = "logarithmic",
            "Quadratic" = "quadratic",
            "Square Root" = "sqrt"
          ),
          selected = c("linear", "logarithmic")
        ),
        
        checkboxInput(
          "show_ci",
          "Show 95% Confidence Intervals",
          value = TRUE
        ),
        
        checkboxInput(
          "show_points",
          "Show Departement Points",
          value = TRUE
        ),
        
        selectInput(
          "point_color",
          "Color Points By",
          choices = c(
            "Geographic Zone" = "geo_zone",
            "Climate Suitability" = "climate_suitability_cat",
            "Priority Level" = "priority_level"
          ),
          selected = "geo_zone"
        ),
        
        hr(),
        
        h6("About This Analysis"),
        p(class = "info-text",
          "The adaptation curve shows how protection behaviors relate to time since 
          Aedes colonization. Multiple functional forms are compared to identify the 
          best-fitting model.")
      ),
      
      layout_columns(
        col_widths = c(8, 4),
        row_heights = c("auto", "auto"),
        
        # Main plot
        card(
          full_screen = TRUE,
          card_header("Behavioral Adaptation Trajectory"),
          card_body(
            plotlyOutput("adaptation_curve_plot", height = "450px")
          )
        ),
        
        # Model comparison table
        card(
          card_header("Model Comparison"),
          card_body(
            tableOutput("model_comparison_table")
          )
        ),
        
        # Interpretation panel
        card(
          col_widths = 8,
          card_header("Interpretation"),
          card_body(
            uiOutput("adaptation_interpretation")
          )
        ),
        
        # Key metrics
        card(
          col_widths = 4,
          card_header("Adaptation Metrics"),
          card_body(
            uiOutput("adaptation_metrics_display")
          )
        )
      )
    )
  ),
  
  # ==========================================================================
  # TAB 3: Climate-Behavior Concordance
  # ==========================================================================
  nav_panel(
    title = "Concordance",
    icon = icon("balance-scale"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Concordance Options",
        width = 300,
        
        sliderInput(
          "concordance_threshold",
          "Discordance Threshold",
          min = 0.05,
          max = 0.30,
          value = 0.15,
          step = 0.05
        ),
        
        selectInput(
          "concordance_view",
          "Visualization Type",
          choices = c(
            "Scatter Plot" = "scatter",
            "Quadrant Analysis" = "quadrant"
          ),
          selected = "quadrant"
        ),
        
        hr(),
        
        h6("Understanding Concordance"),
        p(class = "info-text",
          tags$strong("Concordant:"), " Protection level matches climate risk"),
        p(class = "info-text",
          tags$strong("Under-protected:"), " High climate risk but low protection"),
        p(class = "info-text",
          tags$strong("Over-protected:"), " Low climate risk but high protection")
      ),
      
      layout_columns(
        col_widths = c(7, 5),
        row_heights = c("auto", "auto"),
        
        # Concordance plot
        card(
          full_screen = TRUE,
          card_header("Climate Risk vs. Protection Behavior"),
          card_body(
            plotlyOutput("concordance_plot", height = "400px")
          )
        ),
        
        # Distribution
        card(
          card_header("Concordance Distribution"),
          card_body(
            plotlyOutput("concordance_pie", height = "300px")
          )
        ),
        
        # Priority table
        card(
          col_widths = 12,
          card_header("Under-Protected Departements (Intervention Priority)"),
          card_body(
            DTOutput("priority_table")
          )
        )
      )
    )
  ),
  
  # ==========================================================================
  # TAB 4: Geographic Map
  # ==========================================================================
  nav_panel(
    title = "Geographic View",
    icon = icon("map-location-dot"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Map Options",
        width = 280,
        
        selectInput(
          "map_variable",
          "Display Variable",
          choices = c(
            "Protection Intensity" = "mean_protection_intensity",
            "Exposure Years" = "exposure_years",
            "Climate Suitability" = "climate_suitability_index",
            "Concordance Gap" = "dept_concordance_gap",
            "Knowledge" = "mean_knowledge",
            "Risk Perception" = "mean_risk_perception"
          ),
          selected = "mean_protection_intensity"
        ),
        
        selectInput(
          "map_color_palette",
          "Color Palette",
          choices = c(
            "Viridis" = "viridis",
            "Red-Yellow-Green" = "RdYlGn",
            "Blue-Red" = "RdBu",
            "Spectral" = "Spectral"
          ),
          selected = "viridis"
        ),
        
        checkboxInput(
          "map_show_labels",
          "Show Departement Labels",
          value = FALSE
        ),
        
        hr(),
        
        h6("Map Legend"),
        uiOutput("map_legend_text")
      ),
      
      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Geographic Distribution",
          downloadButton("download_map_data", "Download Data", class = "btn-sm")
        ),
        card_body(
          leafletOutput("france_map", height = "600px")
        )
      )
    )
  ),
  
  # ==========================================================================
  # TAB 5: Policy Insights
  # ==========================================================================
  nav_panel(
    title = "Policy Insights",
    icon = icon("lightbulb"),
    
    layout_columns(
      col_widths = c(12),
      
      card(
        class = "highlight-box",
        card_body(
          h4("Evidence-Based Recommendations for MoustiKAP-P Communication Strategy"),
          p("These insights are derived from the behavioral adaptation analysis and 
            climate-behavior concordance assessment.", class = "mb-0")
        )
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        card_header(icon("clock"), " Adaptation Timeline"),
        card_body(
          uiOutput("policy_timeline")
        )
      ),
      
      card(
        card_header(icon("bullseye"), " Targeting Recommendations"),
        card_body(
          uiOutput("policy_targeting")
        )
      ),
      
      card(
        card_header(icon("triangle-exclamation"), " Key Gaps Identified"),
        card_body(
          uiOutput("policy_gaps")
        )
      ),
      
      card(
        card_header(icon("chart-bar"), " Summary Statistics"),
        card_body(
          plotlyOutput("policy_summary_chart", height = "300px")
        )
      )
    )
  ),
  
  # ==========================================================================
  # Footer
  # ==========================================================================
  nav_spacer(),
  
  nav_item(
    tags$a(
      href = "https://github.com/your-repo/mkp-shiny",
      target = "_blank",
      icon("github"),
      " Source"
    )
  )
)

# ----------------------------------------------------------------------------
# Server Logic
# ----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # ==========================================================================
  # Reactive Data
  # ==========================================================================
  
  # Filter departement data for colonized only (for adaptation curve)
  df_colonized <- reactive({
    df_dept %>%
      filter(aedes_present == 1, exposure_years > 0)
  })
  
  # Concordance classification based on threshold
  df_concordance <- reactive({
    threshold <- input$concordance_threshold
    df_dept %>%
      mutate(
        concordance_cat_dynamic = case_when(
          dept_concordance_gap > threshold ~ "Under-protected",
          dept_concordance_gap < -threshold ~ "Over-protected",
          TRUE ~ "Concordant"
        )
      )
  })
  
  # ==========================================================================
  # TAB 2: Adaptation Curve Outputs
  # ==========================================================================
  
  output$adaptation_curve_plot <- renderPlotly({
    req(input$outcome_var, input$models_show)
    
    df <- df_colonized()
    y_var <- input$outcome_var
    y_label <- switch(y_var,
      "mean_protection_intensity" = "Protection Intensity Index",
      "mean_protection_count" = "Number of Protection Measures",
      "mean_knowledge" = "Knowledge Score (0-10)",
      "mean_risk_perception" = "Risk Perception (1-7)",
      "mean_efficacy" = "Self-Efficacy (1-7)"
    )
    
    # Refit models if outcome variable changed
    if (y_var != "mean_protection_intensity") {
      # Fit models for selected outcome
      models_refit <- list()
      models_refit$linear <- lm(as.formula(paste(y_var, "~ exposure_years")), data = df)
      models_refit$logarithmic <- lm(as.formula(paste(y_var, "~ log(exposure_years + 1)")), data = df)
      models_refit$quadratic <- lm(as.formula(paste(y_var, "~ exposure_years + I(exposure_years^2)")), data = df)
      models_refit$sqrt <- lm(as.formula(paste(y_var, "~ sqrt(exposure_years)")), data = df)
      
      # Generate predictions
      pred_grid <- tibble(exposure_years = seq(0.5, max(df$exposure_years) + 1, by = 0.5))
      
      predictions <- map_df(input$models_show, function(m) {
        if (m %in% names(models_refit)) {
          pred <- predict(models_refit[[m]], newdata = pred_grid, se.fit = TRUE)
          tibble(
            model_name = m,
            exposure_years = pred_grid$exposure_years,
            fitted = pred$fit,
            se = pred$se.fit,
            lower = fitted - 1.96 * se,
            upper = fitted + 1.96 * se
          )
        }
      })
    } else {
      # Use pre-computed predictions for protection intensity
      predictions <- model_predictions %>%
        filter(model_name %in% input$models_show)
    }
    
    # Build plot
    p <- plot_ly()
    
    # Add points if requested
    if (input$show_points) {
      color_var <- input$point_color
      
      p <- p %>%
        add_trace(
          data = df,
          x = ~exposure_years,
          y = as.formula(paste0("~", y_var)),
          type = "scatter",
          mode = "markers",
          color = as.formula(paste0("~", color_var)),
          colors = if(color_var == "geo_zone") colors_zone 
                   else if(color_var == "priority_level") colors_priority
                   else "Set2",
          marker = list(size = 10, opacity = 0.7),
          text = ~paste0(
            "<b>", dept_name, "</b> (", departement, ")<br>",
            "Exposure: ", exposure_years, " years<br>",
            y_label, ": ", round(get(y_var), 2), "<br>",
            "Region: ", region
          ),
          hoverinfo = "text",
          showlegend = TRUE
        )
    }
    
    # Add model lines
    model_labels <- c(
      "linear" = "Linear",
      "logarithmic" = "Logarithmic",
      "quadratic" = "Quadratic",
      "sqrt" = "Square Root"
    )
    
    for (m in input$models_show) {
      pred_m <- predictions %>% filter(model_name == m)
      
      # Add confidence interval if requested
      if (input$show_ci && nrow(pred_m) > 0) {
        p <- p %>%
          add_trace(
            data = pred_m,
            x = ~exposure_years,
            y = ~upper,
            type = "scatter",
            mode = "lines",
            line = list(width = 0),
            showlegend = FALSE,
            hoverinfo = "skip"
          ) %>%
          add_trace(
            data = pred_m,
            x = ~exposure_years,
            y = ~lower,
            type = "scatter",
            mode = "lines",
            line = list(width = 0),
            fill = "tonexty",
            fillcolor = paste0(colors_models[m], "33"),
            showlegend = FALSE,
            hoverinfo = "skip"
          )
      }
      
      # Add fitted line
      if (nrow(pred_m) > 0) {
        p <- p %>%
          add_trace(
            data = pred_m,
            x = ~exposure_years,
            y = ~fitted,
            type = "scatter",
            mode = "lines",
            name = model_labels[m],
            line = list(color = colors_models[m], width = 3),
            hoverinfo = "text",
            text = ~paste0(model_labels[m], "<br>Years: ", exposure_years, 
                          "<br>Predicted: ", round(fitted, 2))
          )
      }
    }
    
    p %>%
      layout(
        xaxis = list(
          title = "Years Since Aedes Colonization",
          zeroline = FALSE
        ),
        yaxis = list(
          title = y_label,
          zeroline = FALSE
        ),
        legend = list(
          orientation = "h",
          y = -0.15,
          x = 0.5,
          xanchor = "center"
        ),
        hovermode = "closest"
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  output$model_comparison_table <- renderTable({
    model_stats %>%
      select(model_label, r_squared, aic, bic) %>%
      mutate(
        r_squared = round(r_squared, 3),
        aic = round(aic, 1),
        bic = round(bic, 1)
      ) %>%
      rename(
        "Model" = model_label,
        "R2" = r_squared,
        "AIC" = aic,
        "BIC" = bic
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$adaptation_interpretation <- renderUI({
    best <- model_stats %>% filter(is_best_aic) %>% pull(model_label)
    r2 <- model_stats %>% filter(is_best_aic) %>% pull(r_squared)
    
    tagList(
      h5(paste("Best-Fitting Model:", best)),
      p(paste0("The ", tolower(best), " model provides the best fit (R2 = ", 
               round(r2, 3), "), explaining ", round(r2 * 100, 1), 
               "% of variance in protection behavior across departements.")),
      
      if (best == "Logarithmic") {
        tagList(
          p(tags$strong("Interpretation:"), " Protection behavior increases rapidly 
            in the first years after colonization, then levels off. This suggests:"),
          tags$ul(
            tags$li("Initial awareness campaigns are effective"),
            tags$li("Behavioral adaptation plateaus over time"),
            tags$li("Additional interventions may be needed to exceed the natural plateau")
          )
        )
      } else if (best == "Linear") {
        p("Protection behavior increases steadily with exposure time, suggesting 
          continuous learning and adaptation.")
      } else if (best == "Quadratic") {
        p("The relationship shows curvature, potentially indicating initial rapid 
          adaptation followed by saturation or even slight decline.")
      }
    )
  })
  
  output$adaptation_metrics_display <- renderUI({
    metrics <- adaptation_metrics
    
    tagList(
      div(class = "metric-card",
        div(class = "metric-value", round(metrics$protection_at_5, 2)),
        div(class = "metric-label", "At 5 Years")
      ),
      div(class = "metric-card",
        div(class = "metric-value", round(metrics$protection_at_10, 2)),
        div(class = "metric-label", "At 10 Years")
      ),
      div(class = "metric-card",
        div(class = "metric-value", round(metrics$protection_at_20, 2)),
        div(class = "metric-label", "At 20 Years")
      ),
      div(class = "metric-card",
        div(class = "metric-value", paste0(round(metrics$r_squared * 100, 0), "%")),
        div(class = "metric-label", "Variance Explained")
      )
    )
  })
  
  # ==========================================================================
  # TAB 3: Concordance Outputs
  # ==========================================================================
  
  output$concordance_plot <- renderPlotly({
    df <- df_concordance()
    
    if (input$concordance_view == "quadrant") {
      p <- plot_ly(df, 
                   x = ~climate_risk_rank, 
                   y = ~protection_rank,
                   type = "scatter",
                   mode = "markers",
                   color = ~concordance_cat_dynamic,
                   colors = colors_concordance,
                   marker = list(size = 12, opacity = 0.7),
                   text = ~paste0(
                     "<b>", dept_name, "</b> (", departement, ")<br>",
                     "Climate Risk Rank: ", round(climate_risk_rank * 100, 0), "%<br>",
                     "Protection Rank: ", round(protection_rank * 100, 0), "%<br>",
                     "Gap: ", round(dept_concordance_gap, 2), "<br>",
                     "Status: ", concordance_cat_dynamic
                   ),
                   hoverinfo = "text") %>%
        # Add diagonal line (perfect concordance)
        add_trace(
          x = c(0, 1),
          y = c(0, 1),
          type = "scatter",
          mode = "lines",
          line = list(color = "gray", dash = "dash", width = 2),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) %>%
        # Add threshold bands
        add_trace(
          x = c(0, 1),
          y = c(-input$concordance_threshold, 1 - input$concordance_threshold),
          type = "scatter",
          mode = "lines",
          line = list(color = "gray", dash = "dot", width = 1),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) %>%
        add_trace(
          x = c(0, 1),
          y = c(input$concordance_threshold, 1 + input$concordance_threshold),
          type = "scatter",
          mode = "lines",
          line = list(color = "gray", dash = "dot", width = 1),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) %>%
        layout(
          xaxis = list(title = "Climate Risk Rank (0 = Low, 1 = High)", range = c(-0.05, 1.05)),
          yaxis = list(title = "Protection Rank (0 = Low, 1 = High)", range = c(-0.05, 1.05)),
          legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center"),
          annotations = list(
            list(x = 0.9, y = 0.1, text = "Under-protected", showarrow = FALSE,
                 font = list(color = colors_concordance["Under-protected"], size = 11)),
            list(x = 0.1, y = 0.9, text = "Over-protected", showarrow = FALSE,
                 font = list(color = colors_concordance["Over-protected"], size = 11))
          )
        )
    } else {
      # Simple scatter without quadrant annotations
      p <- plot_ly(df,
                   x = ~climate_suitability_index,
                   y = ~mean_protection_intensity,
                   type = "scatter",
                   mode = "markers",
                   color = ~geo_zone,
                   colors = colors_zone,
                   marker = list(size = 10, opacity = 0.7),
                   text = ~paste0("<b>", dept_name, "</b><br>",
                                  "Climate Index: ", round(climate_suitability_index, 1), "<br>",
                                  "Protection: ", round(mean_protection_intensity, 2)),
                   hoverinfo = "text") %>%
        layout(
          xaxis = list(title = "Climate Suitability Index (0-100)"),
          yaxis = list(title = "Mean Protection Intensity"),
          legend = list(orientation = "h", y = -0.15)
        )
    }
    
    p %>% config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  output$concordance_pie <- renderPlotly({
    df <- df_concordance()
    
    counts <- df %>%
      count(concordance_cat_dynamic) %>%
      mutate(pct = n / sum(n) * 100)
    
    plot_ly(counts,
            labels = ~concordance_cat_dynamic,
            values = ~n,
            type = "pie",
            marker = list(colors = colors_concordance[counts$concordance_cat_dynamic]),
            textinfo = "label+percent",
            hoverinfo = "text",
            text = ~paste0(concordance_cat_dynamic, "<br>n = ", n, " (", round(pct, 1), "%)")) %>%
      layout(showlegend = FALSE)
  })
  
  output$priority_table <- renderDT({
    df <- df_concordance() %>%
      filter(concordance_cat_dynamic == "Under-protected") %>%
      arrange(desc(dept_concordance_gap)) %>%
      select(
        departement, dept_name, region, geo_zone,
        exposure_years, climate_suitability_index, mean_protection_intensity,
        dept_concordance_gap
      ) %>%
      mutate(
        climate_suitability_index = round(climate_suitability_index, 1),
        mean_protection_intensity = round(mean_protection_intensity, 2),
        dept_concordance_gap = round(dept_concordance_gap, 2)
      ) %>%
      rename(
        "Code" = departement,
        "Departement" = dept_name,
        "Region" = region,
        "Zone" = geo_zone,
        "Exposure (yrs)" = exposure_years,
        "Climate Risk" = climate_suitability_index,
        "Protection" = mean_protection_intensity,
        "Gap" = dept_concordance_gap
      )
    
    datatable(df, 
              options = list(pageLength = 10, dom = 'ftp'),
              rownames = FALSE) %>%
      formatStyle("Gap", 
                  background = styleColorBar(c(0, max(df$Gap, 0.5)), "#e74c3c"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
  
  # ==========================================================================
  # TAB 4: Map Outputs
  # ==========================================================================
  
  output$france_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 2.5, lat = 46.5, zoom = 6)
  })
  
  observe({
    df <- df_dept
    var <- input$map_variable
    
    # Get variable values and create color palette
    values <- df[[var]]
    
    pal <- colorNumeric(
      palette = input$map_color_palette,
      domain = values,
      na.color = "#cccccc"
    )
    
    # Create popup content
    popup_content <- paste0(
      "<strong>", df$dept_name, "</strong> (", df$departement, ")<br>",
      "<b>Region:</b> ", df$region, "<br>",
      "<b>", gsub("_", " ", var), ":</b> ", round(values, 2), "<br>",
      "<b>Exposure:</b> ", df$exposure_years, " years<br>",
      "<b>Colonized:</b> ", ifelse(df$aedes_present == 1, "Yes", "No")
    )
    
    leafletProxy("france_map", data = df) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~sqrt(n_respondents) * 2 + 5,
        color = ~pal(get(var)),
        fillColor = ~pal(get(var)),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1,
        popup = popup_content,
        label = if (input$map_show_labels) ~dept_name else NULL
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = values,
        title = gsub("_", " ", var),
        opacity = 0.8
      )
  })
  
  output$map_legend_text <- renderUI({
    var <- input$map_variable
    
    desc <- switch(var,
      "mean_protection_intensity" = "Composite index (0-10) combining protection measure usage and frequency",
      "exposure_years" = "Years since first Aedes albopictus detection (0 = not colonized)",
      "climate_suitability_index" = "Climate suitability for Aedes (0-100, higher = more suitable)",
      "dept_concordance_gap" = "Gap between climate risk and protection (+ = under-protected)",
      "mean_knowledge" = "Average knowledge score (0-10)",
      "mean_risk_perception" = "Average risk perception (1-7 Likert scale)"
    )
    
    p(class = "info-text", desc)
  })
  
  output$download_map_data <- downloadHandler(
    filename = function() {
      paste0("mkp_departement_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(df_dept, file)
    }
  )
  
  # ==========================================================================
  # TAB 5: Policy Outputs
  # ==========================================================================
  
  output$policy_timeline <- renderUI({
    tagList(
      h5("Adaptation Phases"),
      tags$ol(
        tags$li(
          tags$strong("Years 1-3 (Early Adaptation):"), 
          " Rapid behavioral change. Intensive communication most effective."
        ),
        tags$li(
          tags$strong("Years 4-7 (Consolidation):"), 
          " Behavior stabilizes. Reinforcement messaging needed."
        ),
        tags$li(
          tags$strong("Years 8+ (Plateau):"), 
          " Natural adaptation ceiling reached. Novel interventions required 
          to exceed plateau (~", round(adaptation_metrics$protection_at_10, 1), " measures)."
        )
      ),
      hr(),
      p(icon("lightbulb"), tags$strong(" Key Insight:"), 
        " Focus intensive campaigns on departements colonized within the past 5 years.")
    )
  })
  
  output$policy_targeting <- renderUI({
    df <- df_concordance()
    n_under <- sum(df$concordance_cat_dynamic == "Under-protected")
    
    tagList(
      h5("Priority Departements"),
      p(paste0(n_under, " departements (", round(n_under / nrow(df) * 100, 1), 
               "%) classified as under-protected relative to climate risk.")),
      
      h6("Targeting Strategy:"),
      tags$ul(
        tags$li(tags$strong("High Priority:"), " Under-protected with high climate risk"),
        tags$li(tags$strong("Medium Priority:"), " Under-protected with moderate climate risk"),
        tags$li(tags$strong("Maintenance:"), " Concordant departements (reinforce behaviors)")
      ),
      
      hr(),
      
      h6("Recommended Actions:"),
      tags$ul(
        tags$li("Deploy multi-channel campaigns in high-priority areas"),
        tags$li("Partner with local health authorities (ARS)"),
        tags$li("Tailor messaging to local colonization history")
      )
    )
  })
  
  output$policy_gaps <- renderUI({
    tagList(
      h5("Identified Behavioral Gaps"),
      tags$ul(
        tags$li(
          tags$strong("Knowledge-Behavior Gap:"), 
          " Some departements show high knowledge but low protection adoption"
        ),
        tags$li(
          tags$strong("Northern Exposure:"), 
          " Recently colonized northern departements lag in adaptation"
        ),
        tags$li(
          tags$strong("Plateau Effect:"), 
          " Long-established areas show no further improvement without intervention"
        )
      ),
      hr(),
      p(icon("info-circle"), 
        " These gaps represent opportunities for targeted intervention through 
        MoustiKAP-P communication materials.")
    )
  })
  
  output$policy_summary_chart <- renderPlotly({
    df <- df_concordance()
    
    summary <- df %>%
      group_by(geo_zone) %>%
      summarise(
        mean_protection = mean(mean_protection_intensity, na.rm = TRUE),
        mean_exposure = mean(exposure_years, na.rm = TRUE),
        pct_under = mean(concordance_cat_dynamic == "Under-protected") * 100,
        .groups = "drop"
      ) %>%
      mutate(geo_zone = factor(geo_zone, levels = c("South", "Central", "North")))
    
    plot_ly(summary, x = ~geo_zone, y = ~mean_protection, type = "bar",
            marker = list(color = colors_zone[summary$geo_zone]),
            text = ~round(mean_protection, 2),
            textposition = "outside",
            hoverinfo = "text",
            hovertext = ~paste0(
              geo_zone, "<br>",
              "Protection: ", round(mean_protection, 2), "<br>",
              "Avg Exposure: ", round(mean_exposure, 1), " years<br>",
              "Under-protected: ", round(pct_under, 1), "%"
            )) %>%
      layout(
        xaxis = list(title = "Geographic Zone"),
        yaxis = list(title = "Mean Protection Intensity"),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
}

# ----------------------------------------------------------------------------
# Run App
# ----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
