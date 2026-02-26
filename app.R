library(shiny)
library(ggplot2)
library(zoo)
library(mgcv)
library(dplyr)
library(plotly)
library(ggridges)

df <- read.csv('./Database.csv')

df <- df %>%
  mutate(
    Time = as.POSIXct(df$Time, format = "%Y-%m-%d-T%H:%M"),
    Production_gap = Electric_demand - (PV_production + Wind_production),
    Year = as.integer(format(Time, '%Y')),
    Month = as.integer(format(Time, "%m")),
    Hour = as.integer(format(Time, "%H")),
    Ramp_rate = abs(Electric_demand - lag(Electric_demand)),
    Day_or_night = ifelse(Hour >= 6 & Hour < 20, "Day", "Night"),
    Day_of_the_week = factor(Day_of_the_week,
                             levels = c(0, 1, 2, 3, 4, 5, 6),
                             labels = c("Mon", "Tue", "Wed",
                                        "Thu", "Fri", "Sat", "Sun")),
    Month = factor(Month,
                   levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
    Season = factor(Season,
                    levels = c(1, 2, 3, 4),
                    labels = c("Winter", "Spring", "Summer", "Fall"))
  )

# Remove rows with NA Time
df <- df[!is.na(df$Time), ]

# Take every 10th row to reduce render time
df_plot <- df[seq(1, nrow(df), by = 10), ]

model <- lm(Electric_demand ~ Temperature * Season + Hour + Day_of_the_week + Humidity, data = df)

ui <- fluidPage(
  mainPanel(
    tabsetPanel(
      tabPanel("Introduction",
        h1("Renewable Energy in California (2019-2021)"),
        p("California leads the US in renewable energy production. But despite abundant solar and wind resources,
          renewable energy cover less than 25% of total electricity demand. Using 5-minute interval data from the California
          Independent System Operator (CAISO) and the National Renewable Energy Laboratory (NREL), this dashboard explores
          the relationships between renewable generation, electricity demand, and climate variables that drive both.")
      ),
      tabPanel("Supply and Demand",
               h3("Supply and Demand"),
               sidebarLayout(
                 sidebarPanel(
                   width = 4,
                   
                   checkboxGroupInput(
                     "show_energy",
                     "Energy:",
                     choices = c("Demand", "Gap", "Solar", "Wind"),
                     selected = c("Demand", "Gap", "Solar", "Wind")
                   ),
                   checkboxGroupInput(
                     "show_irradiance",
                     "Irradiance:",
                     choices = c("GHI", "DNI", "DHI"),
                     selected = c("GHI", "DNI", "DHI")
                   ),
                   sliderInput(
                     "date_range",
                     "Date Range:",
                     min = as.Date("2019-01-01"),
                     max = as.Date("2022-01-01"),
                     value = c(as.Date("2019-01-01"), as.Date("2022-01-01")),
                     timeFormat = "%Y-%m"
                   )
                 ),
                 
                 mainPanel(
                   width = 8,
                   plotOutput("total_plot", height = "400px", width = "700px"),
                   plotOutput("breakdown_plot", height = "400px", width = "700px"),
                   hr(),
                   fluidRow(
                     column(4, offset = 2,
                            selectInput("pie_season",
                                        "Season:",
                                        choices = c("All", "Winter", "Spring", "Summer", "Fall"),
                                        selected = "All")
                            ),
                     column(4,
                            selectInput("pie_year",
                                        "Year:",
                                        choices = c("All", "2019", "2020", "2021"),
                                        selected = "All")
                            )
                   ),
                   plotlyOutput("pie_chart", height = "400px", width = "100%")
                 )
               )
      ),
      
      tabPanel("Energy Patterns",
               h3("Energy Patterns"),
               
               # First heat map section
               fluidRow(
                 column(6,
                        selectInput("y_var_1",
                                    "Time Component:",
                                    choices = c("Day of the week", "Month"),
                                    selected = "Day of the week")
                 ),
                 column(6,
                        selectInput("fill_1",
                                    "Observed metric:",
                                    choices = c("Electric Demand", "Production Gap"),
                                    selected = "Electric Demand")
                 )
               ),
               fluidRow(
                 column(12, align = "center",
                        plotOutput("heatmap_1", height = "500px", width = "800px")
                 )
               ),
               hr(),
               # Second heat map section
               fluidRow(
                 column(6,
                        selectInput(
                          "y_var_2",
                          "Time Component:",
                          choices = c("Month", "Season"),
                          selected = "Month")
                 ),
                 column(6,
                        selectInput("fill_2",
                                    "Observed metric:",
                                    choices = c("Humidity",
                                                "Temperature",
                                                "Solar production",
                                                "DNI", "DHI", "GHI"),
                                    selected = "Solar production")
                 )
               ),
               fluidRow(
                 column(12, align = "center",
                        plotOutput("heatmap_2", height = "500px", width = "800px")
                 )
               ),
               hr(),
               # Third heat map section
               fluidRow(
                 column(6,
                        selectInput("y_var_3",
                                    "Time Component:",
                                    choices = c("Month", "Season"),
                                    selected = "Month")
                 ),
                 column(6,
                        selectInput("fill_3",
                                    "Observed metric:",
                                    choices = c("Wind production", "Wind speed"),
                                    selected = "Wind production")
                 )
               ),
               fluidRow(
                 column(12, align = "center",
                        plotOutput("heatmap_3", height = "500px", width = "800px")
                 )
               )
      ),
      tabPanel("Temperature Impact",
               h3("Temperature Impact"),
               fluidRow(
                 column(12,
                        checkboxGroupInput(
                          "gap_seasons",
                          "Season:",
                          choices = c("Winter", "Spring", "Summer", "Fall"),
                          selected = c("Winter", "Spring", "Summer", "Fall"),
                          inline = TRUE
                        )
                 )
               ),
               fluidRow(
                 column(12,
                        plotOutput("gap_under_stress", height = "400px", width = "800px")
                        )
               ),
               fluidRow(
                 column(12,
                        plotOutput("ramp_rate_vs_temp", height = "400px", width = "800px")
                        )
               )
            ),
      tabPanel("Forcasting",
         sidebarLayout(
           sidebarPanel(
             width = 4,
             sliderInput("forecast_temp", "Temperature (ºC):", min = 0, max = 40, value = 20),
             selectInput("forecast_season", "Season:", choices = c("Winter", "Spring", "Summer", "Fall"), selected = "Winter"),
             sliderInput("forecast_hour", "Hour of day:", min = 0, max = 23, value = 12),
             selectInput("forecast_day", "Day of the week:", choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), selected = "Monday"),
             sliderInput("forecast_humidity", "Humidity (%):", min = 0, max = 100, value = 50)
           ),
           mainPanel(
             width = 8,
             plotOutput("forecast_prediction", height = "400px")
           )
         )
       )
    )
  )
)

server <- function(input, output) {
  
  # -------------------- TAB ONE ---------------------
  # -------------------- TAB ONE ---------------------
  # -------------------- TAB ONE ---------------------
  
  df_filtered <- reactive({
    df_plot %>%
      filter(Time >= as.POSIXct(input$date_range[1]) & 
               Time <= as.POSIXct(input$date_range[2]))
  })
  
  output$total_plot <- renderPlot({
    
    p <- ggplot(df_filtered(), aes(x = Time))
    
    if ("Demand" %in% input$show_energy) {
      p <- p + geom_line(aes(y = Electric_demand, color = "Demand"), alpha = 0.06, size = 0.2) +
        geom_smooth(aes(y = Electric_demand, color = "Demand"), se = FALSE, size = 1.3)
    }
    
    if ("Gap" %in% input$show_energy) {
      p <- p + geom_line(aes(y = Production_gap, color = "Gap"), alpha = 0.06, size = 0.2) +
        geom_smooth(aes(y = Production_gap, color = "Gap"), se = FALSE, size = 1.3)
    }
    
    if ("Solar" %in% input$show_energy) {
      p <- p + geom_line(aes(y = PV_production, color = "Solar"), alpha = 0.06, size = 0.2) +
        geom_smooth(aes(y = PV_production, color = "Solar"), se = FALSE, size = 1.3)
    }
    
    if ("Wind" %in% input$show_energy) {
      p <- p + geom_line(aes(y = Wind_production, color = "Wind"), alpha = 0.06, size = 0.2) +
        geom_smooth(aes(y = Wind_production, color = "Wind"), se = FALSE, size = 1.3)
    }
    
    p + scale_color_manual(values = c(
      "Demand" = "red",
      "Solar" = "gold",
      "Wind" = "skyblue",
      "Gap" = "darkgray"
    )) + 
      labs(title = "Renewable Generation and Electricity Demand", y = "Energy (MW)", x = NULL, color = "") +
      theme_minimal() +
      theme(
        legend.position = c(0.88, 0.85),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key.size = unit(0.5, "cm"),
        plot.title = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()
      )
  })
  
  output$breakdown_plot <- renderPlot({
    
    p <- ggplot(df_filtered(), aes(x = Time))
    
    if ("DNI" %in% input$show_irradiance) {
      p <- p + geom_line(aes(y = DNI, color = "DNI"), alpha = 0.06, size = 0.2) +
        geom_smooth(aes(y = DNI, color = "DNI"), se = FALSE, size = 1.3)
    }
    
    if ("GHI" %in% input$show_irradiance) {
      p <- p + geom_line(aes(y = GHI, color = "GHI"), alpha = 0.06, size = 0.2) +
        geom_smooth(aes(y = GHI, color = "GHI"), se = FALSE, size = 1.3)
    }
    
    if ("DHI" %in% input$show_irradiance) {
      p <- p + geom_line(aes(y = DHI, color = "DHI"), alpha = 0.06, size = 0.2) +
        geom_smooth(aes(y = DHI, color = "DHI"), se = FALSE, size = 1.3)
    }
    
    p + scale_color_manual(values = c(
      "GHI" = "darkgoldenrod3",
      "DNI" = "darkorange2",
      "DHI" = "brown4"
    )) +
      labs(title = "Solar Radiation Breakdown", y = "Irradiance (W/m²)", x = NULL, color = "") +
      theme_minimal() +
      theme(
        legend.position = c(0.88, 0.85),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key.size = unit(0.5, "cm"),
        plot.title = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()
      )
  })
  
  output$pie_chart <- renderPlotly({
    df <- df_plot
    
    color_map <- c(
      "Low (0-25%)" = "#d73027",
      "Medium (25-50%)" = "#fc8d59", 
      "Good (50-75%)" = "#fee090",
      "High (75-100%)" = "#4dac26"
    )
    
    if (input$pie_season != "All") {
      df <- df %>% filter(Season == input$pie_season)
    }
    
    if (input$pie_year != "All") {
      df <- df %>% filter(Year == input$pie_year)
    }
    
    df_pie <- df %>%
      mutate(pct = ((PV_production + Wind_production) / Electric_demand) * 100) %>%
      mutate(category = case_when(
        pct >= 75 ~ "High (75-100%)",
        pct >= 50 ~ "Good (50-75%)",
        pct >= 25 ~ "Medium (25-50%)",
        TRUE ~ "Low (0-25%)"
      )) %>%
      count(category) %>%
      mutate(share = n / sum(n) * 100)
    
    plot_ly(
      df_pie,
      labels = ~category,
      values = ~share,
      type = "pie",
      textinfo = "percent",
      hoverinfo = "label+percent",
      marker = list(colors = color_map[df_pie$category])
    ) %>%
      layout(
        title = "How Often Do Renewables Meet Demand?",
        showLegend = TRUE,
        legend = list(orientation = "h")
      )
  })
  
  # -------------------- TAB TWO ---------------------
  # -------------------- TAB TWO ---------------------
  # -------------------- TAB TWO ---------------------
  
  output$heatmap_1 <- renderPlot({
    df_heat <- data.frame(
      Hour = df$Hour,
      Y = switch(input$y_var_1,
                 "Day of the week" = df$Day_of_the_week,
                 "Month" = df$Month),
      fill_var = switch(input$fill_1,
                        "Production Gap" = df$Production_gap,
                        "Electric Demand" = df$Electric_demand)
    )
    
    df_heat <- df_heat %>%
      group_by(Hour, Y) %>%
      summarise(value = mean(fill_var, na.rm = TRUE), .groups = "drop")
    
    ggplot(df_heat, aes(x = Hour, y = Y, fill = value)) +
      geom_tile() +
      scale_fill_viridis_c(option = "plasma") +
      labs(x = "Hour of Day", y = input$y_var_1, fill = input$fill_1) +
      theme_minimal() +
      theme(
        legend.background = element_blank(),
        plot.title = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()
      )
  })
  
  output$heatmap_2 <- renderPlot({
    df_heat <- data.frame(
      Hour = df$Hour,
      Y = switch(input$y_var_2,
                 "Month" = df$Month,
                 "Season" = df$Season),
      fill_var = switch(input$fill_2,
                        "Solar production" = df$PV_production,
                        "GHI" = df$GHI,
                        "DHI" = df$DHI,
                        "DNI" = df$DNI,
                        "Humidity" = df$Humidity,
                        "Temperature" = df$Temperature)
    )
    
    df_heat <- df_heat %>%
      group_by(Hour, Y) %>%
      summarise(value = mean(fill_var, na.rm = TRUE), .groups = "drop")
    
    ggplot(df_heat, aes(x = Hour, y = Y, fill = value)) +
      geom_tile() +
      scale_fill_viridis_c(option = "plasma") +
      labs(x = "Hour of Day", y = input$y_var_2, fill = input$fill_2) +
      theme_minimal() +
      theme(
        legend.background = element_blank(),
        plot.title = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()
      )
  })
  
  output$heatmap_3 <- renderPlot({
    df_heat <- data.frame(
      Hour = df$Hour,
      Y = switch(input$y_var_3,
                 "Month" = df$Month,
                 "Season" = df$Season),
      fill_var = switch(input$fill_3,
                        "Wind speed" = df$Wind_speed,
                        "Wind production" = df$Wind_production)
    )
    
    df_heat <- df_heat %>%
      group_by(Hour, Y) %>%
      summarise(value = mean(fill_var, na.rm = TRUE), .groups = "drop")
    
    ggplot(df_heat, aes(x = Hour, y = Y, fill = value)) +
      geom_tile() +
      scale_fill_viridis_c(option = "plasma") +
      labs(x = "Hour of Day", y = input$y_var_3, fill = input$fill_3) +
      theme_minimal() +
      theme(
        legend.background = element_blank(),
        plot.title = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()
      )
  })
  
  output$gap_under_stress <- renderPlot({
    filtered_df <- df %>% filter(Season %in% input$gap_seasons)
    
    ggplot(filtered_df, aes(x = Temperature, y = Production_gap, color = Season)) +
      geom_point(alpha = 0.01, size = 0.5) +
      geom_smooth(se = FALSE) +
      labs(y = "Production gap (MW)", title = "Production Gap by Temperature") +
      theme_minimal() +
      theme(
        legend.position = c(0.95, 0.85),
        legend.background = element_rect(fill = "white"),
        legend.box.background = element_blank(),
        axis.text = element_text(size = 12),
        axis.title.x = element_blank()
      ) +
      facet_wrap(~Day_or_night)
  })
  
  output$ramp_rate_vs_temp <- renderPlot({
    filtered_df <- df %>% filter(Season %in% input$gap_seasons)
    
    ggplot(filtered_df, aes(x = Temperature, y = Ramp_rate, color = Season)) +
      geom_point(alpha = 0.005, size = 0.5) +
      geom_smooth(se = FALSE) +
      coord_cartesian(ylim = c(0, 500)) +
      labs(y = "Ramp rate (MW)", x = "Temperature (°C)", title = "Ramp Rate by Temperature") +
      theme_minimal() +
      theme(
        legend.position = "none",
        legend.background = element_rect(fill = "white"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)
      ) +
      facet_wrap(~Day_or_night)
  })
  
  output$forecast_prediction <- renderPlot({
    newdata <- data.frame(
      Temperature = input$forecast_temp,
      Season = factor(input$forecast_season, levels = c("Winter", "Spring", "Summer", "Fall")),
      Hour = input$forecast_hour,
      Day_of_the_week = factor(input$forecast_day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
      Humidity = input$forecast_humidity
    )
    
    pred <- predict(model, newdata = newdata, interval = "prediction", level = 0.95)
    
    pred_point <- data.frame(
      Temperature = input$forecast_temp,
      Electric_demand = pred[1]
    )
    
    ggplot(df_plot, aes(x = Temperature, y = Electric_demand)) +
      geom_point(alpha = 0.05, size = 0.5, color = "steelblue") +
      geom_point(data = pred_point, aes(x = Temperature, y = Electric_demand),
                 color = "red", size = 5) +
      annotate("text", x = input$forecast_temp, y = pred[1] + 1500,
               label = paste(round(pred[1], 0), "MW"), color = "red", size = 4) +
      labs(title = "Predicted Demand vs Historical Data",
           x = "Temperature (ºC)", y = "Electric Demand (MW)") +
      theme_minimal()
  })
}

shinyApp(ui, server)
