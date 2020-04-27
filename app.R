#shiny
if (!require(shiny)) {
  install.packages("shiny",
                   repos = c("htt://rstudio.org/_packages", "http://cran.rstudio.com"))
  require(shiny)
}
#DT for DataTables
if (!require(DT)) {
  install.packages("DT",
                   repos = c("htt://rstudio.org/_packages", "http://cran.rstudio.com"))
  require(DT)
}
#library shinyWidgets
if (!require(shinyWidgets)) {
  install.packages(
    "shinyWidgets",
    repos = c("htt://rstudio.org/_packages", "http://cran.rstudio.com")
  )
  require(shinyWidgets)
}
if (!require(shinythemes)) {
  install.packages(
    "shinythemes",
    repos = c("htt://rstudio.org/_packages", "http://cran.rstudio.com")
  )
  require(shinythemes)
}
#library tidyverse
if (!require(tidyverse)) {
  install.packages("tidyverse",
                   repos = c("htt://rstudio.org/_packages", "http://cran.rstudio.com"))
  require(tidyverse)
}
#ggplot2 for the plot
if (!require(ggplot2)) {
  install.packages("ggplot2",
                   repos = c("htt://rstudio.org/_packages", "http://cran.rstudio.com"))
  require(ggplot2)
}
#plotly for the pop-up
if (!require(plotly)) {
  install.packages("plotly",
                   repos = c("htt://rstudio.org/_packages", "http://cran.rstudio.com"))
  require(plotly)
}

#read data
requests_data <- read_csv("data/requests_data_rt.csv")

#utility function gives records for period of time specified in
#start - begin date and stop end date params
data_for_time <- function(data, start, stop, date_format) {
  result <- data %>%
    filter((as.Date(start, date_format) < date_received) &
             (date_received < as.Date(stop, date_format))) %>%
    arrange(date_received)
  return(result)
}
#utility function calculating number of requests by
#category, service description and suburb
requests_category_service_desc_suburb <- function(year) {
  data <- requests_data_2015
  if (year == '2016')
  {
    data <- requests_data_2016
  }
  result <- data %>%
    count(category, service_desc, suburb)
  return(result)
}
#utility function calculating median of days_to_complete for requests by
#category, service description and suburb
median_requests_time_category_service_desc_suburb <-
  function(year) {
    data <- requests_data_2015
    if (year == '2016')
    {
      data <- requests_data_2016
    }
    result <- data %>%
      group_by(suburb, service_desc, category) %>%
      summarise(mdn = as.integer(median(days_to_complete)))
    return(result)
  }
#utility function calculating percent of requests per sevice description
requests_category_service_desc_proportion <-
  function(year) {
    data <- requests_data_2015
    if (year == '2016')
    {
      data <- requests_data_2016
    }
    result <- data %>%
      count(category, service_desc) %>%
      arrange(desc(n)) %>%
      mutate(percentage = (n * 100) / sum(n))
    return(result)
  }

#data for the same period as 2015
requests_data_2015 <-
  data_for_time(requests_data, "12/31/2014", "10/16/2015", "%m/%d/%Y")
#data for the same period as 2016
requests_data_2016 <-
  data_for_time(requests_data, "12/31/2015", "10/16/2016", "%m/%d/%Y")

requests_category_service_desc_suburb_2015 <-
  requests_data_2015  %>%
  count(category, service_desc, suburb)

requests_category_service_desc_suburb_2016 <-
  requests_data_2016  %>%
  count(category, service_desc, suburb)

lst.suburbs <- as.list(unique(requests_data$suburb))
lst.categories <- as.list(unique(requests_data$category))
lst.years <- c("2015", "2016")
suburbs <-
  c(
    'Carlton',
    'Carlton\nNorth',
    'Docklands',
    'East\nMelbourne',
    '\nKensington',
    'Melbourne',
    'North\nMelbourne',
    'Parkville',
    'Port\nMelbourne',
    'South\nYarra',
    'Southbank',
    'West\nMelbourne'
  )
ui <- fluidPage(
  theme = shinytheme("united"),
  #settings to display error messages and table headers
  tags$head(tags$style(
    HTML(
      "
      .shiny-output-error-validation {
      color: red;
      font-size: 20px;
      font-style: bold;
      }
      
      h1 {
      color: black;
      font-size: 23px;
      font-style: bold;
      }
      "
    )
    )),
  titlePanel("Melbourne City Council Public Service Requests"),
  sidebarLayout(
    sidebarPanel(
      #year radio buttons
      radioButtons("year", "Select Year", lst.years),
      hr(),
      h5(
        "To view suburb and service specific plots in 'Requests','Response Times' or 'Requests Servicing' panels"
      ),
      pickerInput(
        inputId = "suburbs",
        label = "Select Suburbs:",
        choices = lst.suburbs,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 2",
          `count-selected-text` = "{0}/{1} suburbs"
        ),
        multiple = TRUE
      ),
      verbatimTextOutput("suburbs"),
      
      pickerInput(
        inputId = "categories",
        label = "Select Categories:",
        choices = lst.categories,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 2",
          `count-selected-text` = "{0}/{1} categories"
        ),
        multiple = TRUE
      ),
      verbatimTextOutput("categories"),
      width = 2
    ),
    #end of sidebar panel
    #Main panel for displaying outputs ----
    mainPanel(
      #Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(
        id = "tabset",
        type = "tabs",
        tabPanel(
          "At a Glance",
          plotlyOutput("glancePlot", height = "600px", width = "1500px")
        ),
        tabPanel(
          "Requests",
          htmlOutput("rsummary"),
          hr(),
          dataTableOutput("requestsTable"),
          hr(),
          plotlyOutput("requestsPlot", height = "600px", width = "1500px")
        ),
        tabPanel(
          "Response Times",
          htmlOutput("tsummary"),
          hr(),
          dataTableOutput("timesTable"),
          hr(),
          plotlyOutput("responseTimePlot", height = "600px", width = "1500px")
        ),
        tabPanel(
          "Requests Servicing",
          htmlOutput("rservsummary"),
          hr(),
          dataTableOutput("rservTable"),
          hr(),
          plotlyOutput("rservicing", height = "600px", width = "1500px")
          
        )
      ),
      width = 10,
      inline = TRUE
    )#end of mainPanel
  )#end of sidebar layout
  ,
  style = 'height: 600px; width: 1800px',
  inline = TRUE
    )#end of fluid page

server <- function(input, output, session) {
  output$year <- renderPrint({
    if (!is.null(input$year))
    {
      return(paste0(input$year))
    }
  })
  output$suburbs <- renderPrint({
    if (!is.null(input$suburbs))
    {
      return(paste0(input$suburbs))
    }
  })
  output$categories <- renderPrint({
    if (!is.null(input$categories))
    {
      return(paste0(input$categories))
    }
  })
  output$all_suburbs  <- renderUI({
    lapply(paste0("suburbs", 1:4),
           function(x)
           {
             updateCheckboxInput(session = session,
                                 inputId = x,
                                 value = TRUE)
           })
  })
  #'at a glance' panel: plot
  output$glancePlot <- renderPlotly({
    p <- requests_category_service_desc_proportion(input$year) %>%
      ggplot(aes(
        x = reorder(service_desc, percentage),
        y = percentage,
        fill = category,
        text = paste(
          category,
          "\n",
          service_desc,
          ":\n",
          round(percentage, 2),
          "% of requests:"
        )
      )) +
      geom_col() +
      scale_y_continuous(trans = 'log2') + #log scale to display small % of requests properly
      coord_flip() +
      ylab("Proportion in %") +
      xlab(" ") +
      scale_fill_brewer(palette = "Set2") +
      theme (
        panel.background = element_rect (fill = 'grey'),
        panel.grid.major = element_line (colour = 'grey90', size = 0.20),
        panel.grid.minor = element_line (colour = 'grey90', size = 0.10),
        plot.title = element_text (lineheight = 1.15, hjust = -10),
        axis.title.y = element_text (angle = 90),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text (size = 12),
        text = element_text (size = 14),
        legend.title = element_blank(),
        strip.text.x = element_text (size = 10,
                                     angle = 5),
        legend.position = c(0.9, 0.2)
      ) +
      labs(
        title = paste("% of requests per service description for ",
                      input$year),
        font.main = 20,
        face = "normal",
        width = "auto",
        height = "auto",
        res = 72,
        scrollable = TRUE
      )
    print(ggplotly(p, tooltip = "text"))
  })
  #requests panel header
  output$rsummary <- renderText({
    HTML(
      paste0(
        "<h1>Number of requests per service description and suburb from highest to lowest in ",
        input$year,
        "</h1>"
      )
    )
    
  })
  #requests panel: table
  output$requestsTable <- renderDataTable({
    p <- requests_category_service_desc_suburb(input$year) %>%
      arrange(desc(n)) %>%
      rename(
        "Category" = category,
        "Suburb" = suburb,
        "Service Description" = service_desc,
        "Number of Requests" = n
      )
    p
  }, options = list(pageLength = 5,
                    lengthMenu = c(5, 10, 15)))
  #requests panel: plot
  output$requestsPlot <- renderPlotly({
    validate(
      need(
        input$suburbs != "",
        "To view a plot please select at least one suburb"
      ),
      need(
        input$categories != "",
        "To view a plot please select at least one category"
      )
    )
    p <- requests_category_service_desc_suburb(input$year) %>%
      filter(suburb %in% input$suburbs) %>%
      filter(category %in% input$categories) %>%
      ggplot(aes(
        x = reorder(service_desc, n),
        y = n,
        fill = category,
        text = paste(
          suburb,
          "\n",
          category,
          "\n",
          service_desc,
          ":\nnumber of requests:",
          n
        )
      )) +
      geom_col() +
      scale_y_continuous(trans = 'log2') + #log scale to display small numbers of requests properly
      coord_flip() +
      ylab("") +
      xlab("") +
      scale_fill_brewer(palette = "Set2") +
      theme (
        panel.background = element_rect (fill = 'grey'),
        panel.grid.major = element_line (colour = 'grey90', size = 0.20),
        panel.grid.minor = element_line (colour = 'grey90', size = 0.10),
        plot.title = element_text (lineheight = 1.15, hjust = -10),
        axis.title.y = element_text (angle = 90),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text (size = 12),
        text = element_text (size = 14),
        legend.title = element_blank(),
        strip.text.x = element_text (size = 10,
                                     angle = 8),
        legend.position = c(0.9, 0.2)
      ) +
      facet_grid( ~ suburb, scales = "free", space = "free") +
      labs(
        title = paste("Number of requests per service description for ",
                      input$year),
        font.main = 20,
        width = "auto",
        height = "auto",
        res = 72,
        scrollable = TRUE
      )
    print(ggplotly(p, tooltip = "text"))
    
  })
  #times panel: header
  output$tsummary <- renderText({
    HTML(
      paste0(
        "<h1>Median request completion times per service description and suburb from longest to shortest in ",
        input$year,
        "</h1>"
      )
    )
    
  })
  #times panel: table
  output$timesTable <- renderDataTable({
    p <-
      median_requests_time_category_service_desc_suburb(input$year) %>%
      arrange(desc(mdn)) %>%
      rename(
        "Category" = category,
        "Suburb" = suburb,
        "Service Description" = service_desc,
        "Median Request Completion Times (days)" = mdn
      )
    p
  }, options = list(pageLength = 5,
                    lengthMenu = c(5, 10, 15)))
  #times panel: plot
  output$responseTimePlot <- renderPlotly({
    validate(
      need(
        input$suburbs != "",
        "To view a plot please select at least one suburb"
      ),
      need(
        input$categories != "",
        "To view a plot please select at least one category"
      )
    )
    p <-
      median_requests_time_category_service_desc_suburb(input$year) %>%
      filter(suburb %in% input$suburbs) %>%
      filter(category %in% input$categories) %>%
      ggplot(
        aes(
          x = suburb,
          y = service_desc,
          size = mdn,
          fill = category,
          text = paste(category, ":\nmedian time:", mdn, " days")
        ),
        show.legend = TRUE
      ) +
      geom_point(alpha = 0.5, shape = 21) +
      scale_fill_brewer(palette = "Set2") +
      theme (
        panel.background = element_rect (fill = 'light grey'),
        panel.grid.major = element_line (colour = 'grey90', size = 0.20),
        panel.grid.minor = element_line (colour = 'grey90', size = 0.10),
        plot.title = element_text (lineheight = 0),
        axis.title.y = element_text (angle = 90),
        axis.title = element_text (size = 12),
        text = element_text (size = 14),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(colour = "black", size =
                                     12),
        legend.position = "bottom",
        legend.box = "vertical",
        strip.text = element_text (size = 12, angle = 0),
        legend.justification = c(1, 0),
        
      ) +
      labs(
        title =
          paste(
            "Median request completion time (in days) for requests from 1 Jan to 15 Oct ",
            input$year
          ),
        font.main = 20,
        width = "auto",
        height = "auto",
        res = 72,
        scrollable = TRUE
      ) +
      ylab(" ") +
      xlab(" ")
    print(ggplotly(p, tooltip = "text"))
  })
  #request servicing panel: heading
  output$rservsummary <- renderText({
    HTML(
      paste0(
        "<h1>Response time versus number of requests from highest to lowest in ",
        input$year,
        "</h1>"
      )
    )
  })
  #request servicing panel: table
  output$rservTable <- renderDataTable({
    p <- merge_data <-
      median_requests_time_category_service_desc_suburb(input$year) %>%
      inner_join(requests_category_service_desc_suburb(input$year)) %>%
      arrange(desc(n, mdn)) %>%
      rename(
        "Category" = category,
        "Suburb" = suburb,
        "Service Description" = service_desc,
        "Number of Requests" = n,
        "Median Response Time (in days)" = mdn
      )
    p
  }, options = list(pageLength = 5,
                    lengthMenu = c(5, 10, 15)))
  
  #request servicing panel: plot
  output$rservicing <- renderPlotly({
    validate(
      need(
        input$suburbs != "",
        "To view a plot please select at least one suburb"
      ),
      need(
        input$categories != "",
        "To view a plot please select at least one category"
      )
    )
    good.shapes = c(1:21)
    merge_data <-
      median_requests_time_category_service_desc_suburb(input$year) %>%
      inner_join(requests_category_service_desc_suburb(input$year))
    
    merge_data$suburb <- factor(merge_data$suburb)
    p <- merge_data %>%
      filter(suburb %in% input$suburbs) %>%
      filter(category %in% input$categories) %>%
      ggplot(aes(
        n,
        mdn,
        shape = suburb,
        color = category,
        text = paste(
          suburb,
          "\n",
          service_desc,
          "\nnumber of requests: ",
          n,
          "\nmedian time:",
          mdn,
          " days"
        )
      )) +
      scale_shape_manual(values = good.shapes[1:13]) +
      geom_point(size = 5) +
      scale_x_continuous(trans = 'log10') + #log scale to display small numbers properly
      scale_fill_brewer(palette = "Set2") +
      theme(
        panel.background = element_rect (fill = 'grey'),
        panel.grid.major = element_line (colour = 'grey90', size = 0.20),
        panel.grid.minor = element_line (colour = 'grey90', size = 0.10),
        axis.title = element_text (size = 12),
        text = element_text (size = 14),
        legend.title = element_blank(),
        legend.text = element_text(colour = "black", size =
                                     12),
        legend.position = "bottom",
        legend.box = "vertical",
        strip.text = element_text (size = 12, angle = 90),
        legend.justification = c(1, 0)
      ) +
      labs(
        title = paste(
          "Response time versus number of requests from 1 Jan to 15 Oct ",
          input$year
        ),
        y = "Median Response Time (in days)",
        x = "Number of Requests",
        color = "Numbers for",
        shape = "Service Description:",
        font.main = 20,
        width = "auto",
        height = "auto",
        res = 72,
        scrollable = TRUE
      )
    print(ggplotly(p, tooltip = "text"))
  })
}
shinyApp(ui, server)