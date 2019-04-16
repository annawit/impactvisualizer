
library(shiny)
library(dplyr)
library(readr)
library(shinyWidgets)
library(tidyr)
library(plotly)
library(DT)




# brings in impact factors
impact <- read_csv("I1.csv")

Material <- unique(impact[c("Material", "Disposition")])
WeightA <- c(1000, 1000, 1000, 3000,
             1000, 1000, 1000, 3000,
             1000, 1000, 2000,
             1000, 1000, 1000, 3000,
             1000, 1000, 1000, 3000,
             1000, 1000, 2000,
             1000, 1000, 1000, 3000,
             1000, 1000, 1000, 3000,
             1000, 1000, 1000, 3000,
             1000, 1000, 2000,
             1000, 1000, 1000, 1000, 4000,
             1000, 1000, 1000, 1000, 4000)
WeightB <- 0
df <- data.frame(Material, WeightA, WeightB)
  
mats <- data.frame(Material, WeightA, WeightB)
# mats$Date <- Sys.time() + seq_len(nrow(mats))

# UI ----------------------------------------------------------------------



ui <- fluidPage(
  navbarPage("Impact Checker",
    tabPanel("Single Material",
      

      sidebarLayout(
        sidebarPanel(
          pickerInput(inputId = "usermaterial",
                      label = "Select a waste material:",
                      choices = unique(impact$Material),
                      selected = "Cardboard",
                      options = list(style = "btn-primary")),
          pickerInput(inputId = "userimpact",
                      label = "Select an impact:",
                      choices = unique(impact$Category),
                      selected = "Energy demand",
                      options = list(style = "btn-primary"))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          wellPanel(
            plotlyOutput("impactfactorplot")
          ),
          wellPanel(div(style = "height:300px;",
                        DT::dataTableOutput("df"))
          )
          
        )
      )
    ),
    

# Multi Panel -------------------------------------------------------------
tabPanel("Multiple materials",
         sidebarLayout(
           sidebarPanel(
             
             pickerInput(inputId = "usermaterial2",
                         label = "Select a waste material:",
                         choices = unique(impact$Material),
                         selected = unique(impact$Material),
                         multiple = TRUE,
                         options = list(style = "btn-primary")),
             pickerInput(inputId = "userimpact2",
                         label = "Select an impact:",
                         choices = unique(impact$Category),
                         selected = "Energy demand",
                         options = list(style = "btn-primary"))
           ),
           
           mainPanel(
             
             wellPanel(
               plotlyOutput("multimaterialplot")
             ),
             wellPanel(
               div(DT::dataTableOutput("df2"))
             )
           )
         )
         
         
),
tabPanel("User Entry",
         fluidPage(
           fluidRow(
             column(5,
                    DTOutput("x1")
             ),
             column(7,
                    pickerInput(inputId = "impactselector",
                                label = "Select an impact:",
                                choices = unique(impact$Category),
                                selected = "Energy demand",
                                options = list(style = "btn-primary")),
                    pickerInput(inputId = "materialselector",
                                label = "Select materials:",
                                choices = unique(impact$Material),
                                selected =c("Food", "Electronics"),
                                multiple = TRUE,
                                options = list(style = "btn-primary")),
                    plotlyOutput("compareplot"),
                    plotlyOutput("weightplot"),
                    DTOutput("x2")
             )
           )
)
)
)
)

# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  df <- reactive({
    impact %>% 
      filter(Material == input$usermaterial) %>% 
      filter(Category == input$userimpact)
  })
  
  
  output$df <- DT::renderDataTable({
    df()
  })
  
  output$impactfactorplot <- renderPlotly({
    p <- plot_ly(df(),
                 x = ~Disposition,
                 y = ~Factor,
                 name = ~Disposition,
                 color = ~`Life Cycle Stage`,
                 type = "bar"
    )
    p %>% 
      layout(
        yaxis = list(title =  paste("Impact in", df()$Units[[1]])),
        xaxis = list(title = ""),
        shapes = list(type = rect, 
                         x0 = min(df()$Disposition), 
                         x1 = max(df()$Disposition, 
                         y0 = -40000, 
                         y1 = 0), 
                         fillcolor='orange', 
                         layer='below'),
                    list(type=rect, 
                         x0=min(df()$Disposition), 
                         x1=max(df()$Disposition), 
                         y0=0,
                         y1=40000, 
                         fillcolor='blue', 
                         layer='below')
      ) %>% 
      config(displaylogo = FALSE,
             collaborate = FALSE,
             modeBarButtonsToRemove = list(
               'sendDataToCloud',
               'zoom2d',
               'pan2d',
               'select2d',
               'lasso2d',
               'zoomIn2d',
               'zoomOut2d',
               'autoScale2d',
               'resetScale2d',
               'toggleSpikelines'
             ))
    
  })
  
  # multi -------------------------------------------------------------------
  
  
  
  df2spread <- reactive({
    impact %>% 
      select(-"Life Cycle Stage") %>% 
      spread("Disposition", "Factor", fill = 0) %>% 
      filter(Material %in% input$usermaterial2) %>% 
      filter(Category %in% input$userimpact2)
    
  })
  
  # df2spread <- reactive({
  #   df2() %>% 
  #     select(-"Life Cycle Stage") %>% 
  #     spread("Disposition", "Factor", fill = 0)
  # })
  # 
  
  
  output$df2 <- DT::renderDataTable({
    df2spread()
  })
  
  output$multimaterialplot <- renderPlotly({
    p <- plot_ly(df2spread(),
                 x = ~Material,
                 type = 'bar',
                 y = ~Production,
                 name = "Production") %>% 
      add_trace(y = ~Landfilling,
                name = "Landfilling") %>% 
      add_trace(y = ~Combustion,
                name = "Combustion") %>% 
      add_trace(y = ~Recycling,
                name = "Recycling") %>% 
      add_trace(y = ~`Anaerobic Digestion`,
                name = "Anaerobic Digestion") %>% 
      add_trace(y = ~Composting,
                name = "Composting") %>% 
      add_trace(y = ~`Use as Aggregate`,
                name = "Use as Aggregate") %>% 
      layout(barmode = 'group')
    
    p %>% 
      layout(
        yaxis = list(title =  paste("Impact in", df()$Units[[1]])),
        xaxis = list(title = "")
      ) %>% 
      config(displaylogo = FALSE,
             collaborate = FALSE,
             modeBarButtonsToRemove = list(
               'sendDataToCloud',
               # 'zoom2d',
               'pan2d',
               'select2d',
               'lasso2d',
               'zoomIn2d',
               'zoomOut2d',
               'autoScale2d',
               # 'resetScale2d',
               'toggleSpikelines'
             ))
    
  })
  
  # User Input ---------------------------------------------------------
  
  output$x1 <- renderDT(mats, 
                        selection = 'none',
                        editable = TRUE
                        )
  
  proxy <- dataTableProxy('x1')

  observeEvent(input$x1_cell_edit, {
    info <- input$x1_cell_edit
    str(info)
    i <- info$row
    j <- info$col
    v <- info$value

    #limits editing to columns specified by j
    if ( j > 1 & j < 5) {
      mats[i, j] <<- DT::coerceValue(v, mats[i, j])
      replaceData(proxy, mats, resetPaging = FALSE)  # important
    } else {}

  })
  
  y <- reactive({
    input$x1_cell_edit
    mats %>% 
      left_join(impact) %>% 
      mutate(ImpactA = WeightA * Factor,
             ImpactB = WeightB * Factor)
  })
  
  
  output$x2 <- renderDT(y(), 
                        selection = 'none')
  
  output$compareplot <- renderPlotly({
    
    p <- plot_ly(y() %>% 
                   filter(Category %in% input$impactselector) %>% 
                   filter(Material %in% input$materialselector),
                 x = ~Material,
                 type = 'bar',
                 y = ~ImpactA,
                 name = ~paste0(Disposition, "- Impact A"),
                 color = ~paste0(Disposition, "- Impact A"),
                 colors = c("#272727", "#fed766", "#009fb7", "#60492c", "#bbbe64")) %>% 
      add_trace(y = ~ImpactB,
                name = ~paste(Disposition, "- Impact B"),
                color = I(c("#0a2e36", "#242424","#a28941","#005764","#3e2f1d", "#ACBEA3", "#55572e"))) %>% 
                # color = ~paste(Disposition, "- Impact B"),
                # colors = c("#242424","#a28941","#005764","#3e2f1d","#55572e")) %>% 
      layout(barmode = 'group')
    
    p %>% 
      layout(
        yaxis = list(title =  paste("Impact in", y()$Units[[1]])),
        xaxis = list(title = "")
      ) %>% 
      config(displaylogo = FALSE,
             collaborate = FALSE,
             modeBarButtonsToRemove = list(
               'sendDataToCloud',
               # 'zoom2d',
               'pan2d',
               'select2d',
               'lasso2d',
               'zoomIn2d',
               'zoomOut2d',
               'autoScale2d',
               # 'resetScale2d',
               'toggleSpikelines'
             ))
  })
  
  
  output$weightplot <- renderPlotly({
    
    p <- plot_ly(y() %>% 
            filter(Category %in% input$impactselector) %>% 
            filter(Material %in% input$materialselector) %>%
              filter(Disposition != "Production"),
                 x = ~Material,
                 type = 'bar',
                 y = ~WeightA,
                 name = ~paste0(Disposition, "- Weight A"),
                 color = ~paste0(Disposition, "- Weight A")) %>% 
      add_trace(y = ~WeightB,
                name = ~paste(Disposition, "- Weight B"),
                color = ~paste(Disposition, "- Weight B")) %>% 
      layout(barmode = 'group')
    
    p %>% 
      layout(
        yaxis = list(title =  "Weights in tons"),
        xaxis = list(title = "")
      ) %>% 
      config(displaylogo = FALSE,
             collaborate = FALSE,
             modeBarButtonsToRemove = list(
               'sendDataToCloud',
               # 'zoom2d',
               'pan2d',
               'select2d',
               'lasso2d',
               'zoomIn2d',
               'zoomOut2d',
               'autoScale2d',
               # 'resetScale2d',
               'toggleSpikelines'
             ))
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

