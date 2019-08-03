library(shiny)
library(tidyverse)

equiv_fn <- function(income, adults, kids, adult_weight = 0.5, kid_weight = 0.3){
  equiv_y <- income / (1 + ((adults - 1) * adult_weight) + (kids * kid_weight))
  equiv_y <- as.integer(equiv_y / 52)
  equiv_y
} 

# data from ABS 6523.0 - Household Income and Wealth, Australia, 2017-18
# see prepare_summary_tables.R for the steps taken to clean this data

equiv_long_latest <- read_csv("equiv_long_latest.csv")
equiv_dec_latest <- read_csv("equiv_dec_latest.csv")

year <- unique(equiv_dec_latest$year)
med_inc <- equiv_dec_latest$income[equiv_dec_latest$percentile == 50]


# Define UI ----
ui <- fluidPage(
  headerPanel('How does your income compare?'),
  sidebarPanel(
    numericInput('income', 'Household income per year after tax', value = 86000, min = 0),
    "This includes the after-tax value of any wages and salaries you or other members of your household receive, as well as any investment income (like interest, rent, or dividends), government pensions and allowances, financial support from relatives, workers' compensation, child support received, etc.",
    br(),
    br(),
    numericInput('adults', 'Number of adults in your house', 2, min = 1, max = 5),
    "'Adults' includes anyone aged 15 or over.",
    br(),
    br(),
    numericInput('kids', 'Number of kids in your house', 0, min = 0, max = 10),
    "Kids are those people in your household aged 14 and under."
  ),
  mainPanel(
    textOutput("incomesummary"),
    strong(textOutput("equivtext")),
    br(),
    plotOutput("Plot", width = "100%")
  )
  
)

# Define server logic ----
server <- function(input, output) {
  
  output$incomesummary <- renderText({
    equiv <- equiv_fn(income = input$income,
                      adults = input$adults,
                      kids = input$kids)
    
    paste0("Your equivalised household disposable income is $", equiv, 
           " per week. The median for all Australians is $", med_inc," per week.")
    
  })
  
  output$equivtext <- renderText({

    equiv <- equiv_fn(income = input$income,
                      adults = input$adults,
                      kids = input$kids)

    if(equiv >= equiv_dec_latest$income[equiv_dec_latest$percentile == 10]){
      paste0(
        "You're better off than at least ",
        equiv_dec_latest[max(which(equiv_dec_latest$income <= equiv)),1], "% of Australians.")
    } else {
      "Your income is in the lowest 10% of Australians."
    }

  })
  
  
  
  output$Plot <- renderPlot({
    
    equiv <- as.integer(equiv_fn(income = input$income,
                                 adults = input$adults,
                                 kids = input$kids))
    
    user.range <- equiv_long_latest$income_range[min(which(equiv < equiv_long_latest$range_top))]
    
    equiv_long_latest %>%
      mutate(label = ifelse(income_range == user.range & id_within_group == 1,
                            "You and 99 999 other\nAustralians are in the blue square",
                            "Each square represents\nabout 100 000 Australians")) %>%
      ggplot(aes(x = reorder(income_range, range_top), group = id, fill = label)) +
      geom_bar(stat = "count", col = "white") +
      labs(title = "How does your income compare?",
           subtitle = "Distribution of equivalised disposable household income in Australia, 2017-18",
           caption = "Source: ABS 6523.0",
           x = "Equivalised disposable household income per week") +
      theme_minimal() +
      scale_fill_manual(values = c("#E33946","#21519B")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.position = "top",
            text = element_text(size = 15),
            panel.grid  = element_blank(),
            legend.title = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            plot.caption = element_text(size = rel(0.7), hjust = 0)) 
    
    
  }, height = 500)
  
}


# Run the app ----
shinyApp(ui = ui, server = server)
