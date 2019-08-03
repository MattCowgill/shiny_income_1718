library(shiny)
library(tidyverse)

equiv_fn <- function(income, adults, kids, adult_weight = 0.5, kid_weight = 0.3){
  equiv_y <- income / (1 + ((adults - 1) * adult_weight) + (kids * kid_weight))
  equiv_y <- as.integer(equiv_y / 52)
  equiv_y
} 

# data from ABS 6523.0 - Household Income and Wealth, Australia, 2015-16
# table 1.1
abs.income <- tibble::tribble(
  ~Percentile, ~Income,
  0L,      0,
  10L,     436,
  20L,     523,
  30L,     635,
  40L,     737,
  50L,     853,
  60L,     987,
  70L,    1153,
  80L,    1371,
  90L,    1705
)

# data from ABS 6523.0 - Household Income and Wealth, Australia, 2015-16
# table 1.3 
fullranges <- tibble(Person = c(1L, 1L, 101L, 1L, 1L, 1L, 101L, 1L, 101L, 1L, 101L, 201L,
                                1L, 101L, 201L, 301L, 1L, 101L, 201L, 301L,
                                401L, 501L, 1L, 101L, 201L, 301L, 401L, 501L,
                                601L, 701L, 801L, 901L, 1001L, 1101L, 1L, 101L,
                                201L, 301L, 401L, 501L, 601L, 701L, 801L, 901L,
                                1001L, 1101L, 1L, 101L, 201L, 301L, 401L, 501L,
                                601L, 701L, 801L, 901L, 1001L, 1L, 101L, 201L,
                                301L, 401L, 501L, 601L, 701L, 801L, 901L,
                                1001L, 1101L, 1L, 101L, 201L, 301L, 401L, 501L,
                                601L, 701L, 801L, 901L, 1L, 101L, 201L, 301L,
                                401L, 501L, 601L, 701L, 801L, 901L, 1001L, 1101L,
                                1L, 101L, 201L, 301L, 401L, 501L, 601L, 701L,
                                801L, 901L, 1001L, 1101L, 1L, 101L, 201L, 301L,
                                401L, 501L, 601L, 701L, 801L, 901L, 1001L, 1L,
                                101L, 201L, 301L, 401L, 501L, 601L, 701L, 801L,
                                901L, 1L, 101L, 201L, 301L, 401L, 501L, 601L,
                                701L, 801L, 1L, 101L, 201L, 301L, 401L, 501L,
                                601L, 701L, 801L, 901L, 1L, 101L, 201L, 301L,
                                401L, 501L, 601L, 701L, 1L, 101L, 201L, 301L,
                                401L, 501L, 601L, 701L, 1L, 101L, 201L, 301L, 401L,
                                501L, 601L, 1L, 101L, 201L, 301L, 401L, 501L,
                                601L, 701L, 801L, 901L, 1001L, 1101L, 1201L,
                                1301L, 1L, 101L, 201L, 301L, 401L, 501L, 601L,
                                701L, 801L, 901L, 1001L, 1101L, 1L, 101L, 201L,
                                301L, 401L, 501L, 601L, 701L, 801L, 901L, 1001L,
                                1101L, 1201L, 1301L, 1401L, 1501L, 1601L,
                                1701L, 1L, 101L, 201L, 301L, 401L, 501L, 601L,
                                701L, 801L, 901L, 1001L, 1101L, 1201L, 1L, 101L,
                                201L, 301L, 401L, 501L, 601L, 701L, 801L, 901L,
                                1001L, 1L, 101L, 201L, 301L, 401L, 501L, 601L,
                                701L, 801L, 901L, 1001L, 1101L, 1201L, 1301L),
                     Range = c("No income", "$1–49", "$1–49", "$50–99", "$100–149",
                               "$150–199", "$150–199", "$200–249",
                               "$200–249", "$250–299", "$250–299", "$250–299",
                               "$300–349", "$300–349", "$300–349", "$300–349",
                               "$350–399", "$350–399", "$350–399", "$350–399",
                               "$350–399", "$350–399", "$400–449", "$400–449",
                               "$400–449", "$400–449", "$400–449", "$400–449",
                               "$400–449", "$400–449", "$400–449", "$400–449",
                               "$400–449", "$400–449", "$450–499", "$450–499",
                               "$450–499", "$450–499", "$450–499", "$450–499",
                               "$450–499", "$450–499", "$450–499", "$450–499",
                               "$450–499", "$450–499", "$500–549", "$500–549",
                               "$500–549", "$500–549", "$500–549",
                               "$500–549", "$500–549", "$500–549", "$500–549",
                               "$500–549", "$500–549", "$550–599", "$550–599",
                               "$550–599", "$550–599", "$550–599", "$550–599",
                               "$550–599", "$550–599", "$550–599", "$550–599",
                               "$550–599", "$550–599", "$600–649", "$600–649",
                               "$600–649", "$600–649", "$600–649", "$600–649",
                               "$600–649", "$600–649", "$600–649", "$600–649",
                               "$650–699", "$650–699", "$650–699", "$650–699",
                               "$650–699", "$650–699", "$650–699", "$650–699",
                               "$650–699", "$650–699", "$650–699", "$650–699",
                               "$700–749", "$700–749", "$700–749", "$700–749",
                               "$700–749", "$700–749", "$700–749",
                               "$700–749", "$700–749", "$700–749", "$700–749",
                               "$700–749", "$750–799", "$750–799", "$750–799",
                               "$750–799", "$750–799", "$750–799", "$750–799",
                               "$750–799", "$750–799", "$750–799", "$750–799",
                               "$800–849", "$800–849", "$800–849", "$800–849",
                               "$800–849", "$800–849", "$800–849", "$800–849",
                               "$800–849", "$800–849", "$850–899", "$850–899",
                               "$850–899", "$850–899", "$850–899", "$850–899",
                               "$850–899", "$850–899", "$850–899", "$900–949",
                               "$900–949", "$900–949", "$900–949", "$900–949",
                               "$900–949", "$900–949", "$900–949", "$900–949",
                               "$900–949", "$950–999", "$950–999",
                               "$950–999", "$950–999", "$950–999", "$950–999",
                               "$950–999", "$950–999", "$1000–1049", "$1000–1049",
                               "$1000–1049", "$1000–1049", "$1000–1049",
                               "$1000–1049", "$1000–1049", "$1000–1049", "$1050–1099",
                               "$1050–1099", "$1050–1099", "$1050–1099",
                               "$1050–1099", "$1050–1099", "$1050–1099",
                               "$1100–1199", "$1100–1199", "$1100–1199", "$1100–1199",
                               "$1100–1199", "$1100–1199", "$1100–1199",
                               "$1100–1199", "$1100–1199", "$1100–1199",
                               "$1100–1199", "$1100–1199", "$1100–1199", "$1100–1199",
                               "$1200–1299", "$1200–1299", "$1200–1299",
                               "$1200–1299", "$1200–1299", "$1200–1299",
                               "$1200–1299", "$1200–1299", "$1200–1299", "$1200–1299",
                               "$1200–1299", "$1200–1299", "$1300–1499",
                               "$1300–1499", "$1300–1499", "$1300–1499", "$1300–1499",
                               "$1300–1499", "$1300–1499", "$1300–1499",
                               "$1300–1499", "$1300–1499", "$1300–1499",
                               "$1300–1499", "$1300–1499", "$1300–1499", "$1300–1499",
                               "$1300–1499", "$1300–1499", "$1300–1499",
                               "$1500–1699", "$1500–1699", "$1500–1699",
                               "$1500–1699", "$1500–1699", "$1500–1699", "$1500–1699",
                               "$1500–1699", "$1500–1699", "$1500–1699",
                               "$1500–1699", "$1500–1699", "$1500–1699",
                               "$1700–1999", "$1700–1999", "$1700–1999", "$1700–1999",
                               "$1700–1999", "$1700–1999", "$1700–1999",
                               "$1700–1999", "$1700–1999", "$1700–1999",
                               "$1700–1999", "$2000 or more", "$2000 or more",
                               "$2000 or more", "$2000 or more", "$2000 or more",
                               "$2000 or more", "$2000 or more", "$2000 or more",
                               "$2000 or more", "$2000 or more", "$2000 or more",
                               "$2000 or more", "$2000 or more",
                               "$2000 or more"),
                     Top.of.range = c(0L, 49L, 49L, 99L, 149L, 199L, 199L, 249L, 249L, 299L,
                                      299L, 299L, 349L, 349L, 349L, 349L, 399L,
                                      399L, 399L, 399L, 399L, 399L, 449L, 449L, 449L,
                                      449L, 449L, 449L, 449L, 449L, 449L, 449L, 449L,
                                      449L, 499L, 499L, 499L, 499L, 499L, 499L, 499L,
                                      499L, 499L, 499L, 499L, 499L, 549L, 549L, 549L,
                                      549L, 549L, 549L, 549L, 549L, 549L, 549L,
                                      549L, 599L, 599L, 599L, 599L, 599L, 599L, 599L,
                                      599L, 599L, 599L, 599L, 599L, 649L, 649L, 649L,
                                      649L, 649L, 649L, 649L, 649L, 649L, 649L, 699L,
                                      699L, 699L, 699L, 699L, 699L, 699L, 699L, 699L,
                                      699L, 699L, 699L, 749L, 749L, 749L, 749L, 749L,
                                      749L, 749L, 749L, 749L, 749L, 749L, 749L,
                                      799L, 799L, 799L, 799L, 799L, 799L, 799L, 799L,
                                      799L, 799L, 799L, 849L, 849L, 849L, 849L, 849L,
                                      849L, 849L, 849L, 849L, 849L, 899L, 899L, 899L,
                                      899L, 899L, 899L, 899L, 899L, 899L, 949L, 949L,
                                      949L, 949L, 949L, 949L, 949L, 949L, 949L, 949L,
                                      999L, 999L, 999L, 999L, 999L, 999L, 999L,
                                      999L, 1049L, 1049L, 1049L, 1049L, 1049L, 1049L,
                                      1049L, 1049L, 1099L, 1099L, 1099L, 1099L, 1099L,
                                      1099L, 1099L, 1199L, 1199L, 1199L, 1199L, 1199L,
                                      1199L, 1199L, 1199L, 1199L, 1199L, 1199L,
                                      1199L, 1199L, 1199L, 1299L, 1299L, 1299L, 1299L,
                                      1299L, 1299L, 1299L, 1299L, 1299L, 1299L, 1299L,
                                      1299L, 1499L, 1499L, 1499L, 1499L, 1499L,
                                      1499L, 1499L, 1499L, 1499L, 1499L, 1499L, 1499L,
                                      1499L, 1499L, 1499L, 1499L, 1499L, 1499L, 1699L,
                                      1699L, 1699L, 1699L, 1699L, 1699L, 1699L, 1699L,
                                      1699L, 1699L, 1699L, 1699L, 1699L, 1999L,
                                      1999L, 1999L, 1999L, 1999L, 1999L, 1999L, 1999L,
                                      1999L, 1999L, 1999L, 999999L, 999999L, 999999L,
                                      999999L, 999999L, 999999L, 999999L, 999999L,
                                      999999L, 999999L, 999999L, 999999L, 999999L,
                                      999999L)
)

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
           " per week. The median for all Australians is $853 per week.")
    
  })
  
  output$equivtext <- renderText({
    
    equiv <- equiv_fn(income = input$income,
                      adults = input$adults,
                      kids = input$kids)
    
    if(equiv >= abs.income$Income[abs.income$Percentile == 10]){
      paste0(
        "You're better off than at least ",
        abs.income[max(which(abs.income$Income <= equiv)),1], "% of Australians.")
    } else {
      "Your income is in the lowest 10% of Australians."
    }
    
  })
    

  
  output$Plot <- renderPlot({
    
    equiv <- as.integer(equiv_fn(income = input$income,
                                 adults = input$adults,
                                 kids = input$kids))
    
    user.range <- fullranges$Range[min(which(equiv < fullranges$Top.of.range))]
    
    fullranges %>%
      mutate(ID = paste0(Range, Person),
             Label = ifelse(Range == user.range & Person == 1,
                            "You and 99 999 other\nAustralians are in the blue square",
                            "Each square represents\nabout 100 000 Australians")) %>%
      ggplot(aes(x = reorder(Range, Top.of.range), group = ID, fill = Label)) +
         geom_bar(stat = "count", col = "white") +
      labs(title = "How does your income compare?",
           subtitle = "Distribution of equivalised disposable household income in Australia, 2015-16",
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
