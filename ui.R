########################### / HEADER / #################################
header <- dashboardHeader(title = "Portfolio Management")

########################## / SIDEBAR / #################################
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about"),
    menuItem("Descriptive", tabName = "stats"),
    menuItem("Modelling", tabName = "model")
  )
)

################################### / BODY / ##########################
body <- dashboardBody(
  tabItems(
    ##################### ******************** About Section #####################
    tabItem(tabName = "about",
            
            fluidRow(
              box(h1('About Section'))
            ),
            
            fluidRow(
              box(width = 12,
                  
                  p('In this section I will try to explain some of the key statistics of the app.'),
                  p('"Shiny Portfolio Management" was created to help with the more quantitative understanding of stocks'),
                  p('and also apply the most used and modern techniques to stock portfolio. Hope you enjoy!'),
                  p('The app was created by Adriel Martins.')
                  
                  )
            ),
    ),
    ##################### ******************** Descriptive Section #####################
    tabItem(tabName = "stats",
            
            ################## Portfolio Overview #################
            # Intro
            fluidRow(
              box(h3('Portfolio Overview'),
                  p('For a weekly update or the start of a in-depth study.')
                  )
            ),
            
            fluidRow(
              # Portfolio Input
              box(title = "Your Portfolio", width = 4, height = 300,
                  solidHeader = TRUE, status = 'primary',
                  textAreaInput('port_csv',
                                height = '150px',
                                'Enter in the CSV format here',
                                value = 'Stock,Weight,\nAAPL,0.25,\nAMZN,0.25,\nTSLA,0.25,\nSQ,0.25,'),
                  #helpText('Or, upload your own CSV below:'),
                  #fileInput('port_csv_upload'),
                  actionButton('go_csv', 'Submit')
              ),
              
              # Date
              box(title = "", width = 2, solidHeader = TRUE,
                  dateInput('date', 'Since when do you want to analyze your Portfolio?',
                            value = start_date)
              ),
              
              # Display Portfolio on a Datatable
              box(title = "Portfolio Weights", width = 6, solidHeader = TRUE,
                  plotlyOutput('port_weights')
              )
            ),
            
            fluidRow(
              # Portfolio's Cumulative Return by Stock
              box(title = "Stock's Growth", width = 6, solidHeader = TRUE,
                  plotlyOutput('last_cr')
              ),
              
              # Prices and Historical Cumulative Return
              tabBox(
                width = 6,
                title = "Historical Prices and Cumulative Returns",
                id = "tabset_return",
                tabPanel("Prices", plotlyOutput('price_plot')),
                tabPanel("Cumulative Returns", plotlyOutput('CR_plot'))
              )
              
            ),
            
            ################## Modified Sharpe Ratio and CAPM #################
            # Intro
            fluidRow(
              box(h3('Modified Sharpe Ratio and CAPM'),
                  p('For better understanding of the statistics, please checkout the About tab.'),
                  p('Keep in mind that in this section, the more data we have the better our analysis is.')
                  )
            ),
            
            fluidRow(
              # Sharpe Ratio comparison
              tabBox(
                width = 10,
                title = "Comparing Modified Sharpe Ratios",
                id = "sharpe_ratio",
                tabPanel("Stocks versus Portfolio", plotlyOutput('stock_portfolio_sr')),
                tabPanel("Market versus Portfolio", plotlyOutput('market_portfolio_sr'))
              ),
              # Inputs
              box(title = '', width = 2, solidHeader = TRUE,
                  helpText('For more precise results, please use at least one year worth of data.'),
                  textInput('market_index',
                            'Which Market Index (or ETF) do you wish to compare your Portfolio?',
                            value = 'SPY'),
                  # numericInput('p_ES_sh', label = 'What is the probability (%) of a loss in order to measure risk?',
                  #              value = 5, min = 1, max = 10),
                  actionButton('go_sharpe', 'Submit')
                  )

            )
     # End of the Descriptive Section       
    )
    # End of the tabitems
  )
  # End of the body
)

############################## Putting it all together in the UI #################################
ui <- dashboardPage(
  skin = 'blue',
  header, sidebar, body)
