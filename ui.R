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
    #####################  About Section #####################
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
    ##################### Descriptive Section #####################
    tabItem(tabName = "stats",
            
            ################## ********************  Portfolio Overview #################
            # Intro
            fluidRow(
              box(h3('Portfolio Overview'),
                  p('For a weekly update or the start of a in-depth study, both for descriptive or modelling.'),
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
              ),
              
              # Date
              box(title = "", width = 3, solidHeader = TRUE, #height = '300px',
                  dateInput('date', 'Since when do you want to analyze your Portfolio?',
                            value = start_date),
                  awesomeCheckbox('quick_update', 'Want some quick update?',
                                  value = F),
                  uiOutput('quick_update_conf'),
                  actionButton('go_csv', 'Submit')
              ),
              
              # Display Portfolio on a Datatable
              box(title = "Portfolio Weights", width = 5, solidHeader = TRUE,
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
            
            ################## ********************  Classic Sharpe Ratio and CAPM #################
            # Intro
            fluidRow(
              box(h3('Classical Sharpe Ratio and CAPM'),
                  p('In this section, we wish to compare our Portfolio with the Market, in order to understand how well we are perfoming against the market.'),
                  p('For better understanding of the statistics, please checkout the About tab.'),
                  p('Keep in mind that in this section, the more data we have the better our analysis is.')
                  ),
              
              # Inputs
              box(title = '', width = 4, solidHeader = TRUE,
                  helpText('For more precise results, please use at least one year worth of data.'),
                  textInput('market_index',
                            'Which Market Index (or ETF) do you wish to compare your Portfolio?',
                            value = 'SPY'),
                  actionButton('go_sharpe_capm', 'Submit')
              )
            ),
            
            fluidRow(
              # Sharpe Ratio comparison
              box(title = 'Comparing Classical Sharpe Ratio (Return/Volatiltiy)', width = 10,
                  plotlyOutput('market_portfolio_sr')
                  ),
              box(title = '', width = 2,
                  selectInput('period_to_analyze_sr',
                              label = 'Since when do you wish to analyze?', 
                              choices = c('1 Week Ago', '2 Weeks Ago', '1 Month Ago', 'Whole Period'),
                              multiple = FALSE,
                              selected = '1 Week Ago'
                              )
                  )
            ),
            
            fluidRow(
              # CAPM 
              box(title = 'Capital Asset Pricing Model (CAPM)', width = 12,
                  plotlyOutput('market_portfolio_CAPM')
              )
            ),
            
            fluidRow(
              # CAPM Beta 
              valueBoxOutput('capm_beta', width = 6),
              # CAPM Alpha
              valueBoxOutput('capm_alpha', width = 6)
            )
            
     # End of the Descriptive Section       
    ),
    #################################### Modelling Section #####################
    tabItem(tabName = "model",
            
            ################## ********************  Percentual Return Distribution #################
            # Intro
            fluidRow(
              box(h3('Percentual Return Distribution'),
                  p('Understanding the probabilistic properties of the return in our Portfolio through some key statistics.'),
                  p('For better understanding of the statistics, please checkout the About tab.'),
                  p('Keep in mind that in this section, the more data we have the better our analysis is.')
              )
            ),
            
            fluidRow(
              # Distribution Plot
              box(title = 'Distribution Plot', width = 6,
                  plotOutput('dist_plot')
              ),
              # Distribution Key-Statistics
              box(title = 'Key Statistics', width = 6,
                  DTOutput('dist_key_stat')
              )
              
            ),
            
            
            ################## ******************** Risk Modelling #######################
            fluidRow(
              box(h3('Risk Modelling'),
                  p('In this section, we wish to create confidence intervals and forecast the VaR and ES of the fowarding day.'),
                  p('For better understanding of the statistics, please checkout the About tab.'),
                  p('Keep in mind that in this section, the more data we have the better our analysis is.')
              ),
            ),
            
            fluidRow(
              # GARCH Plot
              box(title = '', width = 10,
                    plotlyOutput('risk_plot')
                    ),
              # Choosing GARCH Model  
              box(title = '', width = 2,
                  uiOutput('stock_selection_risk'),
                  selectInput('garch_selection', 'Which GARCH Model do you wish to use?',
                                choices = c('N-GARCH', 't-GARCH'),
                                selected = 'N-GARCH', multiple = FALSE),
                  numericInput('p_risk',label = 'What quantile do you wish to consider? Or, how much risk? (%)',
                               value = 5, min = 0.1, max = 15),
                  actionButton('go_garch', 'Submit')
                    )
            ),
            
            fluidRow(
              # Diagnostics of the model
              box(title = 'Model Diagnostics', width = 4,
                  DTOutput('model_diag')
              )
              
            ),
            ################## ******************** Modified Sharpe Ratio #################
            # Intro
            fluidRow(
              box(h3('Modified Sharpe Ratio'),
                  p('In this section, we wish to compare our Portfolio Returns with the Market Returns, in order to understand how well we are perfoming against the market.'),
                  p('For better understanding of the statistics, please checkout the About tab.'),
                  p('Keep in mind that in this section, the more data we have the better our analysis is.')
              ),
              
              # Inputs
              box(title = '', width = 4, solidHeader = TRUE,
                  helpText('For more precise results, please use at least one year worth of data.'),
                  textInput('market_index_model',
                            'Which Market Index (or ETF) do you wish to compare your Portfolio?',
                            value = 'SPY'),
                  actionButton('go_sharpe_model', 'Submit')
              )
            ),
            
            fluidRow(
              # Sharpe Ratio comparison
              box(title = 'Comparing Modified Sharpe Ratio (Return/VaR)', width = 10,
                  plotlyOutput('market_portfolio_model_sr')
              ),
              box(title = '', width = 2,
                  helpText('Keep in mind that the VaR is calculated by the model above.'),
                  selectInput('period_to_analyze_model_sr',
                              label = 'Since when do you wish to analyze?', 
                              choices = c('1 Week Ago', '2 Weeks Ago', '1 Month Ago', 'Whole Period'),
                              multiple = FALSE,
                              selected = '1 Week Ago'
                  )
              )
            )  
            
      # End of the Modelling Section
    )
    # End of the tabitems
  )
  # End of the body
)

############################## Putting it all together in the UI #################################
ui <- dashboardPage(
  skin = 'blue',
  header, sidebar, body)
