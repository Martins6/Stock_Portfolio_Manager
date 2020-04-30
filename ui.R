########################### / HEADER / #################################
header <- dashboardHeader(title = "Shiny Portfolio Management")

########################## / SIDEBAR / #################################
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about"),
    menuItem("App", tabName = "app")
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
    ##################### ******************** App Section #####################
    tabItem(tabName = "app",
            
            fluidRow(
              # Portfolio Input
              box(title = "Your Portfolio", width = 4, solidHeader = TRUE, status = 'primary',
                  textAreaInput('port_csv', 'Enter in the CSV format here',
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
              box(title = "Datatable of Portfolio", width = 6, solidHeader = TRUE,
                  DTOutput('dt.port')
              )
            ),
            
            fluidRow(
              # Portfolio's return distribution
              box(title = "Return Distribution", width = 4, solidHeader = TRUE,
                  renderPlotly('pie_chart')
              ),
              
              # Return Comparison
              box(title = "Historical Return Distribution", width = 6, solidHeader = TRUE,
                  renderPlotly('pie_chart')
              )
              
            )
            
    )
    # End of the tabitems
  )
  # End of the body
)

############################## Putting it all together in the UI #################################
ui <- dashboardPage(
  skin = 'blue',
  header, sidebar, body)
