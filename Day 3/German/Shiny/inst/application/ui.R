

ui <- dashboardPage(title="HASS2024",skin = c("blue"),
                    ### -------- Header -------- ###
                    dashboardHeader(dropdownMenu(type = "notifications",
                                                         notificationItem(href = '',
                                                                          text = "Contact",
                                                                          icon("users")
                                                         ),
                                                         notificationItem(
                                                           text = "12 items delivered",
                                                           icon("truck"),
                                                           status = "success"
                                                         ),
                                                         notificationItem(
                                                           text = "Server load at 86%",
                                                           icon = icon("exclamation-triangle"),
                                                           status = "warning"
                                                         )
                    ),title = tags$img(src = "Logo.png",height = 50, width = 130,align = "center"),titleWidth = 230) ,
                    dashboardSidebar(
                      ### -------- Menu principal -------- ###
                      sidebarMenu(menuItem("tab 1", tabName = "Tab_1",icon = icon("archive")),id = "tabs"),
                      sidebarMenu(menuItem("tab 2", tabName = "Tab_2",icon = icon("archive")),id = "tabs"),
                      sidebarMenu(menuItem("tab 3", tabName = "Tab_3",icon = icon("archive")),id = "tabs")),
                    
                    dashboardBody(
                      useShinyjs(),
                      useShinyalert(),  # Set up shinyalert

                      tabItems(
                        tabItem(tabName = "Tab_1",uiOutput("Tab_1",align="center")),
                        tabItem(tabName = "Tab_2",uiOutput("Tab_2",align="center")),
                        tabItem(tabName = "Tab_3",uiOutput("Tab_3",align="center"))
                        
                      )))




