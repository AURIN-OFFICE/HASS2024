################################## ---------------- Server: ----------------  ##################################

server = shinyServer(function(input, output, session) {
  path <<-gsub(rstudioapi::getActiveDocumentContext()$path,pattern = "Shiny/.+",replacement = "")

  ################################# ---------------- tabs ----------------  ##################################
  output$Tab_1 <- renderUI(
    if(!is.null(input$tabs)){
      if(input$tabs == "Tab_1"){
        tagList(HTML('Content 1!'))}})
  
  output$Tab_2 <- renderUI(
    if(!is.null(input$tabs)){
      if(input$tabs == "Tab_2"){
        tagList(HTML('Content 2!'))}})
  
  output$Tab_3 <- renderUI(
    if(!is.null(input$tabs)){
      if(input$tabs == "Tab_3"){
        tagList(HTML('Content 3!'))}})
  ################################## ----------------  Server ----------------  ##################################
})
