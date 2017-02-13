library(ggplot2)
library(e1071)
library(rbokeh)
library(shinyjs)
library(shinythemes)
library(DT)

x=c(rnorm(1000,1000,100),rnorm(1000,2000,200),rnorm(1000,3000,400))
y=c(abs(rnorm(1000,50,25)),rnorm(1000,200,50),rnorm(1000,100,30))
clases=as.factor(c(rep(1,1000),rep(2,1000),rep(3,1000)))
datos=data.frame(x,y,clases)

server <-  function(input, output) {
  
  output$ui <- renderUI({
    if (is.null(input$kernel))
      return()
    
    switch(input$kernel,
           
           "linear" = tabPanel("Lineal", 
                               sliderInput('C', 'Training parameter C', value = 1, min = 1, max = 1000, step = 1)),
           
           "polynomial" = tabPanel("Polinominal",
                                   sliderInput('C', 'Training parameter C', value = 1, min = 1, max = 1000, step = 1),
                                   sliderInput('gamma', 'Training parameter gamma', value = 0.25, min = 0, max = 10, step = 0.05),
                                   sliderInput('coef0', 'Training parameter coef0', value = 0, min = 0, max = 10, step = 0.5),
                                   sliderInput('degree', 'Training parameter degree', value = 3, min = 0, max = 10, step = 0.5)),
           
           "radial" = tabPanel("Radial",
                               sliderInput('C', 'Training parameter C', value = 1, min = 1, max = 1000, step = 1),
                               sliderInput('gamma', 'Training parameter gamma', value = 0.25, min = 0, max = 10, step = 0.05)),
           
           "sigmoid" = tabPanel("Sigmoid",
                                sliderInput('C', 'Training parameter C', value = 1, min = 1, max = 1000, step = 1),
                                sliderInput('gamma', 'Training parameter gamma', value = 0.25, min = 0, max = 10, step = 0.05),
                                sliderInput('coef0', 'Training parameter coef0', value = 0, min = 0, max = 10, step = 0.5))
    )
  })
  
  output$newPlot <- renderRbokeh({
    set.seed(101)
    #Parametros SVM
    n <- input$numData
    kernel <- input$kernel
    C <- input$C
    gamma <- input$gamma
    cf <- input$coef0
    dg <- input$degree
    
    #Data
    datos.index <- sample(1:nrow(datos),size=n)
    datos.train <- datos[datos.index,]
    datos.test <- datos[-datos.index,]
    
    #SVM
    switch(input$kernel,
           "linear" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                      kernel=kernel, cost = C),
           
           "polynomial" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                          kernel=kernel, cost = C, gamma = gamma, coef0 = cf, degree = dg),
           
           "radial" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                      kernel=kernel, cost = C, gamma = gamma),
           
           "sigmoid" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                       kernel=kernel, cost = C, gamma = gamma, coef0 = cf)         
    )
    
    #grafica
    p <- figure(width = 1000, height = 450) %>%
      ly_points(x, y, data = datos.train,
                color = datos.train$clases, glyph = c(21,25)[1:n %in% datos.svm$index + 1],
                hover = list(x, y))
    p
  })
  
  output$plot1 <- renderPlot({
    set.seed(101)
    #Parametros SVM
    n <- input$numData
    kernel <- input$kernel
    C <- input$C
    gamma <- input$gamma
    cf <- input$coef0
    dg <- input$degree
    
    #Data
    datos.index <- sample(1:nrow(datos),size=n)
    datos.train <- datos[datos.index,]
    datos.test <- datos[-datos.index,]
    
    #SVM
    switch(input$kernel,
           "linear" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                      kernel=kernel, cost = C),
           
           "polynomial" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                          kernel=kernel, cost = C, gamma = gamma, coef0 = cf, degree = dg),
           
           "radial" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                      kernel=kernel, cost = C, gamma = gamma),
           
           "sigmoid" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                       kernel=kernel, cost = C, gamma = gamma, coef0 = cf)         
    )
    
    plot(datos.svm, datos.train, y ~ x, 
         slice = list(x = 1, y = 2))
  })
  
  output$pred <- renderPrint({
    set.seed(101)
    #Parametros SVM
    n <- input$numData
    kernel <- input$kernel
    C <- input$C
    gamma <- input$gamma
    cf <- input$coef0
    dg <- input$degree
    
    #Data
    datos.index <- sample(1:nrow(datos),size=n)
    datos.train <- datos[datos.index,]
    datos.test <- datos[-datos.index,]
    
    #SVM
    switch(input$kernel,
           "linear" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                      kernel=kernel, cost = C),
           
           "polynomial" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                          kernel=kernel, cost = C, gamma = gamma, coef0 = cf, degree = dg),
           
           "radial" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                      kernel=kernel, cost = C, gamma = gamma),
           
           "sigmoid" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                       kernel=kernel, cost = C, gamma = gamma, coef0 = cf)         
    )
    
    #Prediccion de los restantes
    asignado <- predict(datos.svm,new=datos.test)
    
    #Tabla de confusion
    mc <- with(datos.test,(pred=table(asignado,datos.test$clases)))
    print(mc)
    
  })
  output$state <- renderText({
    set.seed(101)
    #Parametros SVM
    n <- input$numData
    kernel <- input$kernel
    C <- input$C
    gamma <- input$gamma
    cf <- input$coef0
    dg <- input$degree
    
    #Data
    datos.index <- sample(1:nrow(datos),size=n)
    datos.train <- datos[datos.index,]
    datos.test <- datos[-datos.index,]
    
    #SVM
    switch(input$kernel,
           "linear" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                      kernel=kernel, cost = C),
           
           "polynomial" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                          kernel=kernel, cost = C, gamma = gamma, coef0 = cf, degree = dg),
           
           "radial" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                      kernel=kernel, cost = C, gamma = gamma),
           
           "sigmoid" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                       kernel=kernel, cost = C, gamma = gamma, coef0 = cf)         
    )
    
    
    #Prediccion de los restantes
    asignado <- predict(datos.svm,new=datos.test)
    
    #Tabla de confusion
    mc <- with(datos.test,(pred=table(asignado,datos.test$clases)))
    
    #porcentaje correctamente clasificados
    if(is.nan(correctos <- sum(diag(mc)) / nrow(datos.test) *100)){
      correctos <- 0
    }else {
      correctos <- sum(diag(mc)) / nrow(datos.test) *100
    }
  })
  
  output$nsv <- renderText({
    set.seed(101)
    #Parametros SVM
    n <- input$numData
    kernel <- input$kernel
    C <- input$C
    gamma <- input$gamma
    cf <- input$coef0
    dg <- input$degree
    
    #Data
    datos.index <- sample(1:nrow(datos),size=n)
    datos.train <- datos[datos.index,]
    datos.test <- datos[-datos.index,]
    
    #SVM
    switch(input$kernel,
           "linear" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                      kernel=kernel, cost = C),
           
           "polynomial" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                          kernel=kernel, cost = C, gamma = gamma, coef0 = cf, degree = dg),
           
           "radial" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                      kernel=kernel, cost = C, gamma = gamma),
           
           "sigmoid" = datos.svm <- svm(clases~y+x, data=datos.train, 
                                       kernel=kernel, cost = C, gamma = gamma, coef0 = cf)         
    )
    
    #total de vectores soporte
    datos.svm$tot.nSV
  })
  output$table <- DT::renderDataTable({
    n <- input$numData
    kernel <- input$kernel
    C <- input$C
    gamma <- input$gamma
    cf <- input$coef0
    dg <- input$degree
    
    #Data
    datos.index <- sample(1:nrow(datos),size=n)
    datos.train <- datos[datos.index,]
    datos.test <- datos[-datos.index,]
    
    DT::datatable(datos.train, options=list(orderClasses = TRUE))
  })
}

ui <- navbarPage(theme = shinytheme("cerulean"),"SVM shiny app",
                 tabPanel("SVM datos",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput('numData', 'Submuestra', value = 50, min = 10, max = nrow(datos), step = 1),
                              selectInput("kernel", "kernel",
                                          c(Lineal = "linear",
                                            Polinominal = "polynomial",
                                            Radial = "radial",
                                            Sigmoid = "sigmoid")),
                              uiOutput("ui"),
                              h4("Matriz de confusion: ", align = "rigth"),
                              verbatimTextOutput('pred'),
                              h4("Porcentaje correctamente clasificados:", align = "rigth"),
                              htmlOutput('state'),
                              h4("Total de vectores soporte:", align = "rigth"),
                              htmlOutput('nsv')
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Grafica 1",
                                         rbokehOutput('newPlot')
                                ),
                                tabPanel("Grafica 2",
                                         plotOutput("plot1")
                                ),
                                tabPanel("Tabla",
                                        DT::dataTableOutput("table")
                                )
                              )
                            )
                          )
                 ),
                 tabPanel("Acerca",
                          h4("Aplicacion de SVM v 3.5 "),
                          p("Esta es una sencilla shiny app que implementa SVM sobre un conjunto de 3000 datos creados por R
                  y clasificados en 3 clasess. 
                  Del total de datos, especifique una submuestra, seleccione el tipo de kernel y en base a 
                  eso configure los parametros necesarios.
                  Se muestra la tabla de confusion, el porcentaje correctamente clasificado segun la prediccion, 
                  el total de vectores soporte y la grafica."),
                          p("Basada en ejemplos y la documentacion de RStudio Shiny"), 
                          a(href="https://www.twitter.com/axelmora93", "Twitter"),
                          a(href="https://www.linkedin.com/in/axel-sunem-mora-olvera-338aa6108/", "LinkedIn")
                 )
)

shinyApp(ui = ui, server = server)