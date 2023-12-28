library(plotly)
library(Cairo)
options(shiny.usecairo=T)


if(FALSE){
  install.packages("matlib")
}



if(FALSE){
  #To run this app
  library(shiny)
  runApp(".")
}


server <- function(input, output, session) {


  ##############################################################################
  ########### General functions ################################################
  ##############################################################################
  
  
  getDataTable <- reactive({    
    current_ds <- input$input_ds
    available_datasets[[current_ds]]
  })
  
  getTrainingIndex <- reactive({    
    thedat <- getDataTable()
    set.seed(input$random_seed)
    usepoint <- sample(1:nrow(thedat), input$num_training_point)
    usepoint
  })
  
  getTestIndex <- reactive({
    setdiff(1:nrow(getDataTable()), getTrainingIndex())
  })
  
  
  getTrainingPoints <- reactive({
    getDataTable()[getTrainingIndex(),,drop=FALSE]
  })
  
  getTestPoints <- reactive({
    getDataTable()[getTestIndex(),,drop=FALSE]
  })
  

  
  
  ##############################################################################
  ########### Callbacks - dataset ##############################################
  ##############################################################################
  
  observeEvent(c(input$input_ds),{
    thedat <- getDataTable()
    numparam <- ncol(thedat)-1
    
    ######### Side bar
    updateSliderInput(session, "num_training_point", min=0, max=nrow(thedat), value = nrow(thedat), step = 1)
    updateSelectizeInput(session, 'input_predict', choices = colnames(thedat), server = TRUE, selected="Outcome")
    

  })
  
  
  getNetworkWidths <- reactive({
    allw <- c()
    for(i in 1:input$num_layers){
      allw <- c(allw, input[[paste0("width_",i)]])
    }
    allw
  })
  
  
  
  ###### native R lib. lacks convergence plots etc
  solveNetNNlib <- function(){
    
    library(neuralnet)
    
    thedat <- getDataTable()
    all_param_name <- colnames(thedat)[colnames(thedat) != input$input_predict]
    
    allw <- getNetworkWidths()
    
    model <- neuralnet(
      as.formula(paste(input$input_predict,"~",do.call("paste",c(as.list(all_param_name), sep="+")))),
      data=thedat,
      hidden=allw,
      stepmax=100,
      linear.output = FALSE
    )
    
  }
  
  predictNeuralnet <- function(){
    
    pred <- predict(model, test_data)

  }
  
  
  
  ########## Not used; seems likely to cause problems
  solveNetKeras <- function(){
    
    library(keras)
    library(tensorflow)
    
    set.seed(69)
    
    c(c(x_train, y_train), c(x_test, y_test)) %<-% dataset_cifar10()
    
    if(FALSE){
      x_train <- x_train / 255
      x_test <-  x_test / 255
    }
    
    model <- keras_model_sequential() %>%
      layer_dense(256, activation = 'leaky_relu') %>%
      # Outputs from dense layer
      layer_dense(1, activation = 'softmax')
    summary(model)
  }
  
  
  
  ##############################################################################
  ########### Callbacks - dataset & outcome ####################################
  ##############################################################################
  
  observeEvent(c(input$input_ds, input$input_predict, input$num_layers),{
    thedat <- getDataTable()
    numparam <- ncol(thedat)-1
    all_param_name <- colnames(thedat)[colnames(thedat) != input$input_predict]
    
    ######### all layer width sliders. these must be made reactive
    #https://mastering-shiny.org/action-dynamic.html#multiple-controls
    all_param_input <- list()
    names_param_width <- paste0("width_", 1:input$num_layers)
    for(i in 1:input$num_layers){
      all_param_input[[names_param_width[i]]] <- sliderInput(
        names_param_width[i], label = paste0("Width ",i,":"),
        min = 1, value = 3, max = 10, ticks = FALSE
      )
      
    }
    output$widths <- renderUI(all_param_input)

    ######### Scatter plot
    updateSelectizeInput(session, 'input_scatter_x', choices = colnames(thedat), server = TRUE)
    updateSelectizeInput(session, 'input_scatter_y', choices = colnames(thedat), server = TRUE)
    
    ######### Posterior 2D
#    updateSelectizeInput(session, 'input_posterior_x', choices = all_param_name, server = TRUE)
 #   updateSelectizeInput(session, 'input_posterior_y', choices = all_param_name, server = TRUE)
    
  })
  
  
  ##############################################################################
  ########### Data table tab ###################################################
  ##############################################################################
  
  output$plotDataTable <- renderTable(getTrainingPoints())
  
  
  ##############################################################################
  ########### Scatter plot tab #################################################
  ##############################################################################

  output$plotScatter <- renderPlot({
    
    
    thedat <- getDataTable()
    thedat <- getTrainingPoints(thedat)
    
    if(input$input_scatter_x %in% colnames(thedat) & 
       input$input_scatter_y %in% colnames(thedat) &
       input$input_scatter_c %in% colnames(thedat)){
      
      ds <- data.frame(
        x=thedat[,input$input_scatter_x],
        y=thedat[,input$input_scatter_y],
        c=thedat[,input$input_scatter_c]
      )
      
      ggplotly(ggplot(ds,aes(x,y, color=c))+geom_point()) 
      
    } else {
      ds <- data.frame(x=c(),y=c())
      ggplotly(ggplot()) 
    }
    

  })
  
  
  
  ##############################################################################
  ########### Network layout tab ###############################################
  ##############################################################################
  
  output$plotNetwork <- renderPlot({
    
    
    thedat <- getDataTable()
    all_param_name <- colnames(thedat)[colnames(thedat) != input$input_predict]
    
    ### Add first layer, input
    input_layer <- data.frame(
      name=all_param_name,
      x=1,
      y=NA,
      type="input"
    )
    input_layer$y <- 1:nrow(input_layer)
    all_layer <- input_layer
    
    ### Add all hidden layers
    for(i in 1:input$num_layers){
      hidden_layer <- data.frame(
        name="",
        x=1+i,
        y=1:input[[paste0("width_",i)]],
        type="hidden"
      )
      hidden_layer$name <- paste0("h",i,":",hidden_layer$y)
      all_layer <- rbind(all_layer, hidden_layer)
    }

    ### Add last layer, output
    output_layer <- data.frame(
      name=input$input_predict,
      x=2+input$num_layers,
      y=1,
      type="output"
    )
    all_layer <- rbind(all_layer, output_layer)
    
    
    ### Construct all lines
    all_lines <- NULL
    ind <- 1
    for(to in 1:nrow(all_layer)){
      for(from in 1:to){
        ## Need only consider up to here
        if(all_layer$x[from] == all_layer$x[to]-1){
          these_lines <- data.frame(
            x = all_layer$x[c(from,to)],
            y = all_layer$y[c(from,to)]
          )
          these_lines$ind <- ind
          print(these_lines)
          all_lines <- rbind(all_lines,these_lines)
          ind <- ind + 1
        }
      }
    }

    
    
    
    ggplot(all_lines, aes(x,y,group=ind)) + 
      geom_line(color="black") +
      geom_label(data=all_layer, mapping=aes(x,y, label=name, fill=type)) + 
      xlab("Depth")+ylab("Width")+
      theme_minimal()
        
  })
  

}



