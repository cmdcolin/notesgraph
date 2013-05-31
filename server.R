library(shiny)
library(Rgraphviz)
library(stringr)
library(RBGL)


# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The output renderers defined 
  # below then all used the value computed from this expression
  data <- reactive({  
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(data(), 
         main=paste('r', dist, '(', n, ')', sep=''))
  })
  
  # Generate a summary of the data
  output$summary <- renderTable({
    #input$users
    ret<-data.frame(users<-strsplit(input$users,'\n'))
    names(ret)<-c('Notes')
    ret
  })
  
  
  output$blah <- renderPrint({
    #input$users
    myfile<-strsplit(input$users,'\n')
    #myfile
    
    reblogSet<-str_match(myfile[[1]],"(.*) reblogged this from (.*)")
    reblogSet<-reblogSet[!is.na(reblogSet[,1]),]
    allBlogs<-unique(c(reblogSet[,2],reblogSet[,3]))
    allBlogs
  })
  # Generate a summary of the data
  output$myoutput <- renderPlot({
    myfile<-strsplit(input$users,'\n')
    reblogSet<-str_match(myfile[[1]],"(.*) reblogged this from (.*)")
    reblogSet<-reblogSet[!is.na(reblogSet[,1]),]
    allBlogs<-unique(c(reblogSet[,2],reblogSet[,3]))
    #allBlogs<-c(allBlogs,"reallyreallygone")
    rEG <- new("graphNEL", nodes=allBlogs, edgemode="directed")
    apply(reblogSet,1,function(row) {
      rEG<<-addEdge(row[3],row[2],rEG,1)
    })
    attrs <- list(node=list(shape="ellipse", fixedsize=FALSE,overlap=FALSE))
    mydistances<-sp.between(rEG,"reallyreallygone",nodes(rEG),detail=FALSE)
    
    mydistances<-as.vector(unlist(mydistances))
    mydistances[is.na(mydistances)]=0
    
    z=rEG
    nA=list()
    nNodes <- length(nodes(z))
    nA$fontsize <- rep(42, nNodes)
    nA$fontcolor <- rep("blue", nNodes)
    nA$fillcolor=(mydistances)+1
    nA <- lapply(nA, function(x) { names(x) <- nodes(z); x})
    plot(z, nodeAttrs=nA,attrs=attrs)
    
    
    #palette(rainbow(max(mydistances)+20))
    #x <- layoutGraph(rEG,nodeAttrs=nA, layoutType="twopi",attrs=attrs)
    
    #renderGraph(x,graph.pars=list(overlap=FALSE))
  })
  
  
  
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  tumblrData <- reactive({
    input$users
  })
  
  # Return the formula text for printing as a caption
  output$users <- renderText({
    tumblrData()
  })
  
  
})
