library(shiny)
library(Rgraphviz)
library(stringr)
library(RBGL)


# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The output renderers defined 
  # below then all used the value computed from this expression
  graphType <- reactive({  
    input$n
  })
  graphType <- reactive({  
    switch(input$graphtype,circo = circo,dot = dot,neato = neato,twopi = twopi,
          twopi)
  })

 
  
  # Generate a summary of the data
  output$myoutput <- renderPlot({
    myfile<-strsplit(input$users,'\n')
    # grep 
    reblogSet<-str_match(myfile[[1]],"(.*) reblogged this from (.*)")
    whoposted<-str_match(myfile[[1]],"(.*) posted this")
    
    #remove non-reblog lines
    reblogSet<-reblogSet[!is.na(reblogSet[,1]),]
    whoposted<-whoposted[!is.na(whoposted[,1]),]
    
    
    allBlogs<-unique(c(reblogSet[,2],reblogSet[,3]))
    rEG <- new("graphNEL", nodes=allBlogs, edgemode="directed")
    apply(reblogSet,1,function(row) {
      rEG<<-addEdge(row[3],row[2],rEG,1)
    })
    attrs <- list(node=list(shape="ellipse", fixedsize=FALSE,overlap=FALSE))
    mydistances<-sp.between(rEG,whoposted[2],nodes(rEG),detail=FALSE)
    
    mydistances<-as.vector(unlist(mydistances))
    mydistances[is.na(mydistances)]=0
    
    z=rEG
    nA=list()
    nNodes <- length(nodes(z))
    nA$fontsize <- rep(42, nNodes)
    nA$fontcolor <- rep("blue", nNodes)
    nA$fillcolor=(mydistances)+1
    nA <- lapply(nA, function(x) { names(x) <- nodes(z); x})
    #plot(z, nodeAttrs=nA,attrs=attrs)
    
    
    palette(rainbow(max(mydistances)+5))
    x <- layoutGraph(rEG,nodeAttrs=nA, layoutType=input$graphtype,attrs=attrs)
    
    renderGraph(x,graph.pars=list(overlap=FALSE))
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
