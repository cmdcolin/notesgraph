library(shiny)
library(Rgraphviz)
#library(RColorBrewer)
library(stringr)
library(RBGL)
library(animation)


# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The output renderers defined 
  # below then all used the value computed from this expression
 
  
  # Generate a summary of the data
  output$myoutput <- renderPlot({
    myfile<-strsplit(input$users,'\n')
    # grep 
    reblogSet<-str_match(myfile[[1]],"([^ ]*) reblogged this from ([^ ]*)")
    whoposted<-str_match(myfile[[1]],"([^ ]*) posted this")
    
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
    
    nA=list()
    nNodes <- length(nodes(rEG))
    nA$fontsize <- rep(12, nNodes)
    nA$fillcolor=(mydistances)+1
    nA <- lapply(nA, function(x) { names(x) <- nodes(rEG); x})
    #plot(z, nodeAttrs=nA,attrs=attrs)
    
    ncol=max(mydistances)+3
    mypal<-switch(input$palettetype,rainbow=rainbow(ncol),
                  topo.colors=topo.colors(ncol),
                  terrain.colors=terrain.colors(ncol),
                  heat.colors=heat.colors(ncol),
                  cm.colors=cm.colors(ncol))
    palette(mypal)
    x <- layoutGraph(rEG,nodeAttrs=nA, layoutType=input$graphtype,attrs=attrs)
    
    renderGraph(x,graph.pars=list(overlap=FALSE))
  })
  
  
  
  
})
