library(shiny)
library(Rgraphviz)
library(stringr)
library(RBGL)
library(animation)



plotSubset<-function(reblogSet, whoposted,i,input) {
  # grep 
  nr<-nrow(reblogSet)
  allBlogs<-unique(c(reblogSet[(nr-i):nr,2],reblogSet[(nr-i):nr,3]))
  rEG <- new("graphNEL", nodes=allBlogs, edgemode="directed")
  apply(reblogSet[(nr-i):nr,],1,function(row) {
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
}



compileGraph<-function(input) {
  myfile<-strsplit(input$users,'\n')
  reblogSet<-str_match(myfile[[1]],"([^ ]*) reblogged this from ([^ ]*)")
  whoposted<-str_match(myfile[[1]],"([^ ]*) posted this")
  
  #remove non-reblog lines
  reblogSet<-reblogSet[!is.na(reblogSet[,1]),]
  whoposted<-whoposted[!is.na(whoposted[,1]),]
  
  nr<-nrow(reblogSet)
  for (i in 2:nrow(reblogSet)) {
    plotSubset(reblogSet, whoposted, i,input)
  }
}
# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The output renderers defined 
  # below then all used the value computed from this expression
  
  
  output$myanimation<-renderUI({
    
    # create tmp directory without using tempdir()
    wordspace<-c(0:9, letters, LETTERS)
    randstr<-paste(sample(wordspace,size=6, replace=TRUE),collapse="")
    myoutdir<-file.path('tmp',randstr)
    dir.create(file.path('www',myoutdir))
    
    if(input$animationtype=='html') {
      saveHTML({
        compileGraph(input)
      }, img.name = "anim_plot", imgdir = "img", autobrowse = FALSE, 
               title = "Demo of animation", 
               description = c("This is a TumblrGraph animation"),
               htmlfile='animation.html',
               outdir=file.path('www',myoutdir))
      
      
      tags$div(tags$h3(myoutdir), tags$br(),
               tags$iframe(src=file.path(myoutdir,'animation.html'), 
                           style="width:100%;height:800px;"))
    } else {
      saveGIF({
        compileGraph(input)
      }, movie.name = "animation.gif", autobrowse = FALSE, 
              title = "Demo of animation", clean=FALSE,
              description = c("This is a TumblrGraph animation"),
              outdir=file.path('www',myoutdir))
      
      tags$div(tags$h3(myoutdir), 
               tags$br(),tags$img(src=file.path(myoutdir,'animation.gif')))
    }

    
  })
  
  
  
})

