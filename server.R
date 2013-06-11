library(shiny)
library(Rgraphviz)
#library(RColorBrewer)
library(stringr)
library(RBGL)
library(animation)

paste(sample(c(0:9, letters, LETTERS),6, replace=TRUE),collapse="")


# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The output renderers defined 
  # below then all used the value computed from this expression
  
  
  output$myanimation<-renderUI({
    #outdir=tempdir()
    #loutdir=unlist(strsplit(outdir,'/'))
    wordspace<-c(0:9, letters, LETTERS)
    length<-6       
    randstr<-paste(sample(wordspace,length, replace=TRUE),collapse="")
    myoutdir<-paste0('tmp/',randstr)
    dir.create(paste0('www/',myoutdir))
    
    saveHTML({
      myfile<-strsplit(input$users,'\n')
      reblogSet<-str_match(myfile[[1]],"([^ ]*) reblogged this from ([^ ]*)")
      whoposted<-str_match(myfile[[1]],"([^ ]*) posted this")
      
      #remove non-reblog lines
      reblogSet<-reblogSet[!is.na(reblogSet[,1]),]
      whoposted<-whoposted[!is.na(whoposted[,1]),]
      
      nr<-nrow(reblogSet)
      for (i in 2:nrow(reblogSet)) {
        # grep 
        
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
                      cm.colors=cm.colors(cnol))
        palette(mypal)
        x <- layoutGraph(rEG,nodeAttrs=nA, layoutType=input$graphtype,attrs=attrs)
        
        renderGraph(x,graph.pars=list(overlap=FALSE))
        ani.pause()
      }
    }, img.name = "anim_plot", imgdir = "img", autobrowse = FALSE, 
             title = "Demo of animation", 
             description = c("This is a NoteGraph animation"),
             htmlfile='animation.html',outdir=paste0('www/',myoutdir))
    
    
    tags$div(tags$h3(myoutdir), tags$br(),tags$iframe(src=paste0(myoutdir,'/animation.html'), 
                id="aniframe", 
                style="width:100%;height:800px;"))
  })
  
  
  
})
