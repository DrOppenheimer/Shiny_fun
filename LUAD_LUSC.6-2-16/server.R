library(shiny)
library(shinyRGL)
library(rgl)

# This is the server file to usee - not server_col.R

#top3pc = read.csv("top3pc.csv", header=TRUE,row.names=1)
#top3pc = read.csv("tcga-lung.csv", header=TRUE,row.names=1)

# Read in a flat file that has the eigen vectors for the first three coordinates/components
#top3pc <- read.csv("GV_4-11-16_top3.with_outlier.csv", header=TRUE, row.names=1) # sample that has an obvious outlier (also visible in the boxplot)
top3pc <- read.csv("LUSC_LUAD.top3pc.csv", header=TRUE, row.names=1)



# for "Cancer_Type"
lusc <- which(grepl("LUSC", rownames(top3pc)))
luad <- which(grepl("LUAD", rownames(top3pc)))

p_cols <- vector() # colors for points

# colors for legend
luscCol <- "red" 
luadCol <- "blue"

shinyServer(function(input, output) {

    #output$distPlot <- renderPlot({
      #legend("center", c("2D Points", "3D Points"), pch = c(1, 16))
        #legend("center", c("Testing"), bty = "n", pt.cex = 1, cex=2)
        #->#legend("center", c("GEUVADIS Subset"), fill=c(luadcol, lusccol), bty = "n", pt.cex = 1, cex=2)
        #legend("center", c("Lung adenocarcinoma (LUAD)","Lung squamous cell carcinoma (LUSC)"), fill=c(luadcol, lusccol), bty = "n", pt.cex = 1, cex=2)
        #  legend("center", c("Lung adenocarcinoma (LUAD)","Lung squamous cell carcinoma (LUSC)"), fill=c(luadcol, lusccol), bty = "n")
    #})

    output$myWebGL <- renderWebGL({

        colorby = input$colorby

    #luadd = apply(top3pc, 1, luad.dist)
    #luscd = apply(top3pc, 1, lusc.dist)

    #distdiff = luadd - luscd
        
        if(colorby == "Cancer_Type"){
            p_cols[lusc] <- luscCol
            p_cols[luad] <- luadCol
            output$distPlot <- renderPlot({
                legend("center", c("LUSC", "LUAD"), fill=c(luscCol,luadCol), bty = "n", pt.cex = 1, cex=1.5)
            })
        }

        
        ## else if(colorby == "Performer (Lab)"){
        ##     cols <- rep("blue", length = nrow(top3pc))
        ##     cols[lumc]     <- lumccol
        ##     cols[unige]    <- unigecol   
        ##     cols[icmb]     <- icmbcol     
        ##     cols[cnag_crg] <- cnag_crgcol 
        ##     cols[mpimg]    <- mpimgcol    
        ##     cols[uu]       <- uucol       
        ##     cols[hmgu]     <- hmgucol
        ##     cols[unk]      <- unkcolperf
        ##     output$distPlot <- renderPlot({
        ##         legend("center", c("LUMC","UNIGE", "ICMB", "CNAG_CRG", "MPIMG", "UU", "HMGU", "Unknown"), fill=c(perf_colors[1:7],'gray'), bty = "n", pt.cex = 1, cex=1.5)
        ##     })
        ## }

    ## else if (colorby == "misluad") {
    ##   cols[luad] = luadcol
    ##   cols[lusc] = lusccol
    ##   for (i in 1:length(distdiff)) {
    ##     if(distdiff[i] > 0 & grepl("LUAD", names(distdiff)[i])){

    ##       #print(names(distdiff)[i])
    ##       #lusc
    ##       cols[i] = highcol
    ##     }
    ##   }
    ## }
    ## else if (colorby == "mislusc") {
    ##   cols[luad] = luadcol
    ##   cols[lusc] = lusccol
    ##   for (i in 1:length(distdiff)) {
    ##     if (distdiff[i] < 0 & grepl("LUSC", names(distdiff)[i])){
    ##       #luad
    ##       #print(names(distdiff)[i])
    ##       cols[i] = highcol
    ##     }
    ##   }
    ## }
    ## else if (colorby == "singlelusc") {
    ##   cols[luad] = luadcol
    ##   cols[lusc] = lusccol
    ##   count = 0
    ##   for (i in 1:length(distdiff)) {
    ##     if (distdiff[i] < 0 & grepl("LUSC", names(distdiff)[i])){
    ##       #luad
    ##       print(count)
    ##       if (count == 10) {
    ##         cols[i] = highcol
    ##       } 
    ##       count = count+1
          
    ##     }
    ##   }
    ## }
    ## else if (colorby == "exp"){
    ##   cols = sapply(distdiff, exp.col)
    ##}
        
        pcx = top3pc[,1]
        pcy = top3pc[,2]
        pcz = top3pc[,3]
    
        plot3d(pcx, pcy, pcz, col=p_cols, alpha=0.6, add=T,type="s",radius=.00005)
        box3d(labels=FALSE, tick=FALSE, box=FALSE)
    
    })

})


#})
