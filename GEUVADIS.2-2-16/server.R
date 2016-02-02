library(shiny)
library(shinyRGL)
library(rgl)

# This is the server file to usee - not server_col.R

#top3pc = read.csv("top3pc.csv", header=TRUE,row.names=1)
#top3pc = read.csv("tcga-lung.csv", header=TRUE,row.names=1)

# Read in a flat file that has the eigen vectors for the first three coordinates/components
top3pc <- read.csv("GV_2-2-16_top3.csv", header=TRUE, row.names=1)
# for "Population"
tsi <- which(grepl("TSI", rownames(top3pc)))
fin <- which(grepl("FIN", rownames(top3pc)))
ceu <- which(grepl("CEU", rownames(top3pc)))
yri <- which(grepl("YRI", rownames(top3pc)))
gbr <- which(grepl("GBR", rownames(top3pc)))
# for "Performer"
lumc     <- which(grepl("LUMC", rownames(top3pc)))
unige    <- which(grepl("UNIGE", rownames(top3pc)))
icmb     <- which(grepl("ICMB", rownames(top3pc)))
cnag_crg <- which(grepl("CNAG_CRG", rownames(top3pc)))
mpimg    <- which(grepl("MPIMG", rownames(top3pc)))
uu       <- which(grepl("UU", rownames(top3pc)))
hmgu     <- which(grepl("HMGU", rownames(top3pc)))



#luad = which(grepl("LUAD", rownames(top3pc)))
#lusc = which(grepl("LUSC", rownames(top3pc)))
#krluad = which(grepl("LC", rownames(top3pc)))
#cols = rep("blue", length(rownames(top3pc)))
## cols = vector(mode="character")

## tsix = mean(top3pc[tsi, 1])
## tsiy = mean(top3pc[tsi, 2])
## tsiz = mean(top3pc[tsi, 3])

## finx = mean(top3pc[fin, 1])
## finy = mean(top3pc[fin, 2])
## finz = mean(top3pc[fin, 3])
    
## ceux = mean(top3pc[ceu, 1])
## ceuy = mean(top3pc[ceu, 2])
## ceuz = mean(top3pc[ceu, 3])

## yrix = mean(top3pc[yri, 1])
## yriy = mean(top3pc[yri, 2])
## yriz = mean(top3pc[yri, 3])
    
## gbrx = mean(top3pc[gbr, 1])
## gbry = mean(top3pc[gbr, 2])
## gbrz = mean(top3pc[gbr, 3])

#luadx = mean(top3pc[luad,1])
#luady = mean(top3pc[luad,2])
#luadz = mean(top3pc[luad,3])

#luscx = mean(top3pc[lusc,1])
#luscy = mean(top3pc[lusc,2])
#luscz = mean(top3pc[lusc,3])





######################
# SUB( ): The inverse function to col2rgb()
# adapted from https://stat.ethz.ch/pipermail/r-help/2002-May/022037.html
######################
rgb2col <- function(rgb) {
  rgb <- as.integer(rgb)
  class(rgb) <- "hexmode"
  rgb <- as.character(rgb)
  rgb <- matrix(rgb, nrow=3)
  paste("#", apply(rgb, MARGIN=2, FUN=paste, collapse=""), sep="")
}
######################
######################


######################
# SUB( ): Convert all colors into format "#rrggbb"
# adapted from https://stat.ethz.ch/pipermail/r-help/2002-May/022037.html
######################
getColorTable <- function(col) {
  rgb <- col2rgb(col);
  col <- rgb2col(rgb);
  sort(unique(col))
}
######################
######################


######################
# SUB( ): Create optimal contrast color selection using a color wheel
# adapted from https://stat.ethz.ch/pipermail/r-help/2002-May/022037.html 
######################
col.wheel <- function(num_col, my_cex=0.75) {
  cols <- rainbow(num_col)
  col_names <- vector(mode="list", length=num_col)
  for (i in 1:num_col){
    col_names[i] <- getColorTable(cols[i])
  }
  cols
}
######################
######################

# colors for the 5 populations
pop_colors <- col.wheel(5)
tsicol = "red"   <- pop_colors[1]
fincol = "green" <- pop_colors[2]
ceucol = "blue"  <- pop_colors[3]
yricol = "gray"  <- pop_colors[4]
gbrcol = "brown" <- pop_colors[5]

# colors for the 7 performers
perf_colors <- col.wheel(7)
lumccol     <- perf_colors[1] 
unigecol    <- perf_colors[2]
icmbcol     <- perf_colors[3]
cnag_crgcol <- perf_colors[4]
mpimgcol    <- perf_colors[5]
uucol       <- perf_colors[6]
hmgucol     <- perf_colors[7]

#luadcol = "purple"
#lusccol = "gray"
#highcol = "chartreuse"


# NOT SURE WHAT THESE DISTANCES ARE FOR
#luad.dist <- function(x) sqrt(sum((x[1] - luadx) ^ 2, (x[2] - luady) ^ 2, (x[3] - luadz) ^ 2))
#lusc.dist <- function(x) sqrt(sum((x[1] - luscx) ^ 2, (x[2] - luscy) ^ 2, (x[3] - luscz) ^ 2))

#mis.col <- function (x,y) if (x < 0) { print(x) } else { print(x) }

#exp.col <- function (x) if (x < 0) { luadcol } else { lusccol }


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
   

        
        if(colorby == "Population"){
            cols <- rep("blue", length = nrow(top3pc))
            cols[tsi] <- tsicol
            cols[fin] <- fincol
            cols[ceu] <- ceucol 
            cols[yri] <- yricol
            cols[gbr] <- gbrcol
            output$distPlot <- renderPlot({
                legend("center", c("TSI", "FIN", "CEU", "YRI", "GBR"), fill=c(pop_colors), bty = "n", pt.cex = 1, cex=1.5)
            })
        }
        else if(colorby == "Performer (Lab)"){
            cols <- rep("blue", length = nrow(top3pc))
            cols[lumc]     <- lumccol
            cols[unige]    <- unigecol   
            cols[icmb]     <- icmbcol     
            cols[cnag_crg] <- cnag_crgcol 
            cols[mpimg]    <- mpimgcol    
            cols[uu]       <- uucol       
            cols[hmgu]     <- hmgucol
            output$distPlot <- renderPlot({
                legend("center", c("LUMC","UNIGE", "ICMB", "CNAG_CRG", "MPIMG", "UU", "HMGU"), fill=c(perf_colors), bty = "n", pt.cex = 1, cex=1.5)
            })
        }
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
    
        plot3d(pcx, pcy, pcz, col=cols, alpha=0.6, add=T,type="s",radius=.0001)
        box3d(labels=FALSE, tick=FALSE, box=FALSE)
    
    })

})


#})
