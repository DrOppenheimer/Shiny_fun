library(shiny)
library(shinyRGL)
library(rgl)


#top3pc = read.csv("top3pc.csv", header=TRUE,row.names=1)
top3pc = read.csv("tcga-lung.csv", header=TRUE,row.names=1)
luad = which(grepl("LUAD", rownames(top3pc)))
lusc = which(grepl("LUSC", rownames(top3pc)))
krluad = which(grepl("LC", rownames(top3pc)))
cols = rep("blue", length(rownames(top3pc)))

luadx = mean(top3pc[luad,1])
luady = mean(top3pc[luad,2])
luadz = mean(top3pc[luad,3])

luscx = mean(top3pc[lusc,1])
luscy = mean(top3pc[lusc,2])
luscz = mean(top3pc[lusc,3])

luadcol = "purple"
lusccol = "gray"
highcol = "chartreuse"

luad.dist <- function(x) sqrt(sum((x[1] - luadx) ^ 2, (x[2] - luady) ^ 2, (x[3] - luadz) ^ 2))
lusc.dist <- function(x) sqrt(sum((x[1] - luscx) ^ 2, (x[2] - luscy) ^ 2, (x[3] - luscz) ^ 2))

mis.col <- function (x,y) if (x < 0) { print(x) } else { print(x) }

exp.col <- function (x) if (x < 0) { luadcol } else { lusccol }



shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
    #legend("center", c("2D Points", "3D Points"), pch = c(1, 16))
  legend("center", c("Lung adenocarcinoma (LUAD)","Lung squamous cell carcinoma (LUSC)"), fill=c(luadcol, lusccol), bty = "n", pt.cex = 1, cex=2)
#  legend("center", c("Lung adenocarcinoma (LUAD)","Lung squamous cell carcinoma (LUSC)"), fill=c(luadcol, lusccol), bty = "n")

  })

  output$myWebGL <- renderWebGL({

    colorby = input$colorby

    luadd = apply(top3pc, 1, luad.dist)
    luscd = apply(top3pc, 1, lusc.dist)

    distdiff = luadd - luscd
   


    if(colorby == "orig"){
      cols[luad] = luadcol
      cols[lusc] = lusccol
    }
    else if (colorby == "misluad") {
      cols[luad] = luadcol
      cols[lusc] = lusccol
      for (i in 1:length(distdiff)) {
        if(distdiff[i] > 0 & grepl("LUAD", names(distdiff)[i])){

          #print(names(distdiff)[i])
          #lusc
          cols[i] = highcol
        }
      }
    }
    else if (colorby == "mislusc") {
      cols[luad] = luadcol
      cols[lusc] = lusccol
      for (i in 1:length(distdiff)) {
        if (distdiff[i] < 0 & grepl("LUSC", names(distdiff)[i])){
          #luad
          #print(names(distdiff)[i])
          cols[i] = highcol
        }
      }
    }
    else if (colorby == "singlelusc") {
      cols[luad] = luadcol
      cols[lusc] = lusccol
      count = 0
      for (i in 1:length(distdiff)) {
        if (distdiff[i] < 0 & grepl("LUSC", names(distdiff)[i])){
          #luad
          print(count)
          if (count == 10) {
            cols[i] = highcol
          } 
          count = count+1
          
        }
      }
    }
    else if (colorby == "exp"){
      cols = sapply(distdiff, exp.col)
    }

    pcx = top3pc[,1]
    pcy = top3pc[,2]
    pcz = top3pc[,3]

    plot3d(pcx, pcy, pcz, col=cols, alpha=0.6, add=T,type="s",radius=5)
    box3d(labels=FALSE, tick=FALSE, box=FALSE)
 
  }) 


})