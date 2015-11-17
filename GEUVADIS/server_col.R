library(shiny)
#library(preprocessCore)
data_trans <- data.matrix(read.table("combined_FPKM.txt.quantile.PREPROCESSED.txt", row.names=1, header=TRUE, sep="\t", comment.char="", quote="", check.names=FALSE))
#data = read.table("RPKM_TCGA_LUNG.txt", header=1)
#data_frame = data.frame(data)
#data_log = log2(data_frame+0.0000000001)
#data_trans = t(data_log)
temp = lapply(data.frame(data_trans), var)
w = which(temp==0)
data_trans = data_log[-w, ]
pc = prcomp(t(data_trans), scale=T)
x = pc$x
matrix = x[,1:3]
matrix = data.frame(matrix)


TSI = which(grepl("TSI", rownames(matrix)))
FIN = which(grepl("FIN", rownames(matrix)))
CEU = which(grepl("CEU", rownames(matrix)))
YRI = which(grepl("YRI", rownames(matrix)))
GBR = which(grepl("GBR", rownames(matrix)))
#luad = which(grepl("LUAD", rownames(matrix)))
#lusc = which(grepl("LUSC", rownames(matrix)))
#korea = which(grepl("LC", rownames(matrix)))
cols = rep("blue", length(rownames(matrix)))


cols[tsi] = "red"
cols[fin] = "green"
cols[ceu] = "blue"
cols[yri] = "gray"
cols[gbr] = "brown"

#cols[luad] = "blue"
#cols[lusc] = "red"
#cols[korea] = "green"

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
plot(matrix$PC1, matrix$PC2, col=cols, xlab="Principal Component 1", ylab = "Principal Component 2")

  })
})
