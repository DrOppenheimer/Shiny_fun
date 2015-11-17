library(shiny)

# read in the abundance table -- is this actually useed for anything? -- eigen vectors are loaded from a different file in server.R
# User server.R instead of this file

data_trans <- data.matrix(read.table("combined_FPKM.txt.quantile.PREPROCESSED.txt", row.names=1, header=TRUE, sep="\t", comment.char="", quote="", check.names=FALSE))
#data = read.table("RPKM_TCGA_LUNG.txt", header=1)
#data_frame = data.frame(data)
#data_log = log2(data_frame+0.0000000001)
#data_trans = t(data_log)
#temp = lapply(data.frame(data_trans), var)
#w = which(temp==0)
#data_trans = data_log[-w, ]
pc = prcomp(t(data_trans), scale=T) # This is calculating pc -- are these used, or the precanned ones?
x = pc$x # x is a matrix that contains eigen vectors
matrix = x[,1:3] # get the first three eigen vectors (components/coordinates)
matrix = data.frame(matrix) # change "matrix" to a dataframe


TSI = which(grepl("TSI", rownames(matrix))) # subset "matrix" by rowname prefix
FIN = which(grepl("FIN", rownames(matrix)))
CEU = which(grepl("CEU", rownames(matrix)))
YRI = which(grepl("YRI", rownames(matrix)))
GBR = which(grepl("GBR", rownames(matrix)))
#luad = which(grepl("LUAD", rownames(matrix)))
#lusc = which(grepl("LUSC", rownames(matrix)))
#korea = which(grepl("LC", rownames(matrix)))
cols = rep("blue", length(rownames(matrix))) # create a default color list


cols[tsi] = "red" # repopulate

cols[fin] = "green"
cols[ceu] = "blue"
cols[yri] = "gray"
cols[gbr] = "brown"

#cols[luad] = "blue"
#cols[lusc] = "red"
#cols[korea] = "green"

# is this being used? -- this is for a 2d plot
shinyServer(function(input, output) {
    output$distPlot <- renderPlot({
        plot(matrix$PC1, matrix$PC2, col=cols, xlab="Principal Component 1", ylab = "Principal Component 2")
    })
})
