library(scatterplot3d)

temp <- seq(-pi, 0, length = 50)
x <- c(1, 5, 1, 5, 1,5,1,5)
y <- c(6, 6,10,10,6,6,10,10)
z <- c(1,1,1,1,2,2,2,2)
scatterplot3d(x, y, z, highlight.3d = TRUE, angle = 750,  xlim=c(1,10) , ylim=c(1,10), zlim=c(1,5),
              xlab ="TA" , ylab ="DA", zlab ="QA" ,
              col.axis = "black", col.grid = "lightblue", cex.axis = 1.0,
              cex.lab = 1.3, main = "Grafico 1", pch = 25)
 
