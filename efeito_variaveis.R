# Create the data for the chart
L <- c(23, -8, 10, -5, 6, -2, -1)
M <- c("TA","DA","QA","TADA","TAQA", "DAQA", "TADAQA")

barplot(L,names.arg=M,xlab="Variáveis",ylab="Efeito",col="blue",
        main="Efeito das variáveis",border="red")
