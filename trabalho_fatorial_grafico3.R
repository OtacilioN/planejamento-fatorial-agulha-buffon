library(formattable)

df <- data.frame(
  id = c("TA","DA","QA","TADA","TAQA","DAQA","TADAQA"), 

  Resultados = accounting(c(23.3846,-8.4491, 10.2907, -5.3382,6.2021,-2.0374,-1.0286), digits = 6,format = "f")
)


formattable(df,align = c("l",rep("r", NCOL(df) - 1)),
            list(
              #id = color_tile("white", "lightblue"),
              
              Resultados =formatter("span",
                                    style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
                                    x ~ sprintf("%.2f", x )
              
            )))

            