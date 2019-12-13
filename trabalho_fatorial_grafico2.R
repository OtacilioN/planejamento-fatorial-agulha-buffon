library(formattable)

df <- data.frame(
  id = 1:8, 
  TA = c("-", "+", "-", "+", "-","+","-","+"),
  DA = c("-", "-", "+", "+", "-","-","+","+"),
  QA = c("-", "-", "-", "-", "+","+","+","+"),
  
  TADA = c("+", "-", "-", "+", "+","-","-","+"),
  TAQA = c("+", "-", "+", "-", "-","+","-","+"),
  DAQA = c("+", "+", "-", "-", "-","-","+","+"),
  TADAQA = c("-", "+", "+", "-", "+","-","-","+"),
  Resultados = accounting(c(5.4037,26.7566,3.2092,16.0482,10.4334,46.3632,6.2859,29.6184), digits = 6,format = "f")
)


formattable(df,align = c("l",rep("r", NCOL(df) - 1)),
            list(
              id = color_tile("white", "lightblue"),
              
              Resultados =formatter("span",
                                    style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
                                    x ~ sprintf("%.2f%s (rank: %2g)", x,'%',rank(-x)))
              
            ))
