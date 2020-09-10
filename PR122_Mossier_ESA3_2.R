# -------------------------------------- Aufgabe 3.2 ----------------------------------------------

###########
#### 1 ####
###########

df <- read.csv2("./gewicht.csv", header = TRUE, sep=";")
plot(df$angabe,
     df$waage,
     xlab = "Angegebene Gewichtsdaten", 
     ylab  = "Gemessene Gewichtsdaten", 
     title ("Vergleich angegeben & gemessen"),
     xlim = c(50, 110),
     ylim = c(50,110),
     )

# Die Gewichtsdaten weisen eine geringe Streuung/Abweichung auf

###########
#### 2 ####
###########

linRegModell <- lm(waage ~ angabe, data=df)
print(linRegModell)
# Gleichung: y = 5.8301 + 0.9547 * x



###########
#### 3 ####
###########

abline(linRegModell)

# oder alternativer plot mit ggplot:

library(ggplot2)

p <- ggplot(
  data=df,
  mapping = aes(x = waage, y = angabe));
p + geom_smooth(method='lm',formula= y~x) + ggtitle("Streudiagramm inkl Regressionsgerade") + geom_point();

###########
#### 4 ####
###########

df_10testColumns <-data.frame(
  "Prognose_40kg"=40,
  "Prognose_50kg"=50,
  "Prognose_60kg"=60,
  "Prognose_70kg"=70,
  "Prognose_80kg"=80,
  "Prognose_90kg"=90,
  "Prognose_100kg"=100,
  "Prognose_110kg"=110,
  "Prognose_120kg"=120,
  "Prognose_130kg"=130
  )

prognose <- function(input){
  for (i in 1:ncol(input)){
   input[i] = 5.8301 + 0.9547 * (input[i])
   }
  return(input)
}

testValues <- prognose(df_10testColumns)
print(testValues)

## Auffallend ist hier das die Streuung/Abweichung mit zunehmenden Gewicht immer geringer ausfaellt
