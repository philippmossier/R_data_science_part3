# -------------------------------------- Aufgabe 3.1 ----------------------------------------------

###########
#### 1 ####
###########

df <- read.csv2("./bl2009.csv", header = TRUE, sep=";")

###########
#### 2 ####
###########

## Einfacher Plot mit Punkten


plot(
  df$Marktwert,
  df$Tore,
  xlab="Marktwert",
  ylab="Tore",
  main="Marktwert & Tore Vergleich",
  type="p",
  xlim=c(0,11),
  ylim=c(0, 35)
);
text(df$Marktwert,df$Tore,labels=df$Verein,pos = 4, cex = 1)

# qplot und ggplot benoetigt eventuell das ggplot2 package 
# (siehe offizieler rstudio link: https://rstudio.com/products/rpackages/)
library(ggplot2)

## Einfacher Plot mit Linien

 qplot(
   df$Marktwert,
   df$Tore,
   xlab="Marktwert",
   ylab="Tore",
   main="Marktwert & Tore Vergleich",
   geom="line"
  );           

## Graphisch detailierter Plot

p <- ggplot(
  data=df, 
  mapping = aes(x = Marktwert, y = Tore)
); 
p + geom_point() + geom_smooth(method = 'loess',formula= y ~ x) + ggtitle("Marktwert & Tore Vergleich");


# Antwort zu Punkt 2:
#
# Man sieht hier einen Zusammenhang zwischen hoeheren Marktwert und mehr Tore 
# jedoch wird der positive Zusammenhang mit steigendem Marktwert immer geringer.
#
# Es gibt 4 Vereine mit einem Marktwet unter 2, welche alle ueber 20 Tore 
# geschossen haben = hohe positive Korrelation.
#
# Man erkennt auch das sich ab einen Marktwert von 4 die Toreanzahl nicht mehr steigert.


###########
#### 3 ####
###########

corPearson <- cor(
  df$Marktwert,
  df$Tore,
  method="pearson"
  );

print(corPearson)

# Antwort zu Punkt 3: 
#
# Der Korrelationskoeffizient mit 0.6525352 sagt aus das es hier eine positive
# Korrelation von Marktwert und Tore gibt. 
#
# Größere Werte von X gehen dann einher mit größeren Werten von Y.

###########
#### 4 ####
###########

corSpearman <- cor(
  df$Marktwert,
  df$Tore,
  method="spearman"
);

print(corSpearman)

## Der Korrelationskoeffizient nach Spearman weicht mit 0.6964843 etwas ab 
## und weist auf eine positvere Korrelation als bei pearson.
