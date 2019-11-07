##Data scince 
## Dia 06/11/19
## Feito por: Laris, Thiago, e Fabiano
## SOM (self-organizing maps)


# Installing packages 
packageurl <- "https://cran.r-project.org/src/contrib/Archive/kohonen/kohonen_2.0.19.tar.gz"
install.packages(packageurl, repos = NULL, type = "source")
install.packages("RCurl")

# Loading packages needed
require(kohonen)
require(RColorBrewer)

library(RCurl)
NBA <- read.csv(text = getURL("https://raw.githubusercontent.com/clarkdatalabs/soms/master/NBA_2016_player_stats_cleaned.csv"), 
                sep = ",", header = T, check.names = FALSE)

# viewing data
colnames(NBA)


# Escolhendo as variareis com quais vamos trabalhar 
# (FTA = Free throw attemps/ 2PA = Point Field Goal Attempts / 3PA = 3-Point Field Goal Attempts)
NBA.measures1 <- c("FTA", "2PA", "3PA")
NBA.SOM1 <- som(scale(NBA[NBA.measures1]), grid = somgrid(4, 4, "rectangular"))
plot(NBA.SOM1)

# reverse color ramp
colors <- function(n, alpha = 1) {
  rev(heat.colors(n, alpha))
}

plot(NBA.SOM1, type = "counts", palette.name = colors, heatkey = TRUE)


par(mfrow = c(1, 2))
plot(NBA.SOM1, type = "mapping", pchs = 20, main = "Mapping Type SOM")
plot(NBA.SOM1, main = "Default SOM Plot")