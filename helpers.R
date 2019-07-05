# Note: percent map is designed to work with the counties data set
# It may not work correctly with other data sets if their row order does 
# not exactly match the order in which the maps package plots counties
varMap <- function(var) {

  # generate vector of fill colors for map
  shades <- rainbow(130)[100:1]
  
  percents <- as.integer(cut(var, 100, 
    include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]

  # map
  plot(df$Longitude, df$Latitude, type = 'n', xlab = 'Longitude', ylab = 'Latitude')
  rect(df$Longitude - 0.5, df$Latitude - 0.5, df$Longitude + 0.5, df$Latitude + 0.5, col = fills, border = NA)
  
  # add a legend
  max = max(var, na.rm = TRUE)
  min = min(var, na.rm = TRUE)
  inc <- (max - min) / 4
  legend.text <- round(c(min, min + inc, min + 2 * inc, min + 3 * inc, max))
  
  legend("bottomleft", 
    legend = legend.text, 
    fill = shades[c(1, 25, 50, 75, 100)])
}