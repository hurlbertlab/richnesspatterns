# Divides variable into 100 bins for shading with rainbow()
# Plots map of shaded grid cells
varMap <- function(var) {

  df = read.table('data/SAM_Western_Hemisphere_1dg_edited.txt', sep = '\t', header = TRUE)
  
  # generate vector of fill colors for map
  shades <- rainbow(130)[100:1]
  
  percents <- as.integer(cut(var, 100, 
    include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]

  # map
  plot(df$Longitude, df$Latitude, type = 'n', xlab = 'Longitude', ylab = 'Latitude')
  rect(df$Longitude - 0.5, df$Latitude - 0.5, df$Longitude + 0.5, df$Latitude + 0.5, col = fills, border = NA)
  
  # add a legend
  maxvar = max(var, na.rm = TRUE)
  minvar = min(var, na.rm = TRUE)
  inc <- (maxvar - minvar) / 4
  legend.text <- round(c(minvar, minvar + inc, minvar + 2 * inc, minvar + 3 * inc, maxvar))
  
  legend("bottomleft", 
    legend = legend.text, 
    fill = shades[c(1, 25, 50, 75, 100)])
}