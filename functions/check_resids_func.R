check_resids <- function(model) {
  par(mfrow = c(1, 2))
  
  # Residuals vs Fitted
  plot(fitted(model), residuals(model), pch = 16, cex = 0.4,
       xlab = "Fitted", ylab = "Residuals",
       main = "Residuals vs Fitted")
  abline(h = 0, lty = "dashed")
  
  # QQ plot
  qqnorm(residuals(model), main = "QQ Plot (Residuals)", pch = 16)
  qqline(residuals(model))
  
  par(mfrow = c(1, 1))
}