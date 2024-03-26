

summary.lsp <- function(object,...) {
  first <- object$type
  if (first == "frequency") {
    second <- "At period"
  } else {
    second <- "At frequency"
  }
  first <- paste("At ", first)
  from <- min(object$scanned)
  to <- max(object$scanned)
  
  Value <- c(object$data[[1]], object$data[[2]], object$n, object$type, object$ofac, from, to, object$n.out, object$peak, object$peak.at[[1]], object$peak.at[[2]], object$p.value,object$normalize)
  options(warn = -1)
  for (i in 1:length(Value)) {
    if (!is.na(as.numeric(Value[i]))) 
      Value[i] <- format(as.numeric(Value[i]), digits = 5)
  }
  options(warn = 0)
  nmes <- c("Time", "Data", "n", "Type", "Oversampling", "From", "To", "# frequencies", "PNmax", first, second, "P-value (PNmax)", "normalized")
  report <- data.frame(Value, row.names = nmes)
  report
}

