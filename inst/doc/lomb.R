## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#    missing=c(5,25,49,26,56,79,88,98,99,112)
#    lmiss=data.frame(year=1:114, capture=lynx)
#    lmiss[missing,]=NA
#    lmiss=na.omit(lmiss)
#  
#    library(lomb)
#    result=lsp(lmiss)

## ----eval=FALSE---------------------------------------------------------------
#  data(caradat)
#  focus=actogram(caradat$Date, caradat$Activity, dble=TRUE, photo=FALSE, zone=1,from="1970-01-01 00:00:00",to="1970-01-14 00:00:00")
#  df=makedf (focus$date, focus$plotvar)
#  lsp(df, type="period",ofac=5,from=12,to=36)

