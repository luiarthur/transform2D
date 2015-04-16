## Polar Barchart Test

## Example data
source("PolarBarchart.R")
dfx <- data.frame(family=rep(c("I", "II", "III", "IV")[sort(sample(1:4, 10, TRUE))], 5), 
                  item=rep(LETTERS[1:10], 5), 
                  score=rep(letters[1:5], each=10), 
                  value=rep((1:5), each=10) + rnorm(50, 0, .5))

## Plot in separate window

#windows(10, 10)
p <- polarBarChart(dfx, familyLabel=TRUE, direction="inwards", 
                   binSize=0.3, spaceBar=0, spaceItem=0.2, spaceFamily=1.0, innerRadius=0.1, nguides=5)
print(p)

