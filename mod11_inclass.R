library(randomForest)

body(randomForest)
#set default value in definition
my_ratio <- function(x=1,y=1){
  x/y
}

my_ratio(3,4)
my_ratio()

