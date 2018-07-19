library(magrittr)

data <- data.frame(
  col1=c(0,0,1,1),
  col2=c(0,1,0,1),
  col3=c(1,1,1,1),
  res =c(0,1,1,0)
)

predictors = c('col1','col2','col3')
targets = c('res')

inputs <- data %>% subset(select=predictors)
outputs <- data %>% subset(select=targets)


weights1 <- matrix(c(c(1,2,3,42), c(4,5,6,42), c(7,8,9,42)), nrow = 4, ncol = 3)
weigths2 <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 1, ncol=4)

for(i in 1:nrow(data)){
  input_row <- data[i,] %>% subset(select=predictors) %>% unlist
  layer1 <- weights1 %*% input_row
  layer2 <- weigths2 %*% layer1
  #No need to transpose vectors, NICE


  print(layer2)

  #Backprop here
}

print(weights1 %*% (inputs %>% as.matrix %>% t))

