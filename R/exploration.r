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


weights1 <- matrix(runif(4*3, 0, 1), nrow = 4, ncol = 3)
weights2 <- matrix(runif(1*4, 0, 1), nrow = 1, ncol=4)

print(weights1)
print(weights2)

for(i in 1:nrow(data)){
  input_row <- data[i,] %>% subset(select=predictors) %>% unlist
  layer1 <- weights1 %*% input_row
  layer2 <- weigths2 %*% layer1
  #No need to transpose vectors, NICE


  print(layer2)

  #Backprop here
}

print(weights1 %*% (inputs %>% as.matrix %>% t))

