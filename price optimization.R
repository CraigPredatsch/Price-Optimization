rm(list=ls())

test = read.csv("test_data.csv")

# Variables for log of number of sales and prices
test$logSales = log(test$Unit.Sales)
test$logPrice = log(test$Price)

# 2 products, 2 stores in data set - need one dummy variable for each
test$Product1 = 1*(test$Product == 1)
test$Store2 = 1*(test$Store == 2)

# Holiday and Promotions dummy variables
test$HolidayDummy = 1*(test$Holiday == "Holiday")
test$PromotionDummy = 1*(test$Promotion == "Yes")

# Linear regression
regression = lm(logSales ~ logPrice + Product1 + Store2 + logPrice*Product1 + HolidayDummy + PromotionDummy, test)

summary(regression)

# coefficients from the second regression
demand_results = regression$coefficients  

## demand function based on the regression results
demand_test = function(Price, Product1, Store2, HolidayDummy, PromotionDummy) {
  # Multiplying coefficients by their respective variable
  Q = exp(demand_results[1] + demand_results[2]*log(Price) + demand_results[3]*Product1 + demand_results[4]*Store2 + demand_results[5]*HolidayDummy + demand_results[6]*PromotionDummy + demand_results[7]*log(Price)*Product1)
  # make sure that demand is not negative
  Q[which(Q < 0)] = 0 
  return(as.numeric(Q))
}


## plot predicted demand
# creates grid of all prices between these ranges
price_grid = seq( 
  from = 0.1,
  to = .5, 
  by = .001)

## Input values for demand test for "price_grid" prices for Product 1 at Store 2, on a non-Holiday, non-Promotion week
demand_grid = demand_test(price_grid, 1, 1, 0, 0) 

#Plot demand function
plot(x = price_grid, 
     y = demand_grid, 
     xlab = "Price", 
     ylab = "Quantity", 
     type ="l",# linetype. 1 = solid line. Default option is 1. 
     lty = 1, 
     main = "Demand function") ## now type is line 

## add a layer of data points
points(x = test$Price, 
       y = test$Unit.Sales,
       col = "blue")  

# mean of data points
abline(v = mean(test$Price),  
       lty = 2, ## linetype. 2 = dash 
       col = 'red')

legend("topright",
       legend = c("Estimated", "Data", "Avg Price"),
       lty=c(1,NA),
       pch=c(NA, 1),
       col= c("black", "blue", "red"))

summary(test$Price)

## Create a graph that shows maximum profitability pricing
# Define profit function based on the demand calculated for each price
profit_test <- function(Price, Product1, Store2, HolidayDummy, PromotionDummy, unit_cost, fixed_cost) {
  quantity = demand_test(Price, Product1, Store2, HolidayDummy, PromotionDummy) ## compute the demand given price
  profit = quantity * Price - quantity * unit_cost - fixed_cost ## compute profit
  return(profit)
}

## profit in the grid points assuming unit_costs = 0.1
price_grid = seq(from = 0.1, 
                 to = .5, 
                 by = .001)

# Profit assuming zero fixed cost, 0.1 per unit, Product 1, Store 2, Non-Holiday, Non-Promotion
profit_grid = profit_test(P = price_grid, 
                             Product1 = 1, 
                             Store2 = 1, 
                             HolidayDummy = 0,
                             PromotionDummy = 0,
                             unit_cost = .1, 
                             fixed_cost = 0)

# Profit assuming $25 fixed cost, 0.1 per unit, Product 1, Store 2, Non-Holiday, Non-Promotion
profit_grid_25 = profit_test(P = price_grid, 
                             Product1 = 1, 
                             Store2 = 1, 
                             HolidayDummy = 0,
                             PromotionDummy = 0,
                             unit_cost = .1, 
                             fixed_cost = 25)


## Optimize profit. Find the price that gives us the highest profit based on demand
optimal_results = optim(par = .5, # starting point for price - highest price 
                        fn = profit_test,  # function to maximize 
                        Product1 = 1, # other fixed input variables of the profit function
                        Store2 = 1, 
                        HolidayDummy = 0,
                        PromotionDummy = 0,
                        unit_cost = 0.1, 
                        fixed_cost = 0, 
                        method = "BFGS",              
                        control = list(fnscale = -1))  #fnscale negative makes this a maximization function


#Plot zero fixed cost profit grid
plot(x = price_grid, 
     y = profit_grid, 
     xlab = "Price", 
     ylab = "Profit", 
     type ="l")

#Plot $25 fixed cost profit grid
lines(x = price_grid,
      y = profit_grid_25,
      col = "green")

#Find price that provides highest amount of profit
abline(v = optimal_results$par, ## x-intercept of the vertical line  
       lty = 2, ## linetype. 2 = dash 
       col = 'red')

#Draw a line at the mean price
abline(v = mean(test$Price), ## x-intercept of the vertical line  
       lty = 2, ## linetype. 2 = dash 
       col = 'blue')


legend("topright",
       legend = c("Profit function", "Optimal price", "Fixed $25 Cost", "Average Price"),
       lty = c(1,2),
       col = c("black", "red", "green", "blue"))