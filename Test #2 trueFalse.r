#1 True -- The model that includes all the variables will always have the largest R^2
    # Y variable selection is important because if you have more variables, it will be overfit and complex. When you have a fit in the training data, it's better to have more variables. We want to balance out the fitting model-- compromise the bias. Bias might be a little bit worse, but that will improve the variance in the testing part. The model becomes complex by adding more variables-- so you'll have less bias.
#2 True -- The model that includes all the variables will always have the smallest RSS.
    # If you have more variables, you have less bias, and more variance. So the prediction part won't work well. We want to choose the variables with the most significant impact-- select best method to predict the testing model.
#3 True -- BIC method adds penalty to RSS for the number of variables in the model || log(n)
    # These are the methods that improve the testing. The training RSS goes a bit higher if there is penalty. N is the number of variables. The other two use constants.
#4 False -- BIC method reduces RSS for the number of variables in the model.
    # 
#5 True -- A small value of Cp and BIC indicates a low error, and thus a better model.
    # You can increase number of variables, so the CP and BIC data will lower. You see what the lowest is, and that's the number of variables you need to pick. When variables get bigger, R^2 increases and flatlines, so you can use either one.
#6 True -- A large value for the adjusted R^2 indicates a low error, a better model.
#7 False -- A large value for the AIC indicates a low error, a better model.
#8 True -- BIC takes log(n) instead of constant 2. When log(n) > 2, it gives heavier penalty with many variables.
    # Log will eventually go higher than the constant number.
#9 False -- Shrinking the coefficients can significantly reduce their bias (increase bias hoping to have less variance)
    # You want to reduce the coefficient value. There is a penalty to reduce each coefficient.
#10 True -- In lasso, when lambda = 0, we get the linear least squares estimates.
#11 True -- In ridge, when lambda = 0, we get the linear least squares estimates.
#12 False -- In ridge, when lambda = 0, it significantly reduces the variance. (no impact)
#13 True -- As we increase lambda from 0, the training RSS will steadily increase.
#14 True -- As we increase lambda from 0, the test RSS will decease initially and then eventually start increasing in a U shape.
#15 True -- As we increase lambda from 0, the test RSS will decrease initially, and then eventually start increasing as a U shape
#16 True -- As we increase lambda from 0, the bias will steadily increase.
#17 True -- As we increase lambda from 0, the irreducible error remains constant
# ...
#23 False -- Tree-based methods only work for classification (categorical)
#24 9 --What is the misclassification loss before the X split?
    # The number that has been incorrectly identified
#25 1 + 2 = 3 -- What is the misclassification loss after the X split?
    #Find the lowest kind in both halves, and then add the number of the lowest kinds together.
#26 9 -3 = 6 -- What is the gain of misclassification loss after the X split? 
    # Loss before the xsplit - loss after the xsplit.
#27 -- What is the tree size for the minimum cross validation error? 3
#28 -- What is the best number of leaves after pruning the tree? 3

