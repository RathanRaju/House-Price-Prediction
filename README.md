# House-Price-Prediction

#### Tools used - R Studio

### Every year, house prices rise, necessitating the creation of a system to forecast future house prices. House price prediction may assist an individual in determining the selling price of a home and in determining the best time to buy a home. Physical conditions, concept, and location are the three factors that mainly affect the price of a home. This project uses regression analysis to forecast house prices, and resampling methods are used to propose the best fit for the model. In addition, regression analysis is used to find the best coefficient for prediction.

### Decision tree model was fit to predict the overall condition of the house. The model fitted on multi-class imbalanced data gave a balanced accuracy of 69.36, 69.46 and 66.24 on classes, “Average”, “Good” and “Poor”, respectively on test data. The performance of the model could be improved by balancing the classes in the data. Further, we can conclude that Random forest regression model performed much better than SVM because random forests are made up of decision trees, each of which draws a sample of random data, they are extremely accurate. As a result, random forests have a higher R squared forecast. After using resampling methods of Bootstrapping and cross validation, we can say bootstrap gives better fit to the model.
