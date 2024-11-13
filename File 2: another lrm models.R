This script performs the following key tasks:
1)Data Loading & Preprocessing:
Loads the concrete dataset, renames columns, and creates a subset for testing.

2)Model Building:
Implements a linear regression model to predict concrete strength.
Creates a second linear model by excluding certain variables (CoarseAggregate and FineAggregate) and compares the two models.

3)Evaluation:
Calculates the Mean Squared Error (MSE) for each model on the test set.
Computes the correlation between actual and predicted concrete strength values.
