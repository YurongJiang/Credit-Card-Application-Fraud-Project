# Credit-Card-Application-Fraud-Project
This report examined an applications dataset to find potential fraud with methods include Feature Selection, Wrapper and Machine Learning Algorithm Modeling.  


#### Objective:  
 Application fraud is one of the most popular forms of identity fraud. Fraudsters use falsified (either stolen or invented) personal information when applying for credit cards, bank accounts, loans or tax rebates. This fraud costs millions of dollars each year, since the process of tracking them with accuracy, without turning down too many valid customers, is challenging to strike.   
 This report examined an applications dataset to find potential fraud with methods include Feature Selection, Wrapper and Machine Learning Algorithm Modeling. Data is processed and analyzed in Python and R.  

#### Project Outline:
The original dataset contains 1,000,000 rows of application records with 9 variables of applicants’ personal information. The general process of analysis step included:  
__1. Data cleaning and filing missing values;__ We proposed the dataset to optimize the results of the analysis. Although there were no missing values in this dataset, there were multiple frivolous values that were addressed prior to building variables.  
__2. Building expert variables and standardizing;__ we built many candidate variables and scaled them before utilizing feature selection methods to select the best candidate variables.  
__3. Feature selection using KS and FDR;__ Both the KS score and the FDR rate help in determining how well candidate variables individually predict fraud. We rank ordered the candidate variables in terms of usefulness for our models.  
__4. Applying fraud algorithms;__ We used supervised algorithms including a logistic regression, a random forest, AdaBoosting, XGBoosting and Gradient Boosting methods to detect fraud in the application dataset provided.  
__5. Recommendations__: Lastly, we will create a threshold for the top 7 percent of applications to reject based on our fraud scoring model to optimize the balance between rejecting legitimate applications and accepting fraudulent ones.  
