# Generalised linear models GLM and ANOVA with R

### This is my master project collaborated with my collegue. The project progresses 
### Step by step workflow
* Data understanding
* Cleaning, missing value handling
* Transforming to their appropriate type (numeric or factor variables)
* Here, we selected two different target variables, count & continous
* For Count variable (`no_flo` number of flowers):
    * Starting by Poisson regression approach
    * Checking all the count data regression assumptions including dsipersion test
    * Negative binomial approach & assumptions check again
    * Decide on the final model approach
    * Making a saturated model with all the predictor/independent variables
    * Checking the model criteria and reducing each time one variable (backward selection process)
    * Performing ANOVA on the selected model
    * In case of Signifance difference, Post Hoc test (Paiswise difference detection)
    * Plotting results & interpretation
* For Continous variable (`height` plant heght):
     * Checking all the linear assumptions such as probability density function (PDF), multicolinearity usw.
     * Starting with simple linear model & test statistics checking if the models has enough predicting power
     * Mixed effect model approach to include the random effect into the model
     * Creating saturated model with all the variables, assumptions checking (QQplot, fitted vs residuals QQplot etc), & reducing one by one if the complex model has enough predcting power compared to the simpler model (backward selection process)
     * Repeating the last until we reach a suitable model.
     * ANOVA for the final model
     * In case of Signifance difference, Post Hoc test (Paiswise difference detection)
     * Plotting results & interpretation
### The raw dataset look like this &#8595;&#8595;&#8595;
![grafik](https://user-images.githubusercontent.com/61450446/129138231-070a141a-d743-4d4c-af6e-dc387db8d1f1.png)

