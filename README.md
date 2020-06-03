# RSModels

Contains functions that enable the construction of linear models with different parameters, cross validation implementation and the calculation of a random goodness-of-fit ditribution. These models are specially useful for studies that use remote sensing variables as independent variables to describe or predict measured-in-field community attributes. 

This package contains functions that construct linear models for a ny number of dependent variables and nx number of independent variables. There are currently six models supported: 1 independent variable with a linear term (y = mx + b), 1 independent variable with a quadratic term (y = mx + nx^2 + b), 2 independent variables (y = mx + ny + b), 2 independent variables with its interaction (y = mx + ny + ox:y + b), 2 independent variables with a quadratic term (y = mx + nx^2 + oy + b) and three independent variables (y = mx + ny + oz + b)

## Cloning and installing functions
Install devtools and gtools packages

```
install.packages("devtools")
install.packages("gtools")
```
Load the package devtools and install RSModels package
gtools will not be loaded, however, a function (permute) is called by RSModels

```
library("devtools")
install_github("JonathanVSV/RSModels")
```

Once the package is downloadad load it in your workspace 

```
library(RSModels)
```

## Using RSModels

The package contains six functions each one for each type of the supported models:
1. model1v2p()
	Model form: y = mx + b
2. model1v3p()
	Model form: y = mx + nx^2 + b
3. model2v3p()
	Model form: y = mx + ny + b
4. model2v4p()
	Model equation: y = mx + ny + ox:y + b
5. model2v4pB()
	Model equation: y = mx + nx^2 + oy + b
6. model3v4p()
	Model equation: y = mx + ny + oz + b

Each function computes, according to its design, all the possible combinations for the dependent variables and independent variables of the dataset. The three basic arguments to pass to any of these functinos are: data, ny and nx. Data stands for a data.frame that contains as the first column an id variable (e.g., number of plot, sampling unit, etc) followed by the dependent and independent variables, ordered as columns. Each row in this data frame stands for a single observation. ny stands for the number of dependent variables in the data (must be located before independent ones). nx stands for the number of independent variables in the data. Please see the example data to see the structure of this data frame.

At the end of the process several files will be created in the working directory.

Additionally, each function can compute two other processes: leave-k-out cross validation and expected at random goodness-of-fit distribution.  By default, these two processes are not performed. 

### Cross-validation
To perform a leave-k-out cross validation, two additional arguments must be passed on to the function: CV = TRUE and CV_n = k (the desired number of left out observations for the cross validation process). For example, a CV_n = 1 will perform a leave-one-out cross validation, while a CV_n = 2 will do a leave-two-out cross validation. A leave-k-out cross validation can be interpreted as the predictive power of a model or the expected goodness-of-fit if the model is applied to new observations.

### Random goodness-of-fit distribution
To perform the calculation of a random goodness-of-fit distribution, two additional arguments must be passed on to the function: r2random = TRUE and runs = x (the desired number of runs to obtain the highest goodness-of-fit distribution in a completely random scenario). For example, an argument of runs = 1000 will shuffle at random 1000 times the position of dependent and independent variables and extract the highest goodness-of-fit value. Therefore, at the end of the process the result table will show the 1000 highest values of goodness-of-fit from each randomization of the data for each dependent variable. This distributions represents the expected values of goodness-of-fit obtained simply by chance. It can be interpreted that if the goodness-of-fit of the best models for each dependent variable is higher than the 95 % of the goodness-of-fit values observed simply by chance, this model is statistically significant from a completely random scenario. 
Models where a higher number of independent variables is available, tend to obtain higher goodness-of-fit values in a completely random scenario tha models with a lower number of independent variables.

## Example Cometa data
Inside the package the Cometa data is available by calling:

```
data(Cometa)
```

This dataframe contains the plant community's structural and diversity attributes for each sampling plot of a Tropical Swamp Forest in Centla, Tabasco, Mexico: mean height, standard deviation of height, crown cover area, basal area, density of individuals, aboveground biomass, species richness, Simpson's diversity index, Shannon's diversity index. Additionaly, for each plot, its glcm metrics  or Haralick's textures are shown, 8 textures for the red band (R) and 8 for NIR (NIR) extracted from a Kompsat-2 multispectral image (approximately 4 m of pixel size in the multispectral bands). These eight textures were mean, variance, homogeneity, contrast, dissimilarity, entropy, second angular moment and correlation.

Performing a model with 1 variable and 2 parameters:

```
#The arguments passed are: data, followed by the number of dependent variables (ny) and then the number of independent variables (nx) in the data.
model1v2p(Cometa, 9, 16)
```

This command should perform every possible model with 1 independent variable for every dependent variable in the data frame.

To perform a leave-1-out cross validation with the same data, the following command is typed:

```
model1v2p(Cometa, 9, 16, CV = T, CV_n = 1)
```

Finally, to obtain the maximum goodness-of-fit distribution expected in a completely random scenario, the following command should be typed:

```
model1v2p(Cometa, 9, 16, r2random = T, runs = 1000)
```

In this example, 1000 randomizations (number of runs) were performed. 

To consult additional details of these study: 
Solórzano et al. (2018) Applied Journal of Remote Sensing 12(3): 036006. https://doi.org/10.1117/1.JRS.12.036006
