Red Wine Quality Data Exploration
========================================================

The wine quality dataset was created by Paulo Cortez (Univ. Minho), Antonio Cerdeira, Fernando Almeida, Telmo Matos and Jose Reis (CVRVV) @ 2009, using red wine samples. The inputs include objective tests (e.g. PH values) and the output is based on sensory data (median of at least 3 evaluations made by wine experts). Each expert graded the wine quality between 0 (very bad) and 10 (very excellent).

```{r echo=FALSE, message=FALSE, warning=FALSE, packages}
# Load all of the packages that you end up using
# in your analysis in this code chunk.

# Notice that the parameter "echo" was set to FALSE for this code chunk.
# This prevents the code from displaying in the knitted HTML output.
# You should set echo=FALSE for all code chunks in your file.

library(ggplot2)
library(gridExtra)
library(lattice)
library(GGally)
library(dplyr)
```

```{r echo=FALSE, Load_the_Data}
# Load the Data
wqr <- read.csv('wineQualityReds.csv', sep = ',')
```

# Univariate Plots Section
```{r echo=FALSE, Univariate_Plots1, fig.height=5, fig.width=10, warning=FALSE}
#Remove the first column "X" since it is just a serial number 
wqr <- subset(wqr, select = -c(X))
wine_dimension <- dim(wqr)
# Below variables are showing in the Red Wine Dataset
wine_attributes <- names(wqr)
# look at the variability in the numerical data
wine_summary <- summary(wqr)

wine_dimension
wine_attributes
wine_summary
```
By creating histogram plots is a good way to have an idea about how each attributes are changing by themselves. The plots will help me to know all features at the first view.
```{r echo=FALSE, Univariate_Plots2, fig.height=10, fig.width=10, warning=FALSE}
create_plot <- function(varname = quality, binwidth = 0.1) {
  return(ggplot(aes_string(x = varname), data = wqr) + 
           geom_histogram(binwidth = binwidth))
}

Plotquality <- ggplot(aes(x = quality), data = wqr) + geom_bar()
Plotfixed.acidity <- create_plot('fixed.acidity')
Plotvolatile.acidity <- create_plot('volatile.acidity', binwidth = 0.03)
Plotcitric.acid <- create_plot('citric.acid', binwidth = 0.01)
Plotresidual.sugar <- create_plot('residual.sugar', binwidth = 0.5)
Plotchlorides <- create_plot('chlorides', binwidth = 0.01) + 
  scale_x_continuous(breaks = seq(0,0.6,0.1))
Plotfree.sulfur.dioxide <- create_plot('free.sulfur.dioxide', binwidth = 1) +
  scale_x_continuous(breaks = seq(0,70,5))
Plottotal.sulfur.dioxide <- create_plot('total.sulfur.dioxide', binwidth = 3) +
  scale_x_continuous(breaks = seq(0,300,30))
Plotdensity <- create_plot('density', binwidth = 0.0005)
PlotpH <- create_plot('pH', binwidth = 0.05)
Plotsulphates <- create_plot('sulphates', binwidth = 0.03)
Plotalcohol <- create_plot('alcohol') + 
  scale_x_continuous(breaks = seq(8.4,14.9,1))

grid.arrange(Plotquality, Plotfixed.acidity, Plotvolatile.acidity, 
             Plotcitric.acid, Plotresidual.sugar, Plotchlorides,
             Plotfree.sulfur.dioxide, Plottotal.sulfur.dioxide,
             Plotdensity,PlotpH, Plotsulphates, Plotalcohol, ncol = 3)
```

Many of the variables look normally distributed. Chlorides, sulphates, alcohol, free sulfur dioxide and total sulfur dioxide look like they have lognormal distributions. Let’s exclude the 95th percentile for all these five features and re-plot their histograms: 

```{r echo=FALSE, Univariate_Plots3, fig.height=5, fig.width=10, warning=FALSE}

Plotchlorides_95 <- create_plot('chlorides', binwidth = 0.01) + 
  xlim(0.03, quantile(wqr$chlorides, c(0.95)))

Plotfree.sulfur.dioxide_95 <- create_plot('free.sulfur.dioxide', binwidth = 1) +
  xlim(0, quantile(wqr$free.sulfur.dioxide, c(0.95)))

Plottotal.sulfur.dioxide_95 <- create_plot('total.sulfur.dioxide', binwidth = 3) +
  xlim(0, quantile(wqr$total.sulfur.dioxide, c(0.95)))

Plotsulphates_95 <- create_plot('sulphates', binwidth = 0.03) +
  xlim(0.3, quantile(wqr$sulphates, c(0.95)))

Plotalcohol_95 <- create_plot('alcohol') + 
  xlim(8.9, quantile(wqr$alcohol, c(0.95)))

grid.arrange(Plotchlorides_95, Plotfree.sulfur.dioxide_95, 
             Plottotal.sulfur.dioxide_95, Plotsulphates_95, 
             Plotalcohol_95, ncol = 3)
```
The distributions for chlorides, sulphates, alcohol, free sulfur dioxide, and total sulfur dioxide look normal after excluding the outliers.

# Univariate Analysis

### What is the structure of your dataset?
Number of red wine instances: 1599
Number of Attributes: 1 Serial Number + 11 Attributes + 1 Output Attribute

11 Attributes:

 1 - fixed acidity: most acids involved with wine or fixed or nonvolatile (do not evaporate readily)
 
 2 - volatile acidity: the amount of acetic acid in wine, which at too high of levels can lead to an unpleasant, vinegar taste
 
 3 - citric acid: found in small quantities, citric acid can add 'freshness' and flavor to wines

 4 - residual sugar: the amount of sugar remaining after fermentation stops, it's rare to find wines with less than 1 gram/liter and wines with greater than 45 grams/liter are considered sweet
 
 5 - chlorides: the amount of salt in the wine
 
 6 - free sulfur dioxide: the free form of SO2 exists in equilibrium between molecular SO2 (as a dissolved gas) and bisulfite ion; it prevents microbial growth and the oxidation of wine
 
 7 - total sulfur dioxide: amount of free and bound forms of S02; in low concentrations, SO2 is mostly undetectable in wine, but at free SO2 concentrations over 50 ppm, SO2 becomes evident in the nose and taste of wine
 
 8 - density: the density of water is close to that of water depending on the percent alcohol and sugar content
 
 9 - pH: describes how acidic or basic a wine is on a scale from 0 (very acidic) to 14 (very basic); most wines are between 3-4 on the pH scale
 
 10 - sulphates: a wine additive which can contribute to sulfur dioxide gas (S02) levels, wich acts as an antimicrobial and antioxidant
 
 11 - alcohol: the percent alcohol content of the wine

Output variable (based on sensory data): 
 
 12 - quality (score between 0 and 10)

### What is/are the main feature(s) of interest in your dataset?
Quality is the main feature.

### What other features in the dataset do you think will help support your investigation into your feature(s) of interest?

Residual sugar, fixed acidity, pH, density and alcohol content may help support the investigation into the quality.

### Did you create any new variables from existing variables in the dataset?
Yes, I do. Since the first column is all serial numbers, there is not any statistical significance. The column, named X, has been remmoved from the original dataset.

### Of the features you investigated, were there any unusual distributions? Did you perform any operations on the data to tidy, adjust, or change the form of the data? If so, why did you do this?
Attributes of chlorides, total sulfur dioxide, and free sulfur dioxide, sulphates, alcohol were all appeared to be long tailed and were log-transformed which revealed a normal distribution for each.

# Bivariate Plots Section
```{r echo=FALSE, Bivariate_Plots0, fig.height=10, fig.width=12, warning=FALSE}
wine_attributes

ggcorr(wqr, method = c("all.obs", "pearson"),
       nbreaks = 4, palette = "default", label = TRUE,
       name = "correlation coef.",
       size = 4) + 
  ggtitle("Pearson Correlation Coefficient Matrix")
```

With our main feature of the dataset, the positive correlation coefficients which are more then 0.1 are:
``` 
 alchol:quality = 0.5
 sulphates:quality = 0.3
 citric.acid:quality = 0.2
 fixed.acidity:quality = 0.1
```
So alcohol content has a high correlation with red wine quality. Other important attributes correlated with red wine quality include sulphates, citric acid and fixed acidity.

```{r echo=FALSE, Bivariate_Plots1, fig.height=5, fig.width=10, warning=FALSE}
ggplot(wqr, aes(x=quality, y = alcohol)) + 
  geom_bin2d(bins = 15)
```
As we can see, from above plot with alcohol contect across quality, there is a large amount of samples with quality score 5 and also 9.5% alcohol.
The samples with a higher quality score also have a higher alcohol percentage.

With our main feature of the dataset, the negative correlation coefficients which are less then -0.1 are:
```
 volatile.acidity:quality = -0.4
 total.sulfur.dioxide:quality = -0.2
 density:quality = -0.2
 chlorides:quality = -0.1
```
So we see that volatile acids are negatively correlated with red wine quality, as described from the document that is at too high of levels can lead to an unpleasant, vinegar taste. Total sulfur dioxide, density and chlorides are also negatively correlated with quality.

Besides, other attributes wiht the highest (positive or negative) correlation are:
```
 fixed.acidity:pH = -0.7
 fixed.acidity:citric.acid = 0.7
 fixed.acidity:density = 0.7
 free.sulfur.dioxide:total.sulfur.dioxide = 0.7
 volatile.acidity:citirc.acid = -0.6
 citric.acid:pH = -0.5
 density:alcohol = -0.5
```

#### pH Attribute
As we all know, the stronger the acid is, the lower pH will be. So it is make sence that either fixed acidity or citric acid has a high negative correlation with pH. 
```{r echo=FALSE, Bivariate_Plots2, fig.height=10, fig.width=10, warning=FALSE}
createVSPlot <- function(var_x, var_y = pH) {
  return(ggplot(aes_string(x = var_x, y = var_y), data = wqr) + 
           geom_jitter(alpha=0.2) + 
           stat_smooth(method = "lm"))
}

fixedAcidityVSpH <- createVSPlot('fixed.acidity','pH')
volatileAcidityVSpH <- createVSPlot('volatile.acidity','pH')
CitricAcidVSpH <- createVSPlot('citric.acid','pH')

grid.arrange(fixedAcidityVSpH, volatileAcidityVSpH, CitricAcidVSpH, ncol = 1)
```
All three features are acids. I've thought all acids will lower the value of pH. However, from above plot of pH across volatile acidity, with more content of volatile acidity, the value of pH increase a little bit. From this set of plots, I found the acidity of volatile is weaker than the other two acids, and fixed acid should be the strongest one here.

I will focus on several other highest correlation relationships in a bit more detail.

### Acid Attributes
Wine Acids play a large role in winemaking. Each acid plays a different role in the winemaking game. 
I would like to see how three kinds of acids working with the quality of red wine.
```{r echo=FALSE, Bivariate_Plots3, fig.height=10, fig.width=10, warning=FALSE}

fixedAcidityVSQuality <- createVSPlot('fixed.acidity','quality')
volatileAcidityVSQuality <- createVSPlot('volatile.acidity','quality')
CitricAcidVSQuality <- createVSPlot('citric.acid','quality')

grid.arrange(fixedAcidityVSQuality, volatileAcidityVSQuality, CitricAcidVSQuality, ncol = 1)
```
Fixed Acidity is a background player, supporting and stabilizing the wine as it evolves. It preserves the stability of the wine. So, the incresing of fixed acidity does not affact a lot on wine's quality, but helped a little bit.
Volatile Acidity, also known as malic acid, is high prior to veraison, but as grapes ripen, it escapes the grapes through respiration. Cooler climates produce grapes with higher levels of malic acid due to the cooler temperatures and low rates of respiration. In another words, volatile acidity can be virtually nothing if it is a really hot year. The malic acid gets used up in the respiration. Malic is a harsher acid, which at too high of levels can lead to an unpleasant, vinegar taste. So as the plot showing, the more volatile acidity the wine contains, the lower quality the wine will be
Citric acid is in a really small amount compared with the other two acids. It can be noticed from the range of each x-axis of above three plots. The data points from the plot of quality with fixed acidity are focusing from 6 to 10, which is almost 14 times of with volatile acidity and almost 32 times of with citric acid.

### Density Attribute
The density of wine somehow descides the taste of thick or refreshing. The description of density says the density of wine is close to that of water depending on the percent alcohol and sugar content. 
I would like to find out how alcohol and sugar content affact density, and also how density would work with the quality. 
```{r echo=FALSE, Bivariate_Plots4, fig.height=10, fig.width=10, warning=FALSE}
alcoholVSDensity <- createVSPlot('alcohol','density')
residualSugarVSDensity <- createVSPlot('residual.sugar','density')
densityVSquality <- createVSPlot('density','quality')

grid.arrange(alcoholVSDensity, residualSugarVSDensity, densityVSquality, ncol = 1)
```
Obviously, the density of wine has been affacted a lot by alcohol and residual sugar. The wine would be more refreshing while having more alcohol, however would be thicker if adding more sugar content. 
Well, the wine sometimes has too much acidity, but winemakers don't want to remove the volatile acid, they balance with sugar. The acids could be balanced off with residual sugar. 
Adding residual sugar might balance the taste of the wine. As we can see from the plot of quality with density, the quality is decreasing while the wine become thicker.

### How Volatile Acidity Affact Wine Quality
As of the feature of volatile acidity has a large negative pearson correlation coefficient. I would like to see more detail of how volatile acidity working with wine quality.
```{r echo=FALSE, Bivariate_Plots5, fig.height=5, fig.width=10, warning=FALSE}
ggplot(wqr, aes(x=factor(quality), y = volatile.acidity)) +
  geom_boxplot(alpha = 0.5) +
  stat_summary(fun.y = 'mean', geom = 'point', color = 'orange') +
  labs(title = "Boxplot of volatile acidity across red wine quality ratings", 
       x = "Quality (score between 3 and 8)",
       y = "volatile acidity (acetic acid - g / dm^3)") +
  theme_bw()
```
Based on the above boxplot, it is really easy to tell the observed result. High value of volatile acidity is truly lower the score of wine quality.

```{r echo=FALSE, Multivariate_Plots0, fig.height=5, fig.width=10, warning=FALSE}
ggplot(wqr, aes(x=factor(quality), y = alcohol, fill = factor(quality))) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(alpha = 0.5) +
  stat_summary(fun.y = 'mean', geom = 'point', color = 'orange') +
  theme_bw()
```
At the end of bbivariate analysis, I would like to re-focus on the main feature of dataset, which is quality. This boxplot is also showing one of the strongest relationship between quality and alcohol.

# Bivariate Analysis

### Talk about some of the relationships you observed in this part of the investigation. How did the feature(s) of interest vary with other features in the dataset?
As of the quality, it appears that when alchol or sulphates is in higher amounts, the quality will be better also. However, the amount of volatile acidity is negatively correlated with the quality. It is likely that fresher wines avoid the bitter taste of acetic acid.

### Did you observe any interesting relationships between the other features (not the main feature(s) of interest)?
As of citric acid, fixed acidity is positively correlated with the citric acid, but the amount of volatile acidity is opposite.
As of density, fixed acidity is also positively correlated with the citric acid, but the amount of alcohol is opposite.

### What was the strongest relationship you found?
From the variables analyzed, the strongest relationship was between fixed.acidity and pH, which had a correlation coefficient of 0.68.

# Multivariate Plots Section
Now let’s visualize the relationship between density, volatile.acidity, alcohol and quality: 
```{r echo=FALSE, Multivariate_Plots1, fig.height=5, fig.width=10, warning=FALSE}
ggplot(wqr, aes(x=alcohol, y=volatile.acidity, size = density, 
                 color=factor(quality))) + 
  geom_point(position = "jitter") +
  scale_color_brewer() +
  theme_dark()
```
On the above scatter plot, the darker the blue is means the wine with higher quality. In other words, white points are with the lowest quality, and darkest blue points are with the highest quality. 
Besides, the most of white points are shown on the up-left part of canvas, and the bottom right corner has more blue or dark blue points. It means that most of the wine with higher quality scores have higher alcohol content and also lower volatile acidity.

Below faceted plots tried to see how sulphates or alcohol affacts the quality of wine
```{r echo=FALSE, Multivariate_Plots2, fig.height=5, fig.width=10, warning=FALSE}
ggplot(data = wqr, aes(x = sulphates, y = alcohol, color = factor(quality))) +
  geom_point(alpha = 0.6, size = 1.2, position = 'jitter') +
  facet_wrap(~quality) +
  scale_color_brewer() +
  labs(x = 'Sulphates (log10(g / dm^3))', y = 'Alcohol (% by Volume)') +
  ggtitle('Scatter plots of Alcohol vs Sulphates faceted by quality') +
  theme_dark()
```
Interesting, sulphates also slightly affact the quality of wine. From above six plots, all scatter points are slightly moving to right. We almost cannot realize between two contiguous plots. But while we compare the first one with the fifth or sixth one, it actually shift a step to right. It comes out that sulphates help a little bit to increase the score of wine quality. 

Next, let’s try to summarize quality using a contour plot of volatile acidity and sulphate content: 
```{r echo=FALSE, Multivariate_Plots3, fig.height=5, fig.width=10, warning=FALSE}
ggplot(wqr, aes(x = volatile.acidity, y = sulphates, 
                 colour=factor(wqr$quality))) + 
  ylim(0.3, quantile(wqr$sulphates, c(0.95))) +
  geom_density2d(bins=2) + 
  scale_color_brewer() + 
  geom_point(color="black", alpha=0.1) +
  theme_dark()
```
Now, we almost can tell the result before plotting. As of sulphates are positive correlated with quality, while volatile acidity is negative correlated with quality. So, the contour plot with the highest score of quality should show up with higher value of sulphates and lower value of volatile acidity. No wonder, the plot exactly shows the result what we are expecting. 

```{r echo=FALSE, Multivariate_Plots4, fig.height=5, fig.width=10, warning=FALSE}
ggplot(wqr, aes(x = volatile.acidity, y = alcohol, colour=factor(wqr$quality))) + 
  geom_density2d(bins=2) + 
  scale_color_brewer() + 
  geom_point(color="black", alpha=0.1) +
  labs(x = 'Volatile Acidity (acetic acid - g / dm^3)', y = 'Alcohol (% by Volume)') +
  ggtitle('Quality contour plot of Alcohol vs Volatile Acidity') +
  theme_dark()
```
This shows that higher quality red wines are generally located near the range from 0.25 to 0.65 of citric acid and slso near the higher alcohol which is more than 10.5%. Whereas lower quality red wines are generally with lower either alcohol or citric acid.

Let’s try to summarize quality using a contour plot of density and alcohol content: 
```{r echo=FALSE, Multivariate_Plots5, fig.height=5, fig.width=10, warning=FALSE}
ggplot(wqr, aes(x = alcohol, y = density, 
                 colour=factor(wqr$quality))) + 
  xlim(8.9, quantile(wqr$alcohol, c(0.95))) +
  geom_density2d(bins=2) + 
  scale_color_brewer() + 
  geom_point(color="black", alpha=0.1) +
  theme_dark()
```
From above plot, we can tell that density does not really affact a lot of quality, but alcohol does. 

```{r echo=FALSE, Multivariate_Plots6, fig.height=5, fig.width=10, warning=FALSE}
ggplot(wqr, aes(x=density, y = alcohol, color = factor(quality))) +
  geom_jitter() +
  geom_smooth(method = 'lm') +
  scale_color_brewer() +
  theme_dark()
```
I am tring to use this plot to tell the same result with the previous one. However, the latest plot can tell more information within one plot.

# Multivariate Analysis

### Talk about some of the relationships you observed in this part of the investigation. Were there features that strengthened each other in terms of looking at your feature(s) of interest?

Based on the multivariate analysis, five features stood out to me: alcohol, sulphates, citric acid, volatile acidity, and quality. Volatile acidity with amount between 0.3 and 0.5 and sulphates with amount between 0.6 and 0.9 were a strong indicator of the presence of good wine. Also, high alcohol content and higher citric acid have more chance to make for a good wine.

# Final Plots and Summary

### Plot One
As analyzing relationship between quality and other 11 attributes, the strongest correlation coefficient was found between alcohol and quality.
```{r echo=FALSE, Plot_One, fig.height=5, fig.width=10, warning=FALSE}
ggplot(wqr, aes(x=factor(quality), y = alcohol, fill = factor(quality))) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(alpha = 0.5) +
  stat_summary(fun.y = 'mean', geom = 'point', color = 'orange') +
  labs(title = "Boxplot of alcohol content across red wine quality ratings", 
       x = "Quality (score between 3 and 8)",
       y = "Alcohol (% by volume)") +
  theme_bw()
```
 
```{r echo=FALSE, Plot_One_summary}
count(wqr, quality)
by(wqr$alcohol, wqr$quality, summary)
```

### Description One
Clearly we see that the box plots for higher quality red wines are up shifted, meaning they have a comparatively higher alcohol content, compared to the lower quality red wines. 


### Plot Two
```{r echo=FALSE, Plot_Two, fig.height=5, fig.width=10, warning=FALSE}
ggplot(wqr, aes(x = volatile.acidity, y = sulphates, 
                 colour=factor(wqr$quality))) + 
  ylim(0.3, quantile(wqr$sulphates, c(0.95))) +
  geom_density2d(bins=2) + 
  scale_color_brewer() + 
  geom_point(color="black", alpha=0.1) +
  labs(title = "2D Density plot of volatile acidity across sulphates to draw red wine quality ratings", 
       x = "volatile acidity (acetic acid - g / dm^3)",
       y = "sulphates (potassium sulphate - g / dm3)") +
  theme_dark()
```

### Description Two
Observe that lower sulphates content typically leads to a bad wine with alcohol varying between 9% and 12%. Average wines have higher concentrations of sulphates, however wines that are rated 6 tend to have higher alcohol content and larger sulphates content. Excellent wines are mostly clustered around higher alcohol contents and higher sulphate contents. 

### Plot Three
```{r echo=FALSE, Plot_Three, fig.height=5, fig.width=10, warning=FALSE}
ggplot(wqr, aes(x=density, y = alcohol, color = factor(quality))) +
  geom_jitter(size = 2) +
  geom_smooth(method = 'lm') +
  scale_color_brewer(type = 'seq', guide = guide_legend(title = "Quality Levels")) +
  labs(title = "Relationship of density and alcohol with quality levels", 
       x = "density (g / cm^3)",
       y = "alcohol (% by volume)") +
  theme_dark()
  
```

### Description Three
This shows that higher quality red wines are generally having higher percentage of alcohol, which is more than 11%, and having slightly lower density, which means the refreshing wine is somehow being more popular. With the help of density, actually, for the low quality levels with score of 3, 4 and 5, it is hard to tell how alcohol percentage affact the quality. Then, for the high levels of 6, 7 and 8, it is so obverious that more alcohol content would result a better wine quality.

------

# Reflection
The red wine dataset contains information on 1,599 red wine instances, 11 attributes and one output attribute. Initially, I tried to get a sense of how is each attribute changing on their own. All univariate plots have been arranged together. Many of the variables look normally distributed. However, some of features have lognormal distributions. I exclude the 95th percentile for these features and re-plot their histograms. 

Then, I tried to find what factors might affect the quality of the wine. At this moment, pearson correlation coefficient can help us to visualize the relationship between each pair of variables. Using the insights from correlation coefficients provided by the paired plots, it was interesting exploring quality using box plots with a different color for each quality. Besides, melting the dataframe and using facet grids was really helpful for visualizing the distribution of the parameters with the use of scatter plots. Finally, using a contour plot of wine quality with a point plot of volatile acidity and alcohol would be a good choice to show that either the lower volatile acidity or higher alcohol have more possible to make a better wine. The result makes sense. Volatile acidity is mostly caused by bacteria in the wine which is the amount of acetic acid in wine. It can lead to an unpleasant, vinegar taste if at too high of levels.

The hardest time for me is to understanding all the features with wiki pedia or other documents. But to be a good data analyst, we must study and understand the data structure as much as we can. Finally, I figured out all attributes of red wine.

The dataset may include more features of the environment where grapes are grown. As we all known, location and temperature play the important roles in the quality of wine.

# Citation
P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. 
  Modeling wine preferences by data mining from physicochemical properties.
  In Decision Support Systems, Elsevier, 47(4):547-553. ISSN: 0167-9236.
