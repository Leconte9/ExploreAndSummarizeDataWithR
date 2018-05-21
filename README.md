# Explore And Summarize Data With R

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
dim(wqr)
# Below variables are showing in the Red Wine Dataset
names(wqr)
# To show types of variables
str(wqr)
# look at the variability in the numerical data
summary(wqr)

qplot(x = quality, data = wqr, binwidth = 0.5)
table(wqr$quality)
summary(wqr$quality)
summary(wqr$fixed.acidity)
```

```{r echo=FALSE, Univariate_Plots2, fig.height=10, fig.width=10, warning=FALSE}
Plotquality <- qplot(x = quality, data = wqr, binwidth = 0.5)
Plotfixed.acidity <- qplot(x = fixed.acidity, data = wqr, binwidth = 0.1)
Plotvolatile.acidity <- qplot(x = volatile.acidity, data = wqr, binwidth = 0.03)
Plotcitric.acid <- qplot(x = citric.acid, data = wqr, binwidth = 0.01)
Plotresidual.sugar <- qplot(x = residual.sugar, data = wqr, binwidth = 0.5)
Plotchlorides <- qplot(x = chlorides, data = wqr, binwidth = 0.01) + 
  scale_x_continuous(breaks = seq(0,0.6,0.1))
Plotfree.sulfur.dioxide <- qplot(x = free.sulfur.dioxide, data = wqr, binwidth = 1) +
  scale_x_continuous(breaks = seq(0,70,5))
Plottotal.sulfur.dioxide <- qplot(x = total.sulfur.dioxide, data = wqr, binwidth = 3) +
  scale_x_continuous(breaks = seq(0,300,30))
Plotdensity <- qplot(x = density, data = wqr, binwidth = 0.0005)
PlotpH <- qplot(x = pH, data = wqr, binwidth = 0.05)
Plotsulphates <- qplot(x = sulphates, data = wqr, binwidth = 0.03)
Plotalcohol <- qplot(x = alcohol, data = wqr, binwidth = 0.1) + 
  scale_x_continuous(breaks = seq(8.4,14.9,1))

grid.arrange(Plotquality, Plotfixed.acidity, Plotvolatile.acidity, 
             Plotcitric.acid, Plotresidual.sugar, Plotchlorides,
             Plotfree.sulfur.dioxide, Plottotal.sulfur.dioxide, Plotdensity,
             PlotpH, Plotsulphates, Plotalcohol, ncol = 3)
```

Many of the variables look normally distributed. Chlorides, sulphates, alcohol, free sulfur dioxide and total sulfur dioxide look like they have lognormal distributions. Let’s exclude the 95th percentile for all these five features and re-plot their histograms: 

```{r echo=FALSE, Univariate_Plots3, fig.height=5, fig.width=10, warning=FALSE}

Plotchlorides_95 <- qplot(x = chlorides, data = wqr, binwidth = 0.01) + 
  xlim(0.03, quantile(wqr$chlorides, c(0.95)))

Plotfree.sulfur.dioxide_95 <- qplot(x = free.sulfur.dioxide, data = wqr, binwidth = 1) +
  xlim(0, quantile(wqr$free.sulfur.dioxide, c(0.95)))

Plottotal.sulfur.dioxide_95 <- qplot(x = total.sulfur.dioxide, data = wqr, binwidth = 3) +
  xlim(0, quantile(wqr$total.sulfur.dioxide, c(0.95)))

Plotsulphates_95 <- qplot(x = sulphates, data = wqr, binwidth = 0.03) +
  xlim(0.3, quantile(wqr$sulphates, c(0.95)))

Plotalcohol_95 <- qplot(x = alcohol, data = wqr, binwidth = 0.1) + 
  xlim(8.9, quantile(wqr$alcohol, c(0.95)))

grid.arrange(Plotchlorides_95, Plotfree.sulfur.dioxide_95, 
             Plottotal.sulfur.dioxide_95, Plotsulphates_95, 
             Plotalcohol_95, ncol = 3)
```

# Univariate Analysis

### What is the structure of your dataset?
Number of red wine instances: 1599
Number of Attributes: 1 Serial Number + 11 Attributes + 1 Output Attribute

11 Attributes:
```
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
```
Output variable (based on sensory data):
```
 12 - quality (score between 0 and 10)
```
### What is/are the main feature(s) of interest in your dataset?
Quality is the main feature.

### What other features in the dataset do you think will help support your investigation into your feature(s) of interest?

Residual sugar, fixed acidity, pH, density and alcohol content may help support the investigation into the quality.

### Did you create any new variables from existing variables in the dataset?
No, I didn't.

### Of the features you investigated, were there any unusual distributions? Did you perform any operations on the data to tidy, adjust, or change the form of the data? If so, why did you do this?
Attributes of chlorides, total sulfur dioxide, and free sulfur dioxide, sulphates, alcohol were all appeared to be long tailed and were log-transformed which revealed a normal distribution for each.

# Bivariate Plots Section
```{r echo=FALSE, Bivariate_Plots1, fig.height=10, fig.width=10, warning=FALSE}
names(wqr[2:13])
ggscatmat(wqr, columns = 2:13) + theme_bw(base_size=12)
```
With our main feature of the dataset, the positive correlation coefficients which are more then 0.1 are:
``` 
 alchol:quality = 0.48
 sulphates:quality = 0.25
 citric.acid:quality = 0.23
 fixed.acidity:quality = 0.12
```
So alcohol content has a high correlation with red wine quality. Other important attributes correlated with red wine quality include sulphates, citric acid and fixed acidity.

```{r echo=FALSE, Bivariate_Plots2, fig.height=5, fig.width=10, warning=FALSE}
ggplot(wqr, aes(x=quality, y = alcohol)) + 
  geom_bin2d(bins = 15)
```

With our main feature of the dataset, the negative correlation coefficients which are less then -0.1 are:
```
 volatile.acidity:quality = -0.39
 total.sulfur.dioxide:quality = -0.19
 density:quality = -0.17
 chlorides:quality = -0.13
```
So we see that volatile acids are negatively correlated with red wine quality, as described from the document that is at too high of levels can lead to an unpleasant, vinegar taste. Total sulfur dioxide, density and chlorides are also negatively correlated with quality.

Besides, other attributes wiht the highest (positive or negative) correlation are:
```
 fixed.acidity:pH = -0.68
 fixed.acidity:citric.acid = 0.67
 fixed.acidity:density = 0.67
 free.sulfur.dioxide:total.sulfur.dioxide = 0.67
 volatile.acidity:citirc.acid = -0.55
 citric.acid:pH = -0.54
 density:alcohol = -0.50
```
As we all know, the stronger the acid is, the lower pH will be. So it is make sence that either fixed acidity or citric acid has a high negative correlation with pH. 
I will focus on several other highest correlation relationships in a bit more detail.

#### "Fixed Acidity VS Citric Acid" and "Volatile Acidity VS Citirc Acid"
```{r echo=FALSE, Bivariate_Plots3, fig.height=5, fig.width=10, warning=FALSE}
fixedAcidityVScitricAcid <- ggplot(wqr, aes(fixed.acidity, citric.acid)) + 
  geom_jitter(alpha=0.2) + 
  stat_smooth(method = "lm")

volatileAcidityVScitircAcid <- ggplot(wqr, aes(volatile.acidity, citric.acid)) + 
  geom_jitter(alpha=0.2) + 
  stat_smooth(method = "lm")

grid.arrange(fixedAcidityVScitricAcid, volatileAcidityVScitircAcid, ncol = 1)
```

#### "Fixed Acidity VS Density" and "Alcohol VS Density"
```{r echo=FALSE, Bivariate_Plots4, fig.height=5, fig.width=10, warning=FALSE}
fixedAcidityVSDensity <- ggplot(wqr, aes(fixed.acidity, density)) + 
  geom_jitter(alpha=0.2) + 
  stat_smooth(method = "lm")

alcoholVSDensity <- ggplot(wqr, aes(alcohol, density)) + 
  geom_jitter(alpha=0.2) + 
  stat_smooth(method = "lm")

grid.arrange(fixedAcidityVSDensity, alcoholVSDensity, ncol = 1)
```

# Bivariate Analysis

### Talk about some of the relationships you observed in this part of the investigation. How did the feature(s) of interest vary with other features in the dataset?
As of the quality, it appears that when alchol or sulphates is in higher amounts, the quality will be better also. However, the amount of volatile acidity is negatively correlated with the quality. It is likely that fresher wines avoid the bitter taste of acetic acid.

### Did you observe any interesting relationships between the other features (not the main feature(s) of interest)?
As of citric acid, fixed acidity is positively correlated with the citric acid, but the amount of volatile acidity is opposite.
As of density, fixed acidity is also positively correlated with the citric acid, but the amount of alcohol is opposite.

### What was the strongest relationship you found?
From the variables analyzed, the strongest relationship was between fixed.acidity and pH, which had a correlation coefficient of 0.68.


# Multivariate Plots Section
Now let’s visualize the relationship between sulphates, volatile.acidity, alcohol and quality: 
```{r echo=FALSE, Multivariate_Plots1, fig.height=5, fig.width=10, warning=FALSE}
ggplot(wqr, aes(x=volatile.acidity, y=sulphates, size = alcohol, 
                 color=factor(quality))) + 
  ylim(0.3, quantile(wqr$sulphates, c(0.95))) +
  geom_point(alpha = 0.2) +
  scale_color_brewer()
```
Let’s try to summarize quality using a contour plot of volatile acidity and sulphate content: 
```{r echo=FALSE, Multivariate_Plots2, fig.height=5, fig.width=10, warning=FALSE}
ggplot(wqr, aes(x = volatile.acidity, y = sulphates, 
                 colour=factor(wqr$quality))) + 
  ylim(0.3, quantile(wqr$sulphates, c(0.95))) +
  geom_density2d(bins=2) + 
  scale_color_brewer() + 
  geom_point(color="black", alpha=0.1) 
```
Let’s try to summarize quality using a contour plot of citric acid and alcohol content: 
```{r echo=FALSE, Multivariate_Plots3, fig.height=5, fig.width=10, warning=FALSE}
ggplot(wqr, aes(x = citric.acid, y = alcohol, 
                 colour=factor(wqr$quality))) + 
  ylim(8.9, quantile(wqr$alcohol, c(0.95))) +
  geom_density2d(bins=2) + 
  scale_color_brewer() + 
  geom_point(color="black", alpha=0.1) 
```

# Multivariate Analysis

### Talk about some of the relationships you observed in this part of the investigation. Were there features that strengthened each other in terms of looking at your feature(s) of interest?

Based on the multivariate analysis, five features stood out to me: alcohol, sulphates, citric acid, volatile acidity, and quality. Volatile acidity with amount between 0.3 and 0.5 and sulphates with amount between 0.6 and 0.9 were a strong indicator of the presence of good wine. Also, high alcohol content and higher citric acid have more chance to make for a good wine.

# Final Plots and Summary

### Plot One
As analyzing relationship between quality and other 11 attributes, the strongest correlation coefficient was found between alcohol and quality.
```{r echo=FALSE, Plot_One, fig.height=5, fig.width=10, warning=FALSE}
ggplot(wqr, aes(x=factor(quality), y = alcohol, fill = factor(quality))) +
  geom_boxplot(alpha = 0.3) +
  labs(title = "Box plot of alcohol content by red wine quality ratings", 
       x = "Quality (score between 0 and 10)",
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
ggplot(data = wqr, aes(x = sulphates, y = alcohol, color = quality)) +
  geom_point(alpha = 0.6, size = 1.2, position = 'jitter') +
  facet_wrap(~quality) +
  labs(x = 'Sulphates (log10(g / dm^3))', y = 'Alcohol (% by Volume)') +
  ggtitle('Scatter plots of Alcohol vs Sulphates faceted by quality') +
  theme_bw()
```

### Description Two
Observe that lower sulphates content typically leads to a bad wine with alcohol varying between 9% and 12%. Average wines have higher concentrations of sulphates, however wines that are rated 6 tend to have higher alcohol content and larger sulphates content. Excellent wines are mostly clustered around higher alcohol contents and higher sulphate contents. 

### Plot Three
```{r echo=FALSE, Plot_Three, fig.height=5, fig.width=10, warning=FALSE}
ggplot(wqr, aes(x = volatile.acidity, y = alcohol, colour=factor(wqr$quality))) + 
  geom_density2d(bins=2) + 
  scale_color_brewer() + 
  geom_point(color="black", alpha=0.1) +
  labs(x = 'Volatile Acidity (acetic acid - g / dm^3)', y = 'Alcohol (% by Volume)') +
  ggtitle('Quality contour plot of Alcohol vs Volatile Acidity') +
  theme_bw()
  
```

### Description Three
This shows that higher quality red wines are generally located near the range from 0.25 to 0.65 of citric acid and slso near the higher alcohol which is more than 10.5%. Whereas lower quality red wines are generally with lower either alcohol or citric acid.

------

# Reflection
The red wine dataset contains information on 1,599 red wine instances, 11 attributes and one output attribute. Initially, I tried to get a sense of how is each attribute changing on their own. All univariate plots have been arranged together. Many of the variables look normally distributed. However, chlorides, sulphates, alcohol, free sulfur dioxide and total sulfur dioxide look like they have lognormal distributions. So I exclude the 95th percentile for all above five features and re-plot their histograms.

Then, I tried to find what factors might affect the quality of the wine. At this moment, pearson correlation coefficient can help us to visualize the relationship between each pair of variables. Using the insights from correlation coefficients provided by the paired plots, it was interesting exploring quality using box plots with a different color for each quality. Besides, melting the dataframe and using facet grids was really helpful for visualizing the distribution of the parameters with the use of scatter plots. Finally, using a contour plot of wine quality with a point plot of volatile acidity and alcohol would be a good choice to show that either the lower volatile acidity or higher alcohol have more possible to make a better wine. The result makes sense. Volatile acidity is mostly caused by bacteria in the wine which is the amount of acetic acid in wine. It can lead to an unpleasant, vinegar taste if at too high of levels.

# Citation
P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. 
  Modeling wine preferences by data mining from physicochemical properties.
  In Decision Support Systems, Elsevier, 47(4):547-553. ISSN: 0167-9236.
