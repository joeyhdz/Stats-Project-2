
<hr>
<center>
<h2>
Project 2
</h2>
</center>
<hr>

# Loading Initial Packages For Cleaning:

<hr>

``` r
#install.packages('janitor')
#install.packages('aplore3')

library(aplore3) # for our dataset
library(tidyverse) # tools for viz/cleaning/etc
library(janitor) # tools for cleaning
library(visdat) # visualize our missing data
```

<hr>

# Data Inspection

<hr>

Takeaways:

- Looking at the data we see that some of the weight values(in KG) are
  very low. assuming the low weight belongs to individuals who are
  older, maybe bed ridden, and have instances of sarcopenia this is
  plausible… but also it’s good to keep this in mind moving forward.

- raterisk: This is a completely subjective topic. maybe interesting to
  see, but not expected that this will provide much insight.

- Duplicate data: There is instances of duplicated data in the site_id,
  phy_id. This makes sense since many sites will occur with repeating
  metrics and many physicians will also occur with those same metrics.

- smoke has a very small sample of “Yes” (35)

``` r
# adding dataset into df call.
df <- glow_bonemed  

# looking at our data from afar 
#glimpse(df) # alot of categorical vars (factor encoding)

# for a look at a brief data description uncomment lines below:
#?glow_bonemed 
#?glow500


# check for duplicated data
# get_dupes(df, sub_id) # no duplicated data here which is what we'd like to see. 
# get_dupes(df, site_id) # makes sense that we would have duplicated study sites.
# get_dupes(df, phy_id) # makes sense that we would have duplicated physician id codes.

# check for missing values
#vis_miss(df) # great! No missing values. 

# visualizing the summary of our data. 
summary(df)
```

    ##      sub_id         site_id          phy_id       priorfrac      age       
    ##  Min.   :  1.0   Min.   :1.000   Min.   :  1.00   No :374   Min.   :55.00  
    ##  1st Qu.:125.8   1st Qu.:2.000   1st Qu.: 57.75   Yes:126   1st Qu.:61.00  
    ##  Median :250.5   Median :3.000   Median :182.50             Median :67.00  
    ##  Mean   :250.5   Mean   :3.436   Mean   :178.55             Mean   :68.56  
    ##  3rd Qu.:375.2   3rd Qu.:5.000   3rd Qu.:298.00             3rd Qu.:76.00  
    ##  Max.   :500.0   Max.   :6.000   Max.   :325.00             Max.   :90.00  
    ##      weight           height           bmi        premeno   momfrac   armassist
    ##  Min.   : 39.90   Min.   :134.0   Min.   :14.88   No :403   No :435   No :312  
    ##  1st Qu.: 59.90   1st Qu.:157.0   1st Qu.:23.27   Yes: 97   Yes: 65   Yes:188  
    ##  Median : 68.00   Median :161.5   Median :26.42                                
    ##  Mean   : 71.82   Mean   :161.4   Mean   :27.55                                
    ##  3rd Qu.: 81.30   3rd Qu.:165.0   3rd Qu.:30.79                                
    ##  Max.   :127.00   Max.   :199.0   Max.   :49.08                                
    ##  smoke        raterisk     fracscore      fracture  bonemed   bonemed_fu
    ##  No :465   Less   :167   Min.   : 0.000   No :375   No :371   No :361   
    ##  Yes: 35   Same   :186   1st Qu.: 2.000   Yes:125   Yes:129   Yes:139   
    ##            Greater:147   Median : 3.000                                 
    ##                          Mean   : 3.698                                 
    ##                          3rd Qu.: 5.000                                 
    ##                          Max.   :11.000                                 
    ##  bonetreat
    ##  No :382  
    ##  Yes:118  
    ##           
    ##           
    ##           
    ## 

<hr>
<center>
<h2>
EDA
</h2>
</center>
<hr>

# EDA - Categorical

Step one: Visualize what the Yes/Nos look like in terms of fractures.

- The plot below will show us without yet incorporating predictors, what
  is the percentages of yes/no that we are seeing in the data.

- Findings are that it’s not a very balanced split. 75% of the data have
  NO for fracture. when we make predictive probabilities how will this
  influence the result? will we find that they are around this value
  with some higher/lower? *potential sanity check to our results later*

- Additional Note… Unbalanced data is OK.. this should maybe be revisted
  in our thresholding

``` r
library(ggplot2)
library(gridExtra)

c <- df %>% # allows us to gather table of y/n fracture, the cnt, and percent. 
  group_by(fracture) %>% 
  summarise(cnt = n()) %>% 
  mutate(perc=round(cnt/sum(cnt),4))

#c # shows the result of the above

# bar plot visual of the above. 
p<- ggplot(c, aes(x = fracture, y = perc, colour = fracture)) + 
  geom_bar(aes(fill = fracture), show.legend = F, stat = 'identity') +
  ylab('Proportion of Fracture') + xlab("Fracture") +
  geom_text(aes(label = paste0(perc *100, "%")), vjust = 2, size = 5, color = 'black')
```

Fracture - Menopause before age 45

- When and individual did not have menopause before age 45
  (pre-menopause) the chances of having a fracture are much higher even
  though there is only a 25% of having a fracture.

- If you are in the no premenopause group, it is approx 80% chance that
  you have a fracture. it decreases as for those who did have
  premenopause.

``` r
c1 <- df %>% 
  group_by(fracture, premeno) %>% 
  summarise(cnt=n()) %>% 
  mutate(perc=round(cnt/sum(cnt),4))%>% 
  arrange(desc(perc))

#c1

p1<- ggplot(c1[c(2,3),], aes(x = reorder(premeno, -perc), y = perc, colour = premeno))+
  geom_bar(aes(fill = premeno), show.legend = F, stat = 'identity') + 
  ylab('Proportion of Fracture') +
  xlab('Menopause before age 45')+
  geom_text(aes(label = paste0(perc *100, "%")), vjust = 2, size = 5, color = 'black')
```

Fracture - momfrac (mother had hip fracture)

- When an individual did not have a mother that had a hip fracture the
  chances of having a fracture are much higher (80.8%) even though there
  is only a 25% of having a fracture.

- If an individual is in the “Mother did not have a hip fracture” group,
  there is an 80.8% chance that you will have a fracture. This decreases
  for those who did have a mother with a hip fracture.

``` r
c2 <- df %>% 
  group_by(fracture, momfrac) %>%
  summarise(cnt=n()) %>% 
  mutate(perc = round(cnt/sum(cnt),4)) %>%
  arrange(desc(perc))

#c2

p2<- ggplot(c2[c(2,3),], aes(x = reorder(momfrac, -perc), y = perc, color = momfrac)) + 
  geom_bar(aes(fill = momfrac), show.legend = F, stat = 'identity') + 
  ylab('Proportion of Fracture') + 
  xlab("Mother had Hip fracture") + 
  geom_text(aes(label = paste0(perc * 100, "%")), vjust = 2, size = 5, color = "black")
```

Fracture - smoke (current or former smoker)

- When an individual falls in the category of former/current smoker the
  chances of having a fracture are much higher(94.4%) even though there
  is only a 25% of having a fracture.

``` r
c3 <- df %>% 
  group_by(fracture, smoke) %>%
  summarise(cnt=n()) %>% 
  mutate(perc = round(cnt/sum(cnt),4)) %>%
  arrange(desc(perc))

#c3

p3<- ggplot(c3[c(1,4),], aes(x = reorder(smoke, -perc), y = perc, color = smoke)) + 
  geom_bar(aes(fill = smoke), show.legend = F, stat = 'identity') + 
  ylab('Proportion of Fracture') + 
  xlab("Former/Current Smoker") + 
  geom_text(aes(label = paste0(perc * 100, "%")), vjust = 1, size = 5, color = "black")
```

fracture - raterisk (self-reported risk of fracture: classified by the
following groups…less than others of same age, same as others of same
age, greater than others of same age)

- An issue with this is that it’s a very subjective measure. WHY are
  they rating this? is it based off non-inclusive criteria that the
  physician provided? is it based on their own physical comparison with
  peers? is it simply low-self esteem/ high self esteem? I would imagine
  that from a predictive model stand point this is NOT something that
  should be included.

``` r
c4 <- df %>% 
  group_by(fracture, raterisk) %>%
  summarise(cnt=n()) %>% 
  mutate(perc = round(cnt/sum(cnt),4)) %>%
  arrange(desc(perc))

#c4

p4<- ggplot(c4[c(1,2,6),], aes(x = reorder(raterisk, -perc), y = perc, color = raterisk)) + 
  geom_bar(aes(fill = raterisk), show.legend = F, stat = 'identity') + 
  ylab('Proportion of Fracture') + 
  xlab("Self-Risk-Score") + 
  geom_text(aes(label = paste0(perc * 100, "%")), vjust = 2, size = 4, color = "black")
```

fracture - bonemed (bone medications at enrollment)

- When an individual is in the category of No medication at enrollment
  the chances of having a fracture are much higher(63.2%) even though
  there is only a 25% of having a fracture.

``` r
c5 <- df %>% 
  group_by(fracture, bonemed) %>%
  summarise(cnt=n()) %>% 
  mutate(perc = round(cnt/sum(cnt),4)) %>%
  arrange(desc(perc))

#c5

p5<- ggplot(c5[c(2,3),], aes(x = reorder(bonemed, -perc), y = perc, color = bonemed)) + 
  geom_bar(aes(fill = bonemed), show.legend = F, stat = 'identity') + 
  ylab('Proportion of Fracture') + 
  xlab("Medication at Enrollment") + 
  geom_text(aes(label = paste0(perc * 100, "%")), vjust = 2, size = 5, color = "black")
```

fracture - bonemed_fu (bone medications at follow-up)

- When an individual is in the category of No medication at Follow-up
  the chances of having a fracture are higher(57.6% vs 42.4%) even
  though there is only a 25% of having a fracture.

``` r
c6 <- df %>% 
  group_by(fracture, bonemed_fu) %>%
  summarise(cnt=n()) %>% 
  mutate(perc = round(cnt/sum(cnt),4)) %>%
  arrange(desc(perc))

#c6

p6<-ggplot(c6[c(2,3),], aes(x = reorder(bonemed_fu, -perc), y = perc, color = bonemed_fu)) + 
  geom_bar(aes(fill = bonemed_fu), show.legend = F, stat = 'identity') + 
  ylab('Proportion of Fracture') + 
  xlab("Medication at Follow-up") + 
  geom_text(aes(label = paste0(perc * 100, "%")), vjust = 2, size = 5, color = "black")
```

Fracture - bonetreat (bone medications both at enrollment and follow-up)

- When an individual is in the category of No treatment at both
  enrollment and follow-up the chances of having a fracture are much
  higher(68% vs. 32%) even though there is only a 25% of having a
  fracture.

``` r
c7 <- df %>% 
  group_by(fracture, bonetreat) %>%
  summarise(cnt=n()) %>% 
  mutate(perc = round(cnt/sum(cnt),4)) %>%
  arrange(desc(perc))

#c7

p7<-ggplot(c7[c(2,3),], aes(x = reorder(bonetreat, -perc), y = perc, color = bonetreat)) + 
  geom_bar(aes(fill = bonetreat), show.legend = F, stat = 'identity') + 
  ylab('Proportion of Fracture') + 
  xlab("Treatment: Enrollment & Follow-up") + 
  geom_text(aes(label = paste0(perc * 100, "%")), vjust = 2, size = 5, color = "black")
```

Fracture - Arms are needed to stand from a chair

- Very balanced between the two groups.

- When an individual is in the category of needing assistance to stand
  the chances of having a fracture are higher(50.4% vs 49.6%) even
  though there is only a 25% of having a fracture.

``` r
c8 <- df %>% 
  group_by(fracture, armassist) %>% 
  summarise(cnt=n()) %>% 
  mutate(perc=round(cnt/sum(cnt),4))%>% 
  arrange(desc(perc))

#c8

p8<- ggplot(c8[c(2,3),], aes(x = reorder(armassist, -perc), y = perc, colour = armassist))+
  geom_bar(aes(fill = armassist), show.legend = F, stat = 'identity') + 
  ylab('Proportion of Fracture') +
  xlab('Assistance to Stand')+
  geom_text(aes(label = paste0(perc *100, "%")), vjust = 2, size = 5, color = 'black')
```

fracture - priorfrac (history of prior fracture)

- When an individual is in the category of NOT having had a prior
  fracture the chances of having a fracture are higher(58.4% vs 41.6%)
  even though there is only a 25% of having a fracture.

``` r
c9 <- df %>% 
  group_by(fracture, priorfrac) %>%
  summarise(cnt=n()) %>% 
  mutate(perc = round(cnt/sum(cnt),4)) %>%
  arrange(desc(perc))

#c9

p9<-ggplot(c9[c(2,3),], aes(x = reorder(priorfrac, -perc), y = perc, color = priorfrac)) + 
  geom_bar(aes(fill = priorfrac), show.legend = F, stat = 'identity') + 
  ylab('Proportion of Fracture') + 
  xlab("Prior Fracture") + 
  geom_text(aes(label = paste0(perc * 100, "%")), vjust = 2, size = 5, color = "black")
```

Fracture - bmi - body mass index

- Some domain expertise here: there is evidence that individuals who are
  heavier tend to have less instances of osteoperosis, and hip/bone
  fractures. this is unsuprising to see here and is a good depiction of
  prior research.

- Including this variable into the model should be something that is
  significat based on the trend we see, although the instance of
  “underwieght” is interesting and may require some investigation.

- When an individual is in the category “Healthy Weight” the chances of
  having a fracture are highest (35.2%), followed by Overweight (32.8%),
  followed by Obesity(30.4%), and lastly Underweight (1.6%).

``` r
# transforming numeric into categorical using widley accepted BMI categories
df$bmi.cat <- ifelse(df$bmi < 18.5, "Underweight", 
                              ifelse(df$bmi < 25, "Healthy weight",
                                     ifelse(df$bmi < 30, "Overweight", "Obesity")))

c10 <- df %>% 
  group_by(fracture, bmi.cat) %>% 
  summarise(cnt=n()) %>% 
  mutate(perc=round(cnt/sum(cnt),4))%>% 
  arrange(desc(perc))

#c10

p10<- ggplot(c10[c(2,3,5,8),], aes(x = reorder(bmi.cat, -perc), y = perc, colour = bmi.cat))+
  geom_bar(aes(fill = bmi.cat), show.legend = F, stat = 'identity') + 
  ylab('Proportion of Fracture') +
  xlab('BMI Categories')+
  geom_text(aes(label = paste0(perc *100, "%")), vjust = 1, size = 4, color = 'black')
```

<hr>

# Visualizing Categorical Plots

<hr>

``` r
library(grid)
g1<- grid.arrange(p,p1,p2,p3,p4,
             heights = c(1,1))
```

<img src="EDA---Models-Only_files/figure-gfm/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

``` r
g2<- grid.arrange(p5,p6,p7,p8,p9,p10,
             heights= c(1,1),
             widths = c(1,1,1.6))
```

<img src="EDA---Models-Only_files/figure-gfm/unnamed-chunk-14-2.png" style="display: block; margin: auto;" />

``` r
g1
```

    ## TableGrob (2 x 3) "arrange": 5 grobs
    ##   z     cells    name           grob
    ## 1 1 (1-1,1-1) arrange gtable[layout]
    ## 2 2 (1-1,2-2) arrange gtable[layout]
    ## 3 3 (1-1,3-3) arrange gtable[layout]
    ## 4 4 (2-2,1-1) arrange gtable[layout]
    ## 5 5 (2-2,2-2) arrange gtable[layout]

``` r
g2
```

    ## TableGrob (2 x 3) "arrange": 6 grobs
    ##   z     cells    name           grob
    ## 1 1 (1-1,1-1) arrange gtable[layout]
    ## 2 2 (1-1,2-2) arrange gtable[layout]
    ## 3 3 (1-1,3-3) arrange gtable[layout]
    ## 4 4 (2-2,1-1) arrange gtable[layout]
    ## 5 5 (2-2,2-2) arrange gtable[layout]
    ## 6 6 (2-2,3-3) arrange gtable[layout]

``` r
#ggsave("g1.jpg", g1)
#ggsave("g2.jpg",g2)
```

<hr>

# EDA - Numerical

<hr>

We would like to look at some of the relationships for the numerical
categories in our data. Before we can do this it is important for us to
create a numerical category for fracture so that we can create our LOESS
plot.

- Note to smooth the curve out some, we can use span values (such as
  1.25), either way this is artificial humps and bumps, try not to pay
  attention to the overall movements and look instead at the
  trend/association of how the data moves with increasing/decreasing
  values.

- Note for complex models We can add categorical values to this
  (interactions) \| also facet wrapping will allow us to view them
  separate.

``` r
# numercial form of fracture
df$fracture.num <- ifelse(df$fracture == "Yes",1,0) # Yes = 1 | No = 0
```

LOESS phy_id:

- No significance here

``` r
lp <- df %>% ggplot(aes(x = phy_id, y = fracture.num)) + 
  geom_point() + ggtitle("LOESS: Phy_ID")+
  geom_smooth(method = "loess", size = 1) + ylim(-.2, 1.2)
```

LOESS site_id:

- No significance here.

``` r
lp1<- df %>% ggplot(aes(x = site_id, y = fracture.num)) + 
  geom_point() + 
  geom_smooth(method = "loess", size = 1) + ylim(-.2, 1.2)+ ggtitle("LOESS: Site_id")
```

LOESS WEIGHT:

- No significance here

``` r
lp2<- df %>% ggplot(aes(x = weight, y = fracture.num)) + 
  geom_point() + 
  geom_smooth(method = "loess", size = 1) + ylim(-.2, 1.2)+ ggtitle("LOESS: Weight")
```

LOESS AGE:

- As age increases the is an increase in the chance of a fracture

``` r
lp3<- df %>% ggplot(aes(x = age, y = fracture.num)) +
  geom_point() +
  geom_smooth(method = "loess", size = 1) + ylim(-.2, 1.2)+ ggtitle("LOESS: Age")
```

LOESS HEIGHT:

- As Height decreases there is a decrease in the chance of a fracture

``` r
lp4<- df %>% ggplot(aes(x = height, y = fracture.num)) + 
  geom_point()+
  geom_smooth(method = "loess", size = 1) + ylim(-.2, 1.2)+ ggtitle("LOESS: Height")
```

LOESS fracscore (Fracture Risk Score (Composite Risk Score)):

- As the score increases there is an increase in the chance of a
  fracture.

``` r
lp5<- df %>% ggplot(aes(x = fracscore, y = fracture.num)) + 
  geom_point()+
  geom_smooth(method = "loess", size = 1) + ylim(-.2, 1.2)+ ggtitle("LOESS: fracscore")
```

``` r
lp6 <- df %>% ggplot(aes(x = bmi, fracture.num)) + 
  geom_point()+
  geom_smooth(method = 'loess', size = 1) + ylim(-.2,1.2) + ggtitle("Loess: BMI")
```

# Numerical Plots

``` r
g3<- grid.arrange(lp,lp1,lp2,ncol(2) )
```

<img src="EDA---Models-Only_files/figure-gfm/unnamed-chunk-23-1.png" style="display: block; margin: auto;" />

``` r
g4<- grid.arrange(lp3,lp4,lp5,lp6,
             heights = c(1,1))
```

<img src="EDA---Models-Only_files/figure-gfm/unnamed-chunk-23-2.png" style="display: block; margin: auto;" />

``` r
# ggsave("g3.jpg",g3)
# ggsave("g4.jpg",g4)
```

# Correlation Plot

<h4>
Correlation Plot
</h4>

``` r
library(ggcorrplot)
library(GGally)
num.df <- df

# ordinal categorical into numerical
rr.mapping <- c(Less = -1,Same = 0,Greater=1)
num.df$raterisk.num <- as.numeric(rr.mapping[num.df$raterisk])


# create a dataframe that is only numerical
num.df <- num.df %>% select(where(is.numeric))

ggpairs(num.df, ggplot2::aes(color = as.factor(fracture.num)))
```

<img src="EDA---Models-Only_files/figure-gfm/unnamed-chunk-24-1.png" style="display: block; margin: auto;" />

``` r
# remove irrelevant columns
num.df.adj <- num.df[,-c(1,2,3,9)]

# create correlation plot

# calling cor function to put into var. 
cor.data <- cor(num.df.adj)

# generate the plot
ggcorrplot(cor.data, outline.color = "black", lab = TRUE, title = 'Fracture Correlation Plot')
```

<img src="EDA---Models-Only_files/figure-gfm/unnamed-chunk-24-2.png" style="display: block; margin: auto;" />

<h4>
Test/Validation Split
<h4>

- Because we have such a small dataset we will use a training size of
  70% and test split of 30%. This is some what of a more aggressive
  split but should help to ensure we have an adequate amount of testing
  data.

``` r
library(caret)
set.seed(12)
trainIndex <- createDataPartition(df$fracture, p= .7, list = F) # p = proportion of data in train

training <- df[trainIndex,]
test <- df[-trainIndex,]

# sanity check
#nrow(training)
#nrow(test)
```

<hr>

# OBJECTIVE 1

<hr>
<h4>
Fit Logistic Regression Model:
</h4>
<h4>
Complete Additive Model:
</h4>

- Using all predictors to understand at a baseline what is “significant”
  while all things are included in the model.

- Note that this may leave out predictors that would otherwise be
  present if not for potential VIF interventions. Weight, AGE, SITE_ID,
  PHY_ID, ARMASSIST, FRACSCORE, BONETREAT all have relatively high VIF
  in this model. although not all warrent removal we will proceed with
  caution and awareness.

``` r
library(ResourceSelection)
library(car)

# removing sub_id, bmi (numerical) ,fracture.num
adj.training <- training[,-c(1,8,20)]

base.model <- glm(fracture~., family = "binomial", data = adj.training)

#summary(base.model)

# anova 
#anova(base.model, test = "Chisq")

vif(base.model)
```

    ##                 GVIF Df GVIF^(1/(2*Df))
    ## site_id    21.927561  1        4.682687
    ## phy_id     21.920574  1        4.681941
    ## priorfrac   3.063436  1        1.750267
    ## age        21.915495  1        4.681399
    ## weight      7.015286  1        2.648638
    ## height      1.726289  1        1.313883
    ## premeno     1.143523  1        1.069357
    ## momfrac     2.186617  1        1.478721
    ## armassist   7.750214  1        2.783921
    ## smoke       1.520721  1        1.233175
    ## raterisk    1.260181  2        1.059518
    ## fracscore  42.413696  1        6.512580
    ## bonemed     7.141448  1        2.672349
    ## bonemed_fu  4.904575  1        2.214627
    ## bonetreat  11.369973  1        3.371939
    ## bmi.cat     6.785284  3        1.375925

``` r
AIC(base.model)
```

    ## [1] 395.9281

# GLM PROCESS

<h4>
Penalized Regression Process
</h4>

- site_id, priorfrac, momfrac, raterisk, fracscore, bonemed_fu, bmi.cat

``` r
set.seed(12)

fitControl<- trainControl(method = "repeatedcv", number = 5, repeats = 1)

glm.fit <- train(fracture~.,
                 data = adj.training,
                 method = "glmnet",
                 trControl = fitControl)

glm.fit
```

    ## glmnet 
    ## 
    ## 351 samples
    ##  16 predictor
    ##   2 classes: 'No', 'Yes' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold, repeated 1 times) 
    ## Summary of sample sizes: 282, 281, 280, 280, 281 
    ## Resampling results across tuning parameters:
    ## 
    ##   alpha  lambda        Accuracy   Kappa     
    ##   0.10   0.0001916456  0.7209862  0.06355342
    ##   0.10   0.0019164565  0.7182107  0.05311766
    ##   0.10   0.0191645646  0.7324171  0.07395306
    ##   0.55   0.0001916456  0.7209862  0.06355342
    ##   0.55   0.0019164565  0.7153938  0.04882086
    ##   0.55   0.0191645646  0.7352743  0.04894156
    ##   1.00   0.0001916456  0.7209862  0.06355342
    ##   1.00   0.0019164565  0.7211081  0.06937412
    ##   1.00   0.0191645646  0.7408252  0.02769993
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final values used for the model were alpha = 1 and lambda = 0.01916456.

``` r
plot(glm.fit)
```

![](EDA---Models-Only_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
opt.pen<- glm.fit$finalModel$lambdaOpt

coef(glm.fit$finalModel, opt.pen)
```

    ## 20 x 1 sparse Matrix of class "dgCMatrix"
    ##                              s1
    ## (Intercept)        -1.900607889
    ## site_id             0.009733071
    ## phy_id              .          
    ## priorfracYes        0.272056288
    ## age                 .          
    ## weight              .          
    ## height              .          
    ## premenoYes          .          
    ## momfracYes          0.003143572
    ## armassistYes        .          
    ## smokeYes            .          
    ## rateriskSame        .          
    ## rateriskGreater     0.299140298
    ## fracscore           0.123501015
    ## bonemedYes          .          
    ## bonemed_fuYes       0.397052793
    ## bonetreatYes        .          
    ## bmi.catObesity      .          
    ## bmi.catOverweight   0.021376094
    ## bmi.catUnderweight -0.507318339

# GLM ADDITIVE Model

``` r
library(pROC)
```

    ## Warning: package 'pROC' was built under R version 4.2.2

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
# parameters for train function
fitControl <- trainControl( method = "repeatedcv", number = 5, repeats = 1, classProbs = T, summaryFunction = mnLogLoss)

# set seed for reproduceability 
set.seed(12)


# training CARET mult. logi. regression model 
red.glm <- train(fracture~site_id+priorfrac+momfrac+raterisk+fracscore+bonemed_fu+bmi.cat,
                 data = training,
                 trControl = fitControl,
                 method = "glm",
                 family = "binomial",
                 metric = "logLoss")

summary(red.glm)
```

    ## 
    ## Call:
    ## NULL
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5901  -0.7454  -0.5845   0.2054   2.2605  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -2.79912    0.46677  -5.997 2.01e-09 ***
    ## site_id             0.06782    0.07194   0.943  0.34583    
    ## priorfracYes        0.30724    0.32818   0.936  0.34917    
    ## momfracYes          0.26969    0.36568   0.738  0.46081    
    ## rateriskSame        0.09047    0.33039   0.274  0.78422    
    ## rateriskGreater     0.55621    0.34620   1.607  0.10814    
    ## fracscore           0.16674    0.06281   2.655  0.00794 ** 
    ## bonemed_fuYes       0.61899    0.29935   2.068  0.03866 *  
    ## bmi.catObesity      0.48634    0.33279   1.461  0.14390    
    ## bmi.catOverweight   0.50820    0.32600   1.559  0.11902    
    ## bmi.catUnderweight -1.44297    1.16820  -1.235  0.21675    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 395.31  on 350  degrees of freedom
    ## Residual deviance: 360.98  on 340  degrees of freedom
    ## AIC: 382.98
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# make preds on the probabilty of each class in TRANING data
glm.predprob <- predict(red.glm, test, type = "prob")

# compute the ROC curve
glm.roc <- roc(response = test$fracture, predictor = glm.predprob$Yes, levels = c("Yes","No"))
```

    ## Setting direction: controls > cases

``` r
# plot ROC curve
plot(glm.roc, print.thres = "best", print.thres.best.method = "closest.topleft", col = "red")
legend("bottomright",# add legend to plot
       legend = 'caret model',
       col = "red",
       lwd = 4, cex = 1, xpd = T, horiz = F)
```

![](EDA---Models-Only_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
# fitting model for AIC below
red.glm <- glm(fracture~site_id+priorfrac+momfrac+raterisk+fracscore+bonemed_fu+bmi.cat,
                 data = training,family = "binomial")

red.glm.aic<-AIC(red.glm)
red.glm.aic
```

    ## [1] 382.983

``` r
# get coord (threshold)
coords <- coords(glm.roc, "best", best.method = "closest.topleft", ret = "threshold")

# get optimal threshold
threshold <- coords[[1]] # currently this is 0.25653

# make changes to threshold if desired
adj.threshold <- threshold + 0

# adjust the labeling by the desired threshold
pred_label <- as.factor(ifelse(glm.predprob$Yes >= adj.threshold, "Yes","No"))

# print confusion matrix
(cm1 <- confusionMatrix(pred_label, test$fracture, positive = "Yes"))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction No Yes
    ##        No  71   5
    ##        Yes 41  32
    ##                                           
    ##                Accuracy : 0.6913          
    ##                  95% CI : (0.6105, 0.7643)
    ##     No Information Rate : 0.7517          
    ##     P-Value [Acc > NIR] : 0.9617          
    ##                                           
    ##                   Kappa : 0.3762          
    ##                                           
    ##  Mcnemar's Test P-Value : 2.463e-07       
    ##                                           
    ##             Sensitivity : 0.8649          
    ##             Specificity : 0.6339          
    ##          Pos Pred Value : 0.4384          
    ##          Neg Pred Value : 0.9342          
    ##              Prevalence : 0.2483          
    ##          Detection Rate : 0.2148          
    ##    Detection Prevalence : 0.4899          
    ##       Balanced Accuracy : 0.7494          
    ##                                           
    ##        'Positive' Class : Yes             
    ## 

``` r
cat("GLMNET Model",
    "\nSensitivity:", cm1$byClass[1],
    "\nSpecificity:", cm1$byClass[2],
    "\nPrevalence:", cm1$byClass[8],
    "\nPositive Predicitve Value:", cm1$byClass[3],
    "\nNegative Predicive Value:",cm1$byClass[4],
    "\nAUROC:", glm.roc$auc)
```

    ## GLMNET Model 
    ## Sensitivity: 0.8648649 
    ## Specificity: 0.6339286 
    ## Prevalence: 0.2483221 
    ## Positive Predicitve Value: 0.4383562 
    ## Negative Predicive Value: 0.9342105 
    ## AUROC: 0.7576014

# LASSO PROCESS

- priorfrac, raterisk, fracscore, bonemed_fu, bmi.cat

``` r
set.seed(12)

fitControl<- trainControl(method = "repeatedcv", number = 5, repeats = 1)

lambda_values <- seq(0,.03,by = .001)

lasso.fit <- train(fracture ~ .,
                 data = adj.training,
                 method = "glmnet",
                 trControl = fitControl,
                 tuneGrid = expand.grid(data.frame(alpha = 1, lambda = lambda_values)))

lasso.fit
```

    ## glmnet 
    ## 
    ## 351 samples
    ##  16 predictor
    ##   2 classes: 'No', 'Yes' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold, repeated 1 times) 
    ## Summary of sample sizes: 282, 281, 280, 280, 281 
    ## Resampling results across tuning parameters:
    ## 
    ##   lambda  Accuracy   Kappa      
    ##   0.000   0.7209862   0.06355342
    ##   0.001   0.7182107   0.05311766
    ##   0.002   0.7211081   0.06937412
    ##   0.003   0.7240066   0.06767039
    ##   0.004   0.7268235   0.07196719
    ##   0.005   0.7296002   0.06852214
    ##   0.006   0.7296002   0.05918829
    ##   0.007   0.7324574   0.06375904
    ##   0.008   0.7381314   0.07536061
    ##   0.009   0.7381314   0.07536061
    ##   0.010   0.7409483   0.08007629
    ##   0.011   0.7352743   0.04894156
    ##   0.012   0.7352743   0.04894156
    ##   0.013   0.7381314   0.05372966
    ##   0.014   0.7381314   0.05372966
    ##   0.015   0.7381314   0.05372966
    ##   0.016   0.7352328   0.03861394
    ##   0.017   0.7408666   0.04984332
    ##   0.018   0.7408252   0.03889769
    ##   0.019   0.7408252   0.02769993
    ##   0.020   0.7408252   0.01643685
    ##   0.021   0.7408252   0.01643685
    ##   0.022   0.7408252   0.01643685
    ##   0.023   0.7408252   0.01643685
    ##   0.024   0.7408252   0.01643685
    ##   0.025   0.7408252   0.01643685
    ##   0.026   0.7408252   0.01643685
    ##   0.027   0.7436421   0.02163579
    ##   0.028   0.7436824   0.01130849
    ##   0.029   0.7408252  -0.00598352
    ##   0.030   0.7408252  -0.00598352
    ## 
    ## Tuning parameter 'alpha' was held constant at a value of 1
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final values used for the model were alpha = 1 and lambda = 0.028.

``` r
plot(lasso.fit)
```

![](EDA---Models-Only_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
opt.pen<- lasso.fit$finalModel$lambdaOpt

coef(lasso.fit$finalModel, opt.pen)
```

    ## 20 x 1 sparse Matrix of class "dgCMatrix"
    ##                             s1
    ## (Intercept)        -1.74204979
    ## site_id             .         
    ## phy_id              .         
    ## priorfracYes        0.23031140
    ## age                 .         
    ## weight              .         
    ## height              .         
    ## premenoYes          .         
    ## momfracYes          .         
    ## armassistYes        .         
    ## smokeYes            .         
    ## rateriskSame        .         
    ## rateriskGreater     0.22591433
    ## fracscore           0.10696891
    ## bonemedYes          .         
    ## bonemed_fuYes       0.32600450
    ## bonetreatYes        .         
    ## bmi.catObesity      .         
    ## bmi.catOverweight   .         
    ## bmi.catUnderweight -0.08213559

# LASSO ADDITIVE model

``` r
lasso.model <- glm(fracture ~ priorfrac+ raterisk + bonemed_fu + fracscore + bmi.cat,
                   data = training,
                   family = "binomial")

summary(lasso.model)
```

    ## 
    ## Call:
    ## glm(formula = fracture ~ priorfrac + raterisk + bonemed_fu + 
    ##     fracscore + bmi.cat, family = "binomial", data = training)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5492  -0.7398  -0.5852   0.2058   2.1811  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -2.56721    0.39521  -6.496 8.26e-11 ***
    ## priorfracYes        0.26995    0.32582   0.829  0.40737    
    ## rateriskSame        0.10806    0.32811   0.329  0.74189    
    ## rateriskGreater     0.60092    0.34146   1.760  0.07844 .  
    ## bonemed_fuYes       0.63246    0.29731   2.127  0.03340 *  
    ## fracscore           0.17789    0.06159   2.888  0.00387 ** 
    ## bmi.catObesity      0.44940    0.33049   1.360  0.17389    
    ## bmi.catOverweight   0.48249    0.32417   1.488  0.13666    
    ## bmi.catUnderweight -1.49052    1.16193  -1.283  0.19956    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 395.31  on 350  degrees of freedom
    ## Residual deviance: 362.39  on 342  degrees of freedom
    ## AIC: 380.39
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# checking for multicollinearity issues. 
vif(lasso.model) #no issues present
```

    ##                GVIF Df GVIF^(1/(2*Df))
    ## priorfrac  1.387965  1        1.178119
    ## raterisk   1.137489  2        1.032730
    ## bonemed_fu 1.151154  1        1.072918
    ## fracscore  1.396673  1        1.181809
    ## bmi.cat    1.161755  3        1.025304

``` r
# parameters for train function
fitControl <- trainControl( method = "repeatedcv", number = 5, repeats = 1, classProbs = T, summaryFunction = mnLogLoss)

# set seed for reproduceability 
set.seed(12)

# training CARET mult. logi. regression model 
lasso.model.cv <- train(fracture ~ priorfrac+ raterisk + bonemed_fu + momfrac + fracscore + bmi.cat,
                 data = training,
                 trControl = fitControl,
                 method = "glm",
                 family = "binomial",
                 metric = "logLoss")

#summary(base.adjust.cv)

base.aic <- AIC(lasso.model) # 381.87

# make preds on the probabilty of each class in TRANING data
lasso.model.predprob <- predict(lasso.model.cv,test, type = "prob")

# compute the ROC curve
lasso.model.roc <- roc(response = test$fracture, predictor = lasso.model.predprob$Yes, levels = c("Yes","No"))
```

    ## Setting direction: controls > cases

``` r
# optimal threshold
optimal.threshold <- coords(lasso.model.roc, "best")

# plot ROC curve
plot(lasso.model.roc, print.thres = "best",col = "red")
# add legend to plot
legend("bottomright",
       legend = 'Additive model',
       col = "red",
       lwd = 4, cex = 1, xpd = T, horiz = F)

text(x = optimal.threshold[1], y = optimal.threshold[2], 
     labels = paste("Optimal Threshold =", round(optimal.threshold[1], 2)), 
     pos = 3)
```

![](EDA---Models-Only_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

``` r
# get coord (threshold)
coords <- coords(lasso.model.roc, "best",
                 #best.method = "closest.topleft",
                 ret = "threshold")

# get optimal threshold
threshold <- coords[[1]] # currently this is 0.3003923

# make changes to threshold if desired
adj.threshold <- threshold + .4 # lower increases sensitivity 

# adjust the labeling by the desired threshold
pred_label <- as.factor(ifelse(lasso.model.predprob$Yes > adj.threshold, "Yes","No"))

# print confusion matrix
(cm2 <- confusionMatrix(pred_label, test$fracture, positive = "Yes"))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  No Yes
    ##        No  109  35
    ##        Yes   3   2
    ##                                           
    ##                Accuracy : 0.745           
    ##                  95% CI : (0.6672, 0.8128)
    ##     No Information Rate : 0.7517          
    ##     P-Value [Acc > NIR] : 0.6175          
    ##                                           
    ##                   Kappa : 0.0384          
    ##                                           
    ##  Mcnemar's Test P-Value : 4.934e-07       
    ##                                           
    ##             Sensitivity : 0.05405         
    ##             Specificity : 0.97321         
    ##          Pos Pred Value : 0.40000         
    ##          Neg Pred Value : 0.75694         
    ##              Prevalence : 0.24832         
    ##          Detection Rate : 0.01342         
    ##    Detection Prevalence : 0.03356         
    ##       Balanced Accuracy : 0.51363         
    ##                                           
    ##        'Positive' Class : Yes             
    ## 

``` r
cat("LASSO Model",
    "\nSensitivity:", cm2$byClass[1],
    "\nSpecificity:", cm2$byClass[2],
    "\nPrevalence:", cm2$byClass[8],
    "\nPositive Predicitve Value:", cm2$byClass[3],
    "\nNegative Predicive Value:",cm2$byClass[4],
    "\nAUROC:", lasso.model.roc$auc)
```

    ## LASSO Model 
    ## Sensitivity: 0.05405405 
    ## Specificity: 0.9732143 
    ## Prevalence: 0.2483221 
    ## Positive Predicitve Value: 0.4 
    ## Negative Predicive Value: 0.7569444 
    ## AUROC: 0.7636342

# Main Additive Model - Winner

``` r
# parameters for train function
fitControl <- trainControl( method = "repeatedcv", number = 5, repeats = 1, classProbs = T, summaryFunction = mnLogLoss)

# set seed for reproduceability 
set.seed(12)


# training CARET mult. logi. regression model 
base.adjust.cv <- train(fracture~age+priorfrac+height+momfrac+bonemed+bmi+bonemed_fu,
               data = training,
                trControl = fitControl,
                method = "glm",
                family = "binomial",
                metric = "logLoss")

summary(base.adjust.cv)
```

    ## 
    ## Call:
    ## NULL
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5591  -0.7545  -0.6050   0.1712   2.1111  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)   -4.7198537  3.8036530  -1.241   0.2147  
    ## age            0.0342221  0.0154401   2.216   0.0267 *
    ## priorfracYes   0.6183073  0.2886238   2.142   0.0322 *
    ## height        -0.0008537  0.0212201  -0.040   0.9679  
    ## momfracYes     0.5171833  0.3548087   1.458   0.1449  
    ## bonemedYes     0.0169533  0.5123700   0.033   0.9736  
    ## bmi            0.0332507  0.0223695   1.486   0.1372  
    ## bonemed_fuYes  0.6969686  0.5015108   1.390   0.1646  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 395.31  on 350  degrees of freedom
    ## Residual deviance: 368.73  on 343  degrees of freedom
    ## AIC: 384.73
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
base.adj <- glm(fracture~age + priorfrac+height+momfrac+bonemed+bmi+bonemed_fu,
                     data = training, 
                     family = "binomial")
AIC(base.adj)
```

    ## [1] 384.7314

``` r
base.aic <- AIC(base.adj) # 384.7314

# make preds on the probabilty of each class in TRANING data
add.model.predprob <- predict(base.adjust.cv,test, type = "prob")

# compute the ROC curve
add.model.roc <- roc(response = test$fracture, predictor = add.model.predprob$Yes, levels = c("Yes","No"))
```

    ## Setting direction: controls > cases

``` r
# optimal threshold
optimal.threshold <- coords(add.model.roc, "best")

# plot ROC curve
plot(add.model.roc, print.thres = "best",col = "red")
# add legend to plot
legend("bottomright",
       legend = 'Additive model',
       col = "red",
       lwd = 4, cex = 1, xpd = T, horiz = F)

text(x = optimal.threshold[1], y = optimal.threshold[2], 
     labels = paste("Optimal Threshold =", round(optimal.threshold[1], 2)), 
     pos = 3)
```

![](EDA---Models-Only_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
# get coord (threshold)
coords <- coords(add.model.roc, "best",
                 #best.method = "closest.topleft",
                 ret = "threshold")

# get optimal threshold
threshold <- coords[[1]] # currently this is 0.3003923

# make changes to threshold if desired
adj.threshold <- threshold + 0
# low incr. ses. 

# adjust the labeling by the desired threshold
pred_label <- as.factor(ifelse(add.model.predprob$Yes > adj.threshold, "Yes","No"))

# print confusion matrix
(cm3 <- confusionMatrix(pred_label, test$fracture, positive = "Yes"))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction No Yes
    ##        No  94  13
    ##        Yes 18  24
    ##                                          
    ##                Accuracy : 0.7919         
    ##                  95% CI : (0.7179, 0.854)
    ##     No Information Rate : 0.7517         
    ##     P-Value [Acc > NIR] : 0.1481         
    ##                                          
    ##                   Kappa : 0.4668         
    ##                                          
    ##  Mcnemar's Test P-Value : 0.4725         
    ##                                          
    ##             Sensitivity : 0.6486         
    ##             Specificity : 0.8393         
    ##          Pos Pred Value : 0.5714         
    ##          Neg Pred Value : 0.8785         
    ##              Prevalence : 0.2483         
    ##          Detection Rate : 0.1611         
    ##    Detection Prevalence : 0.2819         
    ##       Balanced Accuracy : 0.7440         
    ##                                          
    ##        'Positive' Class : Yes            
    ## 

``` r
cat("SME Model",
    "\nSensitivity:", cm3$byClass[1],
    "\nSpecificity:", cm3$byClass[2],
    "\nPrevalence:", cm3$byClass[8],
    "\nPositive Predicitve Value:", cm3$byClass[3],
    "\nNegative Predicive Value:",cm3$byClass[4],
    "\nAUROC:", add.model.roc$auc)
```

    ## SME Model 
    ## Sensitivity: 0.6486486 
    ## Specificity: 0.8392857 
    ## Prevalence: 0.2483221 
    ## Positive Predicitve Value: 0.5714286 
    ## Negative Predicive Value: 0.8785047 
    ## AUROC: 0.7919884

# Confidence Intervals

``` r
# coefficient output for log. reg. model:
base.coef<- coef(base.adj) # normal coefficient output
base.coef
```

    ##   (Intercept)           age  priorfracYes        height    momfracYes 
    ## -4.7198537419  0.0342220593  0.6183072780 -0.0008537245  0.5171833254 
    ##    bonemedYes           bmi bonemed_fuYes 
    ##  0.0169533167  0.0332507021  0.6969685605

``` r
base.odd.ratios <- exp(coef(base.adj)) # exp coefficient
base.odd.ratios
```

    ##   (Intercept)           age  priorfracYes        height    momfracYes 
    ##   0.008916483   1.034814371   1.855784055   0.999146640   1.677296591 
    ##    bonemedYes           bmi bonemed_fuYes 
    ##   1.017097840   1.033809685   2.007657381

``` r
# confidence intervals
base.ci <- exp(confint(base.adj, level = .95)) # confidence interval @ .05 significance
```

    ## Waiting for profiling to be done...

``` r
base.ci
```

    ##                      2.5 %    97.5 %
    ## (Intercept)   4.861661e-06 15.354415
    ## age           1.004013e+00  1.066851
    ## priorfracYes  1.048361e+00  3.258927
    ## height        9.581935e-01  1.041701
    ## momfracYes    8.217671e-01  3.328237
    ## bonemedYes    3.659557e-01  2.775496
    ## bmi           9.891091e-01  1.080101
    ## bonemed_fuYes 7.423341e-01  5.410873

# Effect plots

``` r
library(sjPlot)
library(sjmisc)
library(effects)

# effect plot
plot_model(base.adj, type = "pred", terms = "priorfrac")
```

<img src="EDA---Models-Only_files/figure-gfm/unnamed-chunk-33-1.png" style="display: block; margin: auto;" />

``` r
plot_model(base.adj, type = "pred",terms = "age")
```

<img src="EDA---Models-Only_files/figure-gfm/unnamed-chunk-33-2.png" style="display: block; margin: auto;" />

``` r
# identifies all of the high-order terms in a model and returns a list of "eff" or "effpoly" objects.
all.effects <- allEffects(base.adj)

# plots the effect plosts
plot(all.effects,multiline=T)
```

<img src="EDA---Models-Only_files/figure-gfm/unnamed-chunk-33-3.png" style="display: block; margin: auto;" />

# OBJECTIVE 2

# KNN - Non-parametric model

``` r
fitControl <- trainControl(method = "repeatedcv",number = 5, repeats = 1, classProbs = T, summaryFunction = mnLogLoss)

set.seed(12)

knn.model <- train(fracture~.,
                   method = "knn",
                   data = adj.training,
                   trControl = fitControl,
                   metric = "logLoss")

knn.model
```

    ## k-Nearest Neighbors 
    ## 
    ## 351 samples
    ##  16 predictor
    ##   2 classes: 'No', 'Yes' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold, repeated 1 times) 
    ## Summary of sample sizes: 282, 281, 280, 280, 281 
    ## Resampling results across tuning parameters:
    ## 
    ##   k  logLoss  
    ##   5  2.4196452
    ##   7  1.5907618
    ##   9  0.9423033
    ## 
    ## logLoss was used to select the optimal model using the smallest value.
    ## The final value used for the model was k = 9.

``` r
preds <- predict(knn.model, test, type ="prob")[,"Yes"]

knn.roc <- roc(response = test$fracture, predictor = preds, levels = c("Yes","No"))

plot(knn.roc,print.thres = "best", print.thres.best.method = "closest.topleft", col = "purple")
```

<img src="EDA---Models-Only_files/figure-gfm/unnamed-chunk-34-1.png" style="display: block; margin: auto;" />

``` r
threshold = .3
knn.preds <- factor(ifelse(preds > threshold, "Yes","No"))

(cm6 <- confusionMatrix(data = knn.preds, reference = as.factor(test$fracture), positive = "Yes"))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction No Yes
    ##        No  74  18
    ##        Yes 38  19
    ##                                           
    ##                Accuracy : 0.6242          
    ##                  95% CI : (0.5412, 0.7021)
    ##     No Information Rate : 0.7517          
    ##     P-Value [Acc > NIR] : 0.99980         
    ##                                           
    ##                   Kappa : 0.1475          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.01112         
    ##                                           
    ##             Sensitivity : 0.5135          
    ##             Specificity : 0.6607          
    ##          Pos Pred Value : 0.3333          
    ##          Neg Pred Value : 0.8043          
    ##              Prevalence : 0.2483          
    ##          Detection Rate : 0.1275          
    ##    Detection Prevalence : 0.3826          
    ##       Balanced Accuracy : 0.5871          
    ##                                           
    ##        'Positive' Class : Yes             
    ## 

``` r
cat("\nSensitivity:", cm6$byClass[1],
    "\nSpecificity:", cm6$byClass[2],
    "\nPrevalence:", cm6$byClass[8],
    "\nPositive Predicitve Value:", cm6$byClass[3],
    "\nNegative Predicive Value:",cm6$byClass[4],
    "\nAUROC:", knn.roc$auc)
```

    ## 
    ## Sensitivity: 0.5135135 
    ## Specificity: 0.6607143 
    ## Prevalence: 0.2483221 
    ## Positive Predicitve Value: 0.3333333 
    ## Negative Predicive Value: 0.8043478 
    ## AUROC: 0.5743243

# COMPLEX INTERACTION

<h4>
Complex Model With Cross Validation
</h4>

``` r
# parameters for train function
fitControl <- trainControl( method = "repeatedcv", number = 5, repeats = 1, classProbs = T, summaryFunction = mnLogLoss)

# set seed for reproduceability 
set.seed(12)

# training CARET mult. logi. regression model 
complex.cv <- train(fracture~age+priorfrac+height+momfrac+bonemed+bmi+bonemed_fu+height:bonemed_fu+bonemed:bmi,
                 data = training,
                 trControl = fitControl,
                 method = "glm",
                 family = "binomial",
                 metric = "logLoss")

summary(complex.cv)
```

    ## 
    ## Call:
    ## NULL
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.50928  -0.73283  -0.57208  -0.00166   2.23941  
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)            -1.238e+01  4.906e+00  -2.523  0.01162 * 
    ## age                     3.550e-02  1.584e-02   2.241  0.02505 * 
    ## priorfracYes            6.956e-01  2.983e-01   2.332  0.01970 * 
    ## height                  5.146e-02  2.833e-02   1.816  0.06937 . 
    ## momfracYes              4.877e-01  3.623e-01   1.346  0.17829   
    ## bonemedYes             -3.681e+00  1.669e+00  -2.206  0.02739 * 
    ## bmi                    -2.312e-04  2.687e-02  -0.009  0.99314   
    ## bonemed_fuYes           2.188e+01  7.838e+00   2.791  0.00525 **
    ## `height:bonemed_fuYes` -1.305e-01  4.851e-02  -2.690  0.00714 **
    ## `bonemedYes:bmi`        1.316e-01  5.857e-02   2.247  0.02462 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 395.31  on 350  degrees of freedom
    ## Residual deviance: 354.42  on 341  degrees of freedom
    ## AIC: 374.42
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# make preds on the probabilty of each class in TRANING data
complexcv.predprob <- predict(complex.cv, test, type = "prob")

# compute the ROC curve
complexcv.roc <- roc(response = test$fracture, predictor = complexcv.predprob$Yes, levels = c("Yes","No"))

# plot ROC curve
plot(complexcv.roc, print.thres = "best", print.thres.best.method = "closest.topleft", col = "red")

# add legend to plot
legend("bottomright",
       legend = 'complex model',
       col = "red",
       lwd = 4, cex = 1, xpd = T, horiz = F)
```

<img src="EDA---Models-Only_files/figure-gfm/unnamed-chunk-35-1.png" style="display: block; margin: auto;" />

``` r
# printing out all AIC metrics (note they are based on training data not test data)
# cat("\nbase.adj Model (Additive):", base.aic,
#     "\nRed Model (Additive):", red.aic,
#     "\nComplex Model:", complex.aic)

# get coord (threshold)
coords <- coords(complexcv.roc, "best", best.method = "closest.topleft", ret = "threshold")

# get optimal threshold
threshold <- coords[[1]] # currently this is 0.25653

# make changes to threshold if desired
adj.threshold <- threshold + 0

# adjust the labeling by the desired threshold
pred_label <- as.factor(ifelse(complexcv.predprob$Yes >= adj.threshold, "Yes","No"))

# print confusion matrix
(cm3 <-confusionMatrix(pred_label, test$fracture, positive = "Yes"))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction No Yes
    ##        No  80  11
    ##        Yes 32  26
    ##                                           
    ##                Accuracy : 0.7114          
    ##                  95% CI : (0.6316, 0.7826)
    ##     No Information Rate : 0.7517          
    ##     P-Value [Acc > NIR] : 0.889791        
    ##                                           
    ##                   Kappa : 0.3504          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.002289        
    ##                                           
    ##             Sensitivity : 0.7027          
    ##             Specificity : 0.7143          
    ##          Pos Pred Value : 0.4483          
    ##          Neg Pred Value : 0.8791          
    ##              Prevalence : 0.2483          
    ##          Detection Rate : 0.1745          
    ##    Detection Prevalence : 0.3893          
    ##       Balanced Accuracy : 0.7085          
    ##                                           
    ##        'Positive' Class : Yes             
    ## 

``` r
cat("\nSensitivity:", cm3$byClass[1],
    "\nSpecificity:", cm3$byClass[2],
    "\nPrevalence:", cm3$byClass[8],
    "\nPositive Predicitve Value:", cm3$byClass[3],
    "\nNegative Predicive Value:",cm3$byClass[4],
    "\nAUROC:", complexcv.roc$auc)
```

    ## 
    ## Sensitivity: 0.7027027 
    ## Specificity: 0.7142857 
    ## Prevalence: 0.2483221 
    ## Positive Predicitve Value: 0.4482759 
    ## Negative Predicive Value: 0.8791209 
    ## AUROC: 0.7326255

# COMPLEX GLM - Winner

``` r
library(pROC)
# parameters for train function
fitControl <- trainControl( method = "repeatedcv", number = 5, repeats = 1, classProbs = T, summaryFunction = mnLogLoss)

# set seed for reproduceability 
set.seed(12)


# training CARET mult. logi. regression model 
red.glm <- train(fracture~site_id+priorfrac+momfrac+raterisk+fracscore+bonemed_fu+bmi.cat+ bonemed_fu:bmi.cat,
                 data = training,
                 trControl = fitControl,
                 method = "glm",
                 family = "binomial",
                 metric = "logLoss")
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

``` r
summary(red.glm)
```

    ## 
    ## Call:
    ## NULL
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0259  -0.7461  -0.5593   0.1951   2.2540  
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                         -2.77337    0.48794  -5.684 1.32e-08 ***
    ## site_id                              0.07613    0.07377   1.032  0.30208    
    ## priorfracYes                         0.28915    0.34138   0.847  0.39700    
    ## momfracYes                           0.36177    0.37164   0.973  0.33033    
    ## rateriskSame                         0.07166    0.33799   0.212  0.83210    
    ## rateriskGreater                      0.59024    0.35068   1.683  0.09235 .  
    ## fracscore                            0.16741    0.06395   2.618  0.00885 ** 
    ## bonemed_fuYes                        0.42248    0.44052   0.959  0.33753    
    ## bmi.catObesity                       0.10358    0.40527   0.256  0.79827    
    ## bmi.catOverweight                    0.63204    0.39550   1.598  0.11002    
    ## bmi.catUnderweight                   0.73072    1.48817   0.491  0.62341    
    ## `bonemed_fuYes:bmi.catObesity`       1.88230    0.80214   2.347  0.01895 *  
    ## `bonemed_fuYes:bmi.catOverweight`   -0.47575    0.68077  -0.699  0.48465    
    ## `bonemed_fuYes:bmi.catUnderweight` -16.24228  696.91054  -0.023  0.98141    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 395.31  on 350  degrees of freedom
    ## Residual deviance: 348.11  on 337  degrees of freedom
    ## AIC: 376.11
    ## 
    ## Number of Fisher Scoring iterations: 14

``` r
# make preds on the probabilty of each class in TRANING data
glm.predprob <- predict(red.glm, test, type = "prob")

# compute the ROC curve
glm.roc <- roc(response = test$fracture, predictor = glm.predprob$Yes, levels = c("Yes","No"))
```

    ## Setting direction: controls > cases

``` r
# plot ROC curve
plot(glm.roc, print.thres = "best", print.thres.best.method = "closest.topleft", col = "red")
legend("bottomright",# add legend to plot
       legend = 'caret model',
       col = "red",
       lwd = 4, cex = 1, xpd = T, horiz = F)
```

![](EDA---Models-Only_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
# fitting model for AIC below
red.glm <- glm(fracture~site_id+priorfrac+momfrac+raterisk+fracscore+bonemed_fu+bmi.cat,
                 data = training,family = "binomial")

red.glm.aic<-AIC(red.glm)
red.glm.aic
```

    ## [1] 382.983

``` r
# get coord (threshold)
coords <- coords(glm.roc, "best", best.method = "closest.topleft", ret = "threshold")

# get optimal threshold
threshold <- coords[[1]] # currently this is 0.25653

# make changes to threshold if desired
adj.threshold <- threshold + 0

# adjust the labeling by the desired threshold
pred_label <- as.factor(ifelse(glm.predprob$Yes >= adj.threshold, "Yes","No"))

# print confusion matrix
(cm1 <- confusionMatrix(pred_label, test$fracture, positive = "Yes"))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction No Yes
    ##        No  77  12
    ##        Yes 35  25
    ##                                           
    ##                Accuracy : 0.6846          
    ##                  95% CI : (0.6035, 0.7582)
    ##     No Information Rate : 0.7517          
    ##     P-Value [Acc > NIR] : 0.974450        
    ##                                           
    ##                   Kappa : 0.3006          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.001332        
    ##                                           
    ##             Sensitivity : 0.6757          
    ##             Specificity : 0.6875          
    ##          Pos Pred Value : 0.4167          
    ##          Neg Pred Value : 0.8652          
    ##              Prevalence : 0.2483          
    ##          Detection Rate : 0.1678          
    ##    Detection Prevalence : 0.4027          
    ##       Balanced Accuracy : 0.6816          
    ##                                           
    ##        'Positive' Class : Yes             
    ## 

``` r
cat("GLMNET Model",
    "\nSensitivity:", cm1$byClass[1],
    "\nSpecificity:", cm1$byClass[2],
    "\nPrevalence:", cm1$byClass[8],
    "\nPositive Predicitve Value:", cm1$byClass[3],
    "\nNegative Predicive Value:",cm1$byClass[4],
    "\nAUROC:", glm.roc$auc)
```

    ## GLMNET Model 
    ## Sensitivity: 0.6756757 
    ## Specificity: 0.6875 
    ## Prevalence: 0.2483221 
    ## Positive Predicitve Value: 0.4166667 
    ## Negative Predicive Value: 0.8651685 
    ## AUROC: 0.7520512

# QDA

``` r
fitControl <- trainControl(method="repeatedcv",
                           number=5,
                           repeats=1,
                           classProbs=TRUE,
                           summaryFunction=mnLogLoss)
set.seed(12)
qda.fit <- train(fracture~age+weight+height+bmi+fracscore,
                 data = training,
                 method = "qda",
                 trControl = fitControl,
                 metric = "logLoss")

predictions <- predict(qda.fit, test, type = "prob")[,"Yes"]

threshold = .5
class_pred = as.factor(ifelse(predictions > threshold, "Yes","No"))

(cm7<-confusionMatrix(data = class_pred, reference = test$fracture, positive = "Yes"))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  No Yes
    ##        No  108  34
    ##        Yes   4   3
    ##                                           
    ##                Accuracy : 0.745           
    ##                  95% CI : (0.6672, 0.8128)
    ##     No Information Rate : 0.7517          
    ##     P-Value [Acc > NIR] : 0.6175          
    ##                                           
    ##                   Kappa : 0.0623          
    ##                                           
    ##  Mcnemar's Test P-Value : 2.546e-06       
    ##                                           
    ##             Sensitivity : 0.08108         
    ##             Specificity : 0.96429         
    ##          Pos Pred Value : 0.42857         
    ##          Neg Pred Value : 0.76056         
    ##              Prevalence : 0.24832         
    ##          Detection Rate : 0.02013         
    ##    Detection Prevalence : 0.04698         
    ##       Balanced Accuracy : 0.52268         
    ##                                           
    ##        'Positive' Class : Yes             
    ## 

``` r
qda.roc <- roc(response = test$fracture, predictor = predictions, levels = c("Yes","No"))
```

    ## Setting direction: controls > cases

``` r
plot(qda.roc, print.thres = "best", col = "lightblue")
```

![](EDA---Models-Only_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

# ROC CURVES

``` r
# plot ROC curve
plot(add.model.roc, print.thres = "best",col = "red") # additive
plot(glm.roc, print.thres = "best", col = "purple", add = T) # glm complex
# plot(lasso.model.roc, print.thres = "best",col = "blue", add = T) # lasso
#plot(complexcv.roc, print.thres = "best", col = "purple", add = T) # Complex (interaction)
plot(knn.roc,print.thres = "best", col = "orange", add = T)
plot(qda.roc, print.thres = "best", col = "lightblue", add= T)

# add legend to plot
legend("bottomright",
       legend = c('Additive model', "Complex", "KNN", "QDA"),
       col = c("red", "purple", "orange", "lightblue"),
       lwd = 4, cex = 1, xpd = T, horiz = F)
```

![](EDA---Models-Only_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->
