# Loops and basic functions
  
September 27, 2017  





In this project we will consider loops and basic functions. To begin we will work on using loops to simulate a famous dice game: Yahtzee. 



# Part 1: Yahtzee

In the game of Yahtzee, five dice are tossed, and various combinations of numbers, similar to poker hands, are assigned point values.  In the game, dice can be selected and re-tossed, but we will focus on calculating the probabilities for the first toss only.  We will also deal only with the "lower half" of the score card in the game.  For the interested student, continuing this project to account for the complete rules of play would be an entertaining challenge.

Anyone not familiar with Yahtzee should try a web search for the rules of the game.  Some sites have applets that let you play online.

All you really need to know for this lesson, though, is which combinations are counted.  We will call these "hands," as the combinations in poker are called.  The hands in Yahtzee are:

- Three of a Kind (three of one number and two others that are different)
- Full House (three of one number and two of another number)
- Four of a Kind (four of one number and one other)
- Yahtzee (five of the same number)
- Small Straight (four consecutive numbers)
- Large Straight (five consecutive numbers)
- Chance (anything that does not fit the above patterns)

We begin by simulating dice rolls. For example we can simulate one roll of a die to be:

```r
sample(1:6, size=1, replace=T)
```

```
## [1] 2
```


### Problem 1: Dice Rolls

Using a loop create a matrix of 10 rolls all consisting of 5 dice in each roll. 


```r
x <- matrix(, nrow=10, ncol=5)
for (i in 1:10)
  {
  x[i,] <- sample(1:6, size=5, replace=T)
}
x
```

```
##       [,1] [,2] [,3] [,4] [,5]
##  [1,]    4    5    5    6    1
##  [2,]    2    3    4    3    6
##  [3,]    2    4    4    1    3
##  [4,]    1    1    5    4    1
##  [5,]    4    3    2    1    1
##  [6,]    5    6    2    4    5
##  [7,]    2    1    1    2    1
##  [8,]    3    6    4    1    2
##  [9,]    4    5    6    6    5
## [10,]    4    2    2    5    3
```


### Prolem 2: Sorting Dice Rolls

Your output will look similar to what is show below. 
```
x
      [,1] [,2] [,3] [,4] [,5]
 [1,]    1    5    2    4    1
 [2,]    2    3    3    4    6
 [3,]    4    5    5    2    5
 [4,]    4    1    4    1    5
 [5,]    4    4    3    5    3
 [6,]    3    4    3    5    3
 [7,]    3    4    2    6    2
 [8,]    4    3    5    2    2
 [9,]    4    4    4    6    2
[10,]    6    6    1    4    6
```
We will need to sort this. The following function is called a *Bubble Sort*. Comment on every part of the code to explain what it is doing:


```r
bubblesort <- function(x) {
# x is initially the input vector and will be
# modified to form the output
if (length(x) < 2) return (x)
# last is the last element to compare with
for(last in length(x):2) {
for(first in 1:(last - 1)) {
if(x[first] > x[first + 1]) { # swap the pair
save <- x[first]
x[first] <- x[first + 1]
x[first + 1] <- save
}
}
}
return (x)
}
```

### Problem 3: Applying the Sort with dice rolls

Apply Bubble sort to each row of your matrix to sort the data. (You may wish to do this within your loop so the rows are sorted as they get added.) 

```r
for(i in 1:10){
  x[i,]=bubblesort(x[i,])
}
x
```

```
##       [,1] [,2] [,3] [,4] [,5]
##  [1,]    1    4    5    5    6
##  [2,]    2    3    3    4    6
##  [3,]    1    2    3    4    4
##  [4,]    1    1    1    4    5
##  [5,]    1    1    2    3    4
##  [6,]    2    4    5    5    6
##  [7,]    1    1    1    2    2
##  [8,]    1    2    3    4    6
##  [9,]    4    5    5    6    6
## [10,]    2    2    3    4    5
```



### Problem 4: Looking for Yahtzee Combinations


Sorting the dice means that all dice that are equal will be next to each other.  Thus, to check for a Yahtzee, all we need is to find out if x1=x5. If x1 and x5 are the same, it is not possible (in sorted order) for the numbers in between to be different.

Some examples of Four of a Kind (after sorting) are:

```
    1 2 2 2 2
    2 2 2 2 3
```

As you can see, either x1=x4 or x2=x5.  If it is not a Yahtzee, then these two conditions will identify Four of a Kind.

When it comes to Three of a Kind, we run into a little complication.  If we follow the strategy used for Four of a Kind, we would check if x1=x3, x2=x4, or x3=x5.  Consider the following examples:

```
    1 1 1 2 3
    1 2 2 2 5
    2 4 5 5 5
```

These would all be correctly identified.  But what about:
```
    1 1 1 2 2 
    3 3 5 5 5
```

As you can see, these would all fulfill the first and third conditions proposed above, but they should be classified as Full House.  Therefore  we also need to check for a Full House in these cases.  The following identification routine checks for these types of Hands. At each stage, we have to be very careful that all possibilities are accounted for.


```r
con5=function(x){
  if(x[2]-x[1]==1 & x[3]-x[2]==1 & x[4]-x[3]==1 & x[5]-x[4]==1)
  {return(1)}
  else {return(0)}
}

con4=function(x){
  h=x[!duplicated(x)]
  if(length(h)<4){
    return(0)
  } else if(length(h)==4){
    if(h[4]-h[3]==1 & h[3]-h[2]==1 & h[2]-h[1]==1){
    return(1)
    } else {return(0)}}
    else if((h[4]-h[3]==1 & h[3]-h[2]==1 & h[2]-h[1]==1) | (h[4]-h[3]==1 & h[3]-h[2]==1 & h[5]-h[4]==1))              {return(1)}  
    else{return(0)}
}
                  
combination=function(x){
  
if (x[1]==x[5]){hand="Yahtzee"} 
  else if (x[1]==x[4] | x[2]==x[5]) {hand="Four of a Kind"}
  else if((x[1]==x[3] & x[4]==x[5])| (x[1]==x[2] & x[3]==x[5])){ hand="Full House"} 
  else if(x[1]==x[3] | x[2]==x[4] | x[3]==x[5]) {hand="Three of a Kind"}
  else if(con5(x)==1){hand="Large Straight"}
  else if(con4(x)==1){hand="Small Straight"}
       else{hand="Chance"}
}


hand=vector(length=10)
for(i in 1:10){
  hand[i]=combination(x[i,])
}
hand
```

```
##  [1] "Chance"          "Chance"          "Small Straight" 
##  [4] "Three of a Kind" "Small Straight"  "Chance"         
##  [7] "Full House"      "Small Straight"  "Chance"         
## [10] "Small Straight"
```

The above code lays this out for Yahtzee and 4 of a kind. Find statements that work for the rest. Make sets of x and test them to make sure they work.



### Problem 5: Simulating 100,000 dice rolls


```r
hand=matrix(nrow=100000,ncol=10)

for(k in 1:100000){
x <- matrix(, nrow=10, ncol=5)
for (i in 1:10)
  {
  x[i,] <- sample(1:6, size=5, replace=T)
}
for (i in 1:10){
  x[i,]=bubblesort(x[i,])
}
for(i in 1:10){
  hand[k,i]=combination(x[i,])
}
}

Results=table(hand)/1000000

#Arrange final results
Results=as.data.frame(Results)
names(Results)=c("Hand","Probability")
Results$Probability= round(Results$Probability,digits=4)
Results$order=vector(length=7)
order1=c("Yahtzee","Four of a Kind","Three of a Kind","Full House","Large Straight","Small Straight","Chance")
order2=c(1,2,3,4,5,6,7)
order=data.frame(order1,order2)
for(i in 1:7){
  j=1
  while(Results[i,1]!=order[j,1])
  {j=j+1}
    Results[i,3]=order[j,2]}
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
select(arrange(Results,order),Hand,Probability)
```

```
##              Hand Probability
## 1         Yahtzee      0.0008
## 2  Four of a Kind      0.0195
## 3 Three of a Kind      0.1545
## 4      Full House      0.0387
## 5  Large Straight      0.0311
## 6  Small Straight      0.1229
## 7          Chance      0.6325
```
Put this all together and simulate this game 100,000 times and check your probabilities. You should get the following:

--------------------------------
Hand              Probability
----------------- --------------
Yahtzee           .0008    

Four of a Kind    .0193

Three of a Kind   .1543

Full House        .0386

Large Straight    .0309

Small Straight    .1235

Chance            .6326
-------------------------------------------------------



## Part II: PHP 2560 Only




Standardizing a variable means subtracting the mean, and then dividing by the standard deviation. Let’s use a loop to standardize the numeric columns in the [Western Collaborative Group Study](https://clinicaltrials.gov/ct2/show/NCT00005174). This study began in 1960 with 3154 men ages 39-59, who were employed in one of 11 California based companies. They were followed until 1969 during this time, 257 of these men developed coronary heart disease (CHD). You can read this data in with the code below. You can access this dataset with the following code:

```

suppressMessages(library(foreign))
wcgs <- read.dta("https://drive.google.com/uc?export=download&id=0B8CsRLdwqzbzYWxfN3ExQllBQkU")
```

The data has the following variables:



WCGS has the following variables:

-----------------------------------------------------------
Name    Description
------- -------------------------------------------
id      Subject identification number

age     Age in years

height  Height in inches

weight  Weight in lbs.

sbp     Systolic blood pressure in mm 

dbp     Diastolic blood pressure in mm Hg

chol    Fasting serum cholesterol in mm 

behpat  Behavior

  1       A1

  2       A2

  3       B3

  4       B4

ncigs   Cigarettes per day

dibpat  Behavior

1       type A

2       type B

chd69   Coronary heart disease

1       Yes

0       no

typechd Type of CHD

1       myocardial infarction or death

2       silent myocardial infarction

3       angina perctoris

time169 Time of CHD event or end of follow-up

arcus   Arcus senilis

0       absent

1       present

bmi     Body Mass Index
-----------------------------------------------------------




### Question 6: Standardize Function

A. Create a function called standardize.me() that takes a numeric vector as an argument, and returns the standardized version of the vector. 
B. Assign all the numeric columns of the original WCGS dataset to a new dataset called WCGS.new.
C. Using a loop and your new function, standardize all the variables WCGS.new dataset.
D. What should the mean and standard deviation of all your new standardized variables be? Test your prediction by running a loop




### Question 7: Looping to Calculate

A. Using a loop, calculate the mean weight of the subjects separated by the type of CHD they have.
B. Now do the same thing, but now don’t use a loop

