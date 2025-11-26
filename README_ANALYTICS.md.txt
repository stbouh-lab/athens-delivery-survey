# Athens Bayesian Persona Blocks – Analytics Guide

## 1. Data format
Each respondent now has a **BlockID** (1-6) in the exported .CHO file.  
Columns: RespondentID, BlockID, Set, Alt, Choice, Cost, Walk, CO2save, Reliability, Resched, Noise, ModeFlag.

## 2. Run HB-MXL  (R)
```r
library(mlogit)
dat &lt;- read.csv("raw_choices.csv")
dat$BlockID &lt;- as.factor(dat$BlockID)
m &lt;- mlogit(Choice ~ Cost + Walk + CO2save + Reliability + Resched + Noise | BlockID,
            data = dat, shape = "long", alt.var = "Alt", choice = "Choice")
summary(m)