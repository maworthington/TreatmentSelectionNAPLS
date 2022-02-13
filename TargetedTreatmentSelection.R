### Treatment selection for different risk groups 
### Author: Michelle Worthington, 2018-2019

library(dplyr)
library(rms)
library(glm)

### Are specific interventions more or less effective for different risk groups? 

### This study examines the interaction between likelihood of experiencing negative outcomes and responsiveness to different interventions 
######  Specifically, do higher-risk for psychosis patients benefit more long-term from a family-focused therapy intervention? 

### This analysis incorporates previously calucalted likelhood scores for conversion to psychosis
### These scores were calculated based on Cannon et al. 2016, American Journal of Psychiatry 

### Factors included in calculating this risk score include: 
###### - Age
###### - Unusual thought content
###### - Persecutory delusions
###### - Stressful life events 
###### - Total traumas experienced 
###### - Family history of psychotic illness 
###### - Verbal learning scores 


### This analysis also incorporates data from a randomized controlled trial of the effectiveness of family-focused therapy 
### vs. enhanced care (EC) in a population of clinical high-risk for psychosis youth
###### This dataset contains symptom information at baseline and 6 months 
###### The outcomes of interest include symptom change over 6 months and conversion to psychosis 


# Load data 
### data has already been prepared: RCT data has been merged with risk calculator data
### we also included participants from a larger study (NAPLS2) who were not part of the RCT and did not receive specialized treatment

FFTstudy_only = data.frame(read.csv("/Users/maw/Documents/Documents/Yale/Cannon Lab/FFT/Data/substudy_followup_only_UPDATED.csv"))
riskcalc_FFT_data = read.csv("/Users/maw/Documents/Documents/Yale/Cannon Lab/FFT/Data/FINAL_DATA_2.2.19.csv")
napls_only = read.csv("/Users/maw/Documents/Documents/Yale/Cannon Lab/FFT/Data/napls_only_2.2.19.csv")

# Split data into treatment groups 

substudy_fft = filter(substudy_only, FFT == 1)
substudy_ec = filter(substudy_only, FFT == 0)

# Plot outcome data and primary variables of interest to examine for outliers etc. 

ggplot(substudy_only, aes(x = pos_change, y = FFT)) + 
  geom_boxplot()
ggplot(substudy_only, aes(x = positive.1, y = FFT)) + 
  geom_boxplot() # outliers exist in FFT group 
ggplot(substudy_only, aes(x = positive.2, y = FFT)) + 
  geom_boxplot()
ggplot(substudy_only, aes(x = pred_conv_2yrs, y = FFT)) + 
  geom_boxplot() # outliers in risk score exist in both treatment groups


ggplot(napls_only, aes(x = pos_change, y = FFT)) + 
  geom_boxplot() # outliers exist in the non-specialized treatment group (treatment as usual)
ggplot(napls_only, aes(x = positive.1, y = FFT)) + 
  geom_boxplot() # outliers exist in the non-specialized treatment group (treatment as usual)
ggplot(napls_only, aes(x = positive.2, y = FFT)) + 
  geom_boxplot() 
ggplot(napls_only, aes(x = pred_conv_2yrs, y = FFT)) + 
  geom_boxplot() # outliers exist in the non-specialized treatment group (treatment as usual)


#### Regression models ####

## First run multivariable linear regression models in each treatment group
lm_fft = lm(pos_change ~ Age + Gender + pred_conv_2yrs, 
                  data = substudy_fft)
summary(lm_fft) # risk score highly significant in FFT group, B = 34.05, p < 0.001

lm_ec = lm(pos_change ~ Age + Gender + pred_conv_2yrs, 
           data = substudy_ec)
summary(lm_ec) # risk score not significant in EC group, B = 12.3, p = 0.25

lm_napls = lm(pos_change ~ Age + Gender + pred_conv_2yrs, 
              data = napls_only)
summary(lm_napls) # risk score significant in treatment as usual group, B = 6.6, p < 0.001


## Also run model in overall data to examine interaction effect 
lm_intx = lm(pos_change ~ Age + Gender + FFT + pred_conv_2yrs + FFT:pred_conv_2yrs, 
                  data = final_data)
summary(lm_intx) # significant interaction term for treatment group by risk score, B = -11.13, p = 0.003


## Robust linear regression models were also used to account for non-normal distribution of predicted risk score 
rlm_fft = rlm(pos_change ~ Age + Gender + pred_conv_2yrs, 
              data = substudy_fft)
summary(rlm_fft) # remains significant, B = 34.3, p < 0.001

rlm_ec = rlm(pos_change ~ Age + Gender + pred_conv_2yrs, 
              data = substudy_ec)
summary(rlm_ec) # remains not significant, B = 12.9, p = 0.12

rlm_napls = rlm(pos_change ~ Age + Gender + pred_conv_2yrs, 
              data = napls_only)
summary(rlm_napls) # remains significant, B = 5.9, p = 0.003

## run model in overall data to examine interaction effect 
rlm_intx = rlm(pos_change ~ Age + Gender + FFT + pred_conv_2yrs + FFT:pred_conv_2yrs, 
              data = final_data)
summary(rlm_intx) # remains significant, B = -14.4, p < 0.001


## Plot these findings 
ggplot(final_data, aes(x = pred_conv_2yrs, y = pos_change, group = FFT, color = FFT)) + 
  geom_point(aes(color = FFT, shape = FFT)) + 
  geom_smooth(method = "rlm", se = T, aes(fill = FFT, color = FFT),
              size = 0.7, alpha = 0.1) + 
  scale_color_manual("Treatment Group", labels = c("Family Focused Therapy (FFT-CHR)", "Enhanced Care (EC)", "NAPLS2 Treatment as Usual"), 
                     values = c("#396AB1", "#DA7C30", "#3E9651")) + 
  scale_fill_manual(guide = FALSE, labels = c("NAPLS", "EC", "FFT-CHR"), 
                    values = c("#396AB1", "#DA7C30", "#3E9651")) +
  scale_shape_discrete("Treatment Group", labels = c("Family Focused Therapy (FFT-CHR)", "Enhanced Care (EC)", "NAPLS2 Treatment as Usual")) +
  ylab("Improvement in Positive Symptom Scores (SOPS)") + 
  xlab("Predicted Risk of Conversion") + 
  theme_bw(base_size = 16) +
  theme(legend.position = c(0.875, 0.915),
        legend.background = element_rect(linetype = 1, size = 0.25, colour = 1),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.spacing.x = unit(0.5, "cm"))


## Examine symptom differences across treatment groups

t.test(substudy_fft$positive.1, substudy_ec$positive.1)
t.test(substudy_fft$positive.1, napls_only$positive.1)
t.test(substudy_ec$positive.1, napls_only$positive.1)
t.test(substudy_only$positive.1, napls_only$positive.1)
# no treatment group-level differences in baseline symptoms 

t.test(substudy_fft$positive.2, substudy_ec$positive.2)
t.test(substudy_fft$positive.2, napls_only$positive.2)
t.test(substudy_ec$positive.2, napls_only$positive.2)
t.test(substudy_only$positive.2, napls_only$positive.2)
# no group-level differences in 6 month symptoms 

t.test(substudy_fft$positive.1, substudy_fft$positive.2, type = paired)
t.test(substudy_ec$positive.1, substudy_ec$positive.2, type = paired)
t.test(napls_only$positive.1, napls_only$positive.2, type = paired)
t.test(substudy_only$positive.1, substudy_only$positive.2, type = paired)
# in all groups, positive symptoms decrease over time 

t.test(substudy_fft$pos_change, substudy_ec$pos_change)
t.test(substudy_fft$pos_change, napls_only$pos_change)
t.test(substudy_ec$pos_change, napls_only$pos_change)
t.test(substudy_only$pos_change, napls_only$pos_change)
# but positive symptom change scores are not different across groups 

