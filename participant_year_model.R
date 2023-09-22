

# Most optimistic estimate
m_1yr_optimistic <- lmer(med_pmpm_trunc ~ 1 + spring_dummy + spring_dummy*phase + 
                            (1 | carrier_member_id) +
                            (1 | subclass_nn),
                          weights = weights_nn,
                          data = df_matched)

tab_model(m_1yr_optimistic, show.ci=F, show.se=T)

## Get PPPY in dollars
hypotheses(m_1yr_optimistic, hypothesis = "b4 * 12 = 0")

## Get PPPY in pct
hypotheses(m_1yr_optimistic, hypothesis = "(b4 * 12) / (12 * (b1 + b3))  = 0")



# Best estimate
m_1yr_participant <- lmer(med_pmpm_trunc ~ 1 + spring_dummy + as.factor(months_post) +
                          spring_dummy*as.factor(months_post) + 
                             (1 | carrier_member_id) +
                             (1 | subclass_nn),
                           weights = weights_nn,
                           data = df_matched)

tab_model(m_1yr_participant, show.ci=F, show.se=T)

## Get PPPY in dollars
hypotheses(m_1yr_participant, hypothesis = "b15 + b16 + b17 + b18 + b19 + b20 + b21 + b22 + b23 + b24 + b25 + b26 = 0")

## Get PPPY in pct
hypotheses(m_1yr_participant, hypothesis = "(b15 + b16 + b17 + b18 + b19 + b20 + b21 + b22 + b23 + b24 + b25 + b26) /
          (b1*12 + b3 + b4 + b5 + b6 + b7 + b8 + b9 + b10 + b11 + b12 + b13 + b14)  = 0")
