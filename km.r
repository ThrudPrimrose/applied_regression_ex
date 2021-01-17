library(KMsurv)
library(survival)
library(survminer)

data(bfeed)

surv_fit <- survfit(Surv(duration, delta) ~ smoke, data = bfeed)

ggsurvplot(surv_fit)

survdiff(Surv(duration, delta) ~ smoke, data = bfeed)

coxph_fit <- coxph(Surv(duration, delta) ~ smoke + ybirth, data = bfeed)
print(coxph_fit)

coxph_fit_interact <- coxph(Surv(duration, delta) ~ smoke * ybirth, data = bfeed)
print(coxph_fit_interact)