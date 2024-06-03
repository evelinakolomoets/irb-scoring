library(pglm)
library(plm)

df <- read.csv('final.csv')
df$NA. <- log(df$NA.)
df <- cbind(df,const = 1)



# Преобразование данных в формат panel data
data <- pdata.frame(df, index = c("REGN_GKO", "DATE"))
initial_values <- rep(1, length(names(data)) - 2 -1)  # количество регрессоров

# Подгонка логистической регрессионной модели с фиксированными эффектами
model <- pglm(DEFAULT ~ MAX_NORM + CAP_TO_NA + RES_TO_NA + 
                NA. + 
                BI_TO_NA + NI_TO_NA + L_NORM  + I_NORM + IS_SZKO + REGISTERED_MSC,
              data = data,
              family = binomial("logit"),
              #model = "within",
              #effect = "individual",
              na.action=na.omit,
              start = initial_values)

# Вывод результатов
summary(model)

# ХРЕНЬ
glm_logit <- glm(DEFAULT ~ MAX_NORM + CAP_TO_NA + RES_TO_NA + NA. + BI_TO_NA + NI_TO_NA + L_NORM + IS_SZKO + REGISTERED_MSC + factor(REGN_GKO),
             data = data,
             family = binomial("logit"))
summary(glm_logit)

# СОМНИТЕЛЬНО НО ОКЕЙ
m <- plm(DEFAULT ~ const+MAX_NORM+CAP_TO_NA
             +RES_TO_NA+NA.+BI_TO_NA+NI_TO_NA
             +L_NORM+I_NORM+IS_SZKO+REGISTERED_MSC, 
             data=df)
summary(m)


# ХРЕНЬ
# Установка необходимых пакетов
# Подгонка логистической регрессионной модели с случайными эффектами для индивидов
library(lme4)
library(MuMIn)
library(sandwich)
library(lmtest)

# Настройка контроля оптимизации
control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxit = 10^10))

model2 <- glmer(DEFAULT ~ MAX_NORM + CAP_TO_NA + RES_TO_NA + NA. + BI_TO_NA + 
                 NI_TO_NA + L_NORM + IS_SZKO + REGISTERED_MSC + (1 | REGN_GKO),
                data = data,
                family = binomial("logit"),
                control = control)

r.squaredGLMM(model2)

