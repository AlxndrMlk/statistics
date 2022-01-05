# Simpson's paradox

# Generate data
datalist = list()

for (i in seq(0, 100, 10)) {
  age = runif(10, i, i + 10)
  exercise = (runif(10) + .1 * age) / 10 + .1 * rnorm(10, 0, 2)
  cholesterol = (i / 10) - exercise + .4 * rnorm(10, 0, 2)
  df = data.frame(age, exercise, cholesterol)
  datalist[[(i / 10) + 1]] = df
}

# Combine all data
data_simpson = do.call(rbind, datalist)

# Plot
plot(data_simpson$age, data_simpson$exercise)
plot(data_simpson$age, data_simpson$cholesterol)
plot(data_simpson$exercise, data_simpson$cholesterol)

# Model
model_1 = lm(cholesterol ~ exercise, data=data_simpson)
model_2 = lm(cholesterol ~ exercise + age, data=data_simpson)

# Check
summary(model_1)
summary(model_2)


