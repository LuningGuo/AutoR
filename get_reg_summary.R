# 输出回归分析结果
get_reg_summary = function(regression_model) {
  model_summary = summary(regression_model)
  coefficients = round(model_summary$coefficients, 3)
  result = data.frame(
    变量名称 = rownames(coefficients),
    回归系数 = coefficients[,1],
    显著性 = ifelse(coefficients[,4] <= 0.001, '<0.001', as.character(coefficients[,4])),
    row.names = NULL
  )
  return(result)
}
