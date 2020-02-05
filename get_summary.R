# numeric、integer类变量处理函数
getNumeric = function(x) {
  detail = '单位：'
  range = paste0('[', min(x), ', ', max(x), ']', collapse = '')
  return(c(detail, range))
}

# factor类变量处理函数
getFactor = function(x) {
  categories = levels(x)
  detail = paste0('定性变量：共', length(categories), '个水平', collapse = '')
  if (length(categories) <= 4) {
    range = paste(categories, collapse = '、')
  } else {
    range = paste0(paste(categories[1:4], collapse = '、'), '等', collapse = '')
  }
  return(c(detail, range))
}

# character类变量处理函数
getCharacter = function(x) {
  detail = '字符型变量'
  range = '-'
  return(c(detail, range))
}

# Date类变量处理函数
getDate = function(x) {
  detail = '日期型变量'
  range = paste0('[', min(x, na.rm = T), ', ', max(x, na.rm = T), ']', collapse = '')
  return(c(detail, range))
}

# 根据变量类别选取相应的函数
switcher = function(variable, variable_type) {
  switch (variable_type,
          'numeric' = getNumeric(variable),
          'factor' = getFactor(variable),
          'integer' = getNumeric(variable),
          'character' = getCharacter(variable),
          'Date' = getDate(variable))
}

# 提取变量说明表
get_summary = function(data) {
  summary_all = data.frame()
  for (i in 1:ncol(data)) {
    var_summary = data.frame(
      变量名称 = colnames(data)[i],
      详细说明 = switcher(data[,i], type = class(data[,i]))[1],
      取值范围 = switcher(data[,i], type = class(data[,i]))[2]
    )
    summary = rbind(summary_all, var_summary)
  }
  return(summary_all)
}
