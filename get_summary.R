getNumeric = function(x) {
  detail = '单位：'
  range = paste0('[', min(x), ', ', max(x), ']', collapse = '')
  return(c(detail, range))
}

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

getCharacter = function(x) {
  detail = '字符型变量'
  range = '-'
  return(c(detail, range))
}

getDate = function(x) {
  detail = '日期型变量'
  range = paste0('[', min(x, na.rm = T), ', ', max(x, na.rm = T), ']', collapse = '')
  return(c(detail, range))
}

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
  summary = data.frame()
  for (i in 1:ncol(data)) {
    type = class(data[,i])
    detail = switcher(data[,i], type)[1]
    range = switcher(data[,i], type)[2]
    name = colnames(data)[i]
    sum = data.frame(
      变量名称 = name,
      详细说明 = detail,
      取值范围 = range
    )
    summary = rbind(summary, sum)
  }
  return(summary)
}