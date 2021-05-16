# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/steps/choice_rule.py

#' @include inputs_placeholders.R
#' @include utils.R

#' @import R6

VALIDATORS = Enum(
  String="character",
  Numeric=c("integer", "double"),
  Boolean="logical",
  Timestamp="character",
  Is="logical"
)

BaseRule = R6Class("BaseRule",
  public = list(
    to_list = function(){
      return(list())
    },
    format = function(){
      return(sprintf("%s()", class(self)[1]))
    }
  )
)

#' @title Rule Class
#' @description Class for creating a rule.
#' @export Rule
Rule = R6Class("Rule",
  inherit = BaseRule,
  public = list(

    #' @description Initialize Rule class
    #' @param variable (str): Path to the variable to compare.
    #' @param operator (str): Comparison operator to be applied.
    #' @param value (type depends on *operator*): Constant value or Path to compare `variable` against.
    initialize = function(variable,
                          operator,
                          value){
      # Validate the variable name
      if (!inherits(variable, "StepInput") && !startsWith(variable, '$'))
        stop(sprintf("Expected variable must be a placeholder or must start with '$', but got '%s'", variable))

      # Validate the variable value
      # If operator ends with Path, value must be a Path
      if (endsWith(operator,"Path")){
        if (!inherits(value, "StepInput") && !startsWith(value, '$'))
          stop(sprintf("Expected value must be a placeholder or must start with '$', but got '%s'", value))
      } else {
        for (k in names(VALIDATORS)){
          v = VALIDATORS[[k]]
          if (startsWith(operator, k) && !inherits(value, v))
            stop(sprintf('Expected value to be a %s, but got %s', k, value))
          }
      }
      self$variable = variable
      self$operator = operator
      self$value = value
    },

    #' @description Convert class to list ready to be translated for
    #'              `Amazon States Language <https://states-language.net/spec.html>`.
    to_list = function(){
      if (isinstance(self$variable, "StepInput")){
        result = list('Variable'=self$variable$to_jsonpath())
      } else {
          result = list('Variable'=self$variable)
          result[[self$operator]] = self$value}
      return(result)
    },

    #' @description class formatting
    format = function(){
      cls_fmt = "%s(variable=[%s], operator=[%s], value=[%s])"
      var_fmt = if(!is.null(self$variable)) paste(self$variable, sep=", ", collapse = ", ") else ""
      op_fmt = if(!is.null(self$operator)) paste(self$operator, sep=", ", collapse = ", ") else ""
      val_fmt = if(!is.null(self$value)) paste(self$value, sep=", ", collapse = ", ") else ""
      return(sprintf(cls_fmt,class(self)[1], var_fmt,op_fmt,val_fmt))
    }
  ),
  lock_objects=F
)

#' @title CompoundRule class
#' @description Class for creating a compound rule.
#' @export
CompoundRule = R6Class("CompoundRule",
  inherit = BaseRule,
  public = list(

    #' @description initialize CompoundRule class
    #' @param operator (str): Compounding operator to be applied.
    #' @param rules (list(BaseRule)): List of rules to compound together.
    initialize = function(operator, rules){
      for (rule in rules){
        if (!inherits(rule, "BaseRule"))
          stop(sprintf("Rule '%s' is invalid",rule))
      }
      self$operator = operator
      self$rules = rules
    },

    #' @description Convert class to list ready to be translated for
    #'              `Amazon States Language <https://states-language.net/spec.html>`.
    to_list = function(){
      out = list(lapply(self$rules, function(rule) rule$to_list()))
      names(out) = self$operator
      return(out)
    },

    #' @description class formatting
    format = function(){
      cls_fmt = '%s(operator=[%s], rules=[%s])'
      op_fmt = if(!is.null(self$operator)) paste(self$operator, sep=", ", collapse = ", ") else ""
      rule_fmt = if(!is.null(self$rules)) paste(self$rules, sep=", ", collapse = ", ") else ""
      return(sprintf(cls_fmt, class(self)[1], op_fmt, rule_fmt))
    }
  ),
  lock_objects=F
)

#' @title NotRule class
#' @description Class for creating a negation rule.
#' @export
NotRule = R6Class("NotRule",
  inherit = BaseRule,
  public = list(

    #' @description Initialize NotRule class
    #' @param rule (BaseRule): Rule to negate.
    initialize = function(rule){
      if (!inherits(rule, "BaseRule"))
        stop(sprintf("Rule '%s' is invalid",rule))

      self$rule = rule
    },

    #' @description Convert class to list ready to be translated for
    #'              `Amazon States Language <https://states-language.net/spec.html>`.
    to_list = function(){
      return (list('Not'=self$rule$to_list()))
    },

    #' @description class formatting
    format = function(){
      cls_fmt = '%s(rule=%s)'
      rule_fmt = if(!is.null(self$rule)) paste(self$rules, sep=", ", collapse = ", ") else ""
      return(sprintf(cls_fmt, class(self)[1], rule_fmt))
    }
  ),
  lock_objects=F
)

#' @title ChoiceRule class
#' @description Factory class for creating a choice rule.
#' @export
ChoiceRule = R6Class("ChoiceRule",
  public = list(

    #' @description Creates a rule with the `StringEquals` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Constant value to compare `variable` against.
    #' @return Rule: Rule with `StringEquals` operator.
    StringEquals = function(variable, value){
      return(Rule$new(variable, 'StringEquals', value))
    },

    #' @description Creates a rule with the `StringEqualsPath` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Path to the value to compare `variable` against.
    #' @return Rule: Rule with `StringEqualsPath` operator.
    StringEqualsPath = function(variable, value){
      return(Rule$new(variable, 'StringEqualsPath', value))
    },

    #' @description  Creates a rule with the `StringLessThan` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Constant value to compare `variable` against.
    #' @return Rule: Rule with `StringLessThan` operator.
    StringLessThan = function(variable, value){
      return(Rule$new(variable, 'StringLessThan', value))
    },

    #' @description Creates a rule with the `StringLessThanPath` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Path to the value to compare `variable` against.
    #' @return Rule: Rule with `StringLessThanPath` operator.
    StringLessThanPath = function(variable, value){
      return(Rule$new(variable, 'StringLessThanPath', value))
    },

    #' @description Creates a rule with the `StringGreaterThan` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Constant value to compare `variable` against.
    #' @return Rule: Rule with `StringGreaterThan` operator.
    StringGreaterThan = function(variable, value){
      return(Rule$new(variable, 'StringGreaterThan', value))
    },

    #' @description Creates a rule with the `StringGreaterThanPath` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Path to the value to compare `variable` against.
    #' @return Rule: Rule with `StringGreaterThanPath` operator
    StringGreaterThanPath = function(variable, value){
      return(Rule$new(variable, 'StringGreaterThanPath', value))
    },

    #' @description Creates a rule with the `StringLessThanEquals` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Constant value to compare `variable` against.
    #' @return Rule: Rule with `StringLessThanEquals` operator.
    StringLessThanEquals = function(variable, value){
      return(Rule$new(variable, 'StringLessThanEquals', value))
    },

    #' @description Creates a rule with the `StringLessThanEqualsPath` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Path to the value to compare `variable` against.
    #' @return Rule: Rule with `StringLessThanEqualsPath` operator.
    StringLessThanEqualsPath = function(variable, value){
      return(Rule$new(variable, 'StringLessThanEqualsPath', value))
    },

    #' @description Creates a rule with the `StringGreaterThanEquals` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Constant value to compare `variable` against.
    #' @return Rule: Rule with `StringGreaterThanEquals` operator.
    StringGreaterThanEquals = function(variable, value){
      return(Rule$new(variable, 'StringGreaterThanEquals', value))
    },

    #' @description Creates a rule with the `StringGreaterThanEqualsPath` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Path to the value to compare `variable` against.
    #' @return Rule: Rule with `StringGreaterThanEqualsPath` operator.
    StringGreaterThanEqualsPath = function(variable, value){
      return(Rule$new(variable, 'StringGreaterThanEqualsPath', value))
    },

    #' @description Creates a rule with the `NumericEquals` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (int): Constant value to compare `variable` against.
    #' @return Rule: Rule with `NumericEquals` operator.
    NumericEquals = function(variable, value){
      return(Rule$new(variable, 'NumericEquals', value))
    },

    #' @description Creates a rule with the `NumericEqualsPath` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Path to the value to compare `variable` against.
    #' @return Rule: Rule with `NumericEqualsPath` operator.
    NumericEqualsPath = function(variable, value){
      return(Rule$new(variable, 'NumericEqualsPath', value))
    },

    #' @description Creates a rule with the `NumericLessThan` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (int): Constant value to compare `variable` against.
    #' @return Rule: Rule with `NumericLessThan` operator.
    NumericLessThan = function(variable, value){
      return(Rule$new(variable, 'NumericLessThan', value))
    },

    #' @description Creates a rule with the `NumericLessThanPath` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Path to the value to compare `variable` against.
    #' @return Rule: Rule with `NumericLessThanPath` operator.
    NumericLessThanPath = function(variable, value){
      return(Rule$new(variable, 'NumericLessThanPath', value))
    },

    #' @description Creates a rule with the `NumericGreaterThan` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (int): Constant value to compare `variable` against.
    #' @return Rule: Rule with `NumericGreaterThan` operator.
    NumericGreaterThan = function(variable, value){
      return(Rule$new(variable, 'NumericGreaterThan', value))
    },

    #' @description Creates a rule with the `NumericGreaterThanPath` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Path to the value to compare `variable` against.
    #' @return Rule: Rule with `NumericGreaterThanPath` operator.
    NumericGreaterThanPath = function(variable, value){
      return(Rule$new(variable, 'NumericGreaterThanPath', value))
    },

    #' @description Creates a rule with the `NumericLessThanEquals` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (int): Constant value to compare `variable` against.
    #' @return Rule: Rule with `NumericLessThanEquals` operator.
    NumericLessThanEquals = function(variable, value){
      return(Rule$new(variable, 'NumericLessThanEquals', value))
    },

    #' @description Creates a rule with the `NumericLessThanEqualsPath` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Path to the value to compare `variable` against.
    #' @return Rule: Rule with `NumericLessThanEqualsPath` operator
    NumericLessThanEqualsPath = function(variable, value){
      return(Rule$new(variable, 'NumericLessThanEqualsPath', value))
    },

    #' @description Creates a rule with the `NumericGreaterThanEquals` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (int): Constant value to compare `variable` against.
    #' @return Rule: Rule with `NumericGreaterThanEquals` operator.
    NumericGreaterThanEquals = function(variable, value){
      return(Rule$new(variable, 'NumericGreaterThanEquals', value))
    },

    #' @description Creates a rule with the `NumericGreaterThanEqualsPath` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Path to the value to compare `variable` against.
    #' @return Rule: Rule with `NumericGreaterThanEqualsPath` operator.
    NumericGreaterThanEqualsPath = function(variable, value){
      return(Rule$new(variable, 'NumericGreaterThanEqualsPath', value))
    },

    #' @description Creates a rule with the `BooleanEquals` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (bool): Constant value to compare `variable` against.
    #' @return Rule: Rule with `BooleanEquals` operator.
    BooleanEquals = function(variable, value){
      return(Rule(variable, 'BooleanEquals', value))
    },

    #' @description Creates a rule with the `BooleanEqualsPath` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Path to the value to compare `variable` against.
    #' @return Rule: Rule with `BooleanEqualsPath` operator.
    BooleanEqualsPath = function(variable, value){
      return(Rule$new(variable, 'BooleanEqualsPath', value))
    },

    #' @description Creates a rule with the `TimestampEquals` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Constant value to compare `variable` against.
    #' @return Rule: Rule with `TimestampEquals` operator.
    TimestampEquals = function(variable, value){
      return(Rule$new(variable, 'TimestampEquals', value))
    },

    #' @description Creates a rule with the `TimestampEqualsPath` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Path to the value to compare `variable` against.
    #' @return Rule: Rule with `TimestampEqualsPath` operator.
    TimestampEqualsPath = function(variable, value){
      return(Rule$new(variable, 'TimestampEqualsPath', value))
    },

    #' @description Creates a rule with the `TimestampLessThan` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Constant value to compare `variable` against.
    #' @return Rule: Rule with `TimestampLessThan` operator.
    TimestampLessThan = function(variable, value){
      return(Rule$new(variable, 'TimestampLessThan', value))
    },

    #' @description Creates a rule with the `TimestampLessThanPath` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Path to the value to compare `variable` against.
    #' @return Rule: Rule with `TimestampLessThanPath` operator.
    TimestampLessThanPath = function(variable, value){
      return(Rule$new(variable, 'TimestampLessThanPath', value))
    },

    #' @description Creates a rule with the `TimestampGreaterThan` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Constant value to compare `variable` against.
    #' @return Rule: Rule with `TimestampGreaterThan` operator.
    TimestampGreaterThan = function(variable, value){
      return(Rule$new(variable, 'TimestampGreaterThan', value))
    },

    #' @description Creates a rule with the `TimestampGreaterThanPath` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Path to the value to compare `variable` against.
    #' @return Rule: Rule with `TimestampGreaterThanPath` operator.
    TimestampGreaterThanPath = function(variable, value){
      return(Rule$new(variable, 'TimestampGreaterThanPath', value))
    },

    #' @description Creates a rule with the `TimestampLessThanEquals` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Constant value to compare `variable` against.
    #' @return Rule: Rule with `TimestampLessThanEquals` operator.
    TimestampLessThanEquals = function(variable, value){
      return(Rule$new(variable, 'TimestampLessThanEquals', value))
    },

    #' @description Creates a rule with the `TimestampLessThanEqualsPath` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Path to the value to compare `variable` against.
    #' @return Rule: Rule with `TimestampLessThanEqualsPath` operator.
    TimestampLessThanEqualsPath = function(variable, value){
      return(Rule$new(variable, 'TimestampLessThanEqualsPath', value))
    },

    #' @description Creates a rule with the `TimestampGreaterThanEquals` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Constant value to compare `variable` against.
    #' @return Rule: Rule with `TimestampGreaterThanEquals` operator.
    TimestampGreaterThanEquals = function(variable, value){
      return(Rule$new(variable, 'TimestampGreaterThanEquals', value))
    },

    #' @description Creates a rule with the `TimestampGreaterThanEqualsPath` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): Path to the value to compare `variable` against.
    #' @return Rule: Rule with `TimestampGreaterThanEqualsPath` operator.
    TimestampGreaterThanEqualsPath = function(variable, value){
      return(Rule$new(variable, 'TimestampGreaterThanEqualsPath', value))
    },

    #' @description Creates a rule with the `IsNull` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (bool): Whether the value at `variable` is equal to the JSON literal null or not.
    #' @return Rule: Rule with `IsNull` operator.
    IsNull = function(variable, value){
      return(Rule$new(variable, 'IsNull', value))
    },

    #' @description Creates a rule with the `IsPresent` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (bool): Whether a field at `variable` exists in the input or not.
    #' @return Rule: Rule with `IsPresent` operator.
    IsPresent = function(variable, value){
      return(Rule$new(variable, 'IsPresent', value))
    },

    #' @description Creates a rule with the `IsString` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (bool): Whether the value at `variable` is a string or not.
    #' @return Rule: Rule with `IsString` operator.
    IsString = function(variable, value){
      return(Rule$new(variable, 'IsString', value))
    },

    #' @description Creates a rule with the `IsNumeric` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (bool): Whether the value at `variable` is a number or not.
    #' @return Rule: Rule with `IsNumeric` operator.
    IsNumeric = function(variable, value){
      return(Rule$new(variable, 'IsNumeric', value))
    },

    #' @description Creates a rule with the `IsTimestamp` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (bool): Whether the value at `variable` is a timestamp or not.
    #' @return Rule: Rule with `IsTimestamp` operator.
    IsTimestamp = function(variable, value){
      return(Rule$new(variable, 'IsTimestamp', value))
    },

    #' @description Creates a rule with the `IsBoolean` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (bool): Whether the value at `variable` is a boolean or not.
    #' @return Rule: Rule with `IsBoolean` operator.
    IsBoolean = function(variable, value){
      return(Rule$new(variable, 'IsBoolean', value))
    },

    #' @description Creates a rule with the `StringMatches` operator.
    #' @param variable (str): Path to the variable to compare.
    #' @param value (str): A string pattern that may contain one or more `*` characters
    #'              to compare the value at `variable` to.
    #'              The `*` character can be escaped using two backslashes.
    #'              The comparison yields true if the variable matches the pattern, where
    #'              `*` is a wildcard that matches zero or more characters.
    #' @return Rule: Rule with `StringMatches` operator.
    StringMatches = function(variable, value){
      return(Rule$new(variable, 'StringMatches', value))
    },

    #' @description Creates a compound rule with the `And` operator.
    #' @param rules (list(BaseRule)): List of rules to compound together.
    #' @return CompoundRule: Compound rule with `And` operator.
    And = function(rules){
      return(CompoundRule$new('And', rules))
    },

    #' @description Creates a compound rule with the `Or` operator.
    #' @param rules (list(BaseRule)): List of rules to compound together.
    #' @return CompoundRule: Compound rule with `Or` operator.
    Or = function(rules){
      return(CompoundRule$new('Or', rules))
    },

    #' @description Creates a negation for a rule.
    #' @param rule (BaseRule): Rule to Negate.
    #' @return NotRule: Rule with `Not` operator.
    Not = function(rule){
      return(NotRule$new(rule))
    }
  )
)
