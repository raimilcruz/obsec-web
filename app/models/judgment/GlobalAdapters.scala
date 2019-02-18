package models.judgment

import Common.judgment.{FunctionAdapter, JudgmentAdapter, PredicateAdapter}
import Common.judgment.examples.lambdacalculus.{DomainFunctionAdapter, TypeEqPredicateAdapter, TypingJudgmentAdapter}
import Common.judgment.examples.{EqJudgmentAdapter, LessEqJudgmentAdapter}

object GlobalAdapters{
  def judgmentAdapters : Map[String,JudgmentAdapter] = Map(
    "less-eq" -> new LessEqJudgmentAdapter,
    "eq" -> new EqJudgmentAdapter,
    "typing" -> new TypingJudgmentAdapter
  )
  def predicateAdapters : Map[String,PredicateAdapter] = Map(
    "lambda-calculus-type-eq" -> new TypeEqPredicateAdapter
  )
  def functionAdapters : Map[String,FunctionAdapter] = Map(
    "domain" -> new DomainFunctionAdapter,
    "codomain" -> new DomainFunctionAdapter
  )
}

