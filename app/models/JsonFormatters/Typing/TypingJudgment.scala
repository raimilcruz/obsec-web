package models.JsonFormatters.Typing

import Common.judgment._
import Common.judgment.examples.lambdacalculus._
import models.JsonFormatters.{JudgmentJsonWriters, JudgmentReader, JudgmentWriter, SideConditionWriter}
import models.judgment.JudgmentRequestApi
import play.api.libs.json._
import play.api.libs.functional.syntax._


class TypingJudgmentReader  extends JudgmentReader {
  override def key: String = "typing"


  implicit object ContextReads extends Reads[JudgmentContext] {
    def reads(json: JsValue) = json match {
      case JsNull => JsSuccess(new TypingEnvironment)
      case JsString(s) =>
        EnvironmentParser(s) match{
          case Right(x)=> listToEnv(x) match{
            case Left(env) => JsSuccess(env)
            case Right(e) => JsError(e)
          }
          case Left(e) => JsError(s"error.parsing.context.$e")
        }
    }
  }

  implicit object goalReads extends Reads[TypingGoal]{
    override def reads(json: JsValue): JsResult[TypingGoal] = {
        val sTerm = (JsPath \ "expr").read[String].reads(json)
        sTerm match{
          case JsSuccess(t,_)=>
            LambdaCalculusParser(t) match {
              case Right(e) => JsSuccess(TypingGoal(e))
              case Left(e) => JsError(s"error.parsing.$e")
            }
          case JsError(e) => JsError(e)
        }

    }
  }

  private def listToEnv(pairs:List[(String,String)]):Either[TypingEnvironment,String]= pairs match{
    case List()=> Left(new TypingEnvironment())
    case h::t =>
      val tEnv = listToEnv(t)
      tEnv match{
        case Left(r)=>
          if(r.contains(h._1))
            Right(s"Variable already defined ${h._1}")
          else{
            LambdaCalculusParser.parseType(h._2) match{
              case Right(theType)=> Left(r.extend(h._1,theType))
              case Left(e)=> Right(e.toString)
            }
          }
        case _ => tEnv
      }
  }


  override def reads: Reads[JudgmentRequestApi] =
    (
      (JsPath \ "key").read[String] and
        (JsPath \ "stepId").read[String] and
        (JsPath \ "context").read[JudgmentContext] and
        (JsPath \ "goal").read[TypingGoal]
      )(JudgmentRequestApi.apply _)
}

class TypingJudgmentWriter  extends JudgmentWriter {
  override def key: String = "typing"

  implicit object ContextWrites extends Writes[JudgmentContext] {
    override def writes(o: JudgmentContext): JsValue = o match{
      case tEnv:TypingEnvironment =>
        JsString(tEnv.mapping.map(p=> s"${p._1}:${p._2.toSource}").mkString(","))
      case _ => JsString("")
    }
  }

  implicit val goalWrites: Writes[TypingGoal] = new Writes[TypingGoal] {
    override def writes(o: TypingGoal): JsValue = Json.obj(
      "expr" -> o.e.toSource,
      "rep" -> o.e.mathify
    )
  }

  override def premiseWrites: Writes[JudgmentPremise] = new Writes[JudgmentPremise] {
    override def writes(o: JudgmentPremise): JsValue = Json.obj(
      "key" -> key,
      "context" -> o.context,
      "goal" -> o.goal.asInstanceOf[TypingGoal],
      "output"-> o.output.mathify,
      "rep" -> conclusionRep(o)
     )
  }

  override def stepWrites(stepId:String): Writes[JudgmentStep] = JudgmentJsonWriters.judgmentStepWriter(key,stepId)



  override def conclusionRep(judgment: JudgmentBase): String = {
    val outputRep = getOutputRep(judgment.output)
    s"${judgment.context.asInstanceOf[TypingEnvironment].mathify} \\vdash ${judgment.goal.asInstanceOf[TypingGoal].e.mathify} : $outputRep"
  }
  private def getOutputRep(output: JudgmentOutput) = output match{
    case TypingJudgmentOutput(t) => t.mathify
    case PendingOutput(t)=> t.mathify
    case _ => "??"
  }

}
class TypingJudgmentEqTypeWriter extends SideConditionWriter {
  override def key: String = "lambda-calculus-type-eq"

  override def writes: Writes[JudgmentPredicate] = new Writes[JudgmentPredicate] {
    override def writes(o: JudgmentPredicate): JsValue = {
      val typeEq = o.asInstanceOf[TypeEqPredicate]
      Json.obj(
        "key" -> key,
        "rep" -> s"${typeEq.t1.toSource} == ${typeEq.t2.toSource}",
        "args" -> List(typeEq.t1.toSource,typeEq.t2.toSource).mkString(",")
      )
    }
  }
}

