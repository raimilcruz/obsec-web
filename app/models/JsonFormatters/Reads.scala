package models.JsonFormatters

import models.judgment.JudgmentStepApi
import models.judgment.JudgmentRequestApi
import play.api.libs.json._

//Json Readers
object JudgmentJsonReaders{
  val requestReader : Map[String,JudgmentReader] =
    Map("less-eq" ->  new LessEqJudgmentReader,
      "eq" ->  new EqJudgmentReader,
      "typing" -> new Typing.TypingJudgmentReader
    )

  implicit val judgmentRequesReads = new Reads[JudgmentRequestApi]{
    override def reads(json: JsValue): JsResult[JudgmentRequestApi] = {
      val keyReader = (JsPath \ "key").read[String]
      keyReader.reads(json) match {
        case JsSuccess(key,_) => requestReader (key).reads.reads(json)
        case x2:JsError => x2
      }
    }
  }
}

trait JudgmentReader{
  def key:String
  def reads : Reads[JudgmentRequestApi]
}
