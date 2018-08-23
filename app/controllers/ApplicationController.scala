package controllers

import javax.inject._

import models.IndexModel
import play.api.mvc._


@Singleton
class ApplicationController @Inject()(configuration: play.api.Configuration) extends Controller {
  def index(prod: Int) = Action { implicit request =>
    Ok(views.html.index(prod,
      IndexModel(
        "ObSec Pad",
        "GObSec Pad, an online interpreter to experiment with GObSec, " +
          "a security typed-language with type-base declassification")))
  }
}
