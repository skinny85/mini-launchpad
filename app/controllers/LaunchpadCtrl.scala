package controllers

import models.Ec2InstanceState.Ec2InstanceState
import models._

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.Play.current

import play.api.libs.json._
import play.api.mvc.WebSocket.FrameFormatter
import play.twirl.api.Html

import scala.util.{Failure, Success}

object LaunchpadCtrl extends Controller {
  case class LaunchpadViewModel(awsApiKey: String, awsApiSecret: String, dryRun: Boolean)

  val launchpadForm = Form(
    mapping(
      "awsApiKey" -> text,
      "awsApiSecret" -> text,
      "dryRun" -> boolean
    )(LaunchpadViewModel.apply)(LaunchpadViewModel.unapply)
  )

  def get(dryRun: Boolean) = Action {
    Ok(views.html.launch(launchpadForm.fill(LaunchpadViewModel("", "", dryRun))))
  }

  val AWS_KEY = "awsApiKey"
  val AWS_SECRET = "awsApiSecret"

  def post = Action { implicit req =>
    val boundForm = launchpadForm.bindFromRequest()
    boundForm.fold(
      formWithErrors => {
        BadRequest(views.html.launch(formWithErrors))
      },
      launchpad => {
        val ec2Client = new Ec2Client(launchpad.awsApiKey,
          launchpad.awsApiSecret)

        val maybeCall: Either[Html, Call] = if (launchpad.dryRun) {
          ec2Client.instances() match {
            case Success(instances) =>
              Right(routes.LaunchpadCtrl.instance(instances.head.id))
            case Failure(e) =>
              awsInstanceErrorContent(e)
          }
        } else {
          ec2Client.createInstance(BitnamiWordpressAmi_4_2_2_0_64bit) match {
            case Failure(e) =>
              awsInstanceErrorContent(e)
            case Success(instanceId) =>
              Right(routes.LaunchpadCtrl.instance(instanceId))
          }
        }

        maybeCall match {
          case Left(content) =>
            BadRequest(content)
          case Right(call) =>
            Redirect(call).withSession(
              AWS_KEY -> launchpad.awsApiKey,
              AWS_SECRET -> launchpad.awsApiSecret
            )
        }
      }
    )
  }

  private def awsInstanceErrorContent(e: Throwable): Left[Html, Nothing] = {
    Left(views.html.launch(launchpadForm.withGlobalError(
      s"""|There was an error processing your request (${e.getMessage}).
          |Make sure the AWS credentials are entered correctly and try again.""".stripMargin)))
  }

  def instance(id: String) = Action { req =>
    val ec2Client = makeEc2ClientFromSession(req.session)
    Ok(views.html.instance(ec2Client.instance(id)))
  }

  def terminate(id: String) = Action { req =>
    val ec2Client = makeEc2ClientFromSession(req.session)
    ec2Client.terminateInstance(id) match {
      case Failure(e) =>
        BadRequest(views.html.instance(Failure(e)))
      case Success(_) =>
        Redirect(routes.LaunchpadCtrl.instance(id))
    }
  }

  def websocket(instanceId: String) = WebSocket.acceptWithActor[String, Ec2InstanceStateActorRes] { req => {
      out => Ec2InstanceStateActor.props(
        makeEc2ClientFromSession(req.session), instanceId, out)
    }
  }

  private def makeEc2ClientFromSession(session: Session): Ec2Client = {
    new Ec2Client(session.get(AWS_KEY).getOrElse(""), session.get(AWS_SECRET).getOrElse(""))
  }

  private implicit val f0 = new Format[Ec2InstanceState] {
    override def writes(state: Ec2InstanceState): JsValue = JsString(state.toString)
    override def reads(json: JsValue): JsResult[Ec2InstanceState] = json match {
      case JsString(str) =>
        Ec2InstanceState.unapply(str) match {
          case Some(state) => JsSuccess(state)
          case None        => JsError(s"'$str' is not a valid instance state")
        }
      case j => JsError("expected: JSON String, got: " + j)
    }
  }
  private implicit val f = Json.format[Ec2InstanceStateActorRes]
  private implicit val ff = FrameFormatter.jsonFrame[Ec2InstanceStateActorRes]
}
