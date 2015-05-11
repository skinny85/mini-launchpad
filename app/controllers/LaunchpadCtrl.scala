package controllers

import models.Ec2InstanceState.Ec2InstanceState
import models._

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.Play.current

import play.api.libs.json._
import play.api.mvc.WebSocket.FrameFormatter

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

        val call = if (launchpad.dryRun) {
          val instances = ec2Client.instances()
          routes.LaunchpadCtrl.instance(instances.head.id)
        } else {
          val instanceId = ec2Client.createInstance(BitnamiWordpressAmi_4_2_2_0_64bit)
          routes.LaunchpadCtrl.instance(instanceId)
        }

        Redirect(call).withSession(
          AWS_KEY -> launchpad.awsApiKey,
          AWS_SECRET -> launchpad.awsApiSecret
        )
      }
    )
  }

  def instance(id: String) = Action { req =>
    val ec2Client = makeEc2ClientFromSession(req.session)
    Ok(views.html.instance(ec2Client.instance(id)))
  }

  def terminate(id: String) = Action { req =>
    val ec2Client = makeEc2ClientFromSession(req.session)
    ec2Client.terminateInstance(id)
    Ok(views.html.terminated(id))
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
