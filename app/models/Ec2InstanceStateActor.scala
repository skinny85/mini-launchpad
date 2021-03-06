package models

import akka.actor._
import models.Ec2InstanceState.Ec2InstanceState
import play.api.Logger
import scala.concurrent.duration._
import scala.util.{Success, Failure}

object Ec2InstanceStateActor {
  def props(ec2client: Ec2Client, instanceId: String, out: ActorRef) =
    Props(new Ec2InstanceStateActor(ec2client, instanceId, out))
}

class Ec2InstanceStateActor(ec2Client: Ec2Client, instanceId: String,
                            out: ActorRef) extends Actor {
  override def preStart() = scheduleTick()

  // override postRestart so we don't call preStart and schedule a new message
  override def postRestart(reason: Throwable) = {}

  def receive = {
    case _ =>
      ec2Client.instance(instanceId) match {
        case Failure(e) =>
          Logger.warn("Error checking instance state", e)
          // client probably broken - no point keeping the connection
          closeWebsocket()
        case Success(maybeInstance) => maybeInstance match {
          case None =>
            // no such instance - nothing more to do
            closeWebsocket()
          case Some(instance) =>
            out ! Ec2InstanceStateActorRes(instance.state, instance.publicDnsName)
            if (instance.state != Ec2InstanceState.Running)
              scheduleTick()
            else
              closeWebsocket()
        }
      }
  }

  private def closeWebsocket() {
    self ! PoisonPill
  }

  private def scheduleTick(): Unit = {
    context.system.scheduler.scheduleOnce(1000 millis, self, "tick")
  }

  private implicit val ec = play.api.libs.concurrent.Execution.Implicits.defaultContext
}

case class Ec2InstanceStateActorRes(state: Ec2InstanceState, publicDnsName: String = "")
