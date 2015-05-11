package models

import com.amazonaws.services.ec2.model.InstanceState
import play.api.libs.json.JsString

object Ec2InstanceState extends Enumeration {
  type Ec2InstanceState = Value
  val Pending, Running, ShuttingDown, Terminated, Stopping, Stopped = Value

  def fromAwsState(state: InstanceState): Ec2InstanceState = {
    state.getCode.intValue() match {
      case  0 => Pending
      case 16 => Running
      case 32 => ShuttingDown
      case 48 => Terminated
      case 64 => Stopping
      case 80 => Stopped
      case  _ => throw new IllegalArgumentException(s"Unrecognized instance state: $state")
    }
  }

  def unapply(str: String): Option[Ec2InstanceState] = str match {
    case "Pending"      => Some(Pending)
    case "Running"      => Some(Running)
    case "ShuttingDown" => Some(ShuttingDown)
    case "Terminated"   => Some(Terminated)
    case "Stopping"     => Some(Stopping)
    case "Stopped"      => Some(Stopped)
    case _              => None
  }
}
