package models

import scala.collection.JavaConversions._

import com.amazonaws.services.ec2.model.{Instance, Tag}
import models.Ec2InstanceState.Ec2InstanceState

case class Ec2Instance(id: String, state: Ec2InstanceState,
                       name: String = Ec2Instance.defaultName,
                       publicDnsName: String = "") {
  def this(instance: Instance) = this(instance.getInstanceId,
    Ec2InstanceState.fromAwsState(instance.getState),
    Ec2Instance.findNameInTags(instance.getTags),
    instance.getPublicDnsName)

  val isNamed = name != Ec2Instance.defaultName
}

object Ec2Instance {
  private val defaultName = "<unnamed>"

  private def findNameInTags(tags: java.util.List[Tag]): String = {
    for (tag <- tags) {
      if (tag.getKey == "Name")
        return tag.getValue
    }
    defaultName
  }
}
