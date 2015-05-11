package models

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.ec2.AmazonEC2Client
import play.api.Logger

import scala.collection.JavaConversions._

import com.amazonaws.services.ec2.model._

class Ec2Client(awsKey: String, awsSecret: String) {
  private val ec2Client = new AmazonEC2Client(
    new BasicAWSCredentials(awsKey, awsSecret))

  def instances(): Seq[Ec2Instance] = {
    parseInstancesFromAwsResponse(ec2Client.describeInstances())
  }

  def instance(id: String): Ec2Instance = {
    parseInstancesFromAwsResponse(ec2Client.describeInstances(new DescribeInstancesRequest()
      .withInstanceIds(id))).head
  }

  private def parseInstancesFromAwsResponse(instancesRes: DescribeInstancesResult):
      Seq[Ec2Instance] = {
    var ret = Seq.empty[Ec2Instance]
    val reservations = instancesRes.getReservations
    for (reservation <- reservations) {
      val instances = reservation.getInstances
      for (instance <- instances) {
        ret = ret :+ new Ec2Instance(instance)
      }
    }
    ret
  }

  def createInstance(ami: Ec2Ami): String = {
    val groupName = "mini-lpad-sg-" + timestamp()
    val secGroupReq = new CreateSecurityGroupRequest()
      .withGroupName(groupName)
      .withDescription("My security group")
    val secGroupRes = ec2Client.createSecurityGroup(secGroupReq)

    val ipPermission = new IpPermission()
      .withIpRanges("0.0.0.0/0")
      .withIpProtocol("tcp")
      .withFromPort(80)
      .withToPort(80)
    val secGroupInReq = new AuthorizeSecurityGroupIngressRequest()
      .withGroupName(groupName)
      .withIpPermissions(ipPermission)
    ec2Client.authorizeSecurityGroupIngress(secGroupInReq)

    val keyName = "mini-lpad-kp-" + timestamp()
    val keyPairReq = new CreateKeyPairRequest()
      .withKeyName(keyName)
    val keyPairRes = ec2Client.createKeyPair(keyPairReq)

    val instanceReq = new RunInstancesRequest()
      .withImageId(ami.amiId)
      .withInstanceType(ami.minInstanceType)
      .withMinCount(1)
      .withMaxCount(1)
      .withKeyName(keyName)
      .withSecurityGroups(groupName)
    val instanceRes = ec2Client.runInstances(instanceReq)
    Logger.info("RunInstancesResult: " + instanceRes)
    instanceRes.getReservation.getInstances.get(0).getInstanceId
  }

  def terminateInstance(id: String): Unit = {
    val terminateInstancesRes = ec2Client.terminateInstances(new TerminateInstancesRequest().withInstanceIds(id))
    Logger.info("TerminateInstancesResult: " + terminateInstancesRes)
  }

  private def timestamp(): Long = {
    Math.abs(System.nanoTime())
  }
}
