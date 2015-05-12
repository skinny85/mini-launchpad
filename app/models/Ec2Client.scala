package models

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.ec2.AmazonEC2Client
import play.api.Logger

import scala.collection.JavaConversions._

import com.amazonaws.services.ec2.model._

import scala.util.Try

class Ec2Client(awsKey: String, awsSecret: String) {
  private val ec2Client = new AmazonEC2Client(
    new BasicAWSCredentials(awsKey, awsSecret))

  def instances(): Try[Seq[Ec2Instance]] = {
    Try { ec2Client.describeInstances() } map parseInstancesFromAwsResponse
  }

  def instance(id: String): Try[Option[Ec2Instance]] = {
    Try { ec2Client.describeInstances(new DescribeInstancesRequest()
      .withInstanceIds(id)) } map parseInstancesFromAwsResponse map (s => s.headOption)
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

  def createInstance(ami: Ec2Ami): Try[String] = {
    val groupName = "mini-lpad-sg-" + timestamp()
    val keyName = "mini-lpad-kp-" + timestamp()
    for {
      _ <- createHttpEnabledSecurityGroup(groupName)
      _ <- createKeyPair(keyName)
      instanceRes <- doCreateInstance(ami, groupName, keyName)
    } yield instanceRes.getReservation.getInstances.get(0).getInstanceId
  }

  def createHttpEnabledSecurityGroup(groupName: String): Try[String] = {
    val secGroupReq = new CreateSecurityGroupRequest()
      .withGroupName(groupName)
      .withDescription("My security group")
    val ret = Try { ec2Client.createSecurityGroup(secGroupReq) } map (_.getGroupId)
    ret flatMap { gid =>
      val ipPermission = new IpPermission()
        .withIpRanges("0.0.0.0/0")
        .withIpProtocol("tcp")
        .withFromPort(80)
        .withToPort(80)
      val secGroupInReq = new AuthorizeSecurityGroupIngressRequest()
        .withGroupName(groupName)
        .withIpPermissions(ipPermission)
      Try { ec2Client.authorizeSecurityGroupIngress(secGroupInReq) } map (_ => gid)
    }
  }

  def createKeyPair(keyName: String): Try[Unit] = {
    val keyPairReq = new CreateKeyPairRequest()
      .withKeyName(keyName)
    Try { ec2Client.createKeyPair(keyPairReq) }
  }

  private def doCreateInstance(ami: Ec2Ami, groupName: String, keyName: String): Try[RunInstancesResult] = {
    val instanceReq = new RunInstancesRequest()
      .withImageId(ami.amiId)
      .withInstanceType(ami.minInstanceType)
      .withMinCount(1)
      .withMaxCount(1)
      .withKeyName(keyName)
      .withSecurityGroups(groupName)
    Try { ec2Client.runInstances(instanceReq) }
  }

  def terminateInstance(id: String): Try[Unit] = {
    Try { ec2Client.terminateInstances(new TerminateInstancesRequest().withInstanceIds(id)) }
  }

  private def timestamp(): Long = {
    Math.abs(System.nanoTime())
  }
}
