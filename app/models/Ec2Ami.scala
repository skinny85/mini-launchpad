package models

trait Ec2Ami {
  def amiId: String
  def minInstanceType: String
}
