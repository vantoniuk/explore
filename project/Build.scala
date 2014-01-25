import sbt._
import Keys._

object Explore extends Build {
  val appName = "explore"
  val appVersion = "0.0.1"

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = Project.defaultSettings ++ Seq (
      resolvers := Resolvers.allResolvers,
      libraryDependencies := Akka.dependencies,
      name := appName,
      version := appVersion,
      scalaVersion := "2.10.2"
    )
  )

}

object Resolvers {
  val allResolvers = Seq(Akka.resolver)
}

object Dependencies {
  val allDependencies = Akka.dependencies
}

object DependencyHelper {
  def dependency(group: String, revision: String)(artifact: String) = group %% artifact % revision
  def dependencyIndep(group: String, revision: String)(artifact: String) = group % artifact % revision
}

object Akka {
  val version = "2.2.3"
  val group = "com.typesafe.akka"

  val akkaModule = DependencyHelper.dependency(group, version) _

  val actor = akkaModule("akka-actor")
  val cluster = akkaModule("akka-cluster")
  val slf4j = akkaModule("akka-slf4j")
  val testkit = akkaModule("akka-testkit")

  val resolver = "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

  val dependencies = Seq(actor, cluster, slf4j, testkit)
}