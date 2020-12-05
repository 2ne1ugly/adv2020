name := "adv2020"
ThisBuild / scalaVersion := "2.13.4"
ThisBuild / scalacOptions ++= Seq()

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.1.1"
    )
  )
  .settings(
    resolvers += Resolver.url("typesafe", url("https://repo.typesafe.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns)
  )
