scalaVersion := "2.11.4"

lazy val sprint1 = project.in(file("./sprint1"))

lazy val sprint2 = project.in(file("./sprint2"))

lazy val sprint3 = project.in(file("./sprint3"))

lazy val sprint4 = project.in(file("./sprint4"))

lazy val root = project.in(file(".")).aggregate(
  sprint1,
  sprint2,
  sprint3,
  sprint4
)
