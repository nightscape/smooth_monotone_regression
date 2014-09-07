name := "monotone-regression"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.2"

val BreezeVersion = "0.9"

incOptions := incOptions.value.withNameHashing(true)

unmanagedResourceDirectories in Compile <+= baseDirectory( _ / "src" / "main" / "scala" )


libraryDependencies ++= Seq(
  "com.softwaremill.macwire" %% "macros" % "0.7",
  "com.softwaremill.macwire" %% "runtime" % "0.7",
  "org.scalanlp" %% "breeze-viz" % BreezeVersion,
  "org.scalanlp" %% "breeze" % BreezeVersion,
  "org.scalanlp" %% "breeze-natives" % BreezeVersion,
  "com.chuusai" %% "shapeless" % "2.0.0",
  "org.rogach" %% "scallop" % "0.9.5",
  "com.softwaremill.scalamacrodebug" %% "macros" % "0.4",
  "org.scalactic" %% "scalactic" % "2.2.1",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
  "org.scalamock" %% "scalamock" % "3.1.2" % "test",
  "org.scalamock" %% "scalamock-scalatest-support" % "3.1.2" % "test",
  "junit" % "junit" % "4.11" % "test",
  "org.spire-math" %% "spire" % "0.8.2",
  "com.github.wookietreiber" %% "scala-chart" % "0.4.2"
)

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "org.catch22" at "http://marklister.github.io/product-collections/",
  "bla bintray" at "http://dl.bintray.com/advaitraut/maven",
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  Classpaths.typesafeReleases
)

