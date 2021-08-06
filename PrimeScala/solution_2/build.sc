import mill._, scalalib._, scalanativelib._, mill.scalajslib._

val scalaVersionString = "2.13.5"

object sieveJVM extends ScalaModule {
  def scalaVersion = scalaVersionString
  def millSourcePath = os.pwd / 'sieve
}

object sieve extends ScalaNativeModule {
  def scalaVersion = scalaVersionString
  def scalaNativeVersion = "0.4.0"
  def releaseMode = scalanativelib.api.ReleaseMode.ReleaseFull
  // def releaseMode = scalanativelib.api.ReleaseMode.Debug // optional
  def nativeGC = "immix"
}

object sieveJS extends ScalaJSModule {
  def scalaVersion = scalaVersionString
  def scalaJSVersion = T { "1.6.0" }
  def millSourcePath = os.pwd / 'sieve
}
