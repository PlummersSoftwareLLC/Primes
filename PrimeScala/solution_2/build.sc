import mill._, scalalib._, scalanativelib._, mill.scalajslib._

val scalaVersionString = "2.13.5"

object sieve extends ScalaModule {
  def scalaVersion = scalaVersionString
}

object sieveNative extends ScalaNativeModule {
  def scalaVersion = scalaVersionString
  def scalaNativeVersion = "0.4.0"
  def releaseMode = scalanativelib.api.ReleaseMode.ReleaseFull
  // def releaseMode = scalanativelib.api.ReleaseMode.Debug // optional
  def nativeGC = "immix"
  def millSourcePath = os.pwd / 'sieve
}

object sieveJS extends ScalaJSModule {
  def scalaVersion = scalaVersionString
  def scalaJSVersion = T { "1.6.0" }
  def millSourcePath = os.pwd / 'sieve
}
