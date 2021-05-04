import mill._, scalalib._, scalanativelib._

val scalaVersionString = "2.13.5"

object sieve extends ScalaNativeModule {
  def scalaVersion       = scalaVersionString
  def scalaNativeVersion = "0.4.0"
  def releaseMode = scalanativelib.api.ReleaseMode.ReleaseFull
  // def releaseMode = scalanativelib.api.ReleaseMode.Debug // optional
  def nativeGC = "none"
}

object sieveJVM extends ScalaModule {
  def scalaVersion = scalaVersionString
  def millSourcePath = os.pwd / 'sieve
}
