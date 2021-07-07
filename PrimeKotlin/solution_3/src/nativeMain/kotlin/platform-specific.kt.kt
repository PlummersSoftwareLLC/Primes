import kotlin.system.getTimeMillis

actual fun getSystemTimeMillis() = getTimeMillis().toDouble()

actual val platformName = "Native"