import kotlin.js.Date

actual fun getSystemTimeMillis() = Date.now()

actual val platformName = "JS"