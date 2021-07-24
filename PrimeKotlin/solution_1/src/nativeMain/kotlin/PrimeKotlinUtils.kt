import kotlin.system.getTimeMillis

actual fun currentTimeMillis(): Long {
    return getTimeMillis()
}

actual val label = "native"