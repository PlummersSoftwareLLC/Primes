import kotlin.system.getTimeMillis

actual fun currentTimeMillis(): Long {
    return getTimeMillis()
}
