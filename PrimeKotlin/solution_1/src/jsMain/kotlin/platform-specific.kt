import kotlin.js.Date

actual fun getSystemTimeMillis() = Date.now()
actual fun defaultCoresCount() = -1

external val process: dynamic

actual fun getArgs() = process.argv.slice(2) as Array<String>

actual inline fun <reified T : PrimeSieve> benchmarkPrimeSieve(
    multithreaded: Boolean,
    crossinline newInstance: (Int) -> T
)  {
    if (multithreaded) error("JS doesn't support multi-threading")
    runTest(newInstance).print("js", "single")
}
