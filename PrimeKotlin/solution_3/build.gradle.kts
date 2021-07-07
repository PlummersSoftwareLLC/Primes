plugins {
    kotlin("multiplatform") version "1.5.20"
    application
}

group = "primes"
version = "1.0"

repositories {
    mavenCentral()
}

kotlin {
    jvm {
        compilations.all {
            kotlinOptions.jvmTarget = "16"
        }
        testRuns["test"].executionTask.configure {
            useJUnit()
        }
    }
    js(IR) {
        nodejs {
        }
        binaries.executable()
    }
    val hostOs = System.getProperty("os.name")
    val isMingwX64 = hostOs.startsWith("Windows")
    val nativeTarget = when {
        hostOs == "Mac OS X" -> macosX64("native")
        hostOs == "Linux" -> linuxX64("native")
        isMingwX64 -> mingwX64("native")
        else -> throw GradleException("Host OS is not supported in Kotlin/Native.")
    }

    nativeTarget.binaries {
        executable {
            entryPoint = "main"
        }
    }

    sourceSets {
        val commonMain by getting
        val jvmMain by getting
        val jsMain by getting
        val nativeMain by getting
    }
}
