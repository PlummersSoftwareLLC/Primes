import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

plugins {
    kotlin("multiplatform") version "1.8.10"
    id("org.jetbrains.kotlin.plugin.atomicfu") version "1.8.10"
    id("com.github.johnrengelman.shadow") version "7.1.2"
    application
}

group = "primes"
version = "1.0"

application {
    mainClass.set("MainKt")
}

repositories {
    mavenCentral()
}

kotlin {
    val jvmTarget = jvm {
        compilations.all {
            kotlinOptions.jvmTarget = "19"
        }
    }

    js(IR) {
        nodejs {}
        binaries.executable()
    }

    val hostOs = System.getProperty("os.name")
    val hostArch = System.getProperty("os.arch")
    val isMingwX64 = hostOs.startsWith("Windows")
    val nativeTarget = when {
        hostOs == "Mac OS X" -> if (hostArch == "aarch64") macosArm64("native") else macosX64("native")
        hostOs == "Linux" && hostArch != "aarch64" -> linuxX64("native")
        hostOs == "Linux" && hostArch == "aarch64" -> {
            println("INFO: Skipping Kotlin/Native build for ARM Linux. See KT-36871: https://youtrack.jetbrains.com/issue/KT-36871")
            null
        }
        isMingwX64 -> mingwX64("native")
        else -> throw GradleException("Host OS is not supported in Kotlin/Native.")
    }

    nativeTarget?.binaries {
        executable {
            entryPoint = "main"
        }
    }

    sourceSets["commonMain"].dependencies {
        implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.6.4")
    }

    sourceSets["jsMain"].dependencies {
        implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core-js:1.6.4")
        implementation("org.jetbrains.kotlin-wrappers:kotlin-node:18.14.0-pre.504")
    }

    tasks.withType<ShadowJar> {
        val main = jvmTarget.compilations.getByName("main")
        from(main.output)
        configurations = mutableListOf(
            main.compileDependencyFiles,
            main.runtimeDependencyFiles
        )
    }
}
