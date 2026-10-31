import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar
import org.jetbrains.kotlin.gradle.dsl.JvmTarget

plugins {
    kotlin("multiplatform") version "2.4.21"
    id("org.jetbrains.kotlin.plugin.atomicfu") version "2.4.21"
    id("com.gradleup.shadow") version "9.6.1"
}

group = "primes"
version = "1.0"

repositories {
    mavenCentral()
}

kotlin {
    val jvmTarget = jvm {
        compilerOptions {
            jvmTarget.set(JvmTarget.JVM_25)
        }
    }

    js {
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
        implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.11.0")
    }

    sourceSets["jsMain"].dependencies {
        implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core-js:1.11.0")
        implementation("org.jetbrains.kotlin-wrappers:kotlin-node:2026.10.6-24.12.4")
    }

    tasks.withType<ShadowJar> {
        val main = jvmTarget.compilations.getByName("main")
        from(main.output)
        configurations.set(listOf(
            project.configurations.getByName(main.compileDependencyConfigurationName),
            project.configurations.getByName(main.runtimeDependencyConfigurationName!!)
        ))
        manifest {
            attributes["Main-Class"] = "MainKt"
        }
    }
}
