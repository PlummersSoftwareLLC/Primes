plugins {
    kotlin("multiplatform") version "1.5.21"
    id("com.github.johnrengelman.shadow") version "7.0.0"
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
            kotlinOptions.jvmTarget = "16"
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

    sourceSets["commonMain"].dependencies {
        implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.5.1-native-mt")
    }

    tasks.withType<com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar> {
        from(jvmTarget.compilations.getByName("main").output)
        configurations = mutableListOf(
            jvmTarget.compilations.getByName("main").compileDependencyFiles as Configuration,
            jvmTarget.compilations.getByName("main").runtimeDependencyFiles as Configuration
        )
    }
}
