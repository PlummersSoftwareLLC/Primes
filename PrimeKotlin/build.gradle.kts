plugins {
    kotlin("multiplatform") version "1.5.0-M2"
}

group = "org.xvolks"
version = "1.0.0-SNAPSHOT"

repositories {
    mavenCentral()
}

kotlin {

    val hostOs = System.getProperty("os.name")
    val arch = System.getProperty("os.arch")
    logger.warn("OS: $hostOs/$arch")
    logger.warn("Directory : ${project.projectDir}")
    val isMingwX64 = hostOs.startsWith("Windows")
    val target = when {
        hostOs == "Mac OS X" -> macosX64("native")
        hostOs == "Linux" -> if (arch.contains("arm")) linuxArm32Hfp("native") else linuxX64("native")
        isMingwX64 -> mingwX64("native")
        else -> throw GradleException("Host OS is not supported in Kotlin/Native.")
    }

    sourceSets {
        val commonMain by getting {
            dependencies {
                implementation(kotlin("stdlib-common"))
            }
        }

        val main = when {
            hostOs == "Mac OS X" -> {
                val nativeMain by getting {
                    dependsOn(commonMain)
                }
                nativeMain
            }
            hostOs == "Linux" -> {
                val nativeMain by getting {
                    dependsOn(commonMain)
                }
                nativeMain
            }
            isMingwX64 -> {
                val nativeMain by getting {
                    dependsOn(commonMain)
                }
                nativeMain
            }
            else -> null
        }

        jvm {
            kotlin
        }

        target.apply {
            binaries {
                executable {
                    entryPoint = "prime_kotlin"
                }
            }
            compilations["main"].kotlinOptions.freeCompilerArgs += "-opt"
        }

        val jvmMain by getting {
            this.kotlin.forEach { l ->
                println("Source: $l")
            }
            dependsOn(commonMain)
        }

    }
}