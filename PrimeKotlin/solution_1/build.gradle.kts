plugins {
    kotlin("multiplatform") version "1.5.21"
    java
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

        target.apply {
            binaries {
                executable {
                    entryPoint = "prime_kotlin"
                }

            }
            compilations["main"].kotlinOptions.freeCompilerArgs += "-opt"
        }


        jvm {
            kotlin


            val jvmMain by getting {
                this.kotlin.forEach { l ->
                    println("Source: $l")
                }
                dependsOn(commonMain)
            }

            compilations {
                val mainJ = getByName("main")
                mainJ.source(jvmMain)
                println("FAT JAR")
                tasks {
                    register<Jar>("fatJar") {
                        group = "application"
                        manifest {
                            attributes["Implementation-Title"] = "Gradle Jar File for Primes"
                            attributes["Implementation-Version"] = archiveVersion
                            attributes["Main-Class"] = "PrimeKotlinKt"
                        }
                        archiveBaseName.set("${project.name}-fat")
                        from(configurations.runtimeClasspath.get().map {
                            if (it.isDirectory)
                                it
                            else
                                zipTree(it) }
                        )
                        from(mainJ.output.classesDirs, mainJ.compileDependencyFiles.map {
                            if (it.isDirectory)
                                it
                            else
                                zipTree(it)
                        })
                        with(getByName("jar") as CopySpec)
                    }
                }
            }
        }

    }
}

