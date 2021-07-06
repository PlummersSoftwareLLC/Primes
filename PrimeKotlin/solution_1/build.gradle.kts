plugins {
    kotlin("jvm") version "1.5.20"
    application
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.5.0")
}

application {
    mainClass.set("PrimeSieveKt")
}

tasks {
    compileJava {
        options.release.set(16)
    }
    compileKotlin {
        kotlinOptions.jvmTarget = "16"
    }
}
