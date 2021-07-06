plugins {
    kotlin("jvm") version "1.5.20"
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.5.0")
}

tasks {
    compileJava {
        options.release.set(16)
    }
    compileKotlin {
        kotlinOptions.jvmTarget = "16"
    }
}
