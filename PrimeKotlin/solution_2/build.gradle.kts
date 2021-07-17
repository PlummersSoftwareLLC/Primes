import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm") version "1.5.20"
    id("com.github.johnrengelman.shadow") version "5.2.0"
    application
}

application {
    mainClassName = "PrimeSieveKt"
}

group = "me.wulkanat"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.5.0")
}

tasks.withType<KotlinCompile>() {
    kotlinOptions.jvmTarget = "16"
}

tasks.withType<Jar>() {
    archiveBaseName.set("prime-sieve")

    manifest {
        attributes["Main-Class"] = "PrimeSieveKt"
    }
}
