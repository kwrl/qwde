import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

plugins {
    `java-library`
    application
    id("com.github.johnrengelman.shadow") version "5.2.0"
}

application {
    mainClassName = "qwde.web.App"
}

version = "0.0.1"
val micronautVersion by extra("1.3.3")
val jacksonVersion by extra("2.10.3")

// Tablesaw and micronaut competes for a jackson version, apparently
configurations.all {
    resolutionStrategy {
        force("com.fasterxml.jackson.core:jackson-annotations:$jacksonVersion")
        force("com.fasterxml.jackson.core:jackson-databind:$jacksonVersion")
        force("com.fasterxml.jackson.dataformat:jackson-dataformat-yaml:$jacksonVersion")
        force("com.fasterxml.jackson.datatype:jackson-datatype-jdk8:$jacksonVersion")
        force("com.fasterxml.jackson.datatype:jackson-datatype-jsr310:$jacksonVersion")
    }
}

dependencies {
    implementation("commons-io:commons-io:2.6")
    implementation("info.picocli:picocli:3.9.5")
    implementation("io.prometheus:simpleclient_httpserver:0.6.0")
    implementation("io.prometheus:simpleclient_servlet:0.0.11")
    implementation("org.freemarker:freemarker:2.3.29")
    implementation("org.apache.commons:commons-lang3:3.9")
    implementation("tech.tablesaw:tablesaw-core:${project.rootProject.ext["tablesawVersion"]}")
    implementation("tech.tablesaw:tablesaw-html:${project.rootProject.ext["tablesawVersion"]}")
    implementation("tech.tablesaw:tablesaw-jsplot:${project.rootProject.ext["tablesawVersion"]}")
    api(project(":dataprovider"))
    api(project(":analytics"))
    api(project(":trading"))

    annotationProcessor(platform("io.micronaut:micronaut-bom:$micronautVersion"))
    annotationProcessor("io.micronaut:micronaut-inject-java")
    annotationProcessor("io.micronaut:micronaut-validation")
    annotationProcessor("io.micronaut.configuration:micronaut-openapi")
    implementation(platform("io.micronaut:micronaut-bom:$micronautVersion"))
    implementation("io.micronaut:micronaut-inject")
    implementation("io.swagger.core.v3:swagger-annotations")
    implementation("io.micronaut:micronaut-validation")
    implementation("io.micronaut:micronaut-runtime")
    implementation("io.micronaut:micronaut-http-server-netty")
    implementation("io.micronaut:micronaut-http-client")
    implementation("io.micronaut.configuration:micronaut-picocli:1.2.1")

    testAnnotationProcessor(platform("io.micronaut:micronaut-bom:$micronautVersion"))
    testAnnotationProcessor("io.micronaut:micronaut-inject-java")
    testImplementation("io.micronaut.test:micronaut-test-junit5")
}

sourceSets {
    main {
        java.srcDir("src/main/java")
    }
    test {
        java.srcDir("src/test/java")
    }
}

tasks {
  named<ShadowJar>("shadowJar") {
    archiveBaseName.set("shadow")
      mergeServiceFiles()
      manifest {
        attributes(mapOf("Main-Class" to "qwde.web.App"))
      }
  }
}

tasks {
  build {
    dependsOn(shadowJar)
  }
}

java {
    sourceCompatibility = JavaVersion.VERSION_11
    targetCompatibility = JavaVersion.VERSION_11
}

