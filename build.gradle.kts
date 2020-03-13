group = "qwde"
version = "0.0.1"

val tablesawVersion by extra("0.34.2")
val kafkaVersion by extra("2.3.0")

buildscript {
    repositories {
        maven {
            url = uri("https://plugins.gradle.org/m2/")
        }
        jcenter()
        gradlePluginPortal()
    }
    dependencies {
        classpath("ru.vyarus:gradle-quality-plugin:4.1.0")
    }
}

allprojects {
    group = "qwde"

    repositories {
        mavenLocal()
        mavenCentral()
    }
}

tasks.withType<JavaCompile>().configureEach {
    options.compilerArgs = listOf("-Xmx4g")
}

configure(subprojects.filter { it.name == "analytics" || it.name == "dataprovider" || it.name == "web" || it.name == "trading" }) {
    apply(plugin = "java")
    apply(plugin = "ru.vyarus.quality")

    dependencies {
        "implementation"("com.google.guava:guava:23.0")
        "implementation"("ch.qos.logback:logback-classic:1.3.0-alpha4")
        "implementation"("one.util:streamex:0.7.0")
        "testImplementation"("com.flextrade.jfixture:jfixture:2.7.2")
        "testImplementation"("org.junit.jupiter:junit-jupiter-api:5.4.1")
        "testRuntimeOnly"("org.junit.jupiter:junit-jupiter-engine:5.4.1")
        "testImplementation"("com.google.truth:truth:0.39")
    }

    tasks.named<Test>("test") {
        useJUnitPlatform()

        maxHeapSize = "4G"

        testLogging.showExceptions = true
    }
}

//tasks.whenTaskAdded { task ->
  //if (task.name.contains("spotbugsMain"))
    //task.enabled = false
//}

subprojects {
    version = "1.0"
}
