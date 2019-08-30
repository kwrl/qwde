group = "qwde"
version = "0.0.1"

val tablesawVersion by extra("0.34.2")

buildscript {
  repositories {
    maven {
      url = uri("https://plugins.gradle.org/m2/")
    }
    jcenter()
    gradlePluginPortal()
  }
  dependencies {
    classpath("ru.vyarus:gradle-quality-plugin:3.4.0")
  }
}

allprojects {
  group = "qwde"

    repositories {
      mavenLocal()
        mavenCentral()
    }
}

configure(subprojects.filter { it.name == "analytics"  || it.name == "dataprovider" || it.name == "web" } ) {
  apply(plugin = "java")
    apply(plugin = "ru.vyarus.quality")

  dependencies {
    "implementation"("com.google.guava:guava:23.0")
      "implementation"("ch.qos.logback:logback-classic:1.3.0-alpha4")
      "testImplementation"("com.flextrade.jfixture:jfixture:2.7.2")
      "testImplementation"("org.junit.jupiter:junit-jupiter-api:5.4.1")
      "testRuntimeOnly"("org.junit.jupiter:junit-jupiter-engine:5.4.1")
      "testImplementation"("com.google.truth:truth:0.39")
  }
}

subprojects {
  version = "1.0"
}
