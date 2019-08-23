plugins {
  `java-library`
}

version = "0.0.1-SNAPSHOT"

repositories {
  mavenLocal()
    mavenCentral()
}

dependencies {
  implementation("commons-io:commons-io:2.6")
  implementation("nz.ac.waikato.cms.weka:weka-dev:3.9.2")
  compile(project(":qwde.pystock"))
}

sourceSets {
  main {
    java.srcDir("src/main/java")
  }
  test {
    java.srcDir("src/test/java")
  }
}

java {
  sourceCompatibility = JavaVersion.VERSION_11
    targetCompatibility = JavaVersion.VERSION_11
}

tasks {
  test {
    testLogging.showExceptions = true
  }
}

tasks.named<Test>("test") {
    useJUnitPlatform()
}