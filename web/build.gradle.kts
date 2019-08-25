import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

plugins {
  `application`
    id("com.github.johnrengelman.shadow") version "5.1.0"
}

application {
  mainClassName = "qwde.web.App"
}

version = "0.0.1"

dependencies {
  implementation("commons-io:commons-io:2.6")
    implementation("info.picocli:picocli:3.9.5")
    implementation("io.prometheus:simpleclient_httpserver:0.6.0")
    implementation("io.prometheus:simpleclient_servlet:0.0.11")
    implementation("nz.ac.waikato.cms.weka:weka-dev:3.9.2")
    implementation("org.apache.httpcomponents:httpclient:4.5.9")
	implementation("org.freemarker:freemarker:2.3.29")
    implementation("org.apache.commons:commons-lang3:3.9")
    implementation("tech.tablesaw:tablesaw-core:0.34.1")
    implementation("tech.tablesaw:tablesaw-html:0.34.1")
    implementation("tech.tablesaw:tablesaw-jsplot:0.34.1")
    implementation("tech.tablesaw:tablesaw-plot:0.24.4")
    compile(project(":dataprovider"))
    compile(project(":analytics"))
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

tasks {
  test {
    testLogging.showExceptions = true
  }
}
