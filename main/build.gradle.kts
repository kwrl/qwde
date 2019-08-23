plugins {
  `application`
}

application {
  mainClassName = "qwde.App"
}

version = "0.0.1-SNAPSHOT"

dependencies {
  implementation("commons-io:commons-io:2.6")
    implementation("info.picocli:picocli:3.9.5")
    implementation("io.prometheus:simpleclient_httpserver:0.6.0")
    implementation("io.prometheus:simpleclient_servlet:0.0.11")
    implementation("nz.ac.waikato.cms.weka:weka-dev:3.9.2")
    implementation("org.apache.httpcomponents:httpclient:4.5.9")
    implementation("tech.tablesaw:tablesaw-core:0.34.1")
    implementation("tech.tablesaw:tablesaw-html:0.34.1")
    implementation("tech.tablesaw:tablesaw-jsplot:0.34.1")
    implementation("tech.tablesaw:tablesaw-plot:0.24.4")
    compile(project(":qwde.pystock"))
    compile(project(":qwde"))
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
