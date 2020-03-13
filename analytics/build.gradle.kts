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
    implementation("tech.tablesaw:tablesaw-core:${project.rootProject.ext["tablesawVersion"]}")
    implementation("tech.tablesaw:tablesaw-aggregate:${project.rootProject.ext["tablesawVersion"]}")

    api(project(":dataprovider"))
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
