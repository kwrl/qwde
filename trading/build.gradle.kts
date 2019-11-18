plugins {
    `application`
}

application {
    mainClassName = "qwde.dataprovider.pystock.App"
}

version = "0.0.1-SNAPSHOT"

dependencies {

    implementation("commons-io:commons-io:2.6")
    implementation("info.picocli:picocli:3.9.5")
    implementation("org.apache.kafka:kafka-clients:${project.rootProject.ext["kafkaVersion"]}")
    implementation("com.salesforce.kafka.test:kafka-junit5:3.1.1")
    implementation("org.apache.commons:commons-lang3:3.9")

    compile(project(":analytics"))
    compile(project(":dataprovider"))
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
