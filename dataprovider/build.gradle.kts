plugins {
    `application`
}

application {
    mainClassName = "qwde.dataprovider.pystock.App"
}

version = "0.0.1-SNAPSHOT"

dependencies {
    implementation("com.zaxxer:HikariCP:2.7.8")
    implementation("commons-io:commons-io:2.6")
    implementation("info.picocli:picocli:3.9.5")
    implementation("org.apache.commons:commons-compress:1.15")
    implementation("org.apache.commons:commons-lang3:3.9")
    implementation("org.flywaydb:flyway-core:5.2.4")
    implementation("org.xerial:sqlite-jdbc:3.28.0")

    implementation("org.apache.kafka:kafka-clients:${project.rootProject.ext["kafkaVersion"]}")
    implementation("org.apache.kafka:kafka_2.12:${project.rootProject.ext["kafkaVersion"]}")
    implementation("com.salesforce.kafka.test:kafka-junit5:3.1.1")
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
