package qwde.web;

import java.util.HashMap;
import javax.inject.Singleton;

import io.micronaut.configuration.picocli.PicocliRunner;
import io.micronaut.context.annotation.Property;
import io.micronaut.runtime.Micronaut;
import io.prometheus.client.exporter.HTTPServer;
import io.swagger.v3.oas.annotations.ExternalDocumentation;
import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.info.Contact;
import io.swagger.v3.oas.annotations.info.Info;
import io.swagger.v3.oas.annotations.info.License;
import io.swagger.v3.oas.annotations.servers.Server;
import io.swagger.v3.oas.annotations.servers.ServerVariable;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import qwde.dataprovider.db.DatabaseManager;

import java.io.IOException;
import java.sql.SQLException;
import java.util.concurrent.Callable;

@OpenAPIDefinition(
        info = @Info(
                title = "the title",
                version = "0.0",
                description = "My API",
                license = @License(name = "GPL v3", url = "http://qwde.info"),
                contact = @Contact(url = "Apache 2.0", name = "Anders", email = "dontwantto@sharemy.email")
        ),
        tags = {
                @Tag(name = "Tag 1", description = "desc 1", externalDocs = @ExternalDocumentation(description = "docs desc"))
        },
        externalDocs = @ExternalDocumentation(description = "definition docs desc"),
        servers = {
                @Server(
                        description = "server 1",
                        url = "http://95.216.220.118:8080/",
                        variables = {
                                @ServerVariable(name = "hetzner", description = "8GB 2vCPU", defaultValue = "1", allowableValues = {"1", "2"}),
                        })
        }
)
@Command(name = "qwde stuff", mixinStandardHelpOptions = true, version = "0.1",
// Looks better in terminal. Backslashes has to be double, and dollar signs, too.
    header = {
      "@|green                               $$$$\\             |@",
      "@|green                               $$$$ |            |@",
      "@|green  $$$$$$$$$$$$\\  $$$$\\  $$$$\\  $$$$\\  $$$$$$$$$$$$$$ | $$$$$$$$$$$$\\    |@",
      "@|green  $$$$  __$$$$\\ $$$$ | $$$$ | $$$$ |$$$$  __$$$$ |$$$$  __$$$$\\  |@",
      "@|green  $$$$ /  $$$$ |$$$$ | $$$$ | $$$$ |$$$$ /  $$$$ |$$$$$$$$$$$$$$$$ | |@",
      "@|green  $$$$ |  $$$$ |$$$$ | $$$$ | $$$$ |$$$$ |  $$$$ |$$$$   ____| |@",
      "@|green  \\$$$$$$$$$$$$$$ |\\$$$$$$$$$$\\$$$$$$$$  |\\$$$$$$$$$$$$$$ |\\$$$$$$$$$$$$$$\\  |@",
      "@|green   \\____$$$$ | \\_____\\____/  \\_______| \\_______| |@",
      "@|green        $$$$ |                                   |@",
      "@|green        $$$$ |                                   |@",
      "@|green        \\__                                    |@"
})
@Singleton
public class App implements Callable<Integer> {
  private static final Logger LOG = LoggerFactory.getLogger(App.class);

  @Option(names = { "-p" },
          description = "Port to run prometheus on",
          defaultValue = "8012"
  )
  private String prometheusPort;

  @Option(names = { "-w" },
          description = "Port to run micronaut on"
  )
  private String micronautPort;

  @Property(name = "micronaut.server.port") Integer port;
  @Property(name = "micronaut.application.name") String name;

  @Override
  public Integer call() {
    try {
      DatabaseManager.initialize();
    } catch (ClassNotFoundException | IOException | SQLException exception) {
      LOG.error("", exception);
      return 1;
    }

    try {
      new HTTPServer(Integer.parseInt(this.prometheusPort));
    } catch (IOException exception) {
      LOG.error("Could not start prometheus server at {}", this.prometheusPort, exception);
      return 1;
    }

    if (micronautPort != null && !micronautPort.isEmpty()) {
      port = Integer.valueOf(micronautPort);
      LOG.info("Overriding default application.yml port with input {}", micronautPort);
    }

    Micronaut.build((String) null)
          .mainClass(App.class)
          .properties(new HashMap<String, Object>() {{ put("micronaut.server.port", port); }})
          .start();

    while (Thread.currentThread().isAlive()) {
      try {
        Thread.sleep(100_000);
      } catch (InterruptedException exception) {
        LOG.error("", exception);
        return 1;
      }
    }

    return 0;
  }

  public static void main(String[] args) throws Exception {
    LOG.info("Started.");
    System.exit(PicocliRunner.call(App.class, args));
  }
}
