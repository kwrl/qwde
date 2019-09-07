package qwde.web;

import io.micronaut.runtime.Micronaut;
import io.prometheus.client.exporter.HTTPServer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import qwde.dataprovider.db.DatabaseManager;

import java.io.IOException;
import java.sql.SQLException;
import java.util.concurrent.Callable;

/*
@OpenAPIDefinition(
        info = @Info(
                title = "the title",
                version = "0.0",
                description = "My API",
                license = @License(name = "Apache 2.0", url = "http://foo.bar"),
                contact = @Contact(url = "http://gigantic-server.com", name = "Fred", email = "Fred@gigagantic-server.com")
        ),
        tags = {
                @Tag(name = "Tag 1", description = "desc 1", externalDocs = @ExternalDocumentation(description = "docs desc")),
                @Tag(name = "Tag 2", description = "desc 2", externalDocs = @ExternalDocumentation(description = "docs desc 2")),
                @Tag(name = "Tag 3")
        },
        externalDocs = @ExternalDocumentation(description = "definition docs desc"),
        security = {
                @SecurityRequirement(name = "req 1", scopes = {"a", "b"}),
                @SecurityRequirement(name = "req 2", scopes = {"b", "c"})
        },
        servers = {
                @Server(
                        description = "server 1",
                        url = "http://foo",
                        variables = {
                                @ServerVariable(name = "var1", description = "var 1", defaultValue = "1", allowableValues = {"1", "2"}),
                                @ServerVariable(name = "var2", description = "var 2", defaultValue = "1", allowableValues = {"1", "2"})
                        })
        }
)
*/
@Command(name = "qwde stuff", mixinStandardHelpOptions = true, version = "0.1")
class App implements Callable<Integer> {
  private static final Logger LOG = LoggerFactory.getLogger(App.class);

  @Option(names = { "-p" },
        description = "Port to run prometheus on",
        defaultValue = "8012"
  )
  private String port;

  @Option(names = { "-s" },
        description = "Port to run HTTP server on",
        defaultValue = "8080"
  )
  private String serverPort;

  @Override
  public Integer call() {
/*
    try {
      DatabaseManager.initialize();
    } catch (ClassNotFoundException | IOException | SQLException exception) {
      LOG.error("", exception);
      return 1;
    }

    try {
      new HTTPServer(Integer.parseInt(this.port));
    } catch (IOException exception) {
      LOG.error("Could not start prometheus server at {}", this.port, exception);
      return 1;
    }
*/

    Micronaut.run(App.class);

/*
    try (ServerSocket server = new ServerSocket(Integer.parseInt(this.serverPort), 10)) {
      LOG.info("Started server {}", server);

      while (Thread.currentThread().isAlive()) {
        try {
          Thread t = new Thread(new HttpServer(server.accept()));
          t.start();
        } catch (Exception exception) {
          LOG.error("", exception);
          return 1;
        }
      }
    } catch (IOException exception) {
      LOG.error("Could not open server socket at {}", this.serverPort, exception);
      return 1;
    }
*/
    return 0;
  }

  public static void main(String[] args) {
    LOG.info("Started.");
    System.exit(CommandLine.call(new App(), args));
  }
}
