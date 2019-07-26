package qwde;

import java.io.IOException;

import org.eclipse.jetty.servlet.ServletContextHandler;
import io.prometheus.client.exporter.HTTPServer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;

@Command(name = "GRSS-NIBOR exporter", mixinStandardHelpOptions = true, version = "1.0")
public class App implements Runnable {
  private static Logger logger = LoggerFactory.getLogger(App.class);

  @Option(names = { "-p" },
    description = "Port to run prometheus on",
    defaultValue = "9458"
  )
  private String port;

  @Override
  public void run() {
    logger.trace("Got argument \"{}\"", port);

    @SuppressWarnings("unused")
    HTTPServer server = null;
    try {
      server = new HTTPServer(Integer.valueOf(port));
      ServletContextHandler context = new ServletContextHandler();
      context.setContextPath("/");
    } catch (IOException exception) {
      logger.error("Could not start prometheus server at 9456");
      System.exit(1);
    }
  }

  public static void main(String[] args) {
    System.out.println("Hello, stockerface!");
    logger.info("Starting");
    CommandLine.run(new App(), args);
  }
}
