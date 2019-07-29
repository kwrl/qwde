package qwde;

import java.io.IOException;

import org.eclipse.jetty.servlet.ServletHandler;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.server.Server;
import io.prometheus.client.exporter.HTTPServer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import qwde.servlets.IndexServlet;
import qwde.servlets.SharkToothServlet;

@Command(name = "qwde stuff", mixinStandardHelpOptions = true, version = "0.1")
public class App implements Runnable {
  private static Logger logger = LoggerFactory.getLogger(App.class);

  @Option(names = { "-p" },
  description = "Port to run prometheus on",
  defaultValue = "9458"
  )
    private String port;

  @Option(names = { "-s" },
  description = "Port to run HTTP server on",
  defaultValue = "8080"
  )
  private String serverPort;

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
      logger.error("Could not start prometheus server at 9456", exception);
      System.exit(1);
    }

    Server jettyServer = new Server(Integer.valueOf(serverPort));

    ServletHandler servletHandler = new ServletHandler();
    jettyServer.setHandler(servletHandler);

    servletHandler.addServletWithMapping(IndexServlet.class, "/");
    servletHandler.addServletWithMapping(SharkToothServlet.class, "/sharktooth");

    try {
      jettyServer.start();
      jettyServer.join();
    } catch (Exception exception) {
      logger.error("{}", exception);
    }
  }

  public static void main(String[] args) {
    System.out.println("Hello, stockerface!");
    logger.info("Starting");
    CommandLine.run(new App(), args);
  }
}
