package qwde;

import java.io.IOException;

import org.eclipse.jetty.annotations.AnnotationConfiguration;
import org.eclipse.jetty.plus.webapp.EnvConfiguration;
import org.eclipse.jetty.plus.webapp.PlusConfiguration;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.webapp.Configuration;
import org.eclipse.jetty.webapp.FragmentConfiguration;
import org.eclipse.jetty.webapp.MetaInfConfiguration;
import org.eclipse.jetty.webapp.WebAppContext;
import org.eclipse.jetty.webapp.WebInfConfiguration;
import org.eclipse.jetty.webapp.WebXmlConfiguration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import io.prometheus.client.exporter.HTTPServer;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;

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
    logger.trace("Got argument \"{}\"", this.port);

    @SuppressWarnings("unused")
    HTTPServer server = null;
    try {
      server = new HTTPServer(Integer.valueOf(this.port));
      ServletContextHandler context = new ServletContextHandler();
      context.setContextPath("/");
    } catch (IOException exception) {
      logger.error("Could not start prometheus server at 9456", exception);
      System.exit(1);
    }

    Server jettyServer = new Server(Integer.valueOf(this.serverPort));

    //Create a WebApp
    WebAppContext context = new WebAppContext();
    context.setDescriptor("src/main/webapp/WEB-INF/web.xml");
    context.setContextPath("/");
    context.setResourceBase("src/main/webapp");
    context.setConfigurations(new Configuration[] {
            new AnnotationConfiguration(), new WebXmlConfiguration(),
            new WebInfConfiguration(),
            new PlusConfiguration(), new MetaInfConfiguration(),
            new FragmentConfiguration(), new EnvConfiguration() });

    context.setAttribute("org.eclipse.jetty.server.webapp.ContainerIncludeJarPattern",".*/classes/.*");
    context.setParentLoaderPriority(true);
    jettyServer.setHandler(context);

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
