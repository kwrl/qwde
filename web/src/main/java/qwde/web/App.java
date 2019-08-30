package qwde.web;

import java.io.IOException;
import java.net.ServerSocket;
import java.sql.SQLException;
import java.util.concurrent.Callable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.prometheus.client.exporter.HTTPServer;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import qwde.dataprovider.db.DatabaseManager;
import qwde.web.http.HttpServer;

@Command(name = "qwde stuff", mixinStandardHelpOptions = true, version = "0.1")
class App implements Callable<Integer> {
  private static final Logger LOG = LoggerFactory.getLogger(App.class);

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
  public Integer call() {
    LOG.trace("Got argument \"{}\"", this.port);

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

    ServerSocket server;
    try {
      server = new ServerSocket(Integer.parseInt(serverPort), 10);
    } catch (IOException exception) {
      LOG.error("", exception);
      return 1;
    }

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

    return 0;
  }

  public static void main(String[] args) {
    LOG.info("Starting");

    try {
      System.exit(CommandLine.call(new App(), args));
    } catch (Exception exception) {
      LOG.error("Unexpected error happened", exception);
      System.exit(1);
    }
  }
}
