package qwde;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.InetAddress;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import io.prometheus.client.exporter.HTTPServer;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;

import qwde.web.HttpServer;

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
    HTTPServer prometheusSever = null;
    try {
      prometheusSever = new HTTPServer(Integer.valueOf(this.port));
    } catch (IOException exception) {
      logger.error("Could not start prometheus server at 9456", exception);
      System.exit(1);
    }

    ServerSocket server = null;
    try {
      server = new ServerSocket(Integer.valueOf(serverPort), 10, InetAddress.getByName("127.0.0.1"));
    } catch (IOException exception) {
      logger.error("", exception);
      System.exit(1);
    } 

    logger.info("Started server {}", server);

    while (true) {
      try {
        Thread t = new Thread(new HttpServer(server.accept()));
        t.start();
      } catch (Exception exception) {
        logger.error("{}", exception);
        interruptableSleep(1000);
      }
    }
  }

  private static void interruptableSleep(int ms) {
    try {
      Thread.sleep(100);
    } catch (InterruptedException interruptedException) {
    }
  }

  public static void main(String[] args) {
    System.out.println("Hello, stockerface!");
    logger.info("Starting");
    CommandLine.run(new App(), args);
  }
}
