package qwde.dataprovider.pystock;

import java.io.IOException;
import java.sql.SQLException;
import java.util.concurrent.Callable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import picocli.CommandLine;
import picocli.CommandLine.Command;
import qwde.dataprovider.db.DatabaseManager;

@Command(name = "qwde pystockdata stuff", mixinStandardHelpOptions = true, version = "0.1")
class App implements Callable<Integer> {
  private static final Logger LOG = LoggerFactory.getLogger(App.class);

  @Override
  public Integer call() {
    try {
      DatabaseManager.initialize();
    } catch (ClassNotFoundException | IOException | SQLException exception) {
      LOG.error("", exception);
      return 1;
    }

    return 0;
  }

  public static void main(String[] args) {
    LOG.info("Starting");
    System.exit(CommandLine.call(new App(), args));
  }
}
