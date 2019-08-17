package qwdepystock.pystock;

import java.io.IOException;
import java.sql.SQLException;
import java.util.concurrent.Callable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import picocli.CommandLine;
import qwdepystock.db.DatabaseManager;
import qwdepystock.util.FileUtil;

@Command(name = "qwde pystockdata stuff", mixinStandardHelpOptions = true, version = "0.1")
public class App implements Callable<Integer> {
  private static Logger logger = LoggerFactory.getLogger(App.class);

  @Override
  public Integer call() {
    try {
      DatabaseManager.initialize();
    } catch (ClassNotFoundException | IOException | SQLException   exception) {
      logger.error("", exception);
      return 1;
    }

    return 0;
  }

  public static void main(String[] args) throws IOException {
    logger.info("Starting");
    System.exit(CommandLine.call(new App(), args));
  }
}
