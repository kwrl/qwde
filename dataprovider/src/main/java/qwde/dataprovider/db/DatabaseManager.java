package qwde.dataprovider.db;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import org.flywaydb.core.Flyway;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import qwde.dataprovider.pystock.PystockToDB;
import qwde.dataprovider.util.FileUtil;


public class DatabaseManager {
  private static Logger logger = LoggerFactory.getLogger(DatabaseManager.class);
  private final HikariConfig config = new HikariConfig();
  private final HikariDataSource ds;


  private static DatabaseManager databaseManager;

  private DatabaseManager(String jdbcUrl) {
    this.config.setJdbcUrl(jdbcUrl);
    this.config.addDataSourceProperty("cachePrepStmts", "true");
    this.config.addDataSourceProperty("prepStmtCacheSize", "250");
    this.config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048");
    this.config.setAutoCommit(false);
    this.config.setMaximumPoolSize(2);
    this.ds = new HikariDataSource(config);
  }

  public static Connection getConnection() throws SQLException {
    return databaseManager.ds.getConnection();
  }

  public static void initialize() throws ClassNotFoundException, SQLException, IOException {
    if (databaseManager != null) {
      throw new IllegalStateException("Initialize already called");
    }

    try (InputStream inputstream = DatabaseManager.class.getClassLoader().getResourceAsStream("database.properties")) {
      Properties properties = new Properties();
      properties.load(inputstream);
      Class.forName(properties.getProperty("development.disk.driver"));

      String jdbcUrl = properties.getProperty("development.disk.url");
      String stringRegex = "\\|XDGCACHEDIR\\|([a-zA-Z]+\\.db)";
      Pattern regex = Pattern.compile(stringRegex);
      Matcher m = regex.matcher(jdbcUrl);
      if (m.find()) {
        jdbcUrl = jdbcUrl.replaceFirst(stringRegex, Path.of(FileUtil.createIfNotExists(FileUtil.getCacheDirectory()), m.group(1)).toAbsolutePath().toString().replace("\\", "/"));
      }
      logger.info("Connecting to db {}", jdbcUrl);
      databaseManager = new DatabaseManager(jdbcUrl);

      Flyway flyway = Flyway.configure().dataSource(jdbcUrl, null, null).load();
      flyway.migrate();

      if (PystockToDB.databaseHasData()) {
        logger.info("Found data in DB ({}) : database ready.", jdbcUrl);
      } else {
        logger.info("No data in DB ({}): populating", jdbcUrl);
        PystockToDB.createInitialDB();
      }
    }
  }
}
