package qwdepystock.db;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import java.sql.Connection;
import java.sql.SQLException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import qwdepystock.pystock.PystockToDB;

import org.flywaydb.core.Flyway;

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
      logger.info("Connecting to db {}", jdbcUrl);
      databaseManager = new DatabaseManager(jdbcUrl);

      Flyway flyway = Flyway.configure().dataSource(jdbcUrl, null, null).load();
      flyway.migrate();

      if (PystockToDB.databaseHasData()) {
        logger.info("Found data in DB : database ready.");
      } else {
        logger.info("No data in DB : populating");
        PystockToDB.createInitialDB();
      }
    }
  }
}
