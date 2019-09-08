package qwde.dataprovider.db;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.flywaydb.core.Flyway;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import qwde.dataprovider.pystock.PystockToDB;
import qwde.dataprovider.util.FileUtil;

public final class DatabaseManager {
  private static final Logger LOG = LoggerFactory.getLogger(DatabaseManager.class);
  private final HikariDataSource ds;
  private static DatabaseManager manager;

  private DatabaseManager(String jdbcUrl) {
    HikariConfig config = new HikariConfig();
    config.setJdbcUrl(jdbcUrl);
    config.addDataSourceProperty("cachePrepStmts", "true");
    config.addDataSourceProperty("prepStmtCacheSize", "250");
    config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048");
    config.setAutoCommit(false);
    config.setMaximumPoolSize(2);
    this.ds = new HikariDataSource(config);
  }

  public static Connection getConnection() throws SQLException {
    if (manager == null) {
      try {
        initialize();
      } catch (ClassNotFoundException | IOException exception) {
        // lazy...
        throw new SQLException(exception);
      }
    }

    return manager.ds.getConnection();
  }

  public static void initialize() throws ClassNotFoundException, SQLException, IOException {
    if (manager != null) {
      throw new IllegalStateException("Initialize already called");
    }

    try (InputStream inputstream = DatabaseManager.class.getClassLoader().getResourceAsStream("database.properties")) {
      if (inputstream == null) {
        throw new FileNotFoundException("database.properties");
      }
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
      LOG.info("Connecting to db {}", jdbcUrl);
      manager = new DatabaseManager(jdbcUrl);

      Flyway flyway = Flyway.configure().dataSource(jdbcUrl, null, null).load();
      flyway.migrate();

      if (PystockToDB.databaseHasData()) {
        LOG.info("Found data in DB ({}) : database ready.", jdbcUrl);
      } else {
        LOG.info("No data in DB ({}): populating", jdbcUrl);
        PystockToDB.createInitialDB();
      }
    }
  }
}
