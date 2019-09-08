package qwde.dataprovider.pystock;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.stream.IntStream;

import com.google.common.base.Predicates;
import one.util.streamex.StreamEx;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import qwde.dataprovider.db.DatabaseManager;
import qwde.dataprovider.models.IStockTicker;

public final class PystockToDB {
  private static final Logger LOG = LoggerFactory.getLogger(PystockToDB.class);

  private PystockToDB() {
  }

  public static boolean databaseHasData() throws SQLException {
    try (Connection connection = DatabaseManager.getConnection(); Statement statement = connection.createStatement(); ResultSet resultSet = statement.executeQuery("SELECT symbol, close_price, high_price, low_price, volume, timestamp FROM StockTicker LIMIT 1")) {
      return resultSet.next();
    }
  }

  public static void createInitialDB() {
    try (Connection connection = DatabaseManager.getConnection();
         PreparedStatement ps = connection.prepareStatement("INSERT INTO StockTicker (symbol, close_price, high_price, low_price, volume, timestamp) VALUES(?, ?, ?, ?, ?, ?)");
         Statement statement = connection.createStatement()) {
      Consumer<List<IStockTicker>> writeToDb = stockTickers -> {
        try {
          for (IStockTicker stockPrice : stockTickers) {
            ps.setString(1, stockPrice.getCompany());
            ps.setBigDecimal(2, stockPrice.getPrice());
            ps.setBigDecimal(3, stockPrice.getHigh());
            ps.setBigDecimal(4, stockPrice.getLow());
            ps.setLong(5, stockPrice.getVolume());
            ps.setTimestamp(6, Timestamp.valueOf(stockPrice.getTimestamp()));
            ps.addBatch();
          }

          int[] results = ps.executeBatch();
          connection.commit();
          LOG.debug("Stored {} entries to DB...", IntStream.of(results).sum());
        } catch (SQLException exception) {
          LOG.error("", exception);
        }
      };
      AtomicInteger counter = new AtomicInteger(0);
      int chunkSize = (int) 1e5;
      StreamEx.of(PystockDataReader.read(Predicates.alwaysTrue())).groupRuns((prev, next) -> counter.incrementAndGet() % chunkSize != 0)
              .forEach(writeToDb);

      int duplicates = statement.executeUpdate("DELETE FROM StockTicker WHERE rowId NOT IN (SELECT min(rowId) FROM StockTicker GROUP BY symbol, timestamp)");
      connection.commit();
      LOG.info("{} duplicates deleted", duplicates);
      LOG.info("All entries stored in DB!");
    } catch (Exception exception) {
      LOG.error("", exception);
    }
  }
}
