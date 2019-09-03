package qwde.dataprovider.pystock;

import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import qwde.dataprovider.db.DatabaseManager;
import qwde.dataprovider.models.StockPrice;
import qwde.dataprovider.models.StockTicker;

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
    try (Connection connection = DatabaseManager.getConnection(); PreparedStatement ps = connection.prepareStatement("INSERT INTO StockTicker (symbol, close_price, high_price, low_price, volume, timestamp) VALUES(?, ?, ?, ?, ?, ?)")) {
      for (StockTicker ticker : getStockTickers()) {
        ps.setString(1, ticker.symbol);
        ps.setBigDecimal(2, ticker.closePrice);
        ps.setBigDecimal(3, ticker.highPrice);
        ps.setBigDecimal(4, ticker.lowPrice);
        ps.setLong(5, ticker.volume);
        ps.setTimestamp(6, Timestamp.valueOf(ticker.timestamp));
        ps.addBatch();
      }
      int[] results = ps.executeBatch();
      connection.commit();
      LOG.info("Stored {} entries to DB", IntStream.of(results).sum());
    } catch (Exception exception) {
      LOG.error("", exception);
    }
  }

  private static List<StockTicker> getStockTickers() throws IOException {
    PystockStockPriceReader pyReader = new PystockStockPriceReader(x -> true);

    LOG.info("Sorting and organizing entries before inserting them to DB...");
    Map<String, List<StockPrice>> pricesMappedByCompany = pyReader.read().stream().collect(
        Collectors.groupingBy(StockPrice::getCompany, Collectors.toList())
        );
    List<StockTicker> ret = new ArrayList<>();
    for (Entry<String, List<StockPrice>> entry : pricesMappedByCompany.entrySet()) {
      for (StockPrice stockPrice : entry.getValue()) {
        ret.add(new StockTicker(entry.getKey(), stockPrice.getPrice(), stockPrice.getHigh(), stockPrice.getLow(), stockPrice.getVolume(), stockPrice.getTimestamp()));
      }
    }
    return ret;
  }
}
