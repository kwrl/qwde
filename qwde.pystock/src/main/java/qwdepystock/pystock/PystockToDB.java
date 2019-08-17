package qwdepystock.pystock;

import java.util.Map.Entry;
import java.sql.PreparedStatement;
import java.sql.Statement;
import org.slf4j.Logger;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import java.sql.SQLException;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.slf4j.LoggerFactory;
import java.sql.Connection;
import java.time.LocalDate;
import java.io.IOException;
import java.sql.Timestamp;
import qwdepystock.models.StockTicker;
import qwdepystock.db.DatabaseManager;
import qwdepystock.models.StockPrice;

public class PystockToDB {
  private static Logger logger = LoggerFactory.getLogger(PystockToDB.class);

  public static boolean databaseHasData() throws SQLException {
    try (Connection connection = DatabaseManager.getConnection(); Statement statement = connection.createStatement()) {
      return statement.executeQuery("SELECT symbol, price, timestamp FROM StockTicker LIMIT 1").next();
    } 
  }

  public static void createInitialDB() {
    try (Connection connection = DatabaseManager.getConnection(); PreparedStatement ps = connection.prepareStatement("INSERT INTO StockTicker (symbol, price, timestamp) VALUES(?, ?, ?)")) {
      for (StockTicker ticker : getStockTickers()) {
        ps.setString(1, ticker.symbol);
        ps.setBigDecimal(2, ticker.price);
        ps.setTimestamp(3, Timestamp.valueOf(ticker.timestamp));
        ps.addBatch();
      }
      int[] results = ps.executeBatch();
      connection.commit();
      logger.info("Stored {} entries to DB", IntStream.of(results).sum());
    }  catch (Exception exception) {
      logger.error("", exception);
    }
  }

  private static List<StockTicker> getStockTickers() throws IOException {
    PystockStockPriceReader pyReader = new PystockStockPriceReader();

    Map<String, List<StockPrice>> pricesMappedByCompany = pyReader.read().stream().collect(
        Collectors.groupingBy(p -> p.getCompany(), Collectors.toList())
        );
    List<StockTicker> ret = new ArrayList<>();
    for (Entry<String, List<StockPrice>> entry : pricesMappedByCompany.entrySet()) {
      for(StockPrice stockPrice : entry.getValue()) {
        ret.add(new StockTicker(entry.getKey(), stockPrice.getPrice(), stockPrice.getTimestamp()));
      }
    }
    return ret;
  }
}
