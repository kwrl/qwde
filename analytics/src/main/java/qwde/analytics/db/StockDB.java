package qwde.analytics.db;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import qwde.dataprovider.db.DatabaseManager;
import qwde.dataprovider.models.CompanyStockData;

public final class StockDB {
  private static Logger logger = LoggerFactory.getLogger(StockDB.class);

  private StockDB() {
  }

  public static CompanyStockData getCompanyData(String stockTicker, LocalDate fromDate, LocalDate toDate) throws SQLException {
    try (Connection connection = DatabaseManager.getConnection(); PreparedStatement statement = connection.prepareStatement("SELECT price, timestamp FROM StockTicker WHERE symbol = ? AND timestamp >= ? AND timestamp <= ?")) {
      statement.setString(1, stockTicker);
      statement.setDate(2, java.sql.Date.valueOf(fromDate));
      statement.setDate(3, java.sql.Date.valueOf(toDate));
      try (ResultSet rs = statement.executeQuery()) {
        logger.trace("Executing query {}", String.format("SELECT price, timestamp FROM StockTicker WHERE symbol = %s AND timestamp BETWEEN %s AND %s", stockTicker, java.sql.Date.valueOf(fromDate), java.sql.Date.valueOf(toDate)));
        List<Double> stockPrices = new ArrayList<>();
        List<LocalDateTime> priceTimeStamps = new ArrayList<>();

        while (rs.next()) {
          stockPrices.add(rs.getDouble("price"));
          priceTimeStamps.add(rs.getTimestamp("timestamp").toLocalDateTime());
        }

        if (stockPrices.isEmpty()) {
          logger.warn("Request data {}, from {} to {} - empty results", stockTicker, fromDate, toDate);
        }
        return new CompanyStockData(stockTicker, stockPrices, priceTimeStamps);
      }
    }
  }
}
