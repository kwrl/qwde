package qwde.analytics.db;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.dataprovider.db.DatabaseManager;
import qwde.dataprovider.models.CompanyStockData;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public final class StockDB {
    private static final Logger LOG = LoggerFactory.getLogger(StockDB.class);

    private StockDB() {
    }

    public static CompanyStockData getCompanyData(Collection<String> stockTicker, LocalDate fromDate, LocalDate toDate) throws SQLException {
        final String query = "SELECT close_price, low_price, high_price, volume, timestamp FROM StockTicker WHERE symbol IN ? AND timestamp >= ? AND timestamp <= ?";
        try (Connection connection = DatabaseManager.getConnection(); PreparedStatement statement = connection.prepareStatement(query)) {
            statement.setArray(1, connection.createArrayOf("varchar", stockTicker.toArray()));
            statement.setDate(2, java.sql.Date.valueOf(fromDate));
            statement.setDate(3, java.sql.Date.valueOf(toDate));
            try (ResultSet rs = statement.executeQuery()) {
                List<Double> stockPrices = new ArrayList<>();
                List<Double> lowPrices = new ArrayList<>();
                List<Double> highPrices = new ArrayList<>();
                List<Long> volume = new ArrayList<>();
                List<LocalDateTime> priceTimeStamps = new ArrayList<>();

                while (rs.next()) {
                    stockPrices.add(rs.getDouble("close_price"));
                    lowPrices.add(rs.getDouble("low_price"));
                    highPrices.add(rs.getDouble("high_price"));
                    volume.add(rs.getLong("volume"));
                    priceTimeStamps.add(rs.getTimestamp("timestamp").toLocalDateTime());
                }

                if (stockPrices.isEmpty()) {
                    LOG.warn("Request data {}, from {} to {} - empty results", stockTicker, fromDate, toDate);
                }
                return new CompanyStockData(stockTicker, stockPrices, highPrices, lowPrices, volume, priceTimeStamps);
            }
        }
    }
}
