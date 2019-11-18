package qwde.dataprovider.db;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.dataprovider.models.CompanyStockData;
import qwde.dataprovider.models.StockTicker;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public final class StockDB {
    private static final Logger LOG = LoggerFactory.getLogger(StockDB.class);

    private StockDB() {
    }

    public static CompanyStockData getCompanyData(String stockTicker, LocalDate fromDate, LocalDate toDate) throws SQLException {
        final String query = "SELECT close_price, low_price, high_price, volume, timestamp FROM StockTicker WHERE symbol = ? AND timestamp BETWEEN ? AND ?";
        try (Connection connection = DatabaseManager.getConnection(); PreparedStatement statement = connection.prepareStatement(query)) {
            statement.setString(1, stockTicker);
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

    public static List<StockTicker> getCompanyData(Collection<String> stockTickers, LocalDate fromDate, LocalDate toDate) throws SQLException {
        final String query = "SELECT symbol, close_price, low_price, high_price, volume, timestamp FROM StockTicker WHERE symbol = ? AND timestamp BETWEEN ? AND ? ORDER BY timestamp ASC";
        List<StockTicker> ret = new ArrayList<>();
        for (String ticker : stockTickers) {
            try (Connection connection = DatabaseManager.getConnection(); PreparedStatement statement = connection.prepareStatement(query)) {
                // Sqlite jdbc driver does not have setArray implemented (2019-10-23)
                // https://github.com/xerial/sqlite-jdbc/issues/415
                statement.setString(1, ticker);
                statement.setDate(2, java.sql.Date.valueOf(fromDate));
                statement.setDate(3, java.sql.Date.valueOf(toDate));
                try (ResultSet rs = statement.executeQuery()) {
                    while (rs.next()) {
                        ret.add(new StockTicker(
                              rs.getString("symbol"),
                              rs.getDouble("close_price"),
                              rs.getDouble("high_price"),
                              rs.getDouble("low_price"),
                              rs.getLong("volume"),
                              rs.getTimestamp("timestamp").toLocalDateTime())
                        );
                    }
                }
            }
        }
        if (ret.isEmpty()) {
            LOG.warn("Request data {}, from {} to {} - empty results", stockTickers, fromDate, toDate);
        }

        return ret.stream().sorted((l, r) -> l.timestamp.isEqual(r.timestamp) ? 0 : (l.timestamp.isBefore(r.timestamp) ? 1 : -1)).collect(Collectors.toList());
    }
}
