package qwdepystock.models;

import java.math.BigDecimal;
import java.time.LocalDateTime;

public class StockTicker {
  public final String symbol;
  public final BigDecimal price;
  public final LocalDateTime timestamp;

  public StockTicker(String symbol, BigDecimal price, LocalDateTime timestamp) {
    this.symbol = symbol;
    this.price = price;
    this.timestamp = timestamp;
  }
}
