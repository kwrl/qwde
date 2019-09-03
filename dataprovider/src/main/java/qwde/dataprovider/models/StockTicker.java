package qwde.dataprovider.models;

import java.math.BigDecimal;
import java.time.LocalDateTime;

public class StockTicker {
  public final String symbol;
  public final BigDecimal closePrice;
  public final BigDecimal highPrice;
  public final BigDecimal lowPrice;
  public final Long volume;
  public final LocalDateTime timestamp;

  public StockTicker(String symbol, BigDecimal price, BigDecimal highPrice, BigDecimal lowPrice, Long volume, LocalDateTime timestamp) {
    this.symbol = symbol;
    this.closePrice = price;
    this.highPrice = highPrice;
    this.lowPrice = lowPrice;
    this.volume = volume;
    this.timestamp = timestamp;
  }
}
