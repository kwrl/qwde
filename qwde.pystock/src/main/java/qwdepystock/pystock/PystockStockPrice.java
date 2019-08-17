package qwdepystock.pystock;

import java.math.BigDecimal;
import java.time.LocalDateTime;

import qwdepystock.models.StockPrice;

public class PystockStockPrice implements StockPrice {
  private final BigDecimal highPrice;
  private final BigDecimal lowPrice;
  private final BigDecimal closePrice;
  private final String company;
  private final LocalDateTime timestamp;

  public PystockStockPrice(BigDecimal highPrice, BigDecimal lowPrice, BigDecimal closePrice, String company, LocalDateTime timestamp) {
    super();
    this.highPrice = highPrice;
    this.lowPrice = lowPrice;
    this.company = company;
    this.timestamp = timestamp;
    this.closePrice = closePrice;
  }

  public BigDecimal getHighPrice() {
    return highPrice;
  }

  public BigDecimal getLowPrice() {
    return lowPrice;
  }

  @Override
  public BigDecimal getPrice() {
    return this.closePrice;
  }

  @Override
  public String getCompany() {
    return company;
  }

  @Override
  public LocalDateTime getTimestamp() {
    return timestamp;
  }

  @Override
  public String toString() {
    return company + " [" + timestamp.toString() + "]: " + getPrice();
  }

}
