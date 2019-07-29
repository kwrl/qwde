package qwde.pystock;

import java.math.BigDecimal;
import java.time.LocalDateTime;

import qwde.ml.LinearRegression;
import qwde.models.StockPrice;

public class PystockStockPrice implements StockPrice {
  private final BigDecimal highPrice;
  private final BigDecimal lowPrice;
  private final String company;
  private final LocalDateTime timestamp;
  LinearRegression test;

  public PystockStockPrice(BigDecimal highPrice, BigDecimal lowPrice, String company, LocalDateTime timestamp) {
    super();
    this.highPrice = highPrice;
    this.lowPrice = lowPrice;
    this.company = company;
    this.timestamp = timestamp;
  }

  public BigDecimal getHighPrice() {
    return highPrice;
  }

  public BigDecimal getLowPrice() {
    return lowPrice;
  }

  @Override
  public BigDecimal getPrice() {
    return (highPrice.add(lowPrice)).divide(new BigDecimal(2.0));
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
