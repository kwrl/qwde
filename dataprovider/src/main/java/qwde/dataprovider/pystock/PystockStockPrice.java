package qwde.dataprovider.pystock;

import java.math.BigDecimal;
import java.time.LocalDateTime;

import qwde.dataprovider.models.StockPrice;
import qwde.dataprovider.util.DateUtil;

public class PystockStockPrice implements StockPrice {
  private final BigDecimal highPrice;
  private final BigDecimal lowPrice;
  private final BigDecimal closePrice;
  private final Long volume;
  private final String company;
  private final LocalDateTime timestamp;

  PystockStockPrice(BigDecimal highPrice, BigDecimal lowPrice, BigDecimal closePrice, Long volume, String company, LocalDateTime timestamp) {
    this.highPrice = highPrice;
    this.lowPrice = lowPrice;
    this.company = company;
    this.volume = volume;
    this.timestamp = timestamp;
    this.closePrice = closePrice;
  }

  @Override
  public BigDecimal getHigh() {
    return this.highPrice;
  }

  @Override
  public BigDecimal getLow() {
    return this.lowPrice;
  }

  @Override
  public Long getVolume() {
    return this.volume;
  }

  @Override
  public BigDecimal getPrice() {
    return this.closePrice;
  }

  @Override
  public String getCompany() {
    return this.company;
  }

  @Override
  public LocalDateTime getTimestamp() {
    return this.timestamp;
  }

  @Override
  public String toString() {
    return this.company + " [" + this.timestamp.toString() + "]: " + getPrice();
  }

  @Override
  public int compareTo(StockPrice other) {
    if (this.company.equals(other.getCompany())) {
      return DateUtil.compareDdMmYyyy(this.timestamp, other.getTimestamp());
    } else {
      return this.company.compareTo(other.getCompany());
    }
  }

  @Override
  public boolean equals(Object other) {
    if (other instanceof StockPrice) {
      return compareTo((StockPrice) other) == 0;
    } else {
      return false;
    }
  }

  @Override
  public int hashCode() {
    return super.hashCode();
  }
}
