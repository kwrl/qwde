package qwde.dataprovider.models;

import java.math.BigDecimal;
import java.time.LocalDateTime;

public interface StockPrice extends Comparable<StockPrice> {
  BigDecimal getPrice();

  String getCompany();

  LocalDateTime getTimestamp();
}
