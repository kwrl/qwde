package qwde.models;

import java.math.BigDecimal;
import java.time.LocalDateTime;

public interface StockPrice {
  BigDecimal getPrice();

  String getCompany();

  LocalDateTime getTimestamp();
}

