package qwde.models;

import java.time.LocalDateTime;

public interface StockPrice {
  double getPrice();

  String getCompany();

  LocalDateTime getTimestamp();
}

