package qwde.dataprovider.models;

import java.math.BigDecimal;
import java.time.LocalDateTime;

public interface IStockTicker extends Comparable<IStockTicker> {
  BigDecimal getPrice();

  BigDecimal getHigh();

  BigDecimal getLow();

  Long getVolume();

  String getCompany();

  LocalDateTime getTimestamp();
}
