package qwdepystock.models;

import java.util.List;
import java.time.LocalDateTime;

public class CompanyStockData {
  public final String companyName;
  public final List<Double> prices;
  public final List<LocalDateTime> timestamps;

  public CompanyStockData(String companyName, List<Double> prices, List<LocalDateTime> timestamps) {
    this.companyName = companyName;
    this.prices = prices;
    this.timestamps = timestamps;
  }

  @Override
  public String toString() {
    String fromDate = timestamps.isEmpty() ? "empty" : timestamps.get(0).toString();
    String toDate = timestamps.size() < 1 ? fromDate : timestamps.get(1).toString();
    return super.toString() + String.format("%s <%s:%s>", companyName, fromDate, toDate);
  }
}
