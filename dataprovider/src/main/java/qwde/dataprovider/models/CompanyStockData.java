package qwde.dataprovider.models;

import java.time.LocalDateTime;
import java.util.List;

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
    String toDate = timestamps.isEmpty() ? fromDate : timestamps.get(1).toString();
    return super.toString() + String.format("%s <%s:%s>", companyName, fromDate, toDate);
  }
}
