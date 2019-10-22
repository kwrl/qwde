package qwde.dataprovider.models;

import java.time.LocalDateTime;
import java.util.List;

public class CompanyStockData {
    public final String companyName;
    public final List<Double> closePrices;
    public final List<Double> highPrices;
    public final List<Double> lowPrices;
    public final List<Long> volume;
    public final List<LocalDateTime> timestamps;

    public CompanyStockData(String companyName, List<Double> closePrices, List<Double> highPrices, List<Double> lowPrices, List<Long> volume, List<LocalDateTime> timestamps) {
        this.companyName = companyName;
        this.closePrices = closePrices;
        this.highPrices = highPrices;
        this.lowPrices = lowPrices;
        this.volume = volume;
        this.timestamps = timestamps;
    }

    @Override
    public String toString() {
        String fromDate = timestamps.isEmpty() ? "empty" : timestamps.get(0).toString();
        String toDate = timestamps.isEmpty() ? fromDate : timestamps.get(1).toString();
        return super.toString() + String.format("%s <%s:%s>", companyName, fromDate, toDate);
    }
}
