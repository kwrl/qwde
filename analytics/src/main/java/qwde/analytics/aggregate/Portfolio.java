package qwde.analytics.aggregate;

import qwde.dataprovider.models.CompanyStockData;

import java.util.List;
import java.util.stream.Collectors;

public class Portfolio {
    public final List<StockOverview> stockOverviews;

    public Portfolio(List<CompanyStockData> stockData) {
        this.stockOverviews = stockData.stream().map(sd -> {
            double variance = Variance.variance(sd.closePrices);
            double standardDeviation = StandardDeviation.standardDeviation(sd.closePrices);
            return new StockOverview(sd.companyName, variance, standardDeviation);
        }).collect(Collectors.toList());
    }
}
