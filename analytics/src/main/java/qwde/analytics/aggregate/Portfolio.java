package qwde.analytics.aggregate;

import one.util.streamex.StreamEx;
import qwde.analytics.IndexEstimator;
import qwde.dataprovider.models.CompanyStockData;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class Portfolio {
    public final List<StockOverview> stockOverviews;
    public final Map<String, Map<String, Double>> covariances;
    public final double jensensAlpha;

    private static final class CovarianceTuple {
        final String symbolLeft;
        final String symbolRight;
        final Double covariance;

        private CovarianceTuple(String symbolLeft, String symbolRight, Double covariance) {
            this.symbolLeft = symbolLeft;
            this.symbolRight = symbolRight;
            this.covariance = covariance;
        }
    }

    public Portfolio(List<CompanyStockData> stockData) {
        this.stockOverviews = stockData.stream().map(sd -> {
            double variance = Variance.variance(sd.closePrices);
            double standardDeviation = StandardDeviation.standardDeviation(sd.closePrices);
            return new StockOverview(sd.companyName, variance, standardDeviation);
        }).collect(Collectors.toList());

        this.covariances = StreamEx.of(stockData)
              .cross(stockData)
              .map(e -> new CovarianceTuple(e.getKey().companyName, e.getValue().companyName, Covariance.covariance(e.getKey().closePrices, e.getValue().closePrices)))
              .collect(Collectors.groupingBy(c -> c.symbolLeft,
                    Collectors.groupingBy(c -> c.symbolRight, Collectors.collectingAndThen(Collectors.toList(), l -> l.get(0).covariance))));

        double returnOnPotentialInvestment = stockData.stream().mapToDouble(s -> s.closePrices.get(s.closePrices.size() - 1) - s.closePrices.get(0)).average().getAsDouble();
        // TODO: beta. need market variance, mean and stddev
        this.jensensAlpha = JensensAlpha.jensensAlpha(returnOnPotentialInvestment, IndexEstimator.expectedIncrease(stockData.get(0).closePrices.size()), IndexEstimator.AVERAGE_RISK_FREE_GROWTH, 1.0);
    }
}
