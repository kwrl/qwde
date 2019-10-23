package qwde.analytics.aggregate;

public class StockOverview {
    public final String symbol;
    public final double variance;
    public final double standardDeviation;
    //public final double covariance;

    public StockOverview(String symbol, double variance, double standardDeviation) {
        this.symbol = symbol;
        this.variance = variance;
        this.standardDeviation = standardDeviation;
    }

    public String getSymbol() {
        return symbol;
    }

    public double getVariance() {
        return variance;
    }

    public double getStandardDeviation() {
        return standardDeviation;
    }

}
