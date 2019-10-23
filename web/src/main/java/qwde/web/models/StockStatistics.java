package qwde.web.models;

public class StockStatistics {
    public final String symbol;
    public final double variance;
    public final double standardDeviation;

    public String getSymbol() {
        return symbol;
    }

    public double getVariance() {
        return variance;
    }

    public double getStandardDeviation() {
        return standardDeviation;
    }

    public StockStatistics(String symbol, double variance, double standardDeviation) {
        this.symbol = symbol;
        this.variance = variance;
        this.standardDeviation = standardDeviation;
    }
}
