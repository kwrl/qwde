package qwde.analytics.aggregate;

public final class JensensAlpha {
    private JensensAlpha() {
    }

    public static double jensensAlpha(double returnOnInvestment, double returnOnMarketIndex, double returnRiskFree, double beta) {
        return returnOnInvestment - (returnRiskFree + beta * (returnOnMarketIndex - returnRiskFree));
    }
}
