package qwde.analytics;

import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class IndexEstimator {
    private IndexEstimator() {

    }

    public static final double AVERAGE_INDEX_ANNUAL_GROWTH = 5.0;
    public static final double AVERAGE_RISK_FREE_GROWTH = 2.0;

    public static double expectedIncrease(LocalDate from, LocalDate to) {
        return AVERAGE_INDEX_ANNUAL_GROWTH * (ChronoUnit.YEARS.between(from, to) + (ChronoUnit.DAYS.between(from, to) / 365.0));
    }

    public static double expectedIncrease(int numberOfDays) {
        return AVERAGE_INDEX_ANNUAL_GROWTH * (numberOfDays / 365.0);
    }
}
