package qwde.trading.aggregate;

import org.apache.commons.lang3.NotImplementedException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Class to do one-off jobs such as mean, stddev, etc, for all of
 */
public final class AggregatesComputer {
    private static final Logger LOG = LoggerFactory.getLogger(AggregatesComputer.class);

    private AggregatesComputer() {
    }

    public static void main() {
        LOG.info("Computing averages for each year across all indices");

        throw new NotImplementedException("aaa");
        //StockDB.getCompanyData(ticker.toUpperCase(), fromDate.minusDays(SMOOTHING_PERIOD), endDate);
    }
}
