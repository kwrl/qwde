package qwde.web.servlets;

import com.google.common.collect.Streams;
import io.micronaut.core.convert.format.Format;
import io.micronaut.http.HttpResponse;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.QueryValue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.analytics.aggregate.MovingAverage;
import qwde.analytics.aggregate.StandardDeviation;
import qwde.dataprovider.db.StockDB;
import qwde.dataprovider.models.CompanyStockData;

import java.sql.SQLException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Controller(value = "/bb", produces = MediaType.APPLICATION_JSON)
public final class BollingerBrand {
    private static final Logger LOG = LoggerFactory.getLogger(BollingerBrand.class);
    private static final int SMOOTHING_PERIOD = 20;
    private static final int DEFAULT_VIEW_DAYS = 20;

    @Get("/{ticker}/{fromDate}")
    public HttpResponse<?> doGet(String ticker, @Format("yyyyMMdd") LocalDate fromDate, @QueryValue @Format("yyyyMMdd") Optional<LocalDate> toDate) throws SQLException {
        if (toDate.isPresent() && toDate.get().isBefore(fromDate)) {
            return HttpResponse.badRequest("toDate is before fromDate!");
        }

        LocalDate endDate = toDate.orElse(fromDate.plusDays(DEFAULT_VIEW_DAYS));
        LOG.debug("Doing render with {}, {}, {}", ticker, fromDate, toDate);

        CompanyStockData stockData = StockDB.getCompanyData(ticker.toUpperCase(), fromDate.minusDays(SMOOTHING_PERIOD), endDate);

        if (stockData.closePrices.isEmpty()) {
            return HttpResponse.badRequest("No data found. Are you sure the ticker and date were correct?");
        }

        return HttpResponse.ok(getBollingerBrand(stockData, SMOOTHING_PERIOD));
    }

    private static final class Data {
        final Collection<Double> lowerBand;
        public final Collection<Double> mean;
        final Collection<Double> highBand;
        final Collection<Double> price;

        private Data(Collection<Double> lowerBand, Collection<Double> mean, Collection<Double> highBand, Collection<Double> price) {
            this.lowerBand = lowerBand;
            this.mean = mean;
            this.highBand = highBand;
            this.price = price;
        }
    }

    private static Data getBollingerBrand(CompanyStockData stockData, int windowSize) {
        List<Double> data = new ArrayList<>();
        for (int i = 0; i < stockData.closePrices.size(); i++) {
            data.add((stockData.closePrices.get(i) + stockData.highPrices.get(i) + stockData.lowPrices.get(i)) / 3.0);
        }
        Double[] dataAsArray = new Double[data.size()];
        dataAsArray = data.toArray(dataAsArray);
        List<Double> mean = Arrays.asList(MovingAverage.simpleMovingAverage(dataAsArray, windowSize)).subList(SMOOTHING_PERIOD, data.size());
        List<Double> stdDev = StandardDeviation.rollingStandardDeviation(data, windowSize)
                .stream()
                .skip(windowSize)
                .map(x -> x * 2)
                .collect(Collectors.toList());
        List<Double> standardDeviationUpper = Streams.zip(mean.stream(), stdDev.stream(), Double::sum).collect(Collectors.toList());
        List<Double> standardDeviationLower = Streams.zip(mean.stream(), stdDev.stream(), (m, s) -> m - s).collect(Collectors.toList());

        return new Data(standardDeviationLower, mean, standardDeviationUpper, data.subList(SMOOTHING_PERIOD, data.size()));
    }
}
