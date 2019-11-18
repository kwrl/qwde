package qwde.web.servlets;

import com.google.common.collect.ImmutableList;
import io.micronaut.core.convert.format.Format;
import io.micronaut.http.HttpResponse;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.QueryValue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.analytics.aggregate.MovingAverage;
import qwde.dataprovider.db.StockDB;
import qwde.dataprovider.models.CompanyStockData;

import java.sql.SQLException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

@Controller(value = "/sma", produces = MediaType.APPLICATION_JSON)
public final class SimpleMovingAverage {
    private static final Logger LOG = LoggerFactory.getLogger(SimpleMovingAverage.class);
    public static final DateTimeFormatter DATETIMEFORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");
    static final int WINDOW_PERIOD = 20;

    public static final class Data {
        public final Collection<Double> prices;
        public final Collection<Collection<Double>> sma;

        private Data(Collection<Double> prices, Collection<Collection<Double>> sma) {
            this.prices = prices;
            this.sma = sma;
        }
    }

    @Get("/{ticker}/{fromDate}")
    public HttpResponse<?> doGet(String ticker, @Format("yyyyMMdd") LocalDate fromDate, @QueryValue @Format("yyyyMMdd") Optional<LocalDate> toDate) throws SQLException {
        if (toDate.isPresent() && toDate.get().isBefore(fromDate)) {
            return HttpResponse.badRequest("toDate is before fromDate!");
        }

        LocalDate endDate = toDate.orElse(fromDate.plusDays(WINDOW_PERIOD));
        LOG.debug("Doing render with {}, {}, {}", ticker, fromDate, toDate);

        CompanyStockData stockData;
        stockData = StockDB.getCompanyData(ticker.toUpperCase(), fromDate, endDate);
        if (stockData.closePrices.isEmpty()) {
            return HttpResponse.badRequest("No data found. Are you sure the ticker and date were correct?");
        }

        return HttpResponse.ok(new Data(stockData.closePrices, getAverages(stockData.closePrices)));
    }

    private static Collection<Collection<Double>> getAverages(List<Double> data) {
        Double[] dataAsArray = new Double[data.size()];
        dataAsArray = data.toArray(dataAsArray);
        List<Collection<Double>> ret = new ArrayList<>();
        for (Double[] d : MovingAverage.simpleMovingAverages(dataAsArray, 100, 10)) {
            ret.add(ImmutableList.copyOf(d));
        }

        return ret;
    }
}
