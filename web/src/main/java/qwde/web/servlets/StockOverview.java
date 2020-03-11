package qwde.web.servlets;

import com.google.common.collect.ImmutableMap;
import io.micronaut.core.convert.format.Format;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.reactivex.Single;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.analytics.aggregate.StandardDeviation;
import qwde.analytics.aggregate.Variance;
import qwde.dataprovider.db.StockDB;
import qwde.dataprovider.models.CompanyStockData;
import qwde.web.models.StockStatistics;
import qwde.web.plotly.PageRenderer;

import java.sql.SQLException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

@Controller(value = "/statistics", produces = MediaType.TEXT_HTML)
public final class StockOverview {
    private static final Logger LOG = LoggerFactory.getLogger(StockOverview.class);
    public static final DateTimeFormatter DATETIMEFORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");
    private static final int SMOOTHING_PERIOD = 20;

    @Get("/{ticker}/{fromDate}")
    public Single<String> doGet(String ticker, @Format("yyyyMMdd") LocalDate fromDate) throws SQLException {
        LOG.debug("Doing render with {}, {}", ticker, fromDate);

        CompanyStockData stockData;
        stockData = StockDB.getCompanyData(ticker.toUpperCase(), fromDate.minusDays(SMOOTHING_PERIOD), LocalDate.now());

        if (stockData.closePrices.isEmpty()) {
            return Single.just("No data found. Are you sure the ticker and date were correct?");
        }

        double variance = Variance.variance(stockData.closePrices);
        double standardDeviation = StandardDeviation.standardDeviation(stockData.closePrices);
        StockStatistics stockStatistics = new StockStatistics(ticker, variance, standardDeviation);

        return Single.just(PageRenderer.renderPage("statisticspage.ftl", ImmutableMap.of("stat", stockStatistics, "pageTitle", "StockStatistics")));
    }
}

