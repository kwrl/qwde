package qwde.web.servlets;

import com.google.common.collect.ImmutableMap;
import io.micronaut.core.convert.format.Format;
import io.micronaut.http.HttpMessage;
import io.micronaut.http.HttpResponse;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.reactivex.Single;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.dataprovider.db.StockDB;
import qwde.dataprovider.models.CompanyStockData;
import qwde.web.plotly.PageRenderer;

import java.sql.SQLException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Controller(value = "/portfolio2", produces = MediaType.APPLICATION_JSON)
public final class Portfolio2 {
    private static final Logger LOG = LoggerFactory.getLogger(Portfolio2.class);
    private static final int SMOOTHING_PERIOD = 20;

    @Get("/{tickers}/{fromDate}")
    public HttpResponse<?> portfolio(String tickers, @Format("yyyyMMdd") LocalDate fromDate) throws SQLException {
        List<String> stockTickers = Arrays.asList(tickers.split(","));
        LOG.debug("Doing render with {}, {}", tickers, fromDate);

        List<CompanyStockData> companyStockData = new ArrayList<>();

        for (String stock : stockTickers) {
            CompanyStockData stockData = StockDB.getCompanyData(stock.toUpperCase(), fromDate.minusDays(SMOOTHING_PERIOD), fromDate);

            if (stockData.closePrices.isEmpty()) {
                return HttpResponse.badRequest(String.format("No data found for %s Are you sure the ticker and date were correct?", stock));
            }

            companyStockData.add(stockData);
        }

        if (companyStockData.isEmpty()) {
            return HttpResponse.badRequest("No stock data found. Are tickers correct?");
        }

        qwde.analytics.aggregate.Portfolio portfolio = new qwde.analytics.aggregate.Portfolio(companyStockData);

        return HttpResponse.ok(portfolio);
    }
}
