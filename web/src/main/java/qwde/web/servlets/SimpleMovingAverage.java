package qwde.web.servlets;

import com.google.common.collect.ImmutableMap;
import io.micronaut.core.convert.format.Format;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.QueryValue;
import io.reactivex.Single;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.analytics.aggregate.MovingAverage;
import qwde.dataprovider.db.StockDB;
import qwde.dataprovider.models.CompanyStockData;
import qwde.web.plotly.FigureTemplate;
import qwde.web.plotly.LinePlotRenderer;
import qwde.web.plotly.PageRenderer;
import tech.tablesaw.plotly.traces.ScatterTrace;

import java.sql.SQLException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Controller(value = "/sma", produces = MediaType.TEXT_HTML)
public final class SimpleMovingAverage {
    private static final Logger LOG = LoggerFactory.getLogger(SimpleMovingAverage.class);
    public static final DateTimeFormatter DATETIMEFORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");

    @Get("/{ticker}/{fromDate}")
    public Single<String> doGet(String ticker, @Format("yyyyMMdd") LocalDate fromDate, @QueryValue @Format("yyyyMMdd") Optional<LocalDate> toDate) throws SQLException {
        if (toDate.isPresent() && toDate.get().isBefore(fromDate)) {
            return Single.just("toDate is before fromDate!");
        }

        LocalDate endDate = toDate.orElse(fromDate.plusDays(20));
        LOG.debug("Doing render with {}, {}, {}", ticker, fromDate, toDate);

        CompanyStockData stockData;
        stockData = StockDB.getCompanyData(ticker.toUpperCase(), fromDate, endDate);
        if (stockData.closePrices.isEmpty()) {
            return Single.just("No data found. Are you sure the ticker and date were correct?");
        }

        List<ScatterTrace> traces = new ArrayList<>();
        traces.addAll(getAverages(stockData.closePrices));
        traces.add(LinePlotRenderer.genScatterPlot(stockData.closePrices, "price"));

        return Single.just(PageRenderer.renderPage("graphpage.ftl", ImmutableMap.of("pageTitle", "Price averages", "figures", Collections.singletonList(
                new FigureTemplate(LinePlotRenderer.scatterPlot(traces, ScatterTrace.class, ticker, "day", "closing price"), "Stock closing prices and Simple Moving Averages (SMA)",
                        "$$\n\\left\\{\n\\begin{aligned}\ny_{c,d} &= close(d)\\\\\ny_{sma} &= avg(y_{c, d-10x}\\dots{}y_{c, d})\n\\end{aligned}\n\\right.$$")
        ))));
    }

    private static List<ScatterTrace> getAverages(List<Double> data) {
        Double[] dataAsArray = new Double[data.size()];
        dataAsArray = data.toArray(dataAsArray);
        int step = 10;
        List<ScatterTrace> ret = new ArrayList<>();
        for (Double[] d : MovingAverage.simpleMovingAverages(dataAsArray, 100, 10)) {
            ret.add(LinePlotRenderer.genScatterPlot(Arrays.asList(d), String.format("%d", step)));
            step += 10;
        }

        return ret;
    }
}
