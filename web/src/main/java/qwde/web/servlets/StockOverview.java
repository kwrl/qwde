package qwde.web.servlets;

import com.google.common.collect.Streams;
import io.micronaut.core.convert.format.Format;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.QueryValue;
import io.reactivex.Single;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.analytics.aggregate.MovingAverage;
import qwde.analytics.aggregate.StandardDeviation;
import qwde.analytics.aggregate.Variance;
import qwde.analytics.db.StockDB;
import qwde.dataprovider.models.CompanyStockData;
import qwde.web.models.StockStatistics;
import qwde.web.plotly.FigureTemplate;
import qwde.web.plotly.LinePlotRenderer;
import qwde.web.plotly.PageRenderer;
import tech.tablesaw.plotly.traces.ScatterTrace;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Controller(value = "/statistics", produces = MediaType.TEXT_HTML)
public final class StockOverview {
  private static final Logger LOG = LoggerFactory.getLogger(StockOverview.class);
  public static final DateTimeFormatter DATETIMEFORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");
  private static final int SMOOTHING_PERIOD = 20;

  @Get("/{ticker}/{fromDate}")
  public Single<String> doGet(String ticker, @Format("yyyyMMdd") LocalDate fromDate) {
    LOG.debug("Doing render with {}, {}, {}", ticker, fromDate);

    CompanyStockData stockData;
    try {
      stockData = StockDB.getCompanyData(ticker.toUpperCase(), fromDate.minusDays(SMOOTHING_PERIOD), LocalDate.now());
    } catch (SQLException exception) {
      return Single.just(justGiveTheUserAStackTrace(exception));
    }

    if (stockData.closePrices.isEmpty()) {
      return Single.just("No data found. Are you sure the ticker and date were correct?");
    }

    double variance = Variance.variance(stockData.closePrices);
    double standardDeviation = StandardDeviation.standardDeviation(stockData.closePrices);
    StockStatistics stockStatistics = new StockStatistics(ticker, variance, standardDeviation);

    return Single.just(PageRenderer.renderStatistics("Statistics - " + ticker, stockStatistics));
  }

  private static String justGiveTheUserAStackTrace(Exception exception) {
    StringWriter sw = new StringWriter();
    PrintWriter pw = new PrintWriter(sw);
    exception.printStackTrace(pw);
    return sw.toString();
  }
}

