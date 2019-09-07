package qwde.web.servlets;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.*;

import io.micronaut.core.convert.format.Format;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.QueryValue;
import io.reactivex.Single;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import qwde.analytics.aggregate.MovingAverage;
import qwde.analytics.db.StockDB;
import qwde.dataprovider.models.CompanyStockData;
import qwde.web.plotly.FigureTemplate;
import qwde.web.plotly.LinePlotRenderer;
import qwde.web.plotly.PageRenderer;
import tech.tablesaw.plotly.traces.ScatterTrace;

@Controller(value = "/sma", produces = MediaType.TEXT_HTML)
public final class SimpleMovingAverage {
  private static final Logger LOG = LoggerFactory.getLogger(SimpleMovingAverage.class);
  public static final DateTimeFormatter DATETIMEFORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");

  @Get("/")
  public Single<String> doGet(@QueryValue("ticker") String ticker
                              ) {
                              //@QueryValue @Format("yyyyMMdd") LocalDate fromDate,
                              //@QueryValue @Format("yyyyMMdd") Optional<LocalDate> toDate) {
    //return Single.just("<h1>you did it!</h1><p>" + ticker + fromDate + toDate + "</p>");
    return Single.just("<h1>you did it!</h1><p>" + ticker + "</p>");
/*
    try {
      if (!urlParams.containsKey("ticker") || urlParams.get("ticker").size() != 1) {
        return "Please include ?ticker=<tickername> in the URL";
      }
      if (!urlParams.containsKey("fromdate") || urlParams.get("fromdate").size() != 1) {
        return "Please include ?fromdate=<yyyyMMdd> in the URL. Dates have to be somewhere in the range";
      }

      LocalDate fromDate = LocalDate.parse(urlParams.get("fromdate").get(0), DATETIMEFORMATTER);
      LocalDate toDate = fromDate.plusDays(10);

      if (urlParams.containsKey("todate") && urlParams.get("todate").size() == 1) {
        toDate = LocalDate.parse(urlParams.get("todate").get(0), DATETIMEFORMATTER);
      }

      String ticker = urlParams.get("ticker").get(0).toUpperCase();
      LOG.debug("Doing render with {}, {}, {}", ticker, fromDate, toDate);

      CompanyStockData stockData;
      try {
        stockData = StockDB.getCompanyData(ticker, fromDate, toDate);
      } catch (SQLException exception) {
        return justGiveTheUserAStackTrace(exception);
      }

      if (stockData.closePrices.isEmpty()) {
        return "No data found. Are you sure the ticker and date were correct?";
      }

      List<ScatterTrace> traces = new ArrayList<>();
      traces.addAll(getAverages(stockData.closePrices));
      traces.add(LinePlotRenderer.genScatterPlot(stockData.closePrices, "price"));

      return PageRenderer.renderFigure("Price averages", Collections.singletonList(
              new FigureTemplate(LinePlotRenderer.scatterPlot(traces, ScatterTrace.class, ticker, "day", "closing price"), "Stock closing prices and Simple Moving Averages (SMA)",
                      "$$\n\\left\\{\n\\begin{aligned}\ny_{c,d} &= close(d)\\\\\ny_{sma} &= avg(y_{c, d-10x}\\dots{}y_{c, d})\n\\end{aligned}\n\\right.$$")
      ));
    } catch (Exception exception) {
      return justGiveTheUserAStackTrace(exception);
    }
*/
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

  private static String justGiveTheUserAStackTrace(Exception exception) {
    StringWriter sw = new StringWriter();
    PrintWriter pw = new PrintWriter(sw);
    exception.printStackTrace(pw);
    return sw.toString();
  }
}
