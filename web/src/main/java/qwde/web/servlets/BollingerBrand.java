package qwde.web.servlets;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Streams;
import qwde.analytics.aggregate.MovingAverage;
import qwde.analytics.aggregate.StandardDeviation;
import qwde.analytics.db.StockDB;
import qwde.dataprovider.models.CompanyStockData;
import qwde.web.plotly.FigureTemplate;
import qwde.web.plotly.LinePlotRenderer;
import qwde.web.plotly.PageRenderer;
import tech.tablesaw.plotly.traces.ScatterTrace;

public final class BollingerBrand {
  private static Logger logger = LoggerFactory.getLogger(SimpleMovingAverage.class);
  public static final DateTimeFormatter DATETIMEFORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");

  private BollingerBrand() {
  }

  public static String doGet(Map<String, List<String>> urlParams) {
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
      logger.debug("Doing render with {}, {}, {}", ticker, fromDate, toDate);

      CompanyStockData stockData;
      try {
        stockData = StockDB.getCompanyData(ticker, fromDate, toDate);
      } catch (SQLException exception) {
        return justGiveTheUserAStackTrace(exception);
      }

      if (stockData.prices.isEmpty()) {
        return "No data found. Are you sure the ticker and date were correct?";
      }

      List<ScatterTrace> traces = new ArrayList<>();
      traces.add(LinePlotRenderer.genScatterPlot(stockData.prices, "closing prices"));
      traces.addAll(getBollingerBrand(stockData.prices, 20));

      return PageRenderer.renderFigure("Price averages", Arrays.asList(
        new FigureTemplate(LinePlotRenderer.scatterPlot(traces, ScatterTrace.class, ticker, "day", "closing price"), "Stock closing prices and Simple Moving Averages (SMA)",
          "$$\n\\left\\{\n\\begin{aligned}\ny_{c,d} &= close(d)\\\\\ny_{sma} &= avg(y_{c, d-10x}\\dots{}y_{c, d})\n\\end{aligned}\n\\right.$$")
      ));
    } catch (Exception exception) {
      return justGiveTheUserAStackTrace(exception);
    }
  }

  private static List<ScatterTrace> getBollingerBrand(List<Double> data, int windowSize) {
    Double[] dataAsArray = new Double[data.size()];
    dataAsArray = data.toArray(dataAsArray);
    List<Double> standardDeviationUpper = StandardDeviation.standardDeviaton(data, windowSize);
    List<Double> mean = Arrays.asList(MovingAverage.simpleMovingAverage(dataAsArray, windowSize));
    List<Double> standardDeviationLower = Streams.zip(standardDeviationUpper.stream(), mean.stream(), (std, m) -> std - m).collect(Collectors.toList());

    return Arrays.asList(
        LinePlotRenderer.genScatterPlot(standardDeviationUpper, "upper BB"),
        LinePlotRenderer.genScatterPlot(mean, "mean"),
        LinePlotRenderer.genScatterPlot(standardDeviationLower, "lower BB")
    );
  }

  private static String justGiveTheUserAStackTrace(Exception exception) {
    StringWriter sw = new StringWriter();
    PrintWriter pw = new PrintWriter(sw);
    exception.printStackTrace(pw);
    return sw.toString();
  }
}
