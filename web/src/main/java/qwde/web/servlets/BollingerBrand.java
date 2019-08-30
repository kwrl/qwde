package qwde.web.servlets;

import com.google.common.collect.Streams;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.analytics.aggregate.MovingAverage;
import qwde.analytics.aggregate.StandardDeviation;
import qwde.analytics.db.StockDB;
import qwde.dataprovider.models.CompanyStockData;
import qwde.web.plotly.FigureTemplate;
import qwde.web.plotly.LinePlotRenderer;
import qwde.web.plotly.PageRenderer;
import tech.tablesaw.plotly.traces.ScatterTrace;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

public final class BollingerBrand {
  private static Logger logger = LoggerFactory.getLogger(SimpleMovingAverage.class);
  public static final DateTimeFormatter DATETIMEFORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");
  private static final int smoothingPeriod = 20;

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
        stockData = StockDB.getCompanyData(ticker, fromDate.minusDays(20), toDate);
      } catch (SQLException exception) {
        return justGiveTheUserAStackTrace(exception);
      }

      if (stockData.prices.isEmpty()) {
        return "No data found. Are you sure the ticker and date were correct?";
      }

      List<ScatterTrace> traces = new ArrayList<>();
      traces.add(LinePlotRenderer.genScatterPlot(stockData.prices.subList(smoothingPeriod, stockData.prices.size()), "price"));
      traces.addAll(getBollingerBrand(stockData.prices, 20));

      return PageRenderer.renderFigure("Bollinger Brand", Collections.singletonList(
              new FigureTemplate(LinePlotRenderer.scatterPlot(traces, ScatterTrace.class, ticker, "day", "closing price"), "Stock closing prices and BollingerBrands, windows size = 20",
                      "$$\n\\left\\{\n\\begin{aligned}\nBOLU&=MA(TP,n)+m*\\sigma{}\\\\\nBOLD&=MA(TP,n)-m*\\sigma{}\n\\end{aligned}\n\\right.\n\\\\\n\\\\\\\n\\textbf{where}\\\\\n"
                      + "\\begin{aligned}\nBOLU &= \\text{Upper Bollinger Band}\\\\\nBOLD&=\\text{Lower Bollinger Band}\\\\\nMA&=\\text{Moving average}\\\\\nTP (\\text{typical price)} &= \\frac{High + Low + Close}{3n}\\\\\n"
                      + "\\text{Number of days in smoothing period} &=  20\\\\\nm&=\\text{Number of standard deviations} = 2\\\\\n\\sigma{}&=\\text{Standard Deviation over last n periods of TP}\n\\end{aligned}\n$$"
              )
      ));
    } catch (Exception exception) {
      return justGiveTheUserAStackTrace(exception);
    }
  }

  private static List<ScatterTrace> getBollingerBrand(List<Double> data, int windowSize) {
    Double[] dataAsArray = new Double[data.size() - smoothingPeriod];
    dataAsArray = data.subList(smoothingPeriod, data.size()).toArray(dataAsArray);
    List<Double> mean = Arrays.asList(MovingAverage.simpleMovingAverage(dataAsArray, windowSize));
    //final double stdDev = StandardDeviation.standardDeviation(data, windowSize);
    List<Double> stdDev = StandardDeviation.rollingStandardDeviation(data, windowSize)
            .stream()
            .skip(windowSize)
            .map(x -> x * 2)
            .collect(Collectors.toList());
    //List<Double> standardDeviationUpper = mean.stream().map(m -> m + 2 * stdDev).collect(Collectors.toList());
    //List<Double> standardDeviationLower = mean.stream().map(m -> m - 2 * stdDev).collect(Collectors.toList());
    List<Double> standardDeviationUpper = Streams.zip(mean.stream(), stdDev.stream(), Double::sum).collect(Collectors.toList());
    List<Double> standardDeviationLower = Streams.zip(mean.stream(), stdDev.stream(), (m, s) -> m - s).collect(Collectors.toList());

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
