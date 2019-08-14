package qwde.servlets;

import java.io.IOException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.plotly.LinePlotRenderer;
import qwde.pystock.PystockStockPriceReader;

public class SharkToothServlet {
  private static Logger logger = LoggerFactory.getLogger(SharkToothServlet.class);
  public final static DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd");

  public static String doGet(Map<String, List<String>> urlParams) throws IOException {
    if (!urlParams.containsKey("ticker") || urlParams.get("ticker").size() != 1) {
      return "Please include ?ticker=<tickername> in the URL";
    }
    String ticker = urlParams.get("ticker").get(0).toUpperCase();
    if (!urlParams.containsKey("fromdate") || urlParams.get("fromdate").size() != 1) {
      return "Please include ?fromdate=<yyyyMMdd> in the URL. Dates have to be somewhere in the range";
    }

    LocalDate fromDate = LocalDate.parse(urlParams.get("fromdate").get(0), dateTimeFormatter);
    LocalDate toDate = fromDate.plusDays(10);

    if (urlParams.containsKey("todate") && urlParams.get("todate").size() == 1) {
      toDate = LocalDate.parse(urlParams.get("todate").get(0), dateTimeFormatter);
    }

    logger.debug("Doing render with {}, {}, {}", ticker, fromDate, toDate);

    PystockStockPriceReader pyReader = new PystockStockPriceReader(fromDate, toDate);

    Double[] testdataY = pyReader.read().stream()
      .filter(val -> val.getCompany().equals(ticker))
      .map(val -> val.getPrice().doubleValue()).toArray(Double[]::new);
    if (testdataY.length == 0) {
      return "No data found. Are you sure the ticker and date were correct?";
    }

    return LinePlotRenderer.renderFrom1d(testdataY);
  }
}

