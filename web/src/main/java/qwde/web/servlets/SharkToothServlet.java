package qwde.web.servlets;

import java.io.IOException;
import java.io.StringWriter;
import java.io.PrintWriter;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;
import java.sql.SQLException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import qwde.analytics.db.StockDB;
import qwde.dataprovider.models.CompanyStockData;
import qwde.dataprovider.pystock.PystockStockPriceReader;
import qwde.web.plotly.LinePlotRenderer;

public class SharkToothServlet {
  private static Logger logger = LoggerFactory.getLogger(SharkToothServlet.class);
  public final static DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd");

  public static String doGet(Map<String, List<String>> urlParams) throws IOException {
    try {
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

      CompanyStockData stockData;
      try {
        stockData = StockDB.getCompanyData(ticker, fromDate, toDate);
      } catch (SQLException exception) {
        return justGiveTheUserAStackTrace_Man(exception);
      }

      if (stockData.prices.isEmpty()) {
        return "No data found. Are you sure the ticker and date were correct?";
      }

      return LinePlotRenderer.renderFrom1d(stockData.prices);
    } catch (Exception exception) {
        return justGiveTheUserAStackTrace_Man(exception);
    }
  }
  
  private static String justGiveTheUserAStackTrace_Man(Exception exception) {
      StringWriter sw = new StringWriter();
      PrintWriter pw = new PrintWriter(sw);
      exception.printStackTrace(pw);
      return sw.toString();
  }
}

