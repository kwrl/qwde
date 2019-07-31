package qwde.servlets;

import java.io.IOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.plotly.LinePlotRenderer;
import qwde.pystock.PystockStockPriceReader;

public class SharkToothServlet {
  private static Logger logger = LoggerFactory.getLogger(SharkToothServlet.class);

  public static String doGet() throws IOException {
    PystockStockPriceReader pyReader = PystockStockPriceReader.FromDate("20170102.tar.gz");

    Double[] testdataY = pyReader.read().stream().map(val -> val.getPrice().doubleValue()).toArray(Double[]::new);
    return LinePlotRenderer.renderFrom1d(testdataY);
  }
}

