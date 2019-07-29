package qwde.servlets;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.Enumeration;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import qwde.plotly.LinePlotRenderer;
import qwde.pystock.PystockStockPriceReader;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.NumberColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;
import tech.tablesaw.io.html.HtmlWriteOptions;
import tech.tablesaw.plotly.Plot;
import tech.tablesaw.plotly.api.BubblePlot;
import tech.tablesaw.plotly.components.Figure;
import tech.tablesaw.plotly.components.Layout;
import tech.tablesaw.plotly.components.Marker;
import tech.tablesaw.plotly.components.Page;
import tech.tablesaw.plotly.traces.ScatterTrace;
import tech.tablesaw.selection.Selection;


import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@WebServlet("/sharktooth")
public class SharkToothServlet extends HttpServlet {
  private static Logger logger = LoggerFactory.getLogger(SharkToothServlet.class);
  private static final long serialVersionUID = 1L;

  protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

    Enumeration<String> parameterNames = request.getParameterNames();

    while (parameterNames.hasMoreElements()) {

      String paramName = parameterNames.nextElement();
      System.out.println(paramName);
      String[] paramValues = request.getParameterValues(paramName);
      for (int i = 0; i < paramValues.length; i++) {
        System.out.println(paramValues[i]);
      }
    }

    PystockStockPriceReader pyReader = PystockStockPriceReader.FromDate("20170102.tar.gz");

    Double[] testdataY = pyReader.read().stream().map(val -> val.getPrice().doubleValue()).toArray(Double[]::new);
    LinePlotRenderer.renderFrom1d(testdataY, response.getOutputStream());

    response.setContentType("text/html");
    response.setStatus(HttpServletResponse.SC_OK);
    //response.getWriter().println("<h1>You can write HTML like this :)</h1>");
  }
}

