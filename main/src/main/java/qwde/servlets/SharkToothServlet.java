package qwde.servlets;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Enumeration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.plotly.LinePlotRenderer;
import qwde.pystock.PystockStockPriceReader;

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

