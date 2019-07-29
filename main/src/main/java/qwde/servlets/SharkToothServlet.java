package qwde.servlets;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.Enumeration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
      System.out.println(paramName);
      System.out.println(paramName);
      String[] paramValues = request.getParameterValues(paramName);
      for (int i = 0; i < paramValues.length; i++) {
        System.out.println(paramValues[i]);
        System.out.println(paramValues[i]);
        System.out.println(paramValues[i]);
      }
    }

    double[] testdataX = {1,2,3,4};
    double[] testdataY = {4,3,2,1};
    DoubleColumn testcolumnX = DoubleColumn.create("xcol", testdataX);
    DoubleColumn testcolumnY = DoubleColumn.create("ycol", testdataY);

    Table testtable = Table.create("test table").addColumns(testcolumnX, testcolumnY);
    NumberColumn x = testtable.nCol("xcol");
    NumberColumn y = testtable.nCol("ycol");

    Layout layout = Layout.builder()
            .title("Monthly Boston Armed Robberies Jan. 1966 - Oct. 1975")
            .build();
    ScatterTrace trace = ScatterTrace.builder(x, y)
            .mode(ScatterTrace.Mode.LINE)
            .showLegend(true)
            .build();

    Figure figuretest = new Figure(trace);

    Page page = Page.pageBuilder(figuretest, "testdiv").build();
    String output = page.asJavascript();

    try {
      Writer writer = new OutputStreamWriter(response.getOutputStream(), StandardCharsets.UTF_8);

      try {
        writer.write(output);
      } catch (IOException exception) {
          logger.error("", exception);
      } finally {
          writer.close();
      }
    } catch (IOException var18) {
      var18.printStackTrace();
    }


    ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
    testtable.write().usingOptions(HtmlWriteOptions.builder(byteArrayOutputStream).build());

    response.setContentType("text/html");
    response.setStatus(HttpServletResponse.SC_OK);
    //response.getWriter().println("<h1>New Hello Shark rawr </h1>");
    //response.getWriter().println(byteArrayOutputStream.toString());
    //response.getWriter().println(figuretest.asJavascript("testDiv"));
  }
}

