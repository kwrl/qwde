package qwde.web.servlets;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;

import qwde.web.plotly.FigureTemplate;
import qwde.web.plotly.LinePlotRenderer;
import qwde.web.plotly.PageRenderer;
import tech.tablesaw.plotly.traces.ScatterTrace;

public final class BollingerBrand {
  private BollingerBrand() {
  }

  public static String doGet(Map<String, List<String>> params) {
    List<Double> data = DoubleStream
          .generate(ThreadLocalRandom.current()::nextDouble)
          .limit(100)
          .boxed()
          .collect(Collectors.toList());

    return PageRenderer.renderFigure("Random stock data", Arrays.asList(
        new FigureTemplate(LinePlotRenderer.scatterPlot(Collections.singleton(LinePlotRenderer.genScatterPlot(data, "rand")), ScatterTrace.class, "A 2d scatterplot chart", "x", "y"), "Randomized stock data", "$$x_{0..n} = rand()$$Refreshing the page refreshes the data, too.")
        ));
  }
}
