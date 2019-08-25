package qwde.web.servlets;

import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;

import qwde.web.plotly.FigureTemplate;
import qwde.web.plotly.LinePlotRenderer;
import qwde.web.plotly.PageRenderer;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.plotly.traces.ScatterTrace;
import tech.tablesaw.plotly.traces.Trace;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class IndexServlet {
  public static String doGet(Map<String, List<String>> params) {
	List<Double> data = DoubleStream
		    .generate(ThreadLocalRandom.current()::nextDouble)
		    .limit(100)
		    .boxed()
		    .collect(Collectors.toList());

	List<Double> data2 = DoubleStream
		    .generate(ThreadLocalRandom.current()::nextDouble)
		    .limit(100)
		    .boxed()
		    .collect(Collectors.toList());

	return PageRenderer.renderFigure("Random stock data", Arrays.asList(
			new FigureTemplate(LinePlotRenderer.scatterPlot(Collections.singleton(LinePlotRenderer.genScatterPlot(data, "rand")), ScatterTrace.class, "A 2d scatterplot chart", "x", "y"), "Randomized stock data", "$$x_{0..n} = rand()$$Refreshing the page refreshes the data, too."),
			new FigureTemplate(LinePlotRenderer.scatterPlot(Collections.singleton(LinePlotRenderer.genScatterPlot(data2, "rand")), ScatterTrace.class, "Another 2d scatterplot chart", "x", "y"), "Randomized stock data v2", "$$x_{0..n} = rand()$$Refreshing the page refreshes the data, too.")
			));
  } 
}
